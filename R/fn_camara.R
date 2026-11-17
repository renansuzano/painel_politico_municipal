# fn_camara.R — funções do módulo 2 (Câmara Municipal) ----------------------
# Usadas por 02_camara_vereadores.R. Recebem e devolvem data frames.
# Depende de fn_prefeitos.R (normaliza_nome, mesma_pessoa) e do 00_setup.R
# (normaliza_sigla, classificacao).

resultados_eleitos <- c("eleito", "eleito por qp", "eleito por media")

# 1. Vereadores eleitos nas eleições ordinárias ------------------------------
prepara_vereadores <- function(ver) {
  ver |>
    distinct() |>
    filter(startsWith(tipo_eleicao, "eleicao ordinaria"),
           resultado %in% resultados_eleitos) |>
    transmute(
      id_municipio,
      ano_camara = as.integer(ano),
      sigla_norm = normaliza_sigla(sigla_partido),
      titulo     = na_if(trimws(titulo_eleitoral_candidato), ""),
      nome_norm  = normaliza_nome(nome_candidato)
    ) |>
    distinct()
}

# 2. Cadeiras por partido em cada legislatura ---------------------------------
cadeiras_por_partido <- function(vereadores) {
  vereadores |>
    count(id_municipio, ano_camara, sigla_norm, name = "cadeiras") |>
    left_join(select(classificacao, sigla_tse, media_ideologia, bloco),
              by = c("sigla_norm" = "sigla_tse"))
}

# 3. Indicadores de composição por legislatura -------------------------------
composicao_camara <- function(cad) {
  blocos <- cad |>
    group_by(id_municipio, ano_camara) |>
    summarise(
      n_cadeiras            = sum(cadeiras),
      nep_cadeiras          = 1 / sum((cadeiras / sum(cadeiras))^2),
      pct_cadeiras_esquerda = sum(cadeiras[bloco %in% "esquerda"]) / sum(cadeiras),
      pct_cadeiras_centro   = sum(cadeiras[bloco %in% "centro"])   / sum(cadeiras),
      pct_cadeiras_direita  = sum(cadeiras[bloco %in% "direita"])  / sum(cadeiras),
      pct_cadeiras_sem_bloco = sum(cadeiras[is.na(bloco)])         / sum(cadeiras),
      ideologia_media_camara = weighted.mean(media_ideologia[!is.na(media_ideologia)],
                                             cadeiras[!is.na(media_ideologia)]),
      .groups = "drop"
    )

  # Bloco majoritário (empate -> "empate")
  maj <- cad |>
    filter(!is.na(bloco)) |>
    count(id_municipio, ano_camara, bloco, wt = cadeiras, name = "c") |>
    group_by(id_municipio, ano_camara) |>
    filter(c == max(c)) |>
    summarise(bloco_majoritario_camara = if (n() > 1) "empate" else bloco[1],
              .groups = "drop")

  left_join(blocos, maj, by = c("id_municipio", "ano_camara"))
}

# 4. Renovação: eleitos que já eram vereadores na legislatura anterior -------
# Compara pelo título quando a eleição anterior tem títulos; senão, pelo nome
renovacao_camara <- function(vereadores) {
  anos <- sort(unique(vereadores$ano_camara))
  ant  <- vereadores |>
    mutate(ano_camara = ano_camara + 4L) |>
    filter(ano_camara %in% anos)

  tem_titulo_ant <- ant |>
    group_by(id_municipio, ano_camara) |>
    summarise(usa_titulo = any(!is.na(titulo)), .groups = "drop")

  tit_ant  <- ant |> filter(!is.na(titulo)) |>
    distinct(id_municipio, ano_camara, titulo) |> mutate(no_ant_tit = TRUE)
  nome_ant <- ant |> distinct(id_municipio, ano_camara, nome_norm) |>
    mutate(no_ant_nome = TRUE)

  vereadores |>
    inner_join(tem_titulo_ant, by = c("id_municipio", "ano_camara")) |>
    left_join(tit_ant,  by = c("id_municipio", "ano_camara", "titulo")) |>
    left_join(nome_ant, by = c("id_municipio", "ano_camara", "nome_norm")) |>
    mutate(ja_era = if_else(usa_titulo & !is.na(titulo),
                            coalesce(no_ant_tit, FALSE),
                            coalesce(no_ant_nome, FALSE))) |>
    group_by(id_municipio, ano_camara) |>
    summarise(taxa_reeleicao_vereadores = mean(ja_era), .groups = "drop")
}

# 5. Coligações dos candidatos a prefeito ------------------------------------
# Cada partido recebe o identificador da sua coligação; partido isolado sem
# sequencial forma coligação de um partido só.
membros_coligacao <- function(part) {
  part |>
    filter(as.integer(turno) == 1L) |>
    transmute(
      id_municipio,
      ano        = as.integer(ano),
      tipo_eleicao,
      sigla_norm = normaliza_sigla(sigla),
      id_colig   = coalesce(sequencial_coligacao, paste0("ISOLADO:", normaliza_sigla(sigla)))
    ) |>
    distinct()
}

# Coligação do vencedor de cada pleito (pleitos = saída do módulo 1)
coligacao_vencedor <- function(pleitos, membros, anos_sem_dado = 2000L) {
  venc <- pleitos |>
    transmute(id_pleito, id_municipio, ano = ano_eleicao, tipo_eleicao,
              sigla_norm = normaliza_sigla(sigla_partido))

  id_venc <- venc |>
    inner_join(membros, by = c("id_municipio", "ano", "tipo_eleicao", "sigla_norm")) |>
    distinct(id_pleito, id_municipio, ano, tipo_eleicao, id_colig)

  id_venc |>
    inner_join(membros, by = c("id_municipio", "ano", "tipo_eleicao", "id_colig")) |>
    filter(!ano %in% anos_sem_dado) |>
    distinct(id_pleito, sigla_norm) |>
    left_join(select(classificacao, sigla_tse, bloco),
              by = c("sigla_norm" = "sigla_tse"))
}

# Resumo da coligação por pleito: número de partidos e bloco predominante
resumo_coligacao <- function(colig) {
  colig |>
    group_by(id_pleito) |>
    summarise(
      n_partidos_coligacao = n(),
      coligacao_prefeito   = paste(sort(sigla_norm), collapse = "/"),
      bloco_coligacao      = {
        b <- table(bloco[!is.na(bloco)])
        if (length(b) == 0) NA_character_
        else if (sum(b == max(b)) > 1) "misto"
        else names(b)[which.max(b)]
      },
      .groups = "drop"
    )
}

# 6. Painel município × ano ----------------------------------------------------
# prefeitos: saída anual do módulo 1 (cod_ibge, ano, id_pleito_prefeito, ...)
painel_camara <- function(prefeitos, comp, renov, cad, colig, colig_resumo) {
  base <- prefeitos |>
    select(cod_ibge, ano, mandato, id_pleito_prefeito,
           sigla_partido_prefeito, bloco_prefeito) |>
    mutate(
      ano_camara = ano - ((ano - 1L) %% 4L) - 1L,   # eleição da legislatura
      sigla_norm_prefeito = normaliza_sigla(sigla_partido_prefeito)
    )

  # Cadeiras do partido e da coligação do prefeito em cada ano
  apoio <- base |>
    select(cod_ibge, ano, ano_camara, id_pleito_prefeito, sigla_norm_prefeito) |>
    inner_join(select(cad, id_municipio, ano_camara, sigla_norm, cadeiras),
               by = c("cod_ibge" = "id_municipio", "ano_camara"),
               relationship = "many-to-many") |>
    left_join(transmute(colig, id_pleito, sigla_norm, na_colig = TRUE),
              by = c("id_pleito_prefeito" = "id_pleito", "sigla_norm")) |>
    left_join(transmute(colig_resumo, id_pleito, tem_colig = TRUE),
              by = c("id_pleito_prefeito" = "id_pleito")) |>
    group_by(cod_ibge, ano) |>
    summarise(
      pct_cadeiras_partido_prefeito = if (all(is.na(sigla_norm_prefeito))) NA_real_
        else sum(cadeiras[sigla_norm %in% sigla_norm_prefeito]) / sum(cadeiras),
      pct_cadeiras_coligacao_prefeito = if (!isTRUE(any(tem_colig))) NA_real_
        else sum(cadeiras[na_colig %in% TRUE]) / sum(cadeiras),
      .groups = "drop"
    )

  base |>
    left_join(comp,  by = c("cod_ibge" = "id_municipio", "ano_camara")) |>
    left_join(renov, by = c("cod_ibge" = "id_municipio", "ano_camara")) |>
    left_join(colig_resumo, by = c("id_pleito_prefeito" = "id_pleito")) |>
    left_join(apoio, by = c("cod_ibge", "ano")) |>
    mutate(
      pct_cadeiras_oposicao      = 1 - pct_cadeiras_coligacao_prefeito,
      maioria_coligacao_prefeito = as.integer(pct_cadeiras_coligacao_prefeito > 0.5),
      alinhamento_bloco          = as.integer(bloco_majoritario_camara == bloco_prefeito)
    ) |>
    select(-id_pleito_prefeito, -sigla_partido_prefeito, -bloco_prefeito,
           -sigla_norm_prefeito, -mandato)
}
