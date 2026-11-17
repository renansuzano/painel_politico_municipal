# fn_prefeitos.R — funções do módulo 1 (prefeitos) ---------------------------
# Usadas por 01_prefeitos_tse.R. Recebem e devolvem data frames, sem acesso à
# internet, para poderem ser testadas com dados simulados.

# Ano de início do mandato municipal vigente numa data
# (mandatos começam em 2001, 2005, 2009, ...)
inicio_mandato_vigente <- function(data) {
  y <- as.integer(format(data, "%Y"))
  y - ((y - 1L) %% 4L)
}

# Nome normalizado (maiúsculas, sem acentos, espaços simples)
normaliza_nome <- function(nome) {
  x <- toupper(stringi::stri_trans_general(nome, "Latin-ASCII"))
  x <- gsub("[^A-Z ]", "", x)
  trimws(gsub(" +", " ", x))
}

# Identificador da pessoa dentro de um pleito: título; se faltar, nome
id_pessoa <- function(titulo, nome_norm) {
  ifelse(is.na(titulo), paste0("NOME:", nome_norm), paste0("TIT:", titulo))
}

# Mesma pessoa em dois pleitos: compara o título quando os dois existem
# (o título vem vazio em 2000); senão, compara o nome normalizado
mesma_pessoa <- function(tit_a, nome_a, tit_b, nome_b) {
  as.integer(ifelse(!is.na(tit_a) & !is.na(tit_b), tit_a == tit_b, nome_a == nome_b))
}

# 1. Prepara os resultados brutos do TSE (uma linha por candidato e turno) ---
prepara_resultados <- function(res) {
  res |>
    distinct() |>                       # há linhas duplicadas (ex.: 2000)
    mutate(
      ano          = as.integer(ano),
      turno        = as.integer(turno),
      votos        = as.numeric(votos),
      data_eleicao = as.Date(data_eleicao),
      suplementar  = !startsWith(tipo_eleicao, "eleicao ordinaria"),
      id_pleito    = paste(id_municipio, ano, tipo_eleicao, sep = "|"),
      titulo       = na_if(trimws(titulo_eleitoral_candidato), ""),
      nome_norm    = normaliza_nome(nome_candidato),
      pessoa       = id_pessoa(titulo, nome_norm)
    ) |>
    classifica_partido("sigla_partido")
}

# 2. Resume cada pleito (eleição ordinária ou suplementar) -------------------
# Devolve list(pleitos = ..., problemas = ...)
resume_pleitos <- function(el) {
  el <- el |>
    group_by(id_pleito) |>
    mutate(
      turno_final = max(turno),
      data_ini    = min(data_eleicao),
      data_fim    = max(data_eleicao)
    ) |>
    ungroup()

  n_cand <- el |>
    filter(turno == 1L) |>
    count(id_pleito, name = "n_candidatos_prefeito")

  # Turno decisivo, com % dos votos dados a candidatos
  fin <- el |>
    filter(turno == turno_final) |>
    group_by(id_pleito) |>
    mutate(pct = 100 * votos / sum(votos, na.rm = TRUE)) |>
    arrange(desc(votos), .by_group = TRUE) |>
    mutate(rank = row_number()) |>
    ungroup()

  # Pleitos sem exatamente um eleito ficam de fora:
  #   0 eleitos  -> eleição anulada (mandato preenchido por suplementar)
  #   2+ eleitos -> problema na base, a investigar
  n_eleitos <- fin |>
    group_by(id_pleito) |>
    summarise(n_eleitos = n_distinct(pessoa[resultado == "eleito"]), .groups = "drop")
  problemas <- filter(n_eleitos, n_eleitos != 1L) |>
    mutate(tipo_problema = if_else(n_eleitos == 0L, "sem eleito (anulada)", "mais de um eleito"))

  vencedor <- fin |>
    filter(resultado == "eleito", !id_pleito %in% problemas$id_pleito) |>
    select(id_pleito, id_municipio, sigla_uf, ano_eleicao = ano, tipo_eleicao, suplementar,
           data_ini, data_fim, turno_final, pessoa, titulo, nome_norm, nome_candidato,
           sigla_partido, media_ideologia, classificacao_original, bloco,
           votos_validos_pct = pct)

  # Margem do vencedor sobre o melhor colocado entre os demais
  margem <- fin |>
    inner_join(select(vencedor, id_pleito, pessoa_venc = pessoa), by = "id_pleito") |>
    group_by(id_pleito) |>
    summarise(
      pct_venc = max(pct[pessoa == pessoa_venc], na.rm = TRUE),
      pct_seg  = suppressWarnings(max(pct[pessoa != pessoa_venc], na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      pct_seg = ifelse(is.finite(pct_seg), pct_seg, NA_real_),
      margem_vitoria = pct_venc - pct_seg
    ) |>
    select(id_pleito, margem_vitoria)

  # RDD: os dois mais votados no turno decisivo
  rdd <- fin |>
    filter(rank <= 2L) |>
    group_by(id_pleito) |>
    summarise(
      bloco_1 = bloco[1], bloco_2 = bloco[2],
      pct_1   = pct[1],   pct_2   = pct[2],
      .groups = "drop"
    ) |>
    mutate(
      disputa_esq_naoesq = case_when(
        is.na(pct_2)                   ~ 0L,       # candidato único
        is.na(bloco_1) | is.na(bloco_2) ~ NA_integer_,
        xor(bloco_1 == "esquerda", bloco_2 == "esquerda") ~ 1L,
        TRUE                           ~ 0L
      ),
      margem_esquerda = case_when(
        is.na(disputa_esq_naoesq) | disputa_esq_naoesq != 1L ~ NA_real_,
        bloco_1 == "esquerda"     ~ pct_1 - pct_2,
        TRUE                      ~ pct_2 - pct_1
      )
    ) |>
    select(id_pleito, disputa_esq_naoesq, margem_esquerda)

  pleitos <- vencedor |>
    left_join(n_cand, by = "id_pleito") |>
    left_join(margem, by = "id_pleito") |>
    left_join(rdd,    by = "id_pleito") |>
    mutate(
      segundo_turno = as.integer(turno_final == 2L),
      turno_margem  = turno_final,
      esquerda      = as.integer(bloco == "esquerda"),
      # Mandato: ordinária elege o mandato seguinte; suplementar completa o
      # mandato vigente na data da eleição
      mandato_ini   = if_else(suplementar, inicio_mandato_vigente(data_ini), ano_eleicao + 1L),
      mandato       = paste0(mandato_ini, "-", mandato_ini + 3L),
      inicio_governo = if_else(suplementar, data_fim,
                               as.Date(paste0(ano_eleicao + 1L, "-01-01")))
    )

  list(pleitos = pleitos, problemas = problemas)
}

# 3. Continuidade: compara cada governo com o anterior no mesmo município ---
adiciona_continuidade <- function(pleitos) {
  pleitos |>
    arrange(id_municipio, inicio_governo) |>
    group_by(id_municipio) |>
    mutate(
      reeleito               = mesma_pessoa(titulo, nome_norm, lag(titulo), lag(nome_norm)),
      segundo_mandato        = reeleito,
      mesmo_partido_anterior = as.integer(sigla_partido == lag(sigla_partido)),
      mesmo_bloco_anterior   = as.integer(bloco == lag(bloco)),
      transicao_bloco        = if_else(is.na(lag(bloco)) | is.na(bloco), NA_character_,
                                       paste0(lag(bloco), "->", bloco))
    ) |>
    ungroup()
}

# 4. Governadores: partido e bloco por UF e ano ------------------------------
# gov: resultados brutos do TSE para governador (eleições gerais)
governadores_por_ano <- function(gov) {
  gov |>
    mutate(ano = as.integer(ano), turno = as.integer(turno)) |>
    filter(startsWith(tipo_eleicao, "eleicao ordinaria")) |>
    group_by(sigla_uf, ano) |>
    filter(turno == max(turno), resultado == "eleito") |>
    ungroup() |>
    classifica_partido("sigla_partido") |>
    select(sigla_uf, ano_eleicao = ano,
           sigla_partido_governador = sigla_partido, bloco_governador = bloco) |>
    tidyr::crossing(k = 1:4) |>
    mutate(ano = ano_eleicao + k) |>
    select(-k, -ano_eleicao)
}

# 5. Expande para município × ano --------------------------------------------
# Em cada ano vale o governo em exercício em 1º de julho
expande_anual <- function(pleitos, anos, presidentes, governadores) {
  grade <- tidyr::expand_grid(id_municipio = unique(pleitos$id_municipio),
                              ano = anos) |>
    mutate(
      mandato_ini   = ano - ((ano - 1L) %% 4L),
      mandato       = paste0(mandato_ini, "-", mandato_ini + 3L),
      ano_mandato   = ((ano - 1L) %% 4L) + 1L,
      ano_eleitoral = as.integer(ano_mandato == 4L),
      data_ref      = as.Date(paste0(ano, "-07-01"))
    )

  em_exercicio <- grade |>
    inner_join(pleitos, by = c("id_municipio", "mandato_ini", "mandato"),
               relationship = "many-to-many") |>
    filter(inicio_governo <= data_ref) |>
    group_by(id_municipio, ano) |>
    slice_max(inicio_governo, n = 1, with_ties = FALSE) |>
    ungroup()

  # Houve posse de prefeito eleito em suplementar dentro do ano?
  troca <- pleitos |>
    filter(suplementar) |>
    transmute(id_municipio, ano = as.integer(format(inicio_governo, "%Y"))) |>
    distinct() |>
    mutate(flag_troca_no_ano = 1L)

  uf <- distinct(pleitos, id_municipio, sigla_uf)

  grade |>
    left_join(select(em_exercicio, id_municipio, ano, id_pleito_prefeito = id_pleito,
                     sigla_partido_prefeito = sigla_partido,
                     bloco_prefeito = bloco, esquerda,
                     ideologia_media_prefeito = media_ideologia,
                     classificacao_original_prefeito = classificacao_original,
                     nome_prefeito = nome_candidato, flag_suplementar = suplementar,
                     n_candidatos_prefeito, votos_validos_pct, margem_vitoria,
                     segundo_turno, disputa_esq_naoesq, margem_esquerda, turno_margem,
                     reeleito, segundo_mandato, mesmo_partido_anterior,
                     mesmo_bloco_anterior, transicao_bloco),
              by = c("id_municipio", "ano")) |>
    left_join(troca, by = c("id_municipio", "ano")) |>
    left_join(uf, by = "id_municipio") |>
    left_join(presidentes, by = "ano") |>
    left_join(governadores, by = c("sigla_uf", "ano")) |>
    mutate(
      flag_suplementar  = as.integer(flag_suplementar),
      flag_troca_no_ano = coalesce(flag_troca_no_ano, 0L),
      alinhado_presidente       = as.integer(sigla_partido_prefeito == sigla_partido_presidente),
      alinhado_bloco_presidente = as.integer(bloco_prefeito == bloco_presidente),
      alinhado_governador       = as.integer(sigla_partido_prefeito == sigla_partido_governador),
      alinhado_bloco_governador = as.integer(bloco_prefeito == bloco_governador)
    ) |>
    rename(cod_ibge = id_municipio) |>
    select(-mandato_ini, -data_ref)
}
