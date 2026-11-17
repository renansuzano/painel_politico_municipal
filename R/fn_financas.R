# fn_financas.R — funções do módulo 3 (finanças) ------------------------------
# Usadas por 03_financas_finbra.R. Recebem e devolvem data frames.
# Fonte: br_me_siconfi (Base dos Dados), que já harmoniza o FINBRA (até 2012)
# e o SICONFI (2013 em diante) na coluna id_conta_bd.

# Contas usadas ---------------------------------------------------------------
contas_receita <- c(
  receita_orcamentaria       = "1.0.0.0.0.00.00.00",
  receita_corrente           = "1.1.0.0.0.00.00.00",
  receita_tributaria_propria = "1.1.1.0.0.00.00.00",
  impostos                   = "1.1.1.1.0.00.00.00",
  transferencias_correntes   = "1.1.7.0.0.00.00.00",
  receita_capital            = "1.2.0.0.0.00.00.00"
)
prefixo_fpm <- "1.1.7.1.1.51"   # total em .00.00; partes em .01, .02, .03

contas_natureza <- c(
  despesa_corrente = "2.3.0.00.00.00",
  desp_pessoal     = "2.3.1.00.00.00",
  despesa_capital  = "2.4.0.00.00.00",
  investimento     = "2.4.4.00.00.00"
)

funcoes <- c(
  legislativo = "3.01.000", assistencia = "3.08.000",
  saude       = "3.10.000", educacao    = "3.12.000", urbanismo   = "3.15.000",
  habitacao   = "3.16.000", saneamento  = "3.17.000"
)

# Garante que as colunas existam (conta ausente em todos os municípios)
garante_colunas <- function(df, cols, valor = NA_real_) {
  for (c in setdiff(cols, names(df))) df[[c]] <- valor
  df
}

# Nomes das 28 funções (Portaria 42/1999), para anos em que id_conta_bd vem
# vazio (2002-2003): o nome é traduzido para o código
nomes_funcoes <- c(
  "3.01.000" = "Legislativa", "3.02.000" = "Judiciária",
  "3.03.000" = "Essencial à Justiça", "3.04.000" = "Administração",
  "3.05.000" = "Defesa Nacional", "3.06.000" = "Segurança Pública",
  "3.07.000" = "Relações Exteriores", "3.08.000" = "Assistência Social",
  "3.09.000" = "Previdência Social", "3.10.000" = "Saúde",
  "3.11.000" = "Trabalho", "3.12.000" = "Educação", "3.13.000" = "Cultura",
  "3.14.000" = "Direitos da Cidadania", "3.15.000" = "Urbanismo",
  "3.16.000" = "Habitação", "3.17.000" = "Saneamento",
  "3.18.000" = "Gestão Ambiental", "3.19.000" = "Ciência e Tecnologia",
  "3.20.000" = "Agricultura", "3.21.000" = "Organização Agrária",
  "3.22.000" = "Indústria", "3.23.000" = "Comércio e Serviços",
  "3.24.000" = "Comunicações", "3.25.000" = "Energia",
  "3.26.000" = "Transporte", "3.27.000" = "Desporto e Lazer",
  "3.28.000" = "Encargos Especiais"
)
chave_nome <- function(x) tolower(stringi::stri_trans_general(trimws(x), "Latin-ASCII"))

# 1. Receitas ------------------------------------------------------------------
# rec: ano, id_municipio, id_conta_bd, valor (já filtrado no estágio bruto)
prepara_receitas <- function(rec) {
  rec <- rec |> mutate(ano = as.integer(ano), valor = as.numeric(valor))

  base <- rec |>
    filter(id_conta_bd %in% contas_receita) |>
    mutate(variavel = names(contas_receita)[match(id_conta_bd, contas_receita)]) |>
    group_by(id_municipio, ano, variavel) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variavel, values_from = valor)

  # FPM: usa o total quando existe; senão, soma as partes
  fpm <- rec |>
    filter(startsWith(id_conta_bd, prefixo_fpm)) |>
    group_by(id_municipio, ano) |>
    summarise(
      fpm = if (any(id_conta_bd == paste0(prefixo_fpm, ".00.00")))
              sum(valor[id_conta_bd == paste0(prefixo_fpm, ".00.00")], na.rm = TRUE)
            else sum(valor, na.rm = TRUE),
      .groups = "drop"
    )

  base |>
    garante_colunas(names(contas_receita)) |>
    left_join(fpm, by = c("id_municipio", "ano"))
}

# 2. Despesas por natureza e por função -------------------------------------
prepara_natureza <- function(dorc) {
  dorc |>
    mutate(ano = as.integer(ano), valor = as.numeric(valor)) |>
    filter(id_conta_bd %in% contas_natureza) |>
    mutate(variavel = names(contas_natureza)[match(id_conta_bd, contas_natureza)]) |>
    group_by(id_municipio, ano, variavel) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variavel, values_from = valor) |>
    garante_colunas(names(contas_natureza)) |>
    mutate(despesa_total = despesa_corrente + despesa_capital)
}

prepara_funcoes <- function(dfun) {
  dfun <- dfun |> mutate(ano = as.integer(ano), valor = as.numeric(valor))

  # Linhas sem código (2002-2003): traduz o nome da função para o código e
  # usa só em município-ano que não tenha linhas codificadas
  if ("conta_bd" %in% names(dfun)) {
    com_codigo <- dfun |>
      filter(!is.na(id_conta_bd), id_conta_bd != "") |>
      distinct(id_municipio, ano)
    sem_codigo <- dfun |>
      filter(is.na(id_conta_bd) | id_conta_bd == "") |>
      mutate(id_conta_bd = names(nomes_funcoes)[
        match(chave_nome(conta_bd), chave_nome(nomes_funcoes))]) |>
      filter(!is.na(id_conta_bd)) |>
      anti_join(com_codigo, by = c("id_municipio", "ano"))
    dfun <- bind_rows(filter(dfun, !is.na(id_conta_bd), id_conta_bd != ""), sem_codigo)
  }

  total <- dfun |>
    filter(grepl("^3\\.(0[1-9]|1[0-9]|2[0-8])\\.000$", id_conta_bd)) |>
    group_by(id_municipio, ano) |>
    summarise(despesa_funcoes = sum(valor, na.rm = TRUE), .groups = "drop")

  sel <- dfun |>
    filter(id_conta_bd %in% funcoes) |>
    mutate(variavel = paste0("desp_", names(funcoes)[match(id_conta_bd, funcoes)])) |>
    group_by(id_municipio, ano, variavel) |>
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variavel, values_from = valor)

  # Função sem registro no ano, com o município declarando, vale zero.
  # Antes de 2002 vigorava a classificação funcional anterior (Saúde e
  # Saneamento, Educação e Cultura...), não comparável: esses anos ficam fora.
  full_join(total, sel, by = c("id_municipio", "ano")) |>
    filter(ano >= 2002) |>
    garante_colunas(paste0("desp_", names(funcoes)), 0) |>
    mutate(across(starts_with("desp_"), \(x) coalesce(x, 0)))
}

# 3. Fatores de deflação (IPCA) ---------------------------------------------------
# Devolve tibble(ano, fator) para levar valores a reais de dez/ano_base.
# Usa o mês de julho como referência de cada ano (meio do exercício).
fatores_ipca <- function(anos, ano_base) {
  datas <- as.Date(paste0(anos, "-07-01"))
  fator <- tryCatch(
    deflateBR::deflate(rep(1, length(anos)), datas,
                       paste0("12/", ano_base), index = "ipca"),
    error = function(e) {
      warning("Falha ao obter o IPCA (deflateBR): ", conditionMessage(e),
              ". Valores deflacionados ficam NA.")
      rep(NA_real_, length(anos))
    }
  )
  tibble(ano = as.integer(anos), fator = fator)
}

# 4. Indicadores ---------------------------------------------------------------
monta_financas <- function(receitas, natureza, funcoes_df, pop, fatores) {
  grade <- bind_rows(select(receitas, id_municipio, ano),
                     select(natureza, id_municipio, ano),
                     select(funcoes_df, id_municipio, ano)) |>
    distinct()

  df <- grade |>
    left_join(receitas,   by = c("id_municipio", "ano")) |>
    left_join(natureza,   by = c("id_municipio", "ano")) |>
    left_join(funcoes_df, by = c("id_municipio", "ano")) |>
    left_join(pop,        by = c("id_municipio", "ano")) |>
    left_join(fatores,    by = "ano")

  pc  <- function(x) x / df$populacao
  pct <- function(x, tot) ifelse(is.na(tot) | tot <= 0, NA_real_, x / tot)

  out <- df |>
    mutate(
      autonomia_fiscal      = pct(receita_tributaria_propria, receita_corrente),
      dependencia_transf    = pct(transferencias_correntes, receita_corrente),
      dependencia_fpm       = pct(fpm, receita_corrente),
      desp_pessoal_pct      = pct(desp_pessoal, despesa_total),
      investimento_pct      = pct(investimento, despesa_total),
      desp_legislativo_pct  = pct(desp_legislativo, despesa_funcoes),
      desp_saude_pct        = pct(desp_saude, despesa_funcoes),
      desp_educacao_pct     = pct(desp_educacao, despesa_funcoes),
      desp_assistencia_pct  = pct(desp_assistencia, despesa_funcoes),
      desp_habitacao_pct    = pct(desp_habitacao, despesa_funcoes),
      desp_urbanismo_pct    = pct(desp_urbanismo, despesa_funcoes),
      desp_saneamento_pct   = pct(desp_saneamento, despesa_funcoes),
      flag_sem_declaracao   = as.integer(is.na(receita_corrente) |
                                         (ano >= 2002 & is.na(despesa_funcoes))),
      flag_quebra_2013      = as.integer(ano >= 2013)
    )

  # Valores em reais correntes (_corr), deflacionados (_def) e per capita
  # deflacionados (_pc)
  monet <- c("receita_orcamentaria", "receita_corrente", "receita_tributaria_propria",
             "transferencias_correntes", "fpm", "receita_capital", "despesa_total",
             "despesa_funcoes",
             "desp_pessoal", "investimento", "desp_saude", "desp_educacao",
             "desp_assistencia", "desp_habitacao", "desp_urbanismo",
             "desp_saneamento", "desp_legislativo")
  monet <- intersect(monet, names(out))

  out <- out |>
    mutate(across(all_of(monet), \(x) x * fator, .names = "{.col}_def")) |>
    mutate(across(all_of(paste0(monet, "_def")), \(x) x / populacao,
                  .names = "{sub('_def$', '', .col)}_pc")) |>
    rename_with(\(x) paste0(x, "_corr"), all_of(monet))

  # Outliers: per capita da receita corrente fora de [0,2; 5] × mediana do ano
  out |>
    group_by(ano) |>
    mutate(flag_outlier = as.integer(
      receita_corrente_pc < 0.2 * median(receita_corrente_pc, na.rm = TRUE) |
      receita_corrente_pc > 5   * median(receita_corrente_pc, na.rm = TRUE))) |>
    ungroup() |>
    rename(cod_ibge = id_municipio) |>
    select(-fator, -despesa_corrente, -despesa_capital, -impostos)
}
