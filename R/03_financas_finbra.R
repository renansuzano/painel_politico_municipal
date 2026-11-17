# 03_financas_finbra.R — Módulo 3: finanças municipais 2000-2024 -------------
# O MAPE traz o SICONFI só a partir de 2013. A Base dos Dados tem a série desde
# 1989 (receitas) e 1996 (despesas), harmonizada em id_conta_bd. Este módulo
# extrai 2000-2025 com as mesmas contas e testa a quebra de 2013.
# Saídas:
#   data/processed/financas.parquet   (município × ano)
#   output/diagnostico_financas.txt

source(here::here("R", "00_setup.R"))
source(here::here("R", "fn_financas.R"))

anos_fin <- 2000:2025
ano_base <- 2024   # valores deflacionados em reais de dez/2024

# 1. Coleta (cache em data/raw; cada consulta lê cerca de 2 GB no BigQuery) ---
lista_sql <- function(x) paste0("'", x, "'", collapse = ", ")

arq_rec <- file.path(dir_raw, "siconfi_receitas_2000_2025.rds")
if (!file.exists(arq_rec)) {
  rec <- read_sql(paste0("
    SELECT ano, id_municipio, id_conta_bd, SUM(valor) AS valor
    FROM `basedosdados.br_me_siconfi.municipio_receitas_orcamentarias`
    WHERE estagio_bd = 'Receitas Brutas Realizadas'
      AND ano BETWEEN ", min(anos_fin), " AND ", max(anos_fin), "
      AND (id_conta_bd IN (", lista_sql(contas_receita), ")
           OR STARTS_WITH(id_conta_bd, '", prefixo_fpm, "'))
    GROUP BY 1, 2, 3
  "))
  saveRDS(rec, arq_rec)
}
rec <- readRDS(arq_rec)

arq_nat <- file.path(dir_raw, "siconfi_despesas_natureza_2000_2025.rds")
if (!file.exists(arq_nat)) {
  dorc <- read_sql(paste0("
    SELECT ano, id_municipio, id_conta_bd, SUM(valor) AS valor
    FROM `basedosdados.br_me_siconfi.municipio_despesas_orcamentarias`
    WHERE estagio_bd = 'Despesas Empenhadas'
      AND ano BETWEEN ", min(anos_fin), " AND ", max(anos_fin), "
      AND id_conta_bd IN (", lista_sql(contas_natureza), ")
    GROUP BY 1, 2, 3
  "))
  saveRDS(dorc, arq_nat)
}
dorc <- readRDS(arq_nat)

arq_fun <- file.path(dir_raw, "siconfi_despesas_funcao_2000_2025_v2.rds")
if (!file.exists(arq_fun)) {
  dfun <- read_sql(paste0("
    SELECT ano, id_municipio, id_conta_bd, conta_bd, SUM(valor) AS valor
    FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao`
    WHERE estagio_bd = 'Despesas Empenhadas'
      AND ano BETWEEN ", min(anos_fin), " AND ", max(anos_fin), "
      AND ((REGEXP_CONTAINS(id_conta_bd, r'^3\\.\\d\\d\\.000$')
            AND id_conta_bd != '3.00.000')
           OR (COALESCE(id_conta_bd, '') = '' AND conta_bd != '' AND ano <= 2004))
    GROUP BY 1, 2, 3, 4
  "))
  saveRDS(dfun, arq_fun)
}
dfun <- readRDS(arq_fun)

arq_pop <- file.path(dir_raw, "ibge_populacao_2000_2025.rds")
if (!file.exists(arq_pop)) {
  pop <- read_sql(paste0("
    SELECT ano, id_municipio, populacao
    FROM `basedosdados.br_ibge_populacao.municipio`
    WHERE ano BETWEEN ", min(anos_fin), " AND ", max(anos_fin)))
  saveRDS(pop, arq_pop)
}
pop <- readRDS(arq_pop) |>
  mutate(ano = as.integer(ano), populacao = as.numeric(populacao))

# 2. Processamento -------------------------------------------------------------
receitas   <- prepara_receitas(rec)
natureza   <- prepara_natureza(dorc)
funcoes_df <- prepara_funcoes(dfun)
fatores    <- fatores_ipca(anos_fin, ano_base)
financas   <- monta_financas(receitas, natureza, funcoes_df, pop, fatores)

# 3. Diagnóstico ---------------------------------------------------------------
while (sink.number() > 0) sink()   # fecha redirecionamentos deixados por erros
sink(file.path(dir_output, "diagnostico_financas.txt"))
tryCatch({
cat("Municípios com dados por ano (receita corrente, despesa por função, população)\n")
print(financas |> group_by(ano) |>
        summarise(receita = sum(!is.na(receita_corrente_corr)),
                  funcoes = sum(!is.na(despesa_funcoes_corr)),
                  pop     = sum(!is.na(populacao)),
                  sem_declaracao = sum(flag_sem_declaracao),
                  outliers = sum(flag_outlier, na.rm = TRUE)), n = 30)

cat("\nTotais nacionais em R$ bilhões correntes (continuidade da série)\n")
print(financas |> group_by(ano) |>
        summarise(across(c(receita_corrente_corr, receita_tributaria_propria_corr,
                           transferencias_correntes_corr, fpm_corr, despesa_total_corr,
                           desp_pessoal_corr, investimento_corr, desp_saude_corr,
                           desp_educacao_corr),
                         \(x) round(sum(x, na.rm = TRUE) / 1e9, 1))) |>
        rename_with(\(x) sub("_corr$", "", x)), n = 30, width = 250)

cat("\nMedianas municipais (proporções e R$ per capita de dez/", ano_base, ")\n", sep = "")
print(financas |> filter(flag_outlier %in% 0) |> group_by(ano) |>
        summarise(across(c(autonomia_fiscal, dependencia_fpm, desp_pessoal_pct,
                           investimento_pct, desp_saude_pct, desp_educacao_pct,
                           desp_legislativo_pct, receita_corrente_pc),
                         \(x) round(median(x, na.rm = TRUE), 3))), n = 30, width = 250)

cat("\nTeste da quebra de 2013: variação das medianas 2012 -> 2013 x média das demais variações anuais\n")
med <- financas |> filter(flag_outlier %in% 0) |> group_by(ano) |>
  summarise(across(c(autonomia_fiscal, dependencia_fpm, desp_pessoal_pct,
                     investimento_pct, desp_saude_pct, desp_educacao_pct),
                   \(x) median(x, na.rm = TRUE))) |>
  arrange(ano)
var <- med |> mutate(across(-ano, \(x) x - lag(x)))
print(bind_rows(
  var |> filter(ano == 2013) |> mutate(ano = 2013L),
  var |> filter(ano != 2013, !is.na(autonomia_fiscal)) |>
    summarise(across(-ano, \(x) mean(abs(x), na.rm = TRUE))) |> mutate(ano = 0L)
) |> mutate(across(-ano, \(x) round(x, 3)),
            linha = if_else(ano == 2013L, "variação 2012->2013", "média |variação| outros anos")) |>
  select(linha, everything(), -ano), width = 250)
}, finally = sink())

# 4. Salvar --------------------------------------------------------------------
write_parquet(financas, file.path(dir_processed, "financas.parquet"))
message("Módulo 3 concluído. Veja output/diagnostico_financas.txt")
