# 01_prefeitos_tse.R — Módulo 1: variáveis políticas do prefeito ------------
# O MAPE já traz a votação municipal por partido. Este módulo constrói o que
# não está lá: ideologia, continuidade, competição, RDD, ciclo e alinhamento.
# Fonte: TSE via Base dos Dados (br_tse_eleicoes.resultados_candidato).
# Saídas:
#   data/processed/prefeitos_pleitos.parquet  (uma linha por eleição; base do RDD)
#   data/processed/prefeitos.parquet          (município × ano, 2000-2028)
#   output/diagnostico_prefeitos.txt          (problemas encontrados)

source(here::here("R", "00_setup.R"))
source(here::here("R", "fn_prefeitos.R"))

# 1. Coleta (fica em cache em data/raw; apague o arquivo para baixar de novo) -
arq_pref <- file.path(dir_raw, "tse_prefeito_2000_2024.rds")
if (!file.exists(arq_pref)) {
  res <- read_sql("
    SELECT ano, turno, tipo_eleicao, data_eleicao, sigla_uf, id_municipio,
           sigla_partido, titulo_eleitoral_candidato, nome_candidato,
           resultado, votos
    FROM `basedosdados.br_tse_eleicoes.resultados_candidato`
    WHERE cargo = 'prefeito' AND ano >= 2000
  ")
  saveRDS(res, arq_pref)
}
res <- readRDS(arq_pref)

arq_gov <- file.path(dir_raw, "tse_governador_1998_2022.rds")
if (!file.exists(arq_gov)) {
  gov <- read_sql("
    SELECT ano, turno, tipo_eleicao, sigla_uf, sigla_partido, resultado
    FROM `basedosdados.br_tse_eleicoes.resultados_candidato`
    WHERE cargo = 'governador' AND ano BETWEEN 1998 AND 2022
  ")
  saveRDS(gov, arq_gov)
}
gov <- readRDS(arq_gov)

presidentes <- read_csv2(file.path(dir_external, "presidentes.csv"),
                         show_col_types = FALSE, na = c("", "NA")) |>
  mutate(ano = as.integer(ano)) |>
  select(ano, sigla_partido_presidente, bloco_presidente)

# 2. Processamento -------------------------------------------------------------
el        <- prepara_resultados(res)
resumo    <- resume_pleitos(el)
pleitos   <- adiciona_continuidade(resumo$pleitos)
governos_uf <- governadores_por_ano(gov)
prefeitos <- expande_anual(pleitos, anos = 2000:2028,
                           presidentes = presidentes, governadores = governos_uf)

# 3. Diagnóstico ---------------------------------------------------------------
while (sink.number() > 0) sink()   # fecha redirecionamentos deixados por erros
sink(file.path(dir_output, "diagnostico_prefeitos.txt"))
tryCatch({
cat("Pleitos por ano e tipo\n")
print(count(pleitos, ano_eleicao, suplementar), n = 50)
cat("\nPleitos excluídos por tipo e ano:\n")
print(resumo$problemas |>
        mutate(ano = as.integer(sub("^[^|]*\\|([0-9]{4}).*", "\\1", id_pleito))) |>
        count(tipo_problema, ano), n = 50)
cat("\nPleitos com mais de um eleito:\n")
print(filter(resumo$problemas, n_eleitos > 1L), n = 50)
cat("\nReeleições por eleição ordinária:\n")
print(pleitos |> filter(!suplementar) |> count(ano_eleicao, reeleito), n = 50)
cat("\nSiglas sem classificação:\n")
print(siglas_sem_classificacao(res))
cat("\nPrefeitos por bloco e mandato\n")
print(count(pleitos |> filter(!suplementar), mandato, bloco) |>
        tidyr::pivot_wider(names_from = bloco, values_from = n))
cat("\nMunicípio-ano sem prefeito identificado (2001-2028):\n")
print(prefeitos |> filter(ano >= 2001, is.na(sigla_partido_prefeito)) |> count(ano), n = 30)
cat("\nPleitos com disputa esquerda x não esquerda:\n")
print(count(pleitos, ano_eleicao, disputa_esq_naoesq), n = 50)
}, finally = sink())

# 4. Salvar --------------------------------------------------------------------
write_parquet(pleitos,   file.path(dir_processed, "prefeitos_pleitos.parquet"))
write_parquet(prefeitos, file.path(dir_processed, "prefeitos.parquet"))

message("Módulo 1 concluído. Veja output/diagnostico_prefeitos.txt")
