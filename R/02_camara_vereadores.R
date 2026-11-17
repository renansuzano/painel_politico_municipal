# 02_camara_vereadores.R — Módulo 2: composição da Câmara Municipal ---------
# O MAPE traz a votação para vereador; este módulo calcula as cadeiras, a
# fragmentação, os blocos, a renovação e o peso da coligação do prefeito.
# Fonte: TSE via Base dos Dados (resultados_candidato, partidos, vagas).
# Depende do módulo 1 (data/processed/prefeitos*.parquet).
# Saídas:
#   data/processed/camara_partidos.parquet  (cadeiras por partido e legislatura)
#   data/processed/camara.parquet           (município × ano, 2000-2028)
#   output/diagnostico_camara.txt

source(here::here("R", "00_setup.R"))
source(here::here("R", "fn_prefeitos.R"))
source(here::here("R", "fn_camara.R"))

# 1. Coleta (cache em data/raw) ----------------------------------------------
arq_ver <- file.path(dir_raw, "tse_vereadores_eleitos_2000_2024.rds")
if (!file.exists(arq_ver)) {
  ver <- read_sql("
    SELECT ano, tipo_eleicao, sigla_uf, id_municipio, sigla_partido,
           titulo_eleitoral_candidato, nome_candidato, resultado
    FROM `basedosdados.br_tse_eleicoes.resultados_candidato`
    WHERE cargo = 'vereador' AND ano >= 2000
      AND resultado IN ('eleito', 'eleito por qp', 'eleito por media')
  ")
  saveRDS(ver, arq_ver)
}
ver <- readRDS(arq_ver)

arq_part <- file.path(dir_raw, "tse_partidos_prefeito_2000_2024.rds")
if (!file.exists(arq_part)) {
  part <- read_sql("
    SELECT ano, turno, tipo_eleicao, id_municipio, sigla,
           tipo_agremiacao, sequencial_coligacao
    FROM `basedosdados.br_tse_eleicoes.partidos`
    WHERE cargo = 'prefeito' AND ano >= 2000
  ")
  saveRDS(part, arq_part)
}
part <- readRDS(arq_part)

arq_vagas <- file.path(dir_raw, "tse_vagas_vereador_2000_2024.rds")
if (!file.exists(arq_vagas)) {
  vagas <- read_sql("
    SELECT ano, tipo_eleicao, id_municipio, vagas
    FROM `basedosdados.br_tse_eleicoes.vagas`
    WHERE cargo = 'vereador' AND ano >= 2000
  ")
  saveRDS(vagas, arq_vagas)
}
vagas <- readRDS(arq_vagas)

pleitos   <- read_parquet(file.path(dir_processed, "prefeitos_pleitos.parquet"))
prefeitos <- read_parquet(file.path(dir_processed, "prefeitos.parquet"))
stopifnot("Rode de novo o módulo 1: falta id_pleito_prefeito" =
            "id_pleito_prefeito" %in% names(prefeitos))

# 2. Processamento -------------------------------------------------------------
vereadores   <- prepara_vereadores(ver)
cad          <- cadeiras_por_partido(vereadores)
comp         <- composicao_camara(cad)
renov        <- renovacao_camara(vereadores)
membros      <- membros_coligacao(part)
colig        <- coligacao_vencedor(pleitos, membros, anos_sem_dado = 2000L)
colig_resumo <- resumo_coligacao(colig)
camara       <- painel_camara(prefeitos, comp, renov, cad, colig, colig_resumo)

# 3. Diagnóstico ---------------------------------------------------------------
vagas_chk <- vagas |>
  filter(startsWith(tipo_eleicao, "eleicao ordinaria")) |>
  transmute(id_municipio, ano_camara = as.integer(ano), vagas = as.integer(vagas)) |>
  distinct() |>
  inner_join(select(comp, id_municipio, ano_camara, n_cadeiras),
             by = c("id_municipio", "ano_camara"))

while (sink.number() > 0) sink()   # fecha redirecionamentos deixados por erros
sink(file.path(dir_output, "diagnostico_camara.txt"))
tryCatch({
cat("Vereadores eleitos e municípios por eleição\n")
print(count(vereadores, ano_camara, name = "eleitos") |>
        left_join(count(comp, ano_camara, name = "municipios"), by = "ano_camara"))
cat("\nCadeiras contadas x vagas oficiais (municípios com diferença)\n")
print(vagas_chk |> mutate(dif = n_cadeiras - vagas) |> count(ano_camara, dif) |>
        filter(dif != 0), n = 50)
cat("\nMédias por eleição\n")
print(comp |> group_by(ano_camara) |>
        summarise(across(c(n_cadeiras, nep_cadeiras, pct_cadeiras_esquerda,
                           pct_cadeiras_centro, pct_cadeiras_direita,
                           pct_cadeiras_sem_bloco), \(x) round(mean(x, na.rm = TRUE), 3))),
      width = 200)
cat("\nSiglas de vereadores sem bloco\n")
print(cad |> filter(is.na(bloco)) |> count(sigla_norm, wt = cadeiras, name = "cadeiras"))
cat("\nRenovação média por eleição\n")
print(renov |> group_by(ano_camara) |>
        summarise(taxa_reeleicao = round(mean(taxa_reeleicao_vereadores), 3)))
cat("\nColigação do prefeito identificada (por ano da eleição)\n")
print(pleitos |> left_join(transmute(colig_resumo, id_pleito, achou = TRUE), by = "id_pleito") |>
        count(ano_eleicao, achou = coalesce(achou, FALSE)), n = 50)
cat("\nMédias anuais da relação prefeito x Câmara\n")
print(camara |> group_by(ano) |>
        summarise(pct_partido   = round(mean(pct_cadeiras_partido_prefeito, na.rm = TRUE), 3),
                  pct_coligacao = round(mean(pct_cadeiras_coligacao_prefeito, na.rm = TRUE), 3),
                  maioria       = round(mean(maioria_coligacao_prefeito, na.rm = TRUE), 3),
                  alinhamento   = round(mean(alinhamento_bloco, na.rm = TRUE), 3)),
      n = 30)
}, finally = sink())

# 4. Salvar --------------------------------------------------------------------
write_parquet(cad,    file.path(dir_processed, "camara_partidos.parquet"))
write_parquet(camara, file.path(dir_processed, "camara.parquet"))

message("Módulo 2 concluído. Veja output/diagnostico_camara.txt")
