# 92_dados_site.R — prepara os dados do site (site/dados/) --------------------
# Depende do painel (R/90_painel.R). Gera arquivos leves que o site lê no
# navegador:
#   site/dados/municipios.geojson  geometria simplificada + partido e bloco do
#                                  prefeito no 1º ano de cada mandato
#   site/dados/resumo_uf.json      médias e totais por UF (e Brasil) e ano
#   site/dados/dicionario.json     dicionário de variáveis

source(here::here("R", "00_setup.R"))
for (p in c("geobr", "sf", "jsonlite")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(sf)

dir_site <- here("site", "dados")
dir.create(dir_site, showWarnings = FALSE, recursive = TRUE)

painel  <- read_parquet(file.path(dir_output, "painel_politico_municipal.parquet"))
inicios <- c(2001L, 2005L, 2009L, 2013L, 2017L, 2021L, 2025L)

# 1. Municípios: geometria simplificada + atributos por mandato ---------------
arq_malha_nomes <- file.path(dir_raw, "malha_municipios_2020_nomes.rds")
if (!file.exists(arq_malha_nomes)) {
  geobr::read_municipality(year = 2020, simplified = TRUE, showProgress = FALSE) |>
    transmute(cod_ibge = as.character(code_muni), nome = name_muni, uf = abbrev_state) |>
    saveRDS(arq_malha_nomes)
}
malha <- readRDS(arq_malha_nomes)

atributos <- painel |>
  filter(ano %in% inicios) |>
  transmute(cod_ibge, ano,
            p = sigla_partido_prefeito,
            b = bloco_prefeito) |>
  tidyr::pivot_wider(names_from = ano, values_from = c(p, b), names_sep = "")

municipios <- malha |>
  st_transform(5880) |>                                   # métrico, para simplificar
  st_simplify(dTolerance = 800, preserveTopology = TRUE) |>
  st_transform(4326) |>
  left_join(atributos, by = "cod_ibge")

arq_geo <- file.path(dir_site, "municipios.geojson")
if (file.exists(arq_geo)) file.remove(arq_geo)
st_write(municipios, arq_geo, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=4", "RFC7946=YES"))

# 2. Resumos por UF e ano (com "Brasil") ---------------------------------------
resume <- function(df) {
  df |>
    summarise(
      municipios           = n(),
      reeleito             = mean(reeleito, na.rm = TRUE),
      pct_partido          = mean(pct_cadeiras_partido_prefeito, na.rm = TRUE),
      pct_coligacao        = mean(pct_cadeiras_coligacao_prefeito, na.rm = TRUE),
      maioria              = mean(maioria_coligacao_prefeito, na.rm = TRUE),
      nep                  = mean(nep_cadeiras, na.rm = TRUE),
      renovacao            = mean(taxa_reeleicao_vereadores, na.rm = TRUE),
      receita_corrente_def = sum(receita_corrente_def, na.rm = TRUE),
      desp_pessoal_def     = sum(desp_pessoal_def, na.rm = TRUE),
      investimento_def     = sum(investimento_def, na.rm = TRUE),
      med_pessoal          = median(desp_pessoal_pct[flag_outlier %in% 0], na.rm = TRUE),
      med_investimento     = median(investimento_pct[flag_outlier %in% 0], na.rm = TRUE),
      med_saude            = median(desp_saude_pct[flag_outlier %in% 0], na.rm = TRUE),
      med_educacao         = median(desp_educacao_pct[flag_outlier %in% 0], na.rm = TRUE),
      med_autonomia        = median(autonomia_fiscal[flag_outlier %in% 0], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric), \(x) ifelse(is.finite(x), x, NA_real_)))
}

resumo <- bind_rows(
  painel |> group_by(uf = sigla_uf, ano) |> resume(),
  painel |> group_by(ano) |> resume() |> mutate(uf = "Brasil")
) |>
  filter(!is.na(uf)) |>
  relocate(uf, ano)

jsonlite::write_json(resumo, file.path(dir_site, "resumo_uf.json"),
                     dataframe = "rows", na = "null", digits = 5)

# 3. Dicionário ----------------------------------------------------------------
read_csv(here("docs", "dicionario_variaveis.csv"), show_col_types = FALSE) |>
  select(modulo, variavel, descricao, unidade, nivel, fonte, anos = anos_previstos) |>
  jsonlite::write_json(file.path(dir_site, "dicionario.json"),
                       dataframe = "rows", na = "null")

tamanho <- round(file.size(list.files(dir_site, full.names = TRUE)) / 1e6, 1)
message("Dados do site gerados em site/dados/: ",
        paste0(basename(list.files(dir_site)), " (", tamanho, " MB)", collapse = ", "))
