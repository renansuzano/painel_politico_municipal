# 90_painel.R — junção dos módulos em painel município × ano -----------------
# Entradas: data/processed/{prefeitos, camara, financas}.parquet
# Saídas:
#   output/painel_politico_municipal.parquet
#   output/painel_politico_municipal.csv.gz
#   output/diagnostico_painel.txt

source(here::here("R", "00_setup.R"))

anos <- 2000:2028

prefeitos <- read_parquet(file.path(dir_processed, "prefeitos.parquet"))
camara    <- read_parquet(file.path(dir_processed, "camara.parquet"))
financas  <- read_parquet(file.path(dir_processed, "financas.parquet"))

# Grade: todos os municípios que aparecem em algum módulo × anos.
# O município entra a partir do primeiro ano em que aparece com dado em algum
# módulo (evita linhas antes da instalação, ex.: municípios criados em 2013).
primeiro_ano <- bind_rows(
  prefeitos |> filter(!is.na(sigla_partido_prefeito)) |> select(cod_ibge, ano),
  financas  |> filter(!is.na(receita_corrente_corr))  |> select(cod_ibge, ano)
) |>
  group_by(cod_ibge) |>
  summarise(ano_inicio = min(ano), .groups = "drop")

painel <- tidyr::expand_grid(cod_ibge = primeiro_ano$cod_ibge, ano = anos) |>
  left_join(primeiro_ano, by = "cod_ibge") |>
  filter(ano >= ano_inicio | ano_inicio <= 2001L) |>
  select(-ano_inicio) |>
  left_join(prefeitos, by = c("cod_ibge", "ano")) |>
  left_join(camara,    by = c("cod_ibge", "ano")) |>
  left_join(financas,  by = c("cod_ibge", "ano"))

# Preenche a UF de linhas sem prefeito identificado
uf <- painel |> filter(!is.na(sigla_uf)) |> distinct(cod_ibge, sigla_uf)
painel <- painel |> select(-sigla_uf) |>
  left_join(uf, by = "cod_ibge") |>
  relocate(cod_ibge, sigla_uf, ano, mandato)

# Checagens --------------------------------------------------------------------
dic <- read_csv(here("docs", "dicionario_variaveis.csv"), show_col_types = FALSE)
base_dic <- function(x) sub("_(corr|def|pc)$", "", x)
fora_dic <- setdiff(base_dic(names(painel)), dic$variavel)
dic_fora <- setdiff(dic$variavel, base_dic(names(painel)))

cobertura <- painel |>
  group_by(ano) |>
  summarise(
    municipios = n(),
    prefeito   = sum(!is.na(sigla_partido_prefeito)),
    camara     = sum(!is.na(n_cadeiras)),
    coligacao  = sum(!is.na(pct_cadeiras_coligacao_prefeito)),
    financas   = sum(!is.na(receita_corrente_corr)),
    funcoes    = sum(!is.na(desp_saude_pct)),
    .groups = "drop"
  )
write_csv(cobertura, file.path(dir_output, "cobertura.csv"))

# Comparação com o mape_municipios (opcional) ---------------------------------
# Coloque uma cópia em data/external/ (csv ou parquet) e ajuste o nome das
# colunas de município e ano, se forem diferentes.
arq_mape  <- list.files(dir_external, pattern = "^mape_municipios\\.(csv|parquet)$",
                        full.names = TRUE)
col_mun   <- "id_municipio"
col_ano   <- "ano"
comp_mape <- NULL
if (length(arq_mape) > 0) {
  mape <- if (grepl("parquet$", arq_mape[1])) read_parquet(arq_mape[1])
          else read_csv(arq_mape[1], show_col_types = FALSE)
  chaves_mape <- mape |>
    transmute(cod_ibge = as.character(.data[[col_mun]]),
              ano      = as.integer(.data[[col_ano]])) |>
    distinct()
  comp_mape <- list(
    so_no_painel = anti_join(distinct(painel, cod_ibge, ano), chaves_mape, by = c("cod_ibge", "ano")),
    so_no_mape   = anti_join(chaves_mape, distinct(painel, cod_ibge, ano), by = c("cod_ibge", "ano")),
    colunas_repetidas = intersect(setdiff(names(painel), c("cod_ibge", "ano")), names(mape))
  )
}

while (sink.number() > 0) sink()
sink(file.path(dir_output, "diagnostico_painel.txt"))
tryCatch({
  cat("Dimensões do painel:", nrow(painel), "linhas x", ncol(painel), "colunas\n")
  cat("Municípios:", n_distinct(painel$cod_ibge), "\n")
  cat("Linhas duplicadas (cod_ibge x ano):",
      nrow(painel) - nrow(distinct(painel, cod_ibge, ano)), "\n")
  cat("\nCobertura por ano (municípios com dado em cada módulo)\n")
  print(cobertura, n = 40)
  cat("\nColunas do painel fora do dicionário:\n"); print(fora_dic)
  cat("\nVariáveis do dicionário que não estão no painel:\n"); print(dic_fora)
  if (is.null(comp_mape)) {
    cat("\nComparação com o mape_municipios: arquivo não encontrado em data/external/\n")
  } else {
    cat("\nComparação com o mape_municipios\n")
    cat("Município-ano só no painel:", nrow(comp_mape$so_no_painel), "\n")
    cat("Município-ano só no MAPE:", nrow(comp_mape$so_no_mape), "\n")
    cat("Colunas com o mesmo nome nas duas bases:\n"); print(comp_mape$colunas_repetidas)
  }
}, finally = sink())

# Salvar -----------------------------------------------------------------------
stopifnot(nrow(painel) == nrow(distinct(painel, cod_ibge, ano)))
write_parquet(painel, file.path(dir_output, "painel_politico_municipal.parquet"))
write_csv(painel,     file.path(dir_output, "painel_politico_municipal.csv.gz"))

message("Painel concluído: ", nrow(painel), " linhas e ", ncol(painel),
        " colunas. Veja output/diagnostico_painel.txt")
