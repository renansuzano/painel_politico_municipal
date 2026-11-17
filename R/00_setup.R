# 00_setup.R — pacotes, caminhos e funções auxiliares ------------------------

pacotes <- c(
  "tidyverse",    # manipulação geral
  "janitor",      # limpeza de nomes
  "arrow",        # leitura/escrita em parquet
  "basedosdados", # SICONFI e TSE já tratados (BigQuery)
  "readxl",       # planilhas da MUNIC
  "deflateBR",    # deflação pelo IPCA
  "here"
)

faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando) > 0) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

# Projeto de faturamento do BigQuery (Base dos Dados)
# O id fica fora do código, no arquivo .Renviron da pasta do projeto (que não
# vai para o GitHub), com a linha:  billing_project_id=SEU-ID
# Depois de criar o arquivo, reinicie o R (Session > Restart R).
if (nzchar(Sys.getenv("billing_project_id"))) {
  basedosdados::set_billing_id(Sys.getenv("billing_project_id"))
}

# Caminhos ------------------------------------------------------------------
# Falha com mensagem clara se o projeto não foi aberto pelo .Rproj
if (!file.exists(here("painel_politico_municipal.Rproj"))) {
  stop("Projeto não encontrado em ", here(), ".\n",
       "Abra o arquivo painel_politico_municipal.Rproj (File > Open Project) ",
       "e rode o script de novo.", call. = FALSE)
}
dir_raw       <- here("data", "raw")
dir_external  <- here("data", "external")
dir_processed <- here("data", "processed")
dir_output    <- here("output")

# Parâmetros ----------------------------------------------------------------
anos_eleicao <- seq(2000, 2024, by = 4)
anos_painel  <- 2000:2024

# Funções auxiliares ----------------------------------------------------------

# Padroniza o código IBGE para 7 dígitos (texto)
padroniza_cod_ibge <- function(x) {
  x <- gsub("\\D", "", as.character(x))
  stopifnot(all(nchar(x) %in% c(6, 7) | is.na(x)))
  x  # TODO: converter códigos de 6 dígitos para 7 via tabela de municípios
}

# Mandato associado a um ano de governo
ano_para_mandato <- function(ano) {
  inicio <- ano - ((ano - 1) %% 4)
  paste0(inicio, "-", inicio + 3)
}

# Normaliza sigla partidária do TSE (maiúsculas, sem espaços nem acentos)
# Ex.: "PC do B" -> "PCDOB"; "UNIÃO" -> "UNIAO"
normaliza_sigla <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    gsub(pattern = "[^A-Z]", replacement = "")
}

# Tabela de classificação (Bolognesi, Ribeiro e Codato, 2023)
classificacao <- read_csv2(file.path(dir_external, "classificacao_bolognesi.csv"),
                           show_col_types = FALSE, na = c("", "NA"))

# Junta a classificação a uma base com coluna de sigla
# Retorna media_ideologia, classificacao_original e bloco
classifica_partido <- function(df, col_sigla = "sigla_partido") {
  df |>
    mutate(.sigla_norm = normaliza_sigla(.data[[col_sigla]])) |>
    left_join(classificacao |> select(-observacao),
              by = c(".sigla_norm" = "sigla_tse")) |>
    select(-.sigla_norm)
}

# Checagem: siglas da base sem bloco (fora da tabela ou com bloco vazio)
siglas_sem_classificacao <- function(df, col_sigla = "sigla_partido") {
  com_bloco <- classificacao$sigla_tse[!is.na(classificacao$bloco)]
  sort(setdiff(unique(normaliza_sigla(df[[col_sigla]])), com_bloco))
}
