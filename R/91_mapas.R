# 91_mapas.R — mapas municipais a partir do painel ----------------------------
# Malhas: geobr (IPEA). Saída: output/mapas/*.png
# Depende do painel (R/90_painel.R). Pacotes extras: geobr, sf.

source(here::here("R", "00_setup.R"))
for (p in c("geobr", "sf")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(geobr)
library(sf)

dir_mapas <- file.path(dir_output, "mapas")
dir.create(dir_mapas, showWarnings = FALSE, recursive = TRUE)

painel <- read_parquet(file.path(dir_output, "painel_politico_municipal.parquet"))

# Malhas (baixadas uma vez e guardadas em data/raw) ---------------------------
arq_malha <- file.path(dir_raw, "malha_municipios_2020.rds")
arq_ufs   <- file.path(dir_raw, "malha_ufs_2020.rds")
if (!file.exists(arq_malha)) {
  read_municipality(year = 2020, simplified = TRUE, showProgress = FALSE) |>
    mutate(cod_ibge = as.character(code_muni)) |>
    select(cod_ibge, abbrev_state) |>
    saveRDS(arq_malha)
}
if (!file.exists(arq_ufs)) {
  read_state(year = 2020, simplified = TRUE, showProgress = FALSE) |>
    saveRDS(arq_ufs)
}
malha <- readRDS(arq_malha)
ufs   <- readRDS(arq_ufs)

# Paletas ---------------------------------------------------------------------
cores_bloco <- c(esquerda = "#c0392b", centro = "#f1c40f", direita = "#2c3e50")
cores_transicao <- c(
  "segue na esquerda"      = "#f1a99b",   # vermelho claro
  "saiu da esquerda"       = "#2b6cb0",   # azul (transição)
  "entrou na esquerda"     = "#c0392b",   # vermelho forte (transição)
  "segue fora da esquerda" = "#e2e8f0"    # cinza claro
)

# Desenha e salva -------------------------------------------------------------
desenha_mapa <- function(base, contorno, titulo, subtitulo, cores = NULL,
                         arquivo = NULL) {
  g <- ggplot(base) +
    geom_sf(aes(fill = valor), color = NA) +
    geom_sf(data = contorno, fill = NA, color = "grey30", linewidth = 0.2) +
    labs(title = titulo, subtitle = subtitulo, fill = NULL,
         caption = "Elaboração própria; malhas: geobr/IBGE") +
    theme_void() +
    theme(legend.position = "bottom")

  if (is.numeric(base$valor)) {
    g <- g + scale_fill_viridis_c(na.value = "grey90")
  } else if (!is.null(cores)) {
    g <- g + scale_fill_manual(values = cores, na.value = "grey90", drop = FALSE)
  } else {
    g <- g + scale_fill_brewer(palette = "Set2", na.value = "grey90")
  }

  if (!is.null(arquivo)) ggsave(arquivo, g, width = 8, height = 8, dpi = 300, bg = "white")
  g
}

recorte <- function(base, uf) {
  if (is.null(uf)) list(base = base, contorno = ufs)
  else list(base = filter(base, abbrev_state %in% uf),
            contorno = filter(ufs, abbrev_state %in% uf))
}

sufixo_uf <- function(uf) if (is.null(uf)) "" else paste0("_", paste(uf, collapse = "-"))

# 1. Mapa de qualquer variável do painel num ano ------------------------------
mapa_municipal <- function(var, ano_ref, uf = NULL, titulo = var, salvar = TRUE) {
  dados <- painel |>
    filter(ano == ano_ref) |>
    select(cod_ibge, valor = all_of(var))
  r <- recorte(left_join(malha, dados, by = "cod_ibge"), uf)

  cores <- if (startsWith(var, "bloco")) cores_bloco else NULL
  if (!is.null(cores)) r$base$valor <- factor(r$base$valor, levels = names(cores))

  desenha_mapa(r$base, r$contorno, titulo, ano_ref, cores,
               if (salvar) file.path(dir_mapas, paste0(var, "_", ano_ref, sufixo_uf(uf), ".png")))
}

# 2. Transição entre dois mandatos (grupos do diferenças em diferenças) ------
# Compara o prefeito em exercício no 1º ano de cada mandato.
# Ex.: mapa_transicao(2013, 2017) compara 2013-2016 com 2017-2020.
classifica_transicao <- function(inicio_a, inicio_b) {
  painel |>
    filter(ano %in% c(inicio_a, inicio_b)) |>
    select(cod_ibge, ano, esquerda) |>
    tidyr::pivot_wider(names_from = ano, values_from = esquerda,
                       names_prefix = "esq_") |>
    transmute(
      cod_ibge,
      valor = case_when(
        .data[[paste0("esq_", inicio_a)]] == 1 & .data[[paste0("esq_", inicio_b)]] == 1 ~ "segue na esquerda",
        .data[[paste0("esq_", inicio_a)]] == 1 & .data[[paste0("esq_", inicio_b)]] == 0 ~ "saiu da esquerda",
        .data[[paste0("esq_", inicio_a)]] == 0 & .data[[paste0("esq_", inicio_b)]] == 1 ~ "entrou na esquerda",
        .data[[paste0("esq_", inicio_a)]] == 0 & .data[[paste0("esq_", inicio_b)]] == 0 ~ "segue fora da esquerda"
      ),
      valor = factor(valor, levels = names(cores_transicao))
    )
}

mapa_transicao <- function(inicio_a, inicio_b, uf = NULL, salvar = TRUE) {
  dados <- classifica_transicao(inicio_a, inicio_b)
  r <- recorte(left_join(malha, dados, by = "cod_ibge"), uf)
  subt <- paste0(inicio_a, "-", inicio_a + 3, " para ", inicio_b, "-", inicio_b + 3)
  desenha_mapa(r$base, r$contorno, "Transição do prefeito em relação à esquerda", subt,
               cores_transicao,
               if (salvar) file.path(dir_mapas, paste0("transicao_", inicio_a, "_", inicio_b,
                                                       sufixo_uf(uf), ".png")))
}

# Contagem dos grupos de transição em todos os pares de mandatos consecutivos
tabela_transicoes <- function(inicios = c(2001, 2005, 2009, 2013, 2017, 2021, 2025)) {
  purrr::map2_dfr(head(inicios, -1), tail(inicios, -1), \(a, b)
    classifica_transicao(a, b) |>
      count(grupo = valor) |>
      mutate(transicao = paste0(a, "-", a + 3, " -> ", b, "-", b + 3))) |>
    tidyr::pivot_wider(names_from = grupo, values_from = n) |>
    relocate(transicao)
}

# Exemplos -------------------------------------------------------------------
# Tabela dos grupos de transição em cada par de mandatos
#   tabela_transicoes() |> print(width = 200)
#
# Bloco ideológico do prefeito no 1º ano de cada mandato (Brasil)
#   for (a in c(2001, 2005, 2009, 2013, 2017, 2021, 2025))
#     mapa_municipal("bloco_prefeito", a, titulo = "Bloco ideológico do prefeito")
#
# Transição 2013-2016 -> 2017-2020, Brasil e Rio de Janeiro
#   mapa_transicao(2013, 2017)
#   mapa_transicao(2013, 2017, uf = "RJ")
#
# Câmara e finanças no Rio de Janeiro
#   mapa_municipal("nep_cadeiras",     2017, uf = "RJ", titulo = "Número efetivo de partidos na Câmara")
#   mapa_municipal("autonomia_fiscal", 2019, uf = "RJ", titulo = "Autonomia fiscal")
