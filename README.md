# painel_politico_municipal

Projeto pessoal e independente de Renan Suzano. Não é um produto oficial do MAPE. Reúne bases municipais pensadas para serem usadas junto com o [mape_municipios](https://mape.org.br/dados/) (MAPE/IESP-UERJ). O projeto não recoleta o que o mape_municipios já oferece: cobre só três lacunas.

| Módulo | Conteúdo | Por que não está no MAPE | Script |
|---|---|---|---|
| 1. Prefeito | Bloco ideológico (Bolognesi, Ribeiro e Codato), continuidade partidária, competição eleitoral, margem esquerda × não esquerda (RDD), ciclo do mandato, alinhamento com presidente e governador | O MAPE traz a votação por partido, mas não as variáveis derivadas | `R/01_prefeitos_tse.R` |
| 2. Câmara Municipal | Cadeiras por partido e bloco, número efetivo de partidos, bancada da coligação e da oposição, renovação da Câmara | O MAPE traz votos para vereador, não a composição em cadeiras | `R/02_camara_vereadores.R` |
| 3. Finanças 2000-2024 | FINBRA (2000-2012) harmonizado com o SICONFI (2013 em diante): receitas, FPM, gasto por função, pessoal, investimento, Legislativo | O SICONFI do MAPE começa em 2013 | `R/03_financas_finbra.R` |

`R/90_painel.R` junta os módulos num painel município × ano com as mesmas chaves do mape_municipios. `R/91_mapas.R` produz mapas municipais (Brasil ou por UF) de qualquer variável do painel, usando as malhas do `geobr`.

## Convenções

- **Chave de município:** `cod_ibge` com 7 dígitos (texto), compatível com o mape_municipios.
- **Tempo:** `ano` (civil) e `mandato` (por exemplo, `2001-2004`). A eleição de `t` define o governo de `t+1` a `t+4`.
- **Valores monetários:** em reais correntes (`_corr`) e em reais constantes, deflacionados pelo IPCA (`_def`).
- **Ideologia:** três blocos (esquerda, centro, direita). Partidos de centro-esquerda vão para a esquerda e os de centro-direita para a direita.

## Estrutura

```
R/                  scripts numerados, rodados em ordem
data/raw/           downloads brutos (fora do git)
data/external/      tabelas auxiliares versionadas (classificação ideológica, correspondência de contas)
data/processed/     bases limpas por módulo (fora do git)
docs/               metodologia e dicionário de variáveis
output/             painel final e mapas
```

## Como rodar

Antes da primeira execução, crie um arquivo `.Renviron` na pasta do projeto com o id do seu projeto no Google Cloud (usado pela Base dos Dados) e reinicie o R:

```
billing_project_id=SEU-ID
```

```r
source("R/00_setup.R")
source("R/01_prefeitos_tse.R")
source("R/02_camara_vereadores.R")   # depende do módulo 1
source("R/03_financas_finbra.R")
source("R/90_painel.R")
source("R/91_mapas.R")               # depende do painel
source("R/92_dados_site.R")          # dados do site
```

## Site e relatório

**Site** (`site/`): página única com filtro por UF, mapa interativo, tabelas, gráficos, dicionário e metodologia, publicada no GitHub Pages em https://renansuzano.github.io/painel_politico_municipal/.

```r
source("R/92_dados_site.R")   # gera site/dados/ a partir do painel
```

No Terminal, a partir da pasta do projeto:

```
cd site
quarto preview            # visualizar localmente
quarto publish gh-pages   # publicar (ramo gh-pages)
```

**Relatório** (`relatorio.qmd`): a mesma informação num único arquivo HTML, para enviar por e-mail. Abra no RStudio e clique em **Render**.

## Status

- [x] Estrutura do repositório e dicionário de variáveis planejado
- [x] Módulo 1 — prefeito (escrito e testado com dados simulados; falta rodar com os dados reais)
- [x] Módulo 2 — câmara (escrito e testado com dados simulados; falta rodar com os dados reais)
- [ ] Módulo 2 — completar lacunas da Base dos Dados com os dados abertos do TSE: vereadores de 2024 (55 municípios), coligações de suplementares e de 174 pleitos de 2008/2012 (ver docs/metodologia.md)
- [x] Módulo 3 — finanças 2000-2025 (escrito e testado com dados simulados; falta rodar com os dados reais)
- [x] Painel final (escrito e testado com dados simulados; falta rodar com os dados reais e comparar com uma cópia do mape_municipios)
- [x] Mapas: função genérica, mapa de transição entre mandatos e tabela de transições (testados com polígonos simulados)

## Autor

**Renan Suzano**  
Doutorando em Ciência Política no Instituto de Estudos Sociais e Políticos da Universidade do Estado do Rio de Janeiro (IESP-UERJ) e pesquisador do Laboratório de Monitoramento e Avaliação de Políticas e Eleições (MAPE/IESP-UERJ).

## Licença e citação

A definir: licença do código (sugestão MIT) e dos dados (sugestão CC BY 4.0).
