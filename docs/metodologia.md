# Notas metodológicas

Documento vivo: registrar aqui cada decisão tomada durante a construção.

## Escopo
O projeto cobre só o que o mape_municipios não oferece: variáveis políticas derivadas do prefeito, composição da Câmara em cadeiras e finanças de 2000 a 2012 harmonizadas com o SICONFI. Tudo o mais (convênios, MUNIC, saúde, educação, assistência) é lido do mape_municipios na análise.

## Unidade e chaves
- Unidade: município (`cod_ibge`, 7 dígitos) × ano civil.
- Variáveis eleitorais municipais são medidas por mandato e repetidas nos quatro anos de governo. A eleição de 2000 define os anos de 2001 a 2004.
- Municípios criados no período entram a partir do primeiro mandato próprio.

## Prefeito (módulo 1)
- Fonte: `br_tse_eleicoes.resultados_candidato` (Base dos Dados). `id_municipio` já vem no código IBGE de 7 dígitos, e as siglas vêm atualizadas para o nome mais recente (PRB aparece como REPUBLICANOS, PPS como CIDADANIA etc.).
- Pleito = município × `ano` × `tipo_eleicao`. Eleições suplementares são identificadas por `tipo_eleicao` diferente de "eleicao ordinaria".
- Linhas duplicadas na base (ocorrem em 2000) são removidas.
- Vencedor: `resultado == "eleito"` no último turno do pleito. Pleitos sem eleito são eleições anuladas (o mais votado teve o registro indeferido) e o mandato é preenchido por eleição suplementar; entre a anulação e a suplementar o município tem prefeito interino e fica NA. Pleitos com mais de um eleito são excluídos e listados no diagnóstico.
- Mandato: a eleição ordinária do ano `t` elege o mandato `t+1` a `t+4`; a suplementar completa o mandato vigente na data da eleição.
- Painel anual: em cada ano vale o prefeito em exercício em 1º de julho. Eleito em suplementar entra na data da eleição (a posse real é algumas semanas depois).
- Percentuais de voto calculados sobre a soma dos votos dados a candidatos no turno decisivo (proxy de votos válidos).
- Pessoa entre eleições: compara o título de eleitor quando as duas eleições o têm; senão, o nome normalizado (sem acentos, maiúsculas). O título vem vazio em 2000.
- `reeleito` e `segundo_mandato`: o prefeito é a mesma pessoa do governo imediatamente anterior (inclui quem assumiu por suplementar, que conta como primeiro mandato).
- Presidente: tabela própria (`data/external/presidentes.csv`), referência em 1º de julho. Em 2016 vale Temer (interino desde maio). Bolsonaro sem partido em 2020-2021: alinhamento por partido fica NA; por bloco, direita.
- Governador: eleito no último turno da eleição geral; trocas no meio do mandato (renúncia, cassação) não são captadas.
- Ano 2000 (mandato 1997-2000) fica sem dados de prefeito: a Base dos Dados tem a eleição de 1996 só parcialmente.
- Fonte: Bolognesi, Ribeiro e Codato (2023), "Uma nova classificação ideológica dos partidos políticos brasileiros", DADOS 66(2), Tabela 1 (médias de survey com especialistas, UFPR/ABCP, 2018).
- Cortes do artigo (p. 16): extrema-esquerda 0–1,5; esquerda 1,51–3; centro-esquerda 3,01–4,49; centro 4,5–5,5; centro-direita 5,51–7; direita 7,01–8,5; extrema-direita 8,51–10.
- Três blocos: extrema-esquerda, esquerda e centro-esquerda → esquerda; centro → centro; centro-direita, direita e extrema-direita → direita.
- Resultado: esquerda = PSTU, PCO, PCB, PSOL, PCdoB, PT, PDT, PSB; centro = Rede, PPS/Cidadania, PV; todo o resto é direita, incluindo MDB (7,01), PSD e PSDB.
- Siglas históricas herdam a nota do partido sucessor (PMDB → MDB, PFL → DEM, PPB → Progressistas etc.); cada caso está anotado em `classificacao_bolognesi.csv`.
- Sem nota no artigo: União Brasil e PRD recebem bloco direita (os partidos de origem são todos de direita); PAN herda do PTB e PST e PGT herdam do PR (partidos que os incorporaram); PSN e UP ficam sem classificação.
- Limitação: a classificação é de 2018 e é aplicada a 2000–2024. O caso mais sensível é o MDB, que o próprio artigo (nota 7) registra como centro na maior parte da literatura anterior. Prever teste de robustez com o MDB como centro antes de 2016.
- Ambiguidade de sigla: "PL" designa o partido antigo (até 2006) e o ex-PR (2019 em diante). Tratar pelo ano da eleição.
- Eleições suplementares: vale o prefeito que de fato governou, com flag.
- Eleições apertadas (RDD): `margem_esquerda` compara o candidato de esquerda e o adversário de fora da esquerda mais votados. Usa o segundo turno quando houve; senão, o primeiro. Só existe nos municípios em que a disputa principal foi esquerda × não esquerda (`disputa_esq_naoesq` = 1). Variante a testar: margem esquerda × direita, excluindo o centro.
- Ciclo do mandato: `segundo_mandato` identifica o prefeito que não pode concorrer de novo; `ano_mandato` e `ano_eleitoral` controlam o ciclo político-orçamentário.
- Alinhamento: calculado por ano, porque presidente e governador mudam no meio do mandato municipal (posse em janeiro de 2003, 2007, 2011...).

## Câmara (módulo 2)
- Composição na eleição, sem migração partidária posterior.
- Número efetivo de partidos: Laakso e Taagepera (1979).
- Renovação: a identificação do mesmo vereador entre eleições é o ponto frágil (CPF ou título quando disponível; senão, nome + município com checagem manual de uma amostra).
- O tamanho das Câmaras mudou por regra legal (Resolução TSE de 2004 e EC 58/2009); controlar por `n_cadeiras`.

## Finanças (módulo 3)
- Quebra de série em 2013 com o SICONFI/PCASP: correspondência de contas documentada em `data/external/correspondencia_contas.csv` e teste da descontinuidade (2011-2014).
- Valores em reais correntes e deflacionados pelo IPCA.
- Municípios sem declaração no ano ficam `NA`, com flag, e não viram zero.

## Pendências
- [ ] Confirmar no dicionário do mape_municipios que as variáveis dos módulos 1 e 2 não existem lá
- [ ] Decidir se o SICONFI 2013+ é lido do MAPE ou recoletado
- [ ] Licença dos dados e do código
- [ ] Forma de citação
