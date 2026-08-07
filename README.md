# Análise estatística — descarte irregular de RCD em Recife

Relatório técnico-estatístico e pipeline reprodutível do estudo
*The Geometry of Disposal: Spatial Representation of Construction and Demolition
Waste (CDW) Disposal Points in Recife, Brazil*.

**📊 [Ler o relatório completo](https://dmourasi.github.io/rcd-recife-analise/)**

## O que o relatório cobre

Da auditoria dos dados brutos às análises espaciais confirmatórias e aos testes
de robustez, cada seção documenta **o que foi feito**, **por que aquele método
foi escolhido**, **o resultado** e **como interpretá-lo**.

| Parte | Conteúdo |
|---|---|
| I | Auditoria e preparação dos dados |
| II | Estatística descritiva (medidas de posição, dispersão, forma; normalidade; distribuição acumulada) |
| III | Inferência sobre distribuições de frequência (qui-quadrado, resíduos, tamanhos de efeito, IC de Wilson) |
| IV | Análise espacial (Clark & Evans, função L de Ripley, simulação de aleatoriedade completa, I de Moran, Getis-Ord Gi*, densidade Kernel) |
| V | Escore de risco ambiental (Kruskal-Wallis, Dunn, redundância entre critérios) |
| VI | Robustez (bootstrap BCa, sensibilidade dos pesos, sensibilidade às decisões de limpeza) |

## Reprodutibilidade

```
Rscript analise_R/renderizar.R
```

O pipeline está em quatro arquivos com responsabilidades separadas:

| Arquivo | Papel |
|---|---|
| `analise_R/00_setup.R` | Pacotes, parâmetros globais (CRS, cortes de classe, escores, α, semente) e funções de formatação |
| `analise_R/01_dados.R` | Leitura, auditoria/correção de coordenadas, integração espacial, escore de risco |
| `analise_R/02_analises.R` | Uma função por método estatístico |
| `analise_R/relatorio.qmd` | Texto do relatório |

Requisitos: R ≥ 4.5, Quarto ≥ 1.4 e os pacotes `sf`, `spdep`, `spatstat.explore`,
`readxl`, `dplyr`, `tidyr`, `ggplot2`, `moments`, `nortest`, `FSA`, `DescTools`,
`knitr`, `kableExtra` (o `00_setup.R` instala o que faltar).

## Dados

As **planilhas de campo e as coordenadas dos pontos não estão neste repositório**
enquanto o artigo não for publicado; o relatório apresenta todas as tabelas
agregadas. As camadas geográficas usadas são públicas, do
[Portal de Dados Abertos da Prefeitura do Recife](https://dados.recife.pe.gov.br)
(licença ODbL): RPAs, bairros, hidrografia, rede municipal de saúde, escolas
municipais e ecoestações.

> **Nota metodológica.** As camadas de equipamentos públicos do portal não são
> idênticas às usadas no manuscrito (132 unidades de saúde contra 156; 342
> escolas contra 217; 8 ecoestações que recebem RCD contra 16). As análises de
> distância devem ser refeitas quando as camadas originais forem
> disponibilizadas — basta substituir os arquivos em `dados_geo/`, apagar o
> cache e renderizar novamente.

## Licença

Código sob licença MIT. O conteúdo do relatório integra manuscrito em preparação;
cite os autores do estudo ao reutilizar resultados.
