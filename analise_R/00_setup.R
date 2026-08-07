## -----------------------------------------------------------------------------
## 00_setup.R — Configuração do ambiente e parâmetros globais da análise
## Artigo: "The Geometry of Disposal" (RCD, Recife/PE)
## -----------------------------------------------------------------------------

## Biblioteca de usuário (evita erro de permissão em C:/Program Files)
.lib_usuario <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.5")
if (dir.exists(.lib_usuario)) .libPaths(c(.lib_usuario, .libPaths()))

## Pandoc distribuído com o Quarto/RStudio (necessário para renderizar o .Rmd)
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC"))) {
  .cand <- c("C:/Program Files/Quarto/bin/tools",
             "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
  .ok <- .cand[file.exists(file.path(.cand, "pandoc.exe"))]
  if (length(.ok)) Sys.setenv(RSTUDIO_PANDOC = .ok[1])
}

pacotes <- c("sf", "spdep", "readxl", "dplyr", "tidyr", "tibble", "purrr",
             "ggplot2", "scales", "knitr", "kableExtra", "moments", "e1071",
             "nortest", "FSA", "DescTools", "spatstat.geom", "spatstat.explore")

faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando)) {
  message("Instalando: ", paste(faltando, collapse = ", "))
  install.packages(faltando, lib = .lib_usuario, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages(invisible(lapply(pacotes, library, character.only = TRUE)))

## -----------------------------------------------------------------------------
## Parâmetros globais
## -----------------------------------------------------------------------------
PARAMS <- list(
  ## Diretórios (relativos à raiz do projeto)
  dir_dados   = "..",                 # planilhas .xlsx do estudo
  dir_geo     = "../dados_geo",       # camadas oficiais (GeoJSON/CSV)
  dir_saida   = "../resultados_R",

  ## Sistemas de referência
  crs_geo = 4326,                     # WGS84 (coordenadas coletadas em campo)
  crs_utm = 31985,                    # SIRGAS 2000 / UTM 25S — métrico, para distâncias

  ## Limpeza de coordenadas
  tol_borda_m = 200,                  # tolerância à borda municipal (imprecisão de GPS)

  ## Classes de distância (m) — Tabela 1 do manuscrito
  cortes_equip = c(20, 40, 80, 160),  # saúde e educação
  cortes_agua  = c(20, 40, 60, 120),  # recursos hídricos

  ## Escores da Tabela 1 e limites de classe da Tabela 2
  escores      = c(10, 7.5, 5, 2.5, 0.5),
  limites_sf   = c(0, 7.5, 15, 22.5, 30),
  classes_sf   = c("I (baixo)", "II (médio)", "III (alto)", "IV (muito alto)"),

  ## Inferência
  alfa         = 0.05,
  n_perm       = 999,                 # permutações para Moran's I
  semente      = 42
)

set.seed(PARAMS$semente)
dir.create(PARAMS$dir_saida, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------------
## Funções auxiliares de formatação
## -----------------------------------------------------------------------------

## Formata p-valores no padrão de periódico científico
fmt_p <- function(p, digitos = 3) {
  ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = digitos, decimal.mark = ","))
}

## Formata números no padrão brasileiro (vírgula decimal, ponto de milhar)
fmt_n <- function(x, digitos = 2) {
  formatC(x, format = "f", digits = digitos, big.mark = ".", decimal.mark = ",")
}

## Intervalo de confiança de Wilson para uma proporção
ic_wilson <- function(k, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  d <- 1 + z^2 / n
  centro <- (p + z^2 / (2 * n)) / d
  margem <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(inferior = 100 * (centro - margem), superior = 100 * (centro + margem))
}

## Cohen's w — tamanho de efeito para qui-quadrado de aderência
cohen_w <- function(chi2, n) sqrt(chi2 / n)

## Interpretação de Cohen's w segundo Cohen (1988)
interp_w <- function(w) {
  dplyr::case_when(w < 0.10 ~ "desprezível", w < 0.30 ~ "pequeno",
                   w < 0.50 ~ "médio", TRUE ~ "grande")
}

## Bloco descritivo completo de uma variável quantitativa
descrever <- function(x, nome = "variável") {
  x <- x[is.finite(x)]
  n <- length(x)
  ep_assim <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  ep_curt  <- 2 * ep_assim * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
  tibble::tibble(
    Variável  = nome,
    n         = n,
    Média     = mean(x),
    `DP`      = sd(x),
    `EP`      = sd(x) / sqrt(n),
    `IC95 inf`= mean(x) - qt(0.975, n - 1) * sd(x) / sqrt(n),
    `IC95 sup`= mean(x) + qt(0.975, n - 1) * sd(x) / sqrt(n),
    Mediana   = median(x),
    Q1        = quantile(x, 0.25, names = FALSE),
    Q3        = quantile(x, 0.75, names = FALSE),
    IQR       = IQR(x),
    Mínimo    = min(x),
    Máximo    = max(x),
    `CV (%)`  = 100 * sd(x) / mean(x),
    Assimetria = moments::skewness(x),
    `EP assim.` = ep_assim,
    Curtose   = moments::kurtosis(x) - 3,   # excesso de curtose
    `EP curt.` = ep_curt
  )
}

## Distância de cada feição de `origem` à feição mais próxima de `destino`.
## Definida aqui porque tanto a preparação dos dados quanto as simulações
## espaciais do relatório precisam dela.
dist_min <- function(origem, destino) {
  idx <- sf::st_nearest_feature(origem, destino)
  as.numeric(sf::st_distance(origem, destino[idx, ], by_element = TRUE))
}

## -----------------------------------------------------------------------------
## Modelo de risco (Tabelas 1 e 2 do manuscrito)
## Definidas aqui — e não em 01_dados.R — porque o relatório também as usa na
## análise de sensibilidade, que roda mesmo quando os dados vêm do cache.
## -----------------------------------------------------------------------------

## Converte distância contínua em classe de distância
classificar <- function(d, cortes) {
  rotulos <- c(paste0("0–", cortes[1]),
               paste0(utils::head(cortes, -1), "–", cortes[-1]),
               paste0("> ", cortes[length(cortes)]))
  cut(d, breaks = c(-Inf, cortes, Inf), labels = rotulos, right = TRUE)
}

## Converte distância contínua no escore de risco correspondente
pontuar <- function(d, cortes, escores = PARAMS$escores) {
  escores[as.integer(cut(d, breaks = c(-Inf, cortes, Inf), right = TRUE))]
}

## Tema gráfico consistente para todas as figuras.
## O fundo é branco explícito (não transparente): assim as figuras permanecem
## legíveis também quando o relatório é lido no tema escuro, onde um fundo
## transparente deixaria eixos e rótulos escuros sobre superfície escura.
tema_artigo <- function(base = 10) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e5e5e5", linewidth = 0.3),
      axis.title       = ggplot2::element_text(colour = "#333333"),
      axis.text        = ggplot2::element_text(colour = "#333333"),
      strip.text       = ggplot2::element_text(colour = "#333333", face = "bold"),
      plot.title       = ggplot2::element_text(face = "bold", size = base + 1,
                                               colour = "#1a1a19"),
      plot.subtitle    = ggplot2::element_text(colour = "#767676", size = base - 1),
      plot.caption     = ggplot2::element_text(colour = "#767676", size = base - 2, hjust = 0),
      legend.position  = "bottom"
    )
}

COR <- list(primaria = "#2a78d6", destaque = "#eb6834", escura = "#123f78",
            neutra = "#767676", grade = "#e5e5e5",
            sequencial = c("#f4f8fd", "#c7dcf4", "#7fb0e6", "#2a78d6", "#123f78"))

message("Setup concluído — ", length(pacotes), " pacotes carregados.")
