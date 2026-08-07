## -----------------------------------------------------------------------------
## renderizar.R — Gera o relatório completo a partir de relatorio.qmd
##
## Uso: abra este arquivo no RStudio e execute, ou rode no terminal:
##   "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" renderizar.R
## -----------------------------------------------------------------------------

setwd(dirname(normalizePath(sys.frame(1)$ofile %||% "renderizar.R")))

`%||%` <- function(a, b) if (is.null(a)) b else a

## O Quarto precisa apontar para a instalação de R que tem os pacotes.
## Sem isto, ele usa a versão mais recente encontrada no sistema.
r_alvo <- "C:/Program Files/R/R-4.5.3/bin/R.exe"
if (file.exists(r_alvo)) Sys.setenv(QUARTO_R = r_alvo)

quarto <- Sys.which("quarto")
if (!nzchar(quarto)) {
  candidatos <- c("C:/Program Files/Quarto/bin/quarto.exe",
                  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe")
  quarto <- candidatos[file.exists(candidatos)][1]
}
if (is.na(quarto) || !nzchar(quarto)) {
  stop("Quarto não encontrado. Instale em https://quarto.org/docs/download/")
}

message("Renderizando relatório...")
status <- system2(quarto, c("render", "relatorio.qmd", "--to", "html"))

if (status == 0) {
  destino <- normalizePath("relatorio.html")
  message("Concluído: ", destino)
  ## Copia o relatório para a pasta de resultados do projeto
  dir.create("../resultados_R", showWarnings = FALSE)
  file.copy(destino, "../resultados_R/Relatorio_Estatistico.html", overwrite = TRUE)
  message("Cópia em: resultados_R/Relatorio_Estatistico.html")
} else {
  stop("Falha na renderização (código ", status, ").")
}
