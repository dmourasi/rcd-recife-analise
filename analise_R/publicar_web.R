## -----------------------------------------------------------------------------
## publicar_web.R — Gera uma versão do relatório preparada para publicação web
##
## Diferenças em relação a `relatorio.qmd`:
##   * tema claro único (sem o alternador claro/escuro), porque a página passa a
##     ser hospedada dentro de outro documento, cujo próprio controle de tema
##     conflitaria com o do Quarto;
##   * fundo e texto travados no esquema claro, para que a página permaneça
##     legível mesmo quando o leitor estiver em modo escuro.
##
## Uso:  "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" publicar_web.R
## -----------------------------------------------------------------------------

r_alvo <- "C:/Program Files/R/R-4.5.3/bin/R.exe"
if (file.exists(r_alvo)) Sys.setenv(QUARTO_R = r_alvo)

quarto <- Sys.which("quarto")
if (!nzchar(quarto)) {
  candidatos <- c("C:/Program Files/Quarto/bin/quarto.exe",
                  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe")
  quarto <- candidatos[file.exists(candidatos)][1]
}
stopifnot(nzchar(quarto), !is.na(quarto))

## --- 1. Variante do documento com tema único --------------------------------
fonte <- readLines("relatorio.qmd", encoding = "UTF-8", warn = FALSE)

ini <- grep("^\\s*theme:\\s*$", fonte)[1]
stopifnot(!is.na(ini))
fim <- ini + 2L                                    # linhas "light:" e "dark:"
stopifnot(grepl("light:", fonte[ini + 1L]), grepl("dark:", fonte[fim]))
fonte <- append(fonte[-c(ini, ini + 1L, fim)], "    theme: cosmo", after = ini - 1L)

writeLines(fonte, "_web.qmd", useBytes = TRUE)
on.exit(unlink(c("_web.qmd", "_web.rmarkdown")), add = TRUE)

## --- 2. Renderização ---------------------------------------------------------
message("Renderizando versão web...")
status <- system2(quarto, c("render", "_web.qmd", "--to", "html"))
stopifnot(status == 0, file.exists("_web.html"))

## --- 3. Extração do conteúdo para o esqueleto do hospedeiro ------------------
## A página publicada é inserida dentro de um <html>/<head>/<body> fornecido
## pela plataforma, então entregamos apenas o conteúdo: os <style>/<script> do
## head (válidos no corpo) seguidos do conteúdo do body.
html <- paste(readLines("_web.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")

recorta <- function(txt, abre, fecha) {
  i <- regexpr(abre, txt)[1]
  j <- regexpr(fecha, txt)[1]
  substr(txt, i, j - 1)
}
cabeca <- recorta(html, "<head", "</head>")
corpo  <- substr(html, regexpr("<body[^>]*>", html)[1], regexpr("</body>", html)[1] - 1)
corpo  <- sub("^<body[^>]*>", "", corpo)

titulo <- regmatches(cabeca, regexpr("<title>.*?</title>", cabeca))
estilos <- regmatches(cabeca, gregexpr("<style[^>]*>.*?</style>", cabeca, perl = TRUE))[[1]]
scripts <- regmatches(cabeca, gregexpr("<script[^>]*>.*?</script>", cabeca, perl = TRUE))[[1]]
links   <- regmatches(cabeca, gregexpr("<link[^>]*>", cabeca))[[1]]
links   <- links[grepl("data:", links)]            # apenas recursos embutidos

## Trava o esquema claro: sem isto, um leitor em modo escuro veria o texto
## escuro do tema sobre o fundo escuro do hospedeiro.
trava_clara <- '<style>
:root, :root[data-theme="dark"], :root[data-theme="light"] {
  color-scheme: light !important;
  background-color: #ffffff !important;
}
body, #quarto-content, .page-columns {
  background-color: #ffffff !important;
  color: #262625 !important;
}
#quarto-content { padding: 1rem 1.25rem 4rem; }
img, .figure img { max-width: 100%; height: auto; }
.table-responsive, .cell-output-display, .kable_wrapper { overflow-x: auto; }
</style>'

saida <- c(titulo, links, estilos, trava_clara, scripts, corpo)
destino <- file.path(dirname(getwd()), "resultados_R", "relatorio_web.html")
writeLines(paste(saida, collapse = "\n"), destino, useBytes = TRUE)

message("Gerado: ", destino,
        " (", round(file.size(destino) / 1024^2, 2), " MB)")
