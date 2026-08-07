## -----------------------------------------------------------------------------
## 01_dados.R — Leitura, auditoria, limpeza e integração espacial dos dados
##
## Produz o objeto `DADOS`, uma lista com todos os insumos já tratados que o
## relatório (relatorio.Rmd) consome. Rodar após 00_setup.R.
## -----------------------------------------------------------------------------

## =============================================================================
## 1. LEITURA DAS PLANILHAS DO ESTUDO
## =============================================================================

## As planilhas não têm cabeçalho estruturado: os dados começam em linhas
## variáveis e há colunas vazias à esquerda. Lemos sem cabeçalho e localizamos
## os valores por tipo, o que torna a leitura robusta a deslocamentos de célula.

ler_coordenadas <- function(caminho) {
  bruto <- readxl::read_excel(caminho, col_names = FALSE, .name_repair = "minimal")
  num   <- suppressWarnings(lapply(bruto, function(col) as.numeric(as.character(col))))
  ## As duas colunas com maior quantidade de números válidos são lat e lon
  validos <- vapply(num, function(x) sum(is.finite(x)), integer(1))
  idx     <- order(validos, decreasing = TRUE)[1:2]
  idx     <- sort(idx)
  out     <- data.frame(lat = num[[idx[1]]], lon = num[[idx[2]]])
  out[is.finite(out$lat) & is.finite(out$lon), , drop = FALSE]
}

pontos_bruto <- ler_coordenadas(file.path(PARAMS$dir_dados, "TODOS OS PONTOS.xlsx"))
N_BRUTO <- nrow(pontos_bruto)

## Distribuições por classe de distância já tabuladas pelos autores (Tabela X1/X2
## do manuscrito) — usadas para reproduzir e conferir os testes já publicados.
TAB_MANUSCRITO <- list(
  saude    = c(`0-20` = 0,  `20-40` = 3,  `40-80` = 13, `80-160` = 51,  `>160` = 1241),
  educacao = c(`0-20` = 1,  `20-40` = 8,  `40-80` = 21, `80-160` = 53,  `>160` = 1225),
  agua     = c(`0-20` = 64, `20-40` = 72, `40-80` = 50, `80-120` = 110, `>120` = 1012),
  rpa      = c(`1` = 148, `2` = 113, `3` = 238, `4` = 398, `5` = 143, `6` = 268)
)

## Faixas de renda domiciliar (RENDA E PONTOS.xlsx)
RENDA <- tibble::tibble(
  faixa = factor(c("Sem dados", "0–255", "255–510", "510–1.020", "1.020–1.530",
                   "1.530–2.550", "2.550–5.100", "5.100–7.650", "> 7.650"),
                 levels = c("Sem dados", "0–255", "255–510", "510–1.020", "1.020–1.530",
                            "1.530–2.550", "2.550–5.100", "5.100–7.650", "> 7.650")),
  n = c(8, 45, 216, 314, 189, 273, 261, 2, 0)
)

## Número de bairros por RPA (caracterização da área, manuscrito)
BAIRROS_POR_RPA <- c(11, 17, 25, 12, 16, 8)

## =============================================================================
## 2. CAMADAS GEOGRÁFICAS OFICIAIS
## =============================================================================
## Fonte: Portal de Dados Abertos da Prefeitura do Recife (ODbL).

ler_camada <- function(arquivo) {
  sf::st_read(file.path(PARAMS$dir_geo, arquivo), quiet = TRUE) |>
    sf::st_transform(PARAMS$crs_utm) |>
    sf::st_make_valid()
}

rpa      <- ler_camada("rpa.geojson") |> dplyr::arrange(RPA)
rpa$area_km2 <- as.numeric(sf::st_area(rpa)) / 1e6
bairros  <- ler_camada("bairros.geojson")
bairros$area_km2 <- as.numeric(sf::st_area(bairros)) / 1e6
hidrico  <- ler_camada("hidrico.geojson")
saude    <- ler_camada("saude.geojson")
escolas  <- ler_camada("escolas.geojson")

municipio <- sf::st_union(rpa)

## Ecoestações / pontos de entrega voluntária.
## Atenção ao formato: o arquivo usa ";" como separador mas "." como decimal —
## `read.csv2()` (que assume decimal ",") transformaria as coordenadas em NA.
eco_bruto <- utils::read.csv(file.path(PARAMS$dir_geo, "ecoestacoes.csv"),
                             sep = ";", dec = ".", fileEncoding = "UTF-8",
                             stringsAsFactors = FALSE)
names(eco_bruto) <- trimws(names(eco_bruto))
eco_bruto$latitude  <- as.numeric(eco_bruto$latitude)
eco_bruto$longitude <- as.numeric(eco_bruto$longitude)
eco_bruto <- eco_bruto[is.finite(eco_bruto$latitude) & is.finite(eco_bruto$longitude), ]
eco_bruto <- eco_bruto[!duplicated(eco_bruto[c("latitude", "longitude")]), ]
stopifnot(nrow(eco_bruto) > 0)

para_sf <- function(df, lon = "longitude", lat = "latitude") {
  sf::st_as_sf(df, coords = c(lon, lat), crs = PARAMS$crs_geo) |>
    sf::st_transform(PARAMS$crs_utm)
}
eco_todos <- para_sf(eco_bruto)
eco_rcc   <- eco_todos[grepl("Constru", eco_todos$tiporesiduo), ]

## =============================================================================
## 3. AUDITORIA E CORREÇÃO DAS COORDENADAS
## =============================================================================
## Procedimento determinístico: para cada coordenada fora do município, testamos
## uma lista fixa de correções tipográficas plausíveis, na ordem, e aceitamos a
## primeira que reposiciona o ponto dentro do território (com tolerância de
## `tol_borda_m`). Nenhum ponto é movido "para o lugar mais próximo": ou existe
## uma correção tipográfica que o traz para dentro, ou ele é excluído.

dist_ao_municipio <- function(lat, lon) {
  p <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = PARAMS$crs_geo) |>
    sf::st_transform(PARAMS$crs_utm)
  as.numeric(sf::st_distance(p, municipio)[1, 1])
}

candidatos_correcao <- function(lat, lon) {
  cand <- list(list(lat = lat, lon = lon, tipo = "sem correção"))
  ## Deslocamento decimal na latitude (ex.: -8059635 -> -8,059635)
  if (abs(lat) > 1e5) cand <- c(cand, list(list(lat = lat / 1e6, lon = lon, tipo = "decimal na latitude")))
  if (abs(lat) > 70 && abs(lat) < 90) cand <- c(cand, list(list(lat = lat / 10, lon = lon, tipo = "decimal na latitude")))
  ## Sinal invertido na longitude (hemisfério trocado)
  if (lon > 0) cand <- c(cand, list(list(lat = lat, lon = -lon, tipo = "sinal da longitude")))
  ## Dígito "4" digitado no lugar de "8" na 1ª decimal da longitude (erro sistemático)
  cand <- c(cand, list(list(lat = lat, lon = lon - 0.4, tipo = "dígito 4/8 na longitude")))
  ## Dígito faltando na longitude (ex.: -3,4921 -> -34,921)
  if (lon > -4 && lon < -3) cand <- c(cand, list(list(lat = lat, lon = lon * 10, tipo = "dígito ausente na longitude")))
  cand
}

auditar <- function(df) {
  res <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    escolhido <- NULL
    for (cd in candidatos_correcao(df$lat[i], df$lon[i])) {
      if (!is.finite(cd$lat) || !is.finite(cd$lon)) next
      if (cd$lat > 0 || cd$lat < -90 || cd$lon > 0 || cd$lon < -180) next
      if (dist_ao_municipio(cd$lat, cd$lon) <= PARAMS$tol_borda_m) { escolhido <- cd; break }
    }
    res[[i]] <- if (is.null(escolhido)) {
      data.frame(lat = df$lat[i], lon = df$lon[i], correcao = "excluído (irrecuperável)")
    } else {
      data.frame(lat = escolhido$lat, lon = escolhido$lon, correcao = escolhido$tipo)
    }
  }
  cbind(dplyr::bind_rows(res),
        lat_original = df$lat, lon_original = df$lon)
}

auditoria <- auditar(pontos_bruto)

AUDITORIA_RESUMO <- auditoria |>
  dplyr::count(correcao, name = "n") |>
  dplyr::arrange(dplyr::desc(n)) |>
  dplyr::mutate(`%` = 100 * n / sum(n))

pontos <- auditoria |>
  dplyr::filter(correcao != "excluído (irrecuperável)") |>
  dplyr::mutate(id = dplyr::row_number())

N_VALIDO      <- nrow(pontos)
N_EXCLUIDO    <- N_BRUTO - N_VALIDO
N_CORRIGIDO   <- sum(!pontos$correcao %in% "sem correção")
N_DUPLICADO   <- sum(duplicated(pontos[c("lat", "lon")]))
N_LOCAIS_UNIC <- N_VALIDO - N_DUPLICADO

## =============================================================================
## 4. INTEGRAÇÃO ESPACIAL
## =============================================================================

pts <- para_sf(pontos, lon = "lon", lat = "lat")

## RPA e bairro de cada ponto (vizinho mais próximo cobre pontos exatamente na borda)
pts$RPA    <- rpa$RPA[sf::st_nearest_feature(pts, rpa)]
idx_bairro <- sf::st_nearest_feature(pts, bairros)
pts$bairro <- bairros$EBAIRRNOME[idx_bairro]

## Distância euclidiana ao elemento mais próximo de cada categoria
## (`dist_min()` é definida em 00_setup.R)
pts$d_agua   <- dist_min(pts, hidrico)
pts$d_saude  <- dist_min(pts, saude)
pts$d_escola <- dist_min(pts, escolas)
pts$d_eco    <- dist_min(pts, eco_rcc)

## =============================================================================
## 5. CLASSIFICAÇÃO E ESCORE DE RISCO (Equação 1 / Tabelas 1 e 2)
## =============================================================================

## `classificar()` e `pontuar()` são definidas em 00_setup.R, junto de PARAMS.

pts$cl_agua   <- classificar(pts$d_agua,   PARAMS$cortes_agua)
pts$cl_saude  <- classificar(pts$d_saude,  PARAMS$cortes_equip)
pts$cl_escola <- classificar(pts$d_escola, PARAMS$cortes_equip)

pts$Ss <- pontuar(pts$d_saude,  PARAMS$cortes_equip)
pts$Se <- pontuar(pts$d_escola, PARAMS$cortes_equip)
pts$Sr <- pontuar(pts$d_agua,   PARAMS$cortes_agua)
pts$Sf <- pts$Ss + pts$Se + pts$Sr

pts$classe_risco <- cut(pts$Sf, breaks = PARAMS$limites_sf,
                        labels = PARAMS$classes_sf, include.lowest = TRUE)

## =============================================================================
## 6. AGREGAÇÃO POR BAIRRO (insumo da análise espacial)
## =============================================================================

contagem_bairro <- as.data.frame(table(pts$bairro), stringsAsFactors = FALSE)
names(contagem_bairro) <- c("EBAIRRNOME", "pontos")

bairros <- bairros |>
  dplyr::left_join(contagem_bairro, by = "EBAIRRNOME") |>
  dplyr::mutate(pontos = tidyr::replace_na(pontos, 0),
                densidade = pontos / area_km2)

## =============================================================================
## 7. OBJETO DE SAÍDA
## =============================================================================

DADOS <- list(
  pts = pts, rpa = rpa, bairros = bairros, municipio = municipio,
  hidrico = hidrico, saude = saude, escolas = escolas,
  eco_rcc = eco_rcc, eco_todos = eco_todos,
  auditoria = auditoria, auditoria_resumo = AUDITORIA_RESUMO,
  renda = RENDA, tab_manuscrito = TAB_MANUSCRITO,
  bairros_por_rpa = BAIRROS_POR_RPA,
  n = list(bruto = N_BRUTO, valido = N_VALIDO, excluido = N_EXCLUIDO,
           corrigido = N_CORRIGIDO, duplicado = N_DUPLICADO,
           locais_unicos = N_LOCAIS_UNIC,
           manuscrito = sum(TAB_MANUSCRITO$rpa))
)

message(sprintf("Dados prontos: %d brutos -> %d válidos (%d corrigidos, %d excluídos).",
                N_BRUTO, N_VALIDO, N_CORRIGIDO, N_EXCLUIDO))
