## -----------------------------------------------------------------------------
## 03_covariaveis.R — Covariáveis socioeconômicas e urbanísticas por bairro
##
## Constrói o quadro de dados que sustenta os modelos de contagem e a regressão
## espacial. As covariáveis vêm de camadas públicas do Portal de Dados Abertos
## da Prefeitura do Recife (ODbL).
##
## Nota sobre renda: o Cadastro Único do portal traz renda familiar, mas SEM
## bairro ou coordenada — inútil para análise espacial. Como proxy de condição
## socioeconômica usa-se a camada oficial de assentamentos precários (favelas,
## cortiços e loteamentos irregulares), que é georreferenciada e traz população
## e classificação de precariedade por assentamento.
## -----------------------------------------------------------------------------

construir_covariaveis <- function(bairros, dir_geo = PARAMS$dir_geo) {

  ## --- Assentamentos precários ------------------------------------------------
  favelas <- sf::st_read(file.path(dir_geo, "favelas.geojson"), quiet = TRUE) |>
    sf::st_transform(PARAMS$crs_utm) |>
    sf::st_make_valid()

  ## Área de cada bairro coberta por assentamento precário. A união dissolve
  ## sobreposições entre polígonos, evitando dupla contagem.
  precario <- sf::st_union(sf::st_geometry(favelas))
  inter <- sf::st_intersection(sf::st_geometry(bairros), precario)
  idx <- attr(inter, "idx")
  area_prec <- rep(0, nrow(bairros))
  if (!is.null(idx)) {
    agregado <- tapply(as.numeric(sf::st_area(inter)), idx[, 1], sum)
    area_prec[as.integer(names(agregado))] <- agregado
  } else {
    ## st_intersection sobre sfc devolve as geometrias na ordem de `bairros`
    for (i in seq_len(nrow(bairros))) {
      g <- sf::st_intersection(sf::st_geometry(bairros)[i], precario)
      area_prec[i] <- if (length(g)) sum(as.numeric(sf::st_area(g))) else 0
    }
  }

  ## População residente em assentamentos precários, atribuída pelo centroide
  cent <- sf::st_point_on_surface(favelas)
  cent$bairro_idx <- sf::st_nearest_feature(cent, bairros)
  pop <- tapply(suppressWarnings(as.numeric(cent$Populacao_)), cent$bairro_idx,
                sum, na.rm = TRUE)
  pop_prec <- rep(0, nrow(bairros))
  pop_prec[as.integer(names(pop))] <- pop

  n_assent <- rep(0L, nrow(bairros))
  tb <- table(cent$bairro_idx)
  n_assent[as.integer(names(tb))] <- as.integer(tb)

  ## --- Densidade construída ---------------------------------------------------
  ## ATENÇÃO: a camada de edificações do portal tem COBERTURA INCOMPLETA —
  ## 31 dos 94 bairros aparecem com zero edificações, incluindo áreas
  ## densamente ocupadas como Santo Antônio, Bairro do Recife, Brasília Teimosa
  ## e Ilha Joana Bezerra. São 74.013 registros para uma cidade de 1,5 milhão de
  ## habitantes. Os zeros são ausência de dado, não ausência de construção.
  ##
  ## Por isso estas variáveis são calculadas mas NÃO devem entrar nos modelos:
  ## um zero falso em 33% das unidades produziria viés sistemático. Ficam
  ## disponíveis apenas para documentar a limitação.
  edif <- sf::st_read(file.path(dir_geo, "edificacoes.geojson"), quiet = TRUE) |>
    sf::st_transform(PARAMS$crs_utm) |>
    sf::st_make_valid()
  edif$area_m2 <- as.numeric(sf::st_area(edif))
  ec <- sf::st_point_on_surface(edif)
  ec$bairro_idx <- sf::st_nearest_feature(ec, bairros)

  n_edif <- rep(0L, nrow(bairros))
  tbe <- table(ec$bairro_idx)
  n_edif[as.integer(names(tbe))] <- as.integer(tbe)

  area_edif <- rep(0, nrow(bairros))
  ae <- tapply(ec$area_m2, ec$bairro_idx, sum, na.rm = TRUE)
  area_edif[as.integer(names(ae))] <- ae

  ## --- Quadro final -----------------------------------------------------------
  bairros |>
    dplyr::mutate(
      area_precaria_km2 = area_prec / 1e6,
      pct_precario      = 100 * (area_prec / 1e6) / area_km2,
      pop_precaria      = pop_prec,
      n_assentamentos   = n_assent,
      n_edificacoes     = n_edif,
      dens_edificacoes  = n_edif / area_km2,
      taxa_ocupacao     = 100 * (area_edif / 1e6) / area_km2,
      cobertura_edif    = n_edif > 0        # marca a limitação acima
    )
}

## -----------------------------------------------------------------------------
## Modelos de contagem para o número de pontos de descarte por bairro
## -----------------------------------------------------------------------------
## A variável resposta é uma contagem com exposição desigual (bairros têm áreas
## muito diferentes), daí o offset log(área). Compara-se Poisson e binomial
## negativa porque contagens espaciais costumam apresentar superdispersão.
modelos_contagem <- function(dados, formula_base) {
  poisson <- glm(formula_base, family = poisson(link = "log"), data = dados)
  ## Teste de superdispersão: razão entre deviance e graus de liberdade
  disp <- sum(residuals(poisson, type = "pearson")^2) / df.residual(poisson)
  bn <- try(MASS::glm.nb(formula_base, data = dados), silent = TRUE)

  ajuste <- tibble::tibble(
    Modelo = c("Poisson", "Binomial negativa"),
    `Log-verossimilhança` = c(as.numeric(logLik(poisson)),
                              if (inherits(bn, "try-error")) NA else as.numeric(logLik(bn))),
    AIC = c(AIC(poisson), if (inherits(bn, "try-error")) NA else AIC(bn)),
    `Dispersão (Pearson/gl)` = c(disp, NA),
    `θ (BN)` = c(NA, if (inherits(bn, "try-error")) NA else bn$theta)
  )

  modelo_final <- if (!inherits(bn, "try-error") && AIC(bn) < AIC(poisson)) bn else poisson

  ## Diagnósticos do modelo escolhido: a dispersão residual mostra se a
  ## superdispersão foi de fato absorvida, e o pseudo-R² de McFadden dá a
  ## medida (sempre modesta em modelos de contagem) do poder explicativo.
  nulo <- update(modelo_final, . ~ 1)
  diagnostico <- tibble::tibble(
    `Dispersão residual (Pearson/gl)` =
      sum(residuals(modelo_final, type = "pearson")^2) / df.residual(modelo_final),
    `Pseudo-R² (McFadden)` = 1 - as.numeric(logLik(modelo_final)) / as.numeric(logLik(nulo)),
    `Observações influentes (Cook > 4/n)` = sum(cooks.distance(modelo_final) > 4 / nobs(modelo_final)),
    n = nobs(modelo_final)
  )

  coefs <- summary(modelo_final)$coefficients
  tabela_coef <- tibble::tibble(
    Termo = rownames(coefs),
    Estimativa = coefs[, 1],
    `EP` = coefs[, 2],
    `z` = coefs[, 3],
    `valor-p` = coefs[, 4],
    `IRR (exp β)` = exp(coefs[, 1]),
    `IC95 inf` = exp(coefs[, 1] - 1.96 * coefs[, 2]),
    `IC95 sup` = exp(coefs[, 1] + 1.96 * coefs[, 2])
  )

  list(ajuste = ajuste, coeficientes = tabela_coef, diagnostico = diagnostico,
       modelo = modelo_final,
       nome = if (identical(modelo_final, poisson)) "Poisson" else "Binomial negativa")
}

## -----------------------------------------------------------------------------
## Moran's I sobre os resíduos de um modelo — o teste que decide se é preciso
## passar de um modelo não espacial para um modelo espacial explícito.
## -----------------------------------------------------------------------------
moran_residuos <- function(modelo, geometrias, n_perm = PARAMS$n_perm) {
  res <- residuals(modelo, type = "pearson")
  viz <- spdep::poly2nb(geometrias, queen = TRUE)
  pesos <- spdep::nb2listw(viz, style = "W", zero.policy = TRUE)
  mc <- spdep::moran.mc(res, pesos, nsim = n_perm, zero.policy = TRUE)
  tibble::tibble(
    `I de Moran (resíduos)` = unname(mc$statistic),
    `valor-p (permutação)` = mc$p.value,
    Conclusão = dplyr::if_else(
      mc$p.value < PARAMS$alfa,
      "resíduos autocorrelacionados — requer modelo espacial",
      "resíduos sem autocorrelação — modelo não espacial é suficiente")
  )
}
