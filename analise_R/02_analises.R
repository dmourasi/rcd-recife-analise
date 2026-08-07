## -----------------------------------------------------------------------------
## 02_analises.R — Rotinas estatísticas
##
## Cada função executa um bloco de análise e devolve resultados já organizados
## em data frames prontos para tabela. O relatório (relatorio.Rmd) apenas chama
## estas funções, o que mantém o texto legível e as análises testáveis.
## -----------------------------------------------------------------------------

## =============================================================================
## A. QUI-QUADRADO DE ADERÊNCIA COM DIAGNÓSTICO COMPLETO
## =============================================================================
## Além da estatística e do p-valor, devolve os resíduos padronizados
## (que indicam QUAIS categorias sustentam a rejeição de H0) e o tamanho de
## efeito Cohen's w — exigência de reporte da APA e da maioria dos periódicos.

qui_aderencia <- function(observado, esperado_p = NULL, rotulo = "") {
  k <- length(observado)
  if (is.null(esperado_p)) esperado_p <- rep(1 / k, k)
  teste <- suppressWarnings(chisq.test(observado, p = esperado_p, rescale.p = TRUE))
  n <- sum(observado)
  w <- cohen_w(unname(teste$statistic), n)

  detalhe <- tibble::tibble(
    Categoria      = names(observado) %||% as.character(seq_len(k)),
    Observado      = as.integer(observado),
    `Observado (%)`= 100 * observado / n,
    Esperado       = as.numeric(teste$expected),
    `Resíduo pad.` = as.numeric(teste$residuals),          # (O-E)/sqrt(E)
    `Resíduo ajust.` = as.numeric(teste$stdres),           # padronizado ajustado
    `Contrib. χ² (%)` = 100 * as.numeric(teste$residuals)^2 / unname(teste$statistic)
  )

  resumo <- tibble::tibble(
    Análise = rotulo,
    `χ²` = unname(teste$statistic), gl = unname(teste$parameter),
    `valor-p` = teste$p.value, n = n,
    `Cohen's w` = w, Interpretação = interp_w(w),
    `E mín.` = min(teste$expected),
    `E < 5 (%)` = 100 * mean(teste$expected < 5)
  )
  list(resumo = resumo, detalhe = detalhe, teste = teste)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## =============================================================================
## B. QUI-QUADRADO DE INDEPENDÊNCIA COM V DE CRAMÉR E IC BOOTSTRAP
## =============================================================================

qui_independencia <- function(tabela, rotulo = "", n_boot = 2000) {
  teste <- suppressWarnings(chisq.test(tabela))
  n <- sum(tabela)
  v <- sqrt(unname(teste$statistic) / (n * (min(dim(tabela)) - 1)))

  ## IC bootstrap para o V de Cramér: reamostra as células com probabilidades
  ## observadas, o que preserva o tamanho amostral e a estrutura da tabela.
  probs <- as.vector(tabela) / n
  vs <- replicate(n_boot, {
    reamostra <- matrix(rmultinom(1, n, probs), nrow = nrow(tabela))
    t <- suppressWarnings(chisq.test(reamostra))
    sqrt(unname(t$statistic) / (n * (min(dim(tabela)) - 1)))
  })
  ic <- quantile(vs, c(0.025, 0.975), na.rm = TRUE)

  resumo <- tibble::tibble(
    Análise = rotulo, `χ²` = unname(teste$statistic), gl = unname(teste$parameter),
    `valor-p` = teste$p.value, n = n,
    `V de Cramér` = v, `IC95 inf` = ic[1], `IC95 sup` = ic[2],
    Interpretação = dplyr::case_when(v < 0.10 ~ "desprezível", v < 0.30 ~ "fraca",
                                     v < 0.50 ~ "moderada", TRUE ~ "forte")
  )
  residuos <- as.data.frame.matrix(round(teste$stdres, 2))
  list(resumo = resumo, residuos = residuos, teste = teste)
}

## =============================================================================
## C. BATERIA DE NORMALIDADE
## =============================================================================
## Em n > 1.000 qualquer teste formal rejeita normalidade por desvios triviais;
## por isso reportamos os testes JUNTO de assimetria/curtose padronizadas, que
## medem magnitude e não apenas significância.

bateria_normalidade <- function(x, rotulo = "") {
  x <- x[is.finite(x)]
  n <- length(x)
  ad <- nortest::ad.test(x)
  li <- nortest::lillie.test(x)
  ## Shapiro-Wilk é limitado a n <= 5000
  sw <- if (n <= 5000) shapiro.test(x) else list(statistic = NA, p.value = NA)
  ep_assim <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  ep_curt  <- 2 * ep_assim * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))

  tibble::tibble(
    Variável = rotulo, n = n,
    `Shapiro-Wilk W` = unname(sw$statistic), `p (SW)` = sw$p.value,
    `Anderson-Darling A²` = unname(ad$statistic), `p (AD)` = ad$p.value,
    `Lilliefors D` = unname(li$statistic), `p (KS-L)` = li$p.value,
    `Assimetria/EP` = moments::skewness(x) / ep_assim,
    `Curtose/EP` = (moments::kurtosis(x) - 3) / ep_curt,
    Decisão = "não normal"
  )
}

## =============================================================================
## D. ANÁLISE DE PADRÃO DE PONTOS
## =============================================================================

## D.1 Vizinho mais próximo (Clark & Evans, 1954) com correção de borda de Donnelly
indice_vizinho_proximo <- function(pts_sf, janela_sf) {
  coords <- sf::st_coordinates(pts_sf)
  coords <- unique(coords)
  n <- nrow(coords)
  A <- as.numeric(sf::st_area(janela_sf))
  P <- as.numeric(sf::st_length(sf::st_cast(sf::st_boundary(janela_sf), "MULTILINESTRING")))

  d <- spatstat.geom::nndist(spatstat.geom::ppp(coords[, 1], coords[, 2],
        window = spatstat.geom::owin(range(coords[, 1]), range(coords[, 2]))))
  d_obs <- mean(d)
  d_esp <- 0.5 * sqrt(A / n)                       # esperado sob CSR
  d_esp_donnelly <- 0.5 * sqrt(A / n) + 0.0514 * P / n + 0.041 * P / n^1.5
  ep <- 0.26136 / sqrt(n^2 / A)

  tibble::tibble(
    n_locais = n,
    `Distância observada (m)` = d_obs,
    `Distância esperada sob CSR (m)` = d_esp,
    `Esperada c/ correção de Donnelly (m)` = d_esp_donnelly,
    `Índice R` = d_obs / d_esp,
    `R (Donnelly)` = d_obs / d_esp_donnelly,
    `z` = (d_obs - d_esp) / ep,
    `valor-p` = 2 * pnorm(-abs((d_obs - d_esp) / ep)),
    Padrão = dplyr::if_else(d_obs / d_esp < 1, "agregado (cluster)", "disperso")
  )
}

## D.2 Função K de Ripley com envelope de Monte Carlo — verifica se a agregação
## se sustenta em TODAS as escalas, não só na do vizinho imediato.
##
## Notas de desempenho: a correção isotrópica de Ripley é O(n²) e recalcula os
## pesos de borda contra cada aresta da janela. Com o contorno detalhado do
## município (milhares de vértices) e 100 padrões simulados, o custo é proibitivo.
## Duas decisões resolvem isso sem afetar a inferência:
##   (1) a janela é simplificada (tolerância `tol_simplif`), pois a correção de
##       borda depende da forma geral do território, não do detalhe da costa;
##   (2) usa-se a correção "border" (contagem reduzida), que é consistente e
##       muito mais rápida — e como o envelope é gerado sob a MESMA correção,
##       qualquer viés residual afeta igualmente o observado e o simulado.
funcao_k_ripley <- function(pts_sf, janela_sf, n_sim = 99,
                            tol_simplif = 150, r_max = 3000, n_r = 120) {
  coords <- unique(sf::st_coordinates(pts_sf))

  ## O buffer compensa o recuo do contorno causado pela simplificação, evitando
  ## que pontos legítimos junto à borda sejam descartados do padrão.
  janela_simp <- sf::st_geometry(janela_sf) |>
    sf::st_simplify(dTolerance = tol_simplif, preserveTopology = TRUE) |>
    sf::st_buffer(tol_simplif) |>
    sf::st_union()
  jan <- spatstat.geom::as.owin(janela_simp)

  dentro <- as.logical(sf::st_intersects(
    sf::st_as_sf(as.data.frame(coords), coords = c("X", "Y"),
                 crs = sf::st_crs(janela_sf)), janela_simp, sparse = FALSE))
  coords <- coords[dentro, , drop = FALSE]

  padrao <- spatstat.geom::ppp(coords[, 1], coords[, 2], window = jan)

  env <- spatstat.explore::envelope(
    padrao, fun = spatstat.explore::Lest, nsim = n_sim, verbose = FALSE,
    correction = "border", r = seq(0, r_max, length.out = n_r))
  as.data.frame(env)
}

## Mediana por coluna (evita dependência de matrixStats por uma única função)
matrixStats_colMedians <- function(m) apply(m, 2, median)

## =============================================================================
## D.3 TESTE DE PROXIMIDADE CONTRA MODELO NULO ESPACIAL
## =============================================================================
## Esta é a pergunta que o objetivo do artigo realmente faz: os pontos de
## descarte estão mais próximos dos corpos hídricos (ou dos equipamentos) do que
## estariam SE fossem depositados em qualquer lugar do município?
##
## O qui-quadrado de aderência não responde a isso: sua H0 (classes de distância
## equiprováveis) não tem significado geográfico — a área do território a até
## 20 m de um rio é muito menor que a área a mais de 120 m, então mesmo um
## descarte totalmente aleatório produziria "muitos pontos distantes".
##
## Aqui a H0 correta é gerada por simulação: n pontos sorteados uniformemente
## dentro do município, repetidos n_sim vezes. Compara-se a distância mediana
## observada com a distribuição das medianas simuladas.
##
## Implementação: sortear n pontos e recalcular a distância a cada uma das
## n_sim repetições exigiria n × n_sim buscas de vizinho mais próximo contra
## geometrias complexas (inviável). Em vez disso, sorteia-se UMA vez uma
## população de referência ampla (`n_ref` pontos uniformes no município), a
## distância é calculada uma única vez para ela, e cada repetição é uma
## reamostra de n pontos dessa população. O procedimento é equivalente — a
## população de referência É a distribuição nula — e reduz o custo em duas
## ordens de grandeza.
teste_proximidade_csr <- function(pts_sf, camadas, janela, n_sim = 999,
                                  n_ref = 20000, semente = PARAMS$semente) {
  set.seed(semente)
  n <- nrow(pts_sf)
  janela_geom <- sf::st_geometry(janela)

  observado <- vapply(camadas, function(cam) median(dist_min(pts_sf, cam)), numeric(1))

  referencia <- sf::st_sf(geometry = sf::st_sample(janela_geom, n_ref, type = "random"))
  dist_ref <- vapply(camadas, function(cam) dist_min(referencia, cam),
                     numeric(nrow(referencia)))

  simulado <- matrix(NA_real_, nrow = n_sim, ncol = length(camadas),
                     dimnames = list(NULL, names(camadas)))
  for (i in seq_len(n_sim)) {
    idx <- sample.int(nrow(dist_ref), n, replace = TRUE)
    simulado[i, ] <- matrixStats_colMedians(dist_ref[idx, , drop = FALSE])
  }

  purrr::imap_dfr(camadas, function(cam, nome) {
    obs <- observado[[nome]]
    nulos <- simulado[, nome]
    ## p unilateral (Monte Carlo): proporção de simulações tão próximas quanto a
    ## observada. O menor p possível é 1/(n_sim+1) — daí a necessidade de n_sim
    ## suficientemente grande para detectar efeitos fortes.
    p <- (1 + sum(nulos <= obs)) / (n_sim + 1)
    tibble::tibble(
      Critério = nome,
      `Mediana observada (m)` = obs,
      `Mediana esperada sob CSR (m)` = mean(nulos),
      `IC95 simulado inf` = quantile(nulos, 0.025, names = FALSE),
      `IC95 simulado sup` = quantile(nulos, 0.975, names = FALSE),
      `Razão obs/esperada` = obs / mean(nulos),
      `valor-p` = p,
      Conclusão = dplyr::if_else(
        p <= PARAMS$alfa, "mais próximo que o acaso",
        dplyr::if_else(p >= 1 - PARAMS$alfa, "mais distante que o acaso",
                       "compatível com o acaso"))
    )
  })
}

## =============================================================================
## D.4 REDUNDÂNCIA ENTRE OS CRITÉRIOS DO MODELO DE RISCO
## =============================================================================
## A Equação 1 soma os três escores parciais. Somar componentes fortemente
## correlacionados equivale a dar peso extra ao mesmo fenômeno — por isso a
## correlação entre eles precisa ser verificada e reportada.
correlacao_criterios <- function(dados, colunas, rotulos) {
  m <- as.matrix(dados[, colunas])
  colnames(m) <- rotulos
  pares <- utils::combn(seq_along(colunas), 2)

  purrr::map_dfr(seq_len(ncol(pares)), function(k) {
    i <- pares[1, k]; j <- pares[2, k]
    ct <- suppressWarnings(cor.test(m[, i], m[, j], method = "spearman", exact = FALSE))
    tibble::tibble(
      Par = paste(rotulos[i], "×", rotulos[j]),
      `ρ de Spearman` = unname(ct$estimate),
      `valor-p` = ct$p.value,
      `Variância compartilhada (%)` = 100 * unname(ct$estimate)^2,
      Interpretação = dplyr::case_when(
        abs(unname(ct$estimate)) < 0.10 ~ "independentes na prática",
        abs(unname(ct$estimate)) < 0.30 ~ "correlação fraca",
        abs(unname(ct$estimate)) < 0.50 ~ "correlação moderada",
        TRUE ~ "correlação forte — risco de redundância")
    )
  })
}

## =============================================================================
## D.5 SUPERFÍCIE DE DENSIDADE KERNEL
## =============================================================================
## Devolve a densidade em formato longo, pronta para `geom_raster()`, recortada
## ao território municipal. `sigma = NULL` usa a seleção automática por
## validação cruzada de verossimilhança (bw.diggle).
densidade_kernel <- function(pts_sf, janela, sigma = 500, resolucao = 300) {
  coords <- unique(sf::st_coordinates(pts_sf))
  ## O buffer compensa o recuo do contorno pela simplificação, evitando que
  ## pontos junto à borda sejam descartados da superfície.
  jan <- spatstat.geom::as.owin(
    sf::st_geometry(janela) |>
      sf::st_simplify(dTolerance = 100, preserveTopology = TRUE) |>
      sf::st_buffer(100) |>
      sf::st_union())

  padrao <- spatstat.geom::ppp(coords[, 1], coords[, 2], window = jan)
  dens <- spatstat.explore::density.ppp(padrao, sigma = sigma,
                                        dimyx = c(resolucao, resolucao))
  df <- as.data.frame(dens)
  names(df) <- c("x", "y", "densidade")
  ## de pontos/m² para pontos/km², escala interpretável
  df$densidade <- df$densidade * 1e6
  df[is.finite(df$densidade), ]
}

## =============================================================================
## E. AUTOCORRELAÇÃO ESPACIAL
## =============================================================================

## E.1 Moran's I global com inferência analítica E por permutação
moran_global <- function(valores, geometrias, n_perm = PARAMS$n_perm) {
  viz <- spdep::poly2nb(geometrias, queen = TRUE)
  pesos <- spdep::nb2listw(viz, style = "W", zero.policy = TRUE)
  analitico <- spdep::moran.test(valores, pesos, zero.policy = TRUE,
                                 randomisation = TRUE)
  mc <- spdep::moran.mc(valores, pesos, nsim = n_perm, zero.policy = TRUE)

  resumo <- tibble::tibble(
    `I de Moran` = unname(analitico$estimate[1]),
    `E[I] sob H0` = unname(analitico$estimate[2]),
    `Variância` = unname(analitico$estimate[3]),
    `z (analítico)` = unname(analitico$statistic),
    `p (analítico)` = analitico$p.value,
    `p (permutação)` = mc$p.value,
    `n permutações` = n_perm,
    Interpretação = dplyr::case_when(
      mc$p.value >= PARAMS$alfa ~ "aleatório espacialmente",
      unname(analitico$estimate[1]) > 0 ~ "autocorrelação positiva (clusters)",
      TRUE ~ "autocorrelação negativa (dispersão)")
  )
  list(resumo = resumo, pesos = pesos, mc = mc)
}

## E.2 Getis-Ord Gi* — identifica QUAIS unidades formam os clusters
getis_ord <- function(valores, geometrias, rotulos) {
  viz <- spdep::poly2nb(geometrias, queen = TRUE)
  viz_incl <- spdep::include.self(viz)                 # Gi* inclui a própria unidade
  pesos <- spdep::nb2listw(viz_incl, style = "B", zero.policy = TRUE)
  gi <- as.numeric(spdep::localG(valores, pesos, zero.policy = TRUE))

  tibble::tibble(Unidade = rotulos, Valor = valores, `Gi* (z)` = gi,
                 `valor-p` = 2 * pnorm(-abs(gi))) |>
    dplyr::mutate(
      `p ajustado (FDR)` = p.adjust(`valor-p`, method = "BH"),
      Classificação = dplyr::case_when(
        `Gi* (z)` >=  2.58 & `p ajustado (FDR)` < 0.05 ~ "hot spot (99%)",
        `Gi* (z)` >=  1.96 & `p ajustado (FDR)` < 0.05 ~ "hot spot (95%)",
        `Gi* (z)` <= -1.96 & `p ajustado (FDR)` < 0.05 ~ "cold spot (95%)",
        TRUE ~ "não significativo")) |>
    dplyr::arrange(dplyr::desc(`Gi* (z)`))
}

## =============================================================================
## F. COMPARAÇÃO DE GRUPOS (ESCORE DE RISCO ENTRE RPAs)
## =============================================================================

comparar_grupos <- function(valor, grupo) {
  df <- data.frame(valor = valor, grupo = factor(grupo))
  df <- df[is.finite(df$valor), ]

  kw <- kruskal.test(valor ~ grupo, data = df)
  n <- nrow(df); k <- nlevels(df$grupo)
  eps2 <- (unname(kw$statistic) - k + 1) / (n - k)     # epsilon quadrado
  ## Alternativa robusta a heterocedasticidade (não assume variâncias iguais)
  welch <- oneway.test(valor ~ grupo, data = df, var.equal = FALSE)

  resumo <- tibble::tibble(
    Teste = c("Kruskal-Wallis", "ANOVA de Welch (robusta)"),
    Estatística = c(unname(kw$statistic), unname(welch$statistic)),
    gl = c(unname(kw$parameter), unname(welch$parameter[1])),
    `valor-p` = c(kw$p.value, welch$p.value),
    ## O tamanho de efeito acompanha o teste principal; a ANOVA de Welch entra
    ## apenas como verificação convergente, por isso o traço.
    `Tamanho de efeito` = c(paste0("ε² = ", fmt_n(eps2, 3)), "—"),
    Interpretação = c(dplyr::case_when(eps2 < 0.01 ~ "desprezível", eps2 < 0.06 ~ "pequeno",
                                       eps2 < 0.14 ~ "médio", TRUE ~ "grande"), "—")
  )
  attr(resumo, "eps2") <- eps2

  dunn <- FSA::dunnTest(valor ~ grupo, data = df, method = "bonferroni")$res
  dunn <- tibble::as_tibble(dunn) |>
    dplyr::rename(Comparação = Comparison, Z = Z,
                  `p não ajustado` = P.unadj, `p ajustado (Bonferroni)` = P.adj) |>
    dplyr::arrange(`p ajustado (Bonferroni)`)

  descritiva <- df |>
    dplyr::group_by(Grupo = grupo) |>
    dplyr::summarise(n = dplyr::n(), Média = mean(valor), DP = sd(valor),
                     Mediana = median(valor), Q1 = quantile(valor, .25),
                     Q3 = quantile(valor, .75), .groups = "drop")

  list(resumo = resumo, dunn = dunn, descritiva = descritiva, kw = kw)
}

## =============================================================================
## G. BOOTSTRAP NÃO PARAMÉTRICO (percentil e BCa)
## =============================================================================
## Fornece IC sem supor normalidade — essencial aqui, já que todas as
## distribuições de distância são fortemente assimétricas.

bootstrap_ic <- function(x, estatistica = median, n_boot = 5000, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  obs <- estatistica(x)
  reps <- replicate(n_boot, estatistica(sample(x, n, replace = TRUE)))

  ## Correção de viés (z0) e aceleração (a) para o intervalo BCa
  z0 <- qnorm(mean(reps < obs))
  jack <- vapply(seq_len(n), function(i) estatistica(x[-i]), numeric(1))
  jm <- mean(jack)
  a <- sum((jm - jack)^3) / (6 * sum((jm - jack)^2)^1.5)

  alfa <- (1 - conf) / 2
  z <- qnorm(c(alfa, 1 - alfa))
  p_bca <- pnorm(z0 + (z0 + z) / (1 - a * (z0 + z)))

  tibble::tibble(
    Estimativa = obs,
    `EP bootstrap` = sd(reps),
    `Viés` = mean(reps) - obs,
    `IC95 percentil inf` = quantile(reps, alfa, names = FALSE),
    `IC95 percentil sup` = quantile(reps, 1 - alfa, names = FALSE),
    `IC95 BCa inf` = quantile(reps, p_bca[1], names = FALSE),
    `IC95 BCa sup` = quantile(reps, p_bca[2], names = FALSE),
    `Reamostras` = n_boot
  )
}

## =============================================================================
## H. ANÁLISE DE SENSIBILIDADE DO MODELO DE RISCO
## =============================================================================
## Responde à crítica sobre a arbitrariedade dos pesos: em vez de justificar os
## pesos, testa se a CONCLUSÃO muda quando eles mudam.

sensibilidade_pesos <- function(pts, cenarios) {
  purrr::imap_dfr(cenarios, function(cen, nome) {
    esc <- cen$escores
    ss <- pontuar(pts$d_saude,  PARAMS$cortes_equip, esc) * cen$w[1]
    se <- pontuar(pts$d_escola, PARAMS$cortes_equip, esc) * cen$w[2]
    sr <- pontuar(pts$d_agua,   PARAMS$cortes_agua,  esc) * cen$w[3]
    sf <- ss + se + sr
    maximo <- max(esc) * sum(cen$w)
    cl <- cut(sf, breaks = seq(0, maximo, length.out = 5),
              labels = PARAMS$classes_sf, include.lowest = TRUE)
    pct <- 100 * prop.table(table(cl))
    tibble::tibble(Cenário = nome,
                   `Sf médio` = mean(sf), `Sf mediano` = median(sf),
                   `Classe I (%)` = pct[1], `Classe II (%)` = pct[2],
                   `Classe III (%)` = pct[3], `Classe IV (%)` = pct[4])
  })
}

## =============================================================================
## I. TESTE DE PERMUTAÇÃO PARA DIFERENÇA DE MEDIANAS
## =============================================================================
## Alternativa livre de distribuição para confirmar contrastes do pós-teste.

permutacao_medianas <- function(x, y, n_perm = 10000) {
  obs <- median(x) - median(y)
  combinado <- c(x, y); nx <- length(x)
  nulos <- replicate(n_perm, {
    emb <- sample(combinado)
    median(emb[1:nx]) - median(emb[-(1:nx)])
  })
  tibble::tibble(
    `Diferença observada` = obs,
    `Média sob H0` = mean(nulos),
    `valor-p (bicaudal)` = (1 + sum(abs(nulos) >= abs(obs))) / (n_perm + 1),
    Permutações = n_perm
  )
}
