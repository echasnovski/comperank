context("offense-defense")


# Input data --------------------------------------------------------------
cr_data <- ncaa2005


# rate_od -----------------------------------------------------------------
test_that("rate_od works", {
  output_1 <- rate_od(
    cr_data = cr_data,
    ifelse(player1[1] == player2[1], 0, mean(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )
  output_1$rating_off <- round(output_1$rating_off, 3)
  output_1$rating_def <- round(output_1$rating_def, 3)
  output_1$rating_od <- round(output_1$rating_od, 3)

  output_ref_1 <- tibble::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_off = c(34.012, 151.563, 48.679,  82.05, 114.863),
    rating_def = c( 1.691,   0.803,  1.164,  0.967,   0.411),
    rating_od  = c(20.111, 188.779, 41.817, 84.806, 279.719)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rate_od(
    cr_data = cr_data,
    mean(score1),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )
  output_2$rating_off <- round(output_2$rating_off, 3)
  output_2$rating_def <- round(output_2$rating_def, 3)
  output_2$rating_od <- round(output_2$rating_od, 3)

  output_ref_2 <- tibble::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_off = c( 39.67, 181.033, 58.113,  94.951, 182.859),
    rating_def = c( 1.567,    0.86,  1.149,   0.914,   0.532),
    rating_od  = c(25.311, 210.536, 50.569, 103.838, 343.972)
  )

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("rate_od handles factor `player`", {
  fac_levs <- c("Duke", "Extra", "Miami")
  input <- cr_data
  input$player <- factor(input$player, levels = fac_levs)

  output <- rate_od(
    cr_data = input,
    ifelse(player1[1] == player2[1], 0, mean(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )
  output$rating_off <- round(output$rating_off, 3)
  output$rating_def <- round(output$rating_def, 3)
  output$rating_od <- round(output$rating_od, 3)

  output_ref <- tibble::tibble(
    player = factor(fac_levs, levels = fac_levs),
    rating_off = c( 3.96, 0.127, 11.288),
    rating_def = c(4.659, 0.057,  1.826),
    rating_od  = c( 0.85, 2.216,  6.181)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_od handles numeric `player`", {
  input <- cr_data
  input$player <- as.integer(factor(input$player))

  output <- rate_od(
    cr_data = input,
    ifelse(player1[1] == player2[1], 0, mean(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )
  output$rating_off <- round(output$rating_off, 3)
  output$rating_def <- round(output$rating_def, 3)
  output$rating_od <- round(output$rating_od, 3)

  output_ref <- tibble::tibble(
    player = 1:5,
    rating_off = c(34.012, 151.563, 48.679,  82.05, 114.863),
    rating_def = c( 1.691,   0.803,  1.164,  0.967,   0.411),
    rating_od  = c(20.111, 188.779, 41.817, 84.806, 279.719)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_od throws error on no Head-to-Head expression", {
  expect_error(rate_od(cr_data), "Head-to-Head")
})


# rank_od -----------------------------------------------------------------
test_that("rank_od works", {
  output_1 <- rank_od(
    cr_data = cr_data,
    ifelse(player1[1] == player2[1], 0, mean(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )

  output_ref_1 <- tibble::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    ranking_off = c(5, 1, 4, 3, 2),
    ranking_def = c(5, 2, 4, 3, 1),
    ranking_od  = c(5, 2, 4, 3, 1)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rank_od(
    cr_data = cr_data,
    ifelse(player1[1] == player2[1], 0, length(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )

  output_ref_2 <- output_ref_1
  output_ref_2$ranking_off <- 3
  output_ref_2$ranking_def <- 3
  output_ref_2$ranking_od <- 3

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("rank_od handles factor `player`", {
  fac_levs <- c("Duke", "Extra", "Miami")
  input <- cr_data
  input$player <- factor(input$player, levels = fac_levs)

  output <- rank_od(
    cr_data = input,
    ifelse(player1[1] == player2[1], 0, mean(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )

  output_ref <- tibble::tibble(
    player = factor(fac_levs, levels = fac_levs),
    ranking_off = c(2, 3, 1),
    ranking_def = c(3, 1, 2),
    ranking_od  = c(3, 2, 1)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rank_od handles numeric `player`", {
  input <- cr_data
  input$player <- as.integer(factor(input$player))

  output <- rank_od(
    cr_data = input,
    ifelse(player1[1] == player2[1], 0, mean(score1)),
    eps = 1e-3, tol = 1e-4, max_iterations = 100
  )

  output_ref <- tibble::tibble(
    player = 1:5,
    ranking_off = c(5, 1, 4, 3, 2),
    ranking_def = c(5, 2, 4, 3, 1),
    ranking_od  = c(5, 2, 4, 3, 1)
  )

  expect_equal_tbls(output, output_ref)
})


# od_def_iteration --------------------------------------------------------
test_that("od_def_iteration works", {
  input_vec <- 1:5
  input_mat <- matrix(1:25, nrow = 5, byrow = TRUE)

  output_ref <- matrix(c(1.7828088106533, 2.12486520906126, 2.46692160746923,
                         2.80897800587719, 3.15103440428515),
                       nrow = 5)

  expect_equal(od_def_iteration(input_mat, input_vec), output_ref)
})


# od_stop_stat ------------------------------------------------------------
test_that("od_stop_stat works", {
  expect_equal(od_stop_stat(1:10, 1:10), 0)
  expect_equal(od_stop_stat(1:10, (1:10)*2), 10)
})
