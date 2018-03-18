context("elo")


# Input data --------------------------------------------------------------
cr_data <- ncaa2005


# rate_elo ----------------------------------------------------------------
test_that("rate_elo works", {
  output <- rate_elo(cr_data, K = 30, ksi = 400, initial_ratings = 0)
  output_ref <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_elo = c(-56.2377413877926, 57.9315106719875, -1.25948933788439,
                   -29.2442777446668, 28.8099977983563)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_elo handles factor `player`", {
  fac_levs <- c("Duke", "Miami", "UNC", "UVA", "VT", "Extra")
  input <- cr_data
  input$player <- factor(input$player, levels = fac_levs)

  output <- rate_elo(input, K = 30, ksi = 400, initial_ratings = 0)
  output_ref <- dplyr::tibble(
    player = factor(fac_levs, levels = fac_levs),
    rating_elo = c(-56.2377413877926, 57.9315106719875, -1.25948933788439,
                   -29.2442777446668, 28.8099977983563, 0)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_elo handles numeric `player`", {
  input <- cr_data
  input$player <- as.integer(factor(input$player))
  output <- rate_elo(input, K = 30, ksi = 400, initial_ratings = 0)
  output_ref <- dplyr::tibble(
    player = 1:5,
    rating_elo = c(-56.2377413877926, 57.9315106719875, -1.25948933788439,
                   -29.2442777446668, 28.8099977983563)
  )

  expect_equal_tbls(output, output_ref)
})


# rank_elo ----------------------------------------------------------------
test_that("rank_elo works", {
  output_1 <- rank_elo(cr_data)
  output_ref_1 <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    ranking_elo = c(5, 1, 3, 4, 2)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rank_elo(cr_data, keep_rating = TRUE)
  output_ref_2 <- output_ref_1
  output_ref_2$rating_elo <- c(
    -56.2377413877926, 57.9315106719875, -1.25948933788439,
    -29.2442777446668, 28.8099977983563
  )
  output_ref_2 <- output_ref_2[, c("player", "rating_elo", "ranking_elo")]

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("rank_elo handles factor `player`", {
  fac_levs <- c("Duke", "Miami", "UNC", "UVA", "VT", "Extra")
  input <- cr_data
  input$player <- factor(input$player, levels = fac_levs)

  output <- rank_elo(input, K = 30, ksi = 400, initial_ratings = 0)
  output_ref <- dplyr::tibble(
    player = factor(fac_levs, levels = fac_levs),
    ranking_elo = c(6, 1, 4, 5, 2, 3)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rank_elo handles numeric `player`", {
  input <- cr_data
  input$player <- as.integer(factor(input$player))
  output <- rank_elo(input, K = 30, ksi = 400, initial_ratings = 0)
  output_ref <- dplyr::tibble(
    player = 1:5,
    ranking_elo = c(5, 1, 3, 4, 2)
  )

  expect_equal_tbls(output, output_ref)
})


# add_elo_ratings ---------------------------------------------------------
test_that("add_elo_ratings works", {
  output_ref <- as_widecr(ncaa2005)
  output_ref$rating1Before <-
    c(0, -15, -29.3528000084656, -43.0885442384277,
      15, 29.9720581625813, 44.2715899777618,
      -0.619258154115673, 14.3831367028335, -15.5661824421677)
  output_ref$rating2Before <-
    c(0, 0, 0, 0,
      14.3528000084656, 13.735744229962, 13.1491971493649,
      -0.563787585218492, -0.510723544860779, 15.1319024958571)
  output_ref$rating1After <-
    c(-15, -29.3528000084656, -43.0885442384277, -56.2377413877926,
      29.9720581625813, 44.2715899777618, 57.9315106719875,
      14.3831367028335, -1.25948933788439, -29.2442777446668)
  output_ref$rating2After <-
    c(15, 14.3528000084656, 13.735744229962, 13.1491971493649,
      -0.619258154115673, -0.563787585218492, -0.510723544860779,
      -15.5661824421677, 15.1319024958571, 28.8099977983563)

  output <- add_elo_ratings(ncaa2005, K = 30, ksi = 400, initial_ratings = 0)

  expect_is(output, "widecr")
  expect_identical(output[, 1:5], output_ref[, 1:5])
  expect_equal(output$rating1Before, output_ref$rating1Before)
  expect_equal(output$rating2Before, output_ref$rating2Before)
  expect_equal(output$rating1After, output_ref$rating1After)
  expect_equal(output$rating2After, output_ref$rating2After)
})


# elo ---------------------------------------------------------------------
test_that("elo works", {
  elo_rate_fun_vect <-
    Vectorize(
      elo_rate_fun,
      c("rating1", "score1", "rating2", "score2", "K", "ksi")
    )

  expect_equal(elo(1:3, 1, 0, 2, K = 1, ksi = 1),
               t(elo_rate_fun_vect(1:3, 1, 0, 2, K = 1, ksi = 1)))
  expect_equal(elo(1:3, 1, 0, 2, K = 1:3, ksi = 1),
               t(elo_rate_fun_vect(1:3, 1, 0, 2, K = 1:3, ksi = 1)))

  expect_equal(
    suppressWarnings(elo(1:3, 1:2, 0, -2, K = 30, ksi = 200)),
    suppressWarnings(t(elo_rate_fun_vect(1:3, 1:2, 0, -2, K = 30, ksi = 200)))
  )
  expect_warning(elo(1:3, 1:2, 0, 2))
})

test_that("elo returns a matrix", {
  expect_elo_str <- function(...) {
    expect_true(is.matrix(...))
    expect_true(ncol(...) == 2)
  }
  expect_elo_str(elo(1, 1, 0, 0))
  expect_elo_str(elo(1:2, 1, 0, 0))
  expect_elo_str(elo(1:2, 1:0, 0:1, 0:1))
  expect_elo_str(elo(1:2, 1:0, 0:1, 0:1, K = c(25, 35), ksi = c(300, 400)))
})


# elo_fgen ----------------------------------------------------------------
test_that("elo_fgen works", {
  elo_generated <- elo_fgen(K = 10, ksi = 100)

  expect_equal(
    sapply((0:12)*10, elo_generated,
           score1 = 1, rating2 = 0, score2 = 0),
    sapply((0:12)*10, elo_rate_fun,
           score1 = 1, rating2 = 0, score2 = 0,
           K = 10, ksi = 100)
  )
})


# elo_rate_fun ------------------------------------------------------------
test_that("elo_rate_fun works", {
  output <-
    t(sapply((0:12)*100, elo_rate_fun,
             score1 = 1, rating2 = 0, score2 = 0, K = 30, ksi = 400))
  output_ref <-
    matrix(
      c(              15,                 -15,
        110.798050005913,   -10.7980500059134,
        207.207592200561,   -7.20759220056126,
         304.52938671634,    -4.5293867163397,
        402.727272727273,   -2.72727272727273,
        501.597206456061,   -1.59720645606067,
        600.919602900951,  -0.919602900951468,
        700.524162744845,  -0.524162744845003,
         800.29702970297,  -0.297029702970297,
        900.167759019265,  -0.167759019265045,
         1000.0945692755, -0.0945692754978045,
        1100.05325368237, -0.0532536823742946,
        1200.02997002997, -0.0299700299700256
      ),
      ncol = 2, byrow = TRUE
    )

  expect_equal(output, output_ref)
})
