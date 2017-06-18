context("offense-defense")


# rate_od -----------------------------------------------------------------
test_that("rate_od works", {
  output_ref_1 <- matrix(
    c(34.012, 151.563, 48.679,  82.05, 114.863,
       1.691,   0.803,  1.164,  0.967,   0.411,
      20.111, 188.779, 41.817, 84.806, 279.719),
    ncol = 3,
    dimnames = list(
      c("Duke", "Miami", "UNC", "UVA", "VT"),
      c("off", "def", "od")
    )
  )

  output_1 <- rate_od(
    cr_data = ncaa2005,
    h2h_fun = h2h_mean_score,
    self_play = 0,
    eps = 1e-3,
    tol = 1e-4,
    max_iterations = 100
  )

  expect_equal(round(output_1, 3), output_ref_1)

  output_ref_2 <- output_ref_1
  output_ref_2[, ] <-
    c( 39.67, 181.033, 58.113,  94.951, 182.859,
       1.567,    0.86,  1.149,   0.914,   0.532,
      25.311, 210.536, 50.569, 103.838, 343.972)

  output_2 <- rate_od(
    cr_data = ncaa2005,
    h2h_fun = h2h_mean_score,
    self_play = NULL,
    eps = 1e-3,
    tol = 1e-4,
    max_iterations = 100
  )

  expect_equal(round(output_2, 3), output_ref_2)
})


# od_def_iteration --------------------------------------------------------
test_that("od_def_iteration works", {
  input_vec <- 1:5
  input_mat <- matrix(1:25, nrow = 5)

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
