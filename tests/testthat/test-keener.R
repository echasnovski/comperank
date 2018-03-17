context("keener")

library(comperes)
library(rlang)

# rate_keener -------------------------------------------------------------
test_that("rate_keener works", {
  output_1 <- rate_keener(ncaa2005, !!h2h_funs[["num"]])
  output_ref_1 <- rep(0.2, 5)
  names(output_ref_1) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output_1, output_ref_1)

  output_2 <- rate_keener(ncaa2005, !!h2h_funs[["sum_score"]])
  output_ref_2 <- c(0.0670593277911044, 0.350554576300443, 0.158498338171409,
                    0.160517490640876, 0.263370267096167)
  names(output_ref_2) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output_2, output_ref_2)
})

test_that("rate_keener handles `NULL` arguments", {
  output_1 <- rate_keener(ncaa2005, !!h2h_funs[["sum_score"]], skew_fun = NULL)
  output_ref_1 <- c(0.0898263460024877, 0.294757692678364, 0.164946133479184,
                    0.189136529764359, 0.261333298075606)
  names(output_ref_1) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output_1, output_ref_1)

  output_2 <- rate_keener(ncaa2005, !!h2h_funs[["sum_score"]],
                          normalize_fun = NULL)
  output_ref_2 <- c(0.0670593277911044, 0.350554576300443, 0.158498338171409,
                    0.160517490640876, 0.263370267096167)
  names(output_ref_2) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output_2, output_ref_2)
})


# rank_keener -------------------------------------------------------------
test_that("rank_keener works", {
  output_1 <- rank_keener(ncaa2005, !!h2h_funs[["sum_score"]])
  output_ref_1 <- c(5, 1, 4, 3, 2)
  names(output_ref_1) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output_1, output_ref_1)

  output_2 <- rank_keener(ncaa2005, !!h2h_funs[["num"]])
  output_ref_2 <- output_ref_1
  output_ref_2[] <- rep(3, 5)

  expect_equal(output_2, output_ref_2)
})

# force_nonneg ----------------------------------------------------------
test_that("force_nonneg works", {
  expect_equal(force_nonneg(1:10), 1:10)
  expect_equal(force_nonneg(-1:10, force = FALSE), -1:10)
  expect_true(all(force_nonneg(-1:10, force = TRUE) >= 0))
  expect_true(all(force_nonneg(-1:10) >= 0))
})


# skew_keener -------------------------------------------------------------
test_that("skew_keener works", {
  output <- skew_keener(-10:10)
  output_ref <-
    c(
      -1.79128784747792, -1.67944947177034, -1.56155281280883,
      -1.43649167310371, -1.30277563773199, -1.1583123951777,
      -1, -0.822875655532295, -0.618033988749895, -0.366025403784439, 0, 1,
      1.36602540378444, 1.61803398874989, 1.8228756555323, 2, 2.1583123951777,
      2.30277563773199, 2.43649167310371, 2.56155281280883, 2.67944947177034
    )

  expect_equal(output, output_ref)
})


# normalize_keener --------------------------------------------------------
test_that("normalize_keener works", {
  mat <- h2h_mat(ncaa2005, !!h2h_funs[["sum_score"]])

  output_ref <- matrix(
    c( 8.75, 1.75, 5.25, 1.75,    0,
         13, 34.5,  8.5, 6.25, 6.75,
          6,    4, 12.5, 1.75, 0.75,
        9.5, 4.25, 1.25, 18.5,  3.5,
      11.25, 1.75,  7.5,   13, 33.5),
    byrow = TRUE, nrow = 5,
    dimnames = list(
      c("Duke", "Miami", "UNC", "UVA", "VT"),
      c("Duke", "Miami", "UNC", "UVA", "VT")
      )
  )
  output_ref <- add_class(output_ref, "h2h_mat")

  expect_identical(normalize_keener(mat, ncaa2005), output_ref)
  expect_identical(normalize_keener(mat[, c(1, 3, 2)], ncaa2005),
                   output_ref[, c(1, 3, 2)])
})
