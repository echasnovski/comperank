context("keener")


# rate_keener -------------------------------------------------------------
test_that("rate_keener works", {
  output <- rate_keener(ncaa2005, h2h_fun = h2h_num)
  output_ref <- rep(0.2, 5)
  names(output_ref) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output, output_ref)
})

test_that("rate_keener handles extra arguments", {
  output <- rate_keener(ncaa2005, h2h_fun = h2h_sum_score, trans = TRUE)
  output_ref <- c(0.0670593277911044, 0.350554576300443, 0.158498338171409,
                  0.160517490640876, 0.263370267096167)
  names(output_ref) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(output, output_ref)
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

test_that("skew_keener handles extra arguments", {
  expect_silent(skew_keener(1:10, extraArg = TRUE))
})


# normalize_keener --------------------------------------------------------
test_that("normalize_keener works", {
  h2h_mat <- get_h2h(ncaa2005, h2h_sum_score)

  output_ref <- matrix(
    c(8.75,   13,    6,  9.5, 11.25,
      1.75, 34.5,    4, 4.25,  1.75,
      5.25,  8.5, 12.5, 1.25,   7.5,
      1.75, 6.25, 1.75, 18.5,    13,
      0,    6.75, 0.75,  3.5,  33.5),
    byrow = TRUE, nrow = 5,
    dimnames = list(
      c("Duke", "Miami", "UNC", "UVA", "VT"),
      c("Duke", "Miami", "UNC", "UVA", "VT")
      )
  )
  output_ref <- add_class(output_ref, "h2h")

  expect_identical(normalize_keener(h2h_mat, ncaa2005), output_ref)
  expect_identical(normalize_keener(h2h_mat[, c(1, 3, 2)], ncaa2005),
                   output_ref[, c(1, 3, 2)])
  expect_message(normalize_keener(h2h_mat, ncaa2005[-(1:8), ]),
                 "^normalize_keener.*row.*names.*absent.*data.*Duke")
})

test_that("normilize_keener handles extra arguments", {
  h2h_mat <- get_h2h(ncaa2005, h2h_sum_score)
  expect_silent(normalize_keener(h2h_mat, ncaa2005, extraArg = TRUE))
})



