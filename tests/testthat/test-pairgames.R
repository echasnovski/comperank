context("pairgames")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = c(rep(1:5, each = 3), 6, 6, 7),
  player = c(rep(1:5, times = 3), 1, 2, 3),
  score = 101:118,
  extraCol = -(1:18)
)


# to_pairgames ------------------------------------------------------------
test_that("to_pairgames works", {
  output <- dplyr::tibble(
    game = 1:16,
    player1 = c(1, 1, 2, 4, 4, 5, 2, 2, 3, 5,
                5, 1, 3, 3, 4, 1),
    score1 = c(101L, 101L, 102L, 104L, 104L, 105L, 107L, 107L, 108L, 110L,
               110L, 111L, 113L, 113L, 114L, 116L),
    player2 = c(2, 3, 3, 5, 1, 1, 3, 4, 4, 1,
                2, 2, 4, 5, 5, 2),
    score2 = c(102L, 103L, 103L, 105L, 106L, 106L, 108L, 109L, 109L, 111L,
               112L, 112L, 114L, 115L, 115L, 117L)
  )
  output <- add_class(output, "widecr")

  expect_identical(to_pairgames(input), output)
})

test_that("to_pairgames handles NA and NaN", {
  input_na <- data.frame(
    game = rep(1L, 3),
    player = c(1, NA, NaN),
    score = 1:3
  )
  output_na <- dplyr::tibble(
    game = 1:3,
    player1 = c(1, 1, NA),
    score1 = c(1L, 1L, 2L),
    player2 = c(NA_real_, NaN, NaN),
    score2 = c(2L, 3L, 3L)
  )
  output_na <- add_class(output_na, "widecr")

  expect_identical(to_pairgames(input_na), output_na)
})

test_that("to_pairgames doesn't change pairgames", {
  expect_identical(to_widecr(ncaa2005), to_pairgames(ncaa2005))
})


# is_pairgames ------------------------------------------------------------
test_that("is_pairgames works", {
  expect_false(is_pairgames(input))
  expect_true(is_pairgames(to_pairgames(input)))
})
