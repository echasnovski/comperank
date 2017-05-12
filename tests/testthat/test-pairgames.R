context("pairgames")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = c(rep(1:5, each = 3), 6),
  player = c(rep(1:5, times = 3), 1),
  score = 101:116,
  extraCol = -(1:16)
)


# to_pairgames ------------------------------------------------------------
test_that("to_pairgames works", {
  output <- dplyr::tbl_df(data.frame(
    game = 1:15,
    player1 = c(1, 1, 2, 4, 1, 1, 2, 2, 3, 1,
                1, 2, 3, 3, 4),
    score1 = c(101L, 101L, 102L, 104L, 106L, 106L, 107L, 107L, 108L, 111L,
               111L, 112L, 113L, 113L, 114L),
    player2 = c(2, 3, 3, 5, 4, 5, 3, 4, 4, 5,
                2, 5, 4, 5, 5),
    score2 = c(102L, 103L, 103L, 105L, 104L, 105L, 108L, 109L, 109L, 110L,
               112L, 110L, 114L, 115L, 115L)
  ))
  output <- add_class(output, "widecr")

  expect_identical(to_pairgames(input), output)
})
