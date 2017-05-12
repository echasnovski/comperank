context("head-to-head-helpers")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game = rep(1:2, each = 2),
  player = rep(1:2, times = 2),
  score = 31:34,
  scoreSP = 44:41
)


# players_drop -------------------------------------------------------------
test_that("players_drop works", {
  h2h_mat_original <- matrix(
    as.numeric(1:9), ncol = 3,
    dimnames = list(c(1, 2, 4), c(1, 2, 4))
  )
  h2h_mat_extra_players <- rbind(cbind(h2h_mat_original, NA_real_), NA_real_)
  colnames(h2h_mat_extra_players)[4] <- "3"
  rownames(h2h_mat_extra_players)[4] <- "3"

  expect_identical(players_drop(h2h_mat_extra_players), h2h_mat_original)

  expect_identical(players_drop(h2h_mat_extra_players, extraArg = TRUE),
                   h2h_mat_original)
})

# fill_h2h ----------------------------------------------------------------
test_that("fill_h2h works", {
  input <- matrix(NA_real_, nrow = 10, ncol = 10)
  output <- matrix(1, nrow = 10, ncol = 10)

  expect_equal(fill_h2h(h2h_mat = input, fill = 1),
               output)
})

# print.h2h ---------------------------------------------------------------
test_that("print.h2h works", {
  input <- matrix(1:9, nrow = 3, dimnames = list(c("a", "b", "c"),
                                                 c("a", "b", "c")))
  class(input) <- "h2h"

  expect_output(print(input), "  a b c\na 1 4 7\nb 2 5 8\nc 3 6 9")
})

# get_cr_players ----------------------------------------------------------
test_that("get_cr_players works", {
  input <- data.frame(
    game = 1:10,
    player = c(19L, 18L, 13L, 14L, 16L, 17L, 12L, 15L, 20L, 11L),
    score = 101:110
  )

  expect_identical(get_cr_players(input, NULL), 11:20)
  expect_identical(get_cr_players(input, 21:10), 21:10)

  expect_identical(get_cr_players(input, NULL, extraArg = TRUE), 11:20)
  expect_identical(get_cr_players(input, 21:10, extraArg = TRUE), 21:10)
})


# get_cr_matchups ------------------------------------------------------------
test_that("get_cr_matchups works", {
  output <- dplyr::tbl_df(data.frame(
    game = rep(1:2, each = 4),
    player1 = rep(rep(1:2, each = 2), times = 2),
    score1 = rep(31:34, each = 2),
    player2 = rep(1:2, times = 4),
    score2 = c(rep(31:32, times = 2), rep(33:34, times = 2))
  ))

  expect_equal(get_cr_matchups(cr_data), output)
})
