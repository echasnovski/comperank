context("head-to_head")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  player = rep(1:2, times = 2),
  game = rep(1:2, each = 2),
  score = 31:34,
  scoreSP = 41:44
)


# h2h_abstract ------------------------------------------------------------
test_that("h2h_abstract works", {
  output_1 <- matrix(c(0, -1, 1, 0), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_1) <- c("h2h", "matrix")

  output_res_1 <- h2h_abstract(
    cr_data = cr_data, h2h_fun = score_diff,
    aggr_fun = mean,
    score_game = NULL, players = NULL, absent_players = players_drop,
    absent_h2h = h2h_fill
  )

  expect_identical(output_res_1, output_1)
  expect_equal(class(output_res_1)[1], "h2h")
  expect_true(inherits(output_res_1, "matrix"))

  output_2 <- output_1 * 2
  output_res_2 <- h2h_abstract(
    cr_data = cr_data, h2h_fun = score_diff,
    aggr_fun = sum,
    score_game = NULL, players = NULL, absent_players = players_drop,
    absent_h2h = h2h_fill
  )

  expect_identical(output_res_2, output_2)

  expect_silent(h2h_abstract(
    cr_data = cr_data, h2h_fun = score_diff,
    aggr_fun = sum,
    score_game = NULL, players = NULL, absent_players = players_drop,
    absent_h2h = h2h_fill,
    extrArg = TRUE
  ))
})

# players_drop -------------------------------------------------------------
test_that("players_drop works", {
  h2h_mat_original <- matrix(
    as.numeric(1:9), ncol = 3,
    dimnames = list(c(1, 2, 4), c(1, 2, 4))
  )
  h2h_mat_extra <- rbind(cbind(h2h_mat_original, NA_real_), NA_real_)
  colnames(h2h_mat_extra)[4] <- "3"
  rownames(h2h_mat_extra)[4] <- "3"

  expect_identical(players_drop(h2h_mat_extra), h2h_mat_original)

  expect_identical(players_drop(h2h_mat_extra, extraArg = TRUE),
                   h2h_mat_original)
})

# h2h_fill ----------------------------------------------------------------
test_that("h2h_fill works", {
  input <- matrix(NA_real_, nrow = 10, ncol = 10)
  output <- matrix(1, nrow = 10, ncol = 10)

  expect_equal(h2h_fill(h2h_mat = input, fill = 1),
               output)
})

# print.h2h ---------------------------------------------------------------
test_that("print.h2h works", {
  input <- matrix(1:9, nrow = 3, dimnames = list(c("a", "b", "c"),
                                                 c("a", "b", "c")))
  class(input) <- "h2h"

  expect_output(print(input), "  a b c\na 1 4 7\nb 2 5 8\nc 3 6 9")
})

# get_players ----------------------------------------------------------
test_that("get_players works", {
  input <- data.frame(
    game = 1:10,
    player = 11:20,
    score = 101:110
  )

  expect_identical(get_players(input, NULL), 11:20)
  expect_identical(get_players(input, 10:21), 10:21)

  expect_identical(get_players(input, NULL, extraArg = TRUE), 11:20)
  expect_identical(get_players(input, 10:21, extraArg = TRUE), 10:21)
})


# get_matchups ------------------------------------------------------------
test_that("get_matchups works", {
  output <- dplyr::tbl_df(data.frame(
    game = rep(1:2, each = 4),
    player1 = rep(rep(1:2, each = 2), times = 2),
    score1 = rep(31:34, each = 2),
    player2 = rep(1:2, times = 4),
    score2 = c(rep(31:32, times = 2), rep(33:34, times = 2))
  ))

  expect_equal(get_matchups(cr_data), output)
})

# score_diff --------------------------------------------------------------
test_that("score_diff works", {
  expect_equal(score_diff(1, 2), 1)
  expect_equal(score_diff(2, 1), -1)
  expect_equal(score_diff(1, 1), 0)
})
