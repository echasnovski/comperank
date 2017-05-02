context("results-widecr")


# Input data --------------------------------------------------------------
input_1 <- data.frame(
  playerA = 1:10,
  playerB = 2:11,
  scoreC = 11:20,
  scoreB = 12:21,
  scoreA = 13:22,
  otherColumn =  101:110
)

player_vec <- 10:19
score_vec <- 100:109
input_2 <- as.data.frame(c(
  setNames(lapply(1:10, `+`, e2 = player_vec),
           paste0("player", 1:10)),
  setNames(lapply(1:10, `+`, e2 = score_vec),
           paste0("score", 1:10))
))

input_good <- data.frame(
  player1 = 1:10,
  score1 = 11:20,
  player2 = 2:11,
  score2 = 12:21
)

input_longcr <- dplyr::tbl_df(data.frame(
  game = rep(1:10, each = 2),
  player = rep(11:20, times = 2),
  score = 101:110
))
input_longcr <- add_class(input_longcr, "longcr")


# is_widecr ---------------------------------------------------------------
test_that("is_widecr works", {
  output_2 <- dplyr::tbl_df(input_2)
  output_2 <- add_class(output_2, "widecr")
  expect_true(is_widecr(output_2))

  expect_false(is_widecr(output_2[, -20]))

  class(output_2) <- "widecr"
  expect_false(is_widecr(output_2))
})


# to_widecr.default -------------------------------------------------------
test_that("to_widecr.default handles simple repairing", {
  output_1 <- dplyr::tbl_df(data.frame(
    player1 = 1:10,
    score1 = 13:22,
    player2 = 2:11,
    score2 = 12:21,
    player3 = rep(NA, 10),
    score3 = 11:20,
    otherColumn = 101:110
  ))
  output_1 <- add_class(output_1, "widecr")

  expect_identical(to_widecr(input_1, repair = TRUE), output_1)
})

test_that("to_widecr.default renames too many columns correctly", {
  expect_identical(
    colnames(to_widecr(input_2, repair = TRUE)),
    paste0(rep(c("player", "score"), times = 2),
           formatC(rep(1:10, each = 2), width = 2,
                   format = "d", flag = "0"))
  )
})

test_that("to_widecr.default works properly on good inputs", {
  output_good <- dplyr::tbl_df(input_good)
  output_good <- add_class(output_good, "widecr")

  expect_identical(to_widecr(input_good, repair = TRUE), output_good)
})

test_that("to_widecr.default works without repairing", {
  output_2 <- dplyr::tbl_df(input_2)
  output_2 <- add_class(output_2, "widecr")

  expect_identical(to_widecr(input_2, repair = FALSE), output_2)
})

test_that("to_widecr.default handles extra inputs", {
  expect_silent(to_widecr(input_good, repair = TRUE, extraArg = 1))
  expect_silent(to_widecr(input_good, repair = FALSE, extraArg = 1))
})


# to_widecr.longcr --------------------------------------------------------
test_that("to_widecr.longcr does simple converting", {
  output_widecr_from_longcr <- dplyr::tbl_df(data.frame(
    player1 = seq(from = 11L, to = 19L, by = 2L),
    score1 = seq(from = 101L, to = 109L, by = 2L),
    player2 = seq(from = 12L, to = 20L, by = 2L),
    score2 = seq(from = 102L, to = 110L, by = 2L),
    game = 1:10
  ))
  output_widecr_from_longcr <- add_class(output_widecr_from_longcr,
                                         "widecr")

  to_widecr_res <- to_widecr(input_longcr)
  expect_identical(to_widecr_res, output_widecr_from_longcr)
})

test_that("to_widecr.longcr throws error on corrupted longcr object", {
  longcr_corrupt <- input_longcr[, -1]
  class(longcr_corrupt) <- "longcr"
  expect_error(to_widecr(longcr_corrupt),
               "not.*longcr")
})


# to_widecr.widecr --------------------------------------------------------
test_that("to_widecr.widecr works", {
  to_widecr_res <- to_widecr(input_1, repair = TRUE)
  expect_identical(to_widecr(to_widecr_res, repair = TRUE), to_widecr_res)

  class(to_widecr_res) <- "widecr"
  expect_error(to_widecr(to_widecr_res, repair = TRUE), "not.*widecr")
})
