context("results-widecr")

input_1 <- data.frame(
  player_1 = 1:10,
  player__2 = 2:11,
  score_1 = 11:20,
  score1 = 12:21,
  score__2 = 13:22,
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

test_that("is_widecr works", {
  output_2 <- dplyr::tbl_df(input_2)
  output_2 <- add_class(output_2, "widecr")
  expect_true(is_widecr(output_2))
  expect_false(is_widecr(output_2[, -20]))
})

test_that("to_widecr.default works", {
  # Test repairing
  output_1 <- tbl_df(data.frame(
    player1 = rep(NA, 10),
    score1 = 12:21,
    player2 = 1:10,
    score2 = 11:20,
    player3 = 2:11,
    score3 = 13:22,
    otherColumn = 101:110
  ))
  output_1 <- add_class(output_1, "widecr")
  expect_identical(to_widecr(input_1, repair = TRUE), output_1)

  expect_identical(
    colnames(to_widecr(input_2, repair = TRUE)),
    paste0(rep(c("player", "score"), times = 2),
           formatC(rep(1:10, each = 2), width = 2,
                   format = "d", flag = "0"))
  )

  # Test usage without repairing
  output_2 <- dplyr::tbl_df(input_2)
  output_2 <- add_class(output_2, "widecr")
  expect_identical(to_widecr(input_2, repair = FALSE), output_2)
})

test_that("to_widecr.longcr works", {
  input_longcr <- dplyr::tbl_df(data.frame(
    game = rep(1:10, each = 2),
    player = rep(11:20, times = 2),
    score = 101:110
  ))
  input_longcr <- add_class(input_longcr, "longcr")

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

  # Converting twice doesn't affect the result
  expect_identical(to_widecr(to_widecr_res), to_widecr_res)

  # Throwing error on corrupted longcr object
  longcr_corrupt <- input_longcr[, -1]
  class(longcr_corrupt) <- "longcr"
  expect_error(to_widecr(longcr_corrupt),
               "not.*longcr")
})
