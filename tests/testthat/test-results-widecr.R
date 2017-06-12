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
  game = 1:10,
  player1 = 1:10,
  score1 = 11:20,
  player2 = 2:11,
  score2 = 12:21
)

input_longcr <- dplyr::tibble(
  game = rep(1:10, each = 2),
  player = rep(11:20, times = 2),
  score = rep(101:110, times = 2),
  otherCol = rep(-(1:10), times = 2)
)
input_longcr <- add_class(input_longcr, "longcr")


# is_widecr ---------------------------------------------------------------
test_that("is_widecr works", {
  output_2 <- dplyr::as_tibble(input_2)
  output_2 <- add_class(output_2, "widecr")
  expect_true(is_widecr(output_2))

  expect_false(is_widecr(output_2[, -20]))

  class(output_2) <- "widecr"
  expect_false(is_widecr(output_2))
})

test_that("is_widecr returns FALSE on non-digit pair identifier", {
  output_2 <- dplyr::as_tibble(input_2)
  output_2 <- add_class(output_2, "widecr")
  colnames(output_2)[c(1, 11)] <- c("playerA", "scoreA")

  expect_false(is_widecr(output_2))
})

test_that("is_widecr handles data with no 'player' or 'score' columns", {
  input_nocols <- dplyr::tibble(x = 1:10)
  input_nocols <- add_class(input_nocols, "widecr")
  expect_false(is_widecr(input_nocols))
})


# to_widecr.default -------------------------------------------------------
test_that("to_widecr.default handles simple repairing", {
  output_1 <- dplyr::tibble(
    player1 = 1:10,
    score1 = 13:22,
    player2 = 2:11,
    score2 = 12:21,
    player3 = rep(NA_integer_, 10),
    score3 = 11:20,
    otherColumn = 101:110
  )
  output_1 <- add_class(output_1, "widecr")

  expect_identical(to_widecr(input_1, repair = TRUE), output_1)
})

test_that("to_widecr.default throws an error if no column is matched", {
  input_bad_colnames <- input_1
  colnames(input_bad_colnames) <- seq_len(ncol(input_bad_colnames))

  expect_warning(to_widecr(input_bad_colnames, repair = TRUE),
                 "Neither 'player' nor 'score' columns are detected.")
  expect_equal(suppressWarnings(to_widecr(input_bad_colnames, repair = TRUE)),
               input_bad_colnames)
})

test_that("to_widecr.default places column 'game' on first place", {
  input_game_col <- input_1
  input_game_col$game <- 1001:1010

  output_game_col <- dplyr::tibble(
    game = 1001:1010,
    player1 = 1:10,
    score1 = 13:22,
    player2 = 2:11,
    score2 = 12:21,
    player3 = rep(NA_integer_, 10),
    score3 = 11:20,
    otherColumn = 101:110
  )
  output_game_col <- add_class(output_game_col, "widecr")

  expect_identical(to_widecr(input_game_col, repair = TRUE), output_game_col)
})

test_that("to_widecr.default correctly renames many columns", {
  expect_identical(
    colnames(to_widecr(input_2, repair = TRUE)),
    paste0(rep(c("player", "score"), times = 2),
           formatC(rep(1:10, each = 2), width = 2,
                   format = "d", flag = "0"))
  )
})

test_that("to_widecr.default works properly on good inputs", {
  output_good <- dplyr::as_tibble(input_good)
  output_good <- add_class(output_good, "widecr")

  expect_identical(to_widecr(input_good, repair = TRUE), output_good)
})

test_that("to_widecr.default preserves column types", {
  input_types <- input_good
  input_types$game <- 1:10
  input_types$extraCol <- -(1:10)
  output_types <- dplyr::as_tibble(input_good)
  output_types$game <- 1:10
  output_types$extraCol <- -(1:10)
  output_types <- add_class(output_types, "widecr")

  input_types1 <- input_types
  output_types1 <- output_types
  input_types1$game <- factor(input_types1$game, levels = 1:10)
  output_types1$game <- factor(output_types1$game, levels = 1:10)
  expect_identical(to_widecr(input_types1, repair = TRUE), output_types1,
                   info = "Factor 'game'")

  input_types2 <- input_types
  output_types2 <- output_types
  input_types2$game <- as.character(input_types2$game)
  output_types2$game <- as.character(output_types2$game)
  expect_identical(to_widecr(input_types2, repair = TRUE), output_types2,
                   info = "Character 'game'")

  input_types3 <- input_types
  output_types3 <- output_types
  input_types3$player1 <- factor(input_types3$player1, levels = 1:11)
  input_types3$player2 <- factor(input_types3$player2, levels = 1:11)
  output_types3$player1 <- factor(output_types3$player1, levels = 1:11)
  output_types3$player2 <- factor(output_types3$player2, levels = 1:11)
  expect_identical(to_widecr(input_types3, repair = TRUE), output_types3,
                   info = "Factor 'player'")

  input_types4 <- input_types
  output_types4 <- output_types
  input_types4$player1 <- as.character(input_types4$player1)
  input_types4$player2 <- as.character(input_types4$player2)
  output_types4$player1 <- as.character(output_types4$player1)
  output_types4$player2 <- as.character(output_types4$player2)
  expect_identical(to_widecr(input_types4, repair = TRUE), output_types4,
                   info = "Character 'player'")

  input_types5 <- input_types
  output_types5 <- output_types
  input_types5$score1 <- as.character(input_types5$score1)
  input_types5$score2 <- as.character(input_types5$score2)
  output_types5$score1 <- as.character(output_types5$score1)
  output_types5$score2 <- as.character(output_types5$score2)
  expect_identical(to_widecr(input_types5, repair = TRUE), output_types5,
                   info = "Character 'score'")

  input_types6 <- input_types
  output_types6 <- output_types
  list_scores <- lapply(1:10, function(i) {
    c(points = 100 + i, type = i %% 2)
  })
  input_types6$score1 <- I(list_scores)
  class(input_types6$score1) <- NULL
  input_types6$score2 <- I(list_scores)
  class(input_types6$score2) <- NULL
  output_types6$score1 <- list_scores
  output_types6$score2 <- list_scores
  expect_identical(to_widecr(input_types6, repair = TRUE), output_types6,
                   info = "List-column 'score'")
})

test_that("to_widecr.default works without repairing", {
  output_2 <- dplyr::as_tibble(input_2)
  output_2 <- add_class(output_2, "widecr")

  expect_identical(to_widecr(input_2, repair = FALSE), output_2)
})

test_that("to_widecr.default handles extra arguments", {
  expect_silent(to_widecr(input_good, repair = TRUE, extraArg = 1))
  expect_silent(to_widecr(input_good, repair = FALSE, extraArg = 1))
})


# to_widecr.longcr --------------------------------------------------------
test_that("to_widecr.longcr does simple converting", {
  output_widecr_from_longcr <- dplyr::tibble(
    game = 1:10,
    player1 = rep(seq(from = 11L, to = 19L, by = 2L), 2),
    score1 = rep(seq(from = 101L, to = 109L, by = 2L), 2),
    player2 = rep(seq(from = 12L, to = 20L, by = 2L), 2),
    score2 = rep(seq(from = 102L, to = 110L, by = 2L), 2)
  )
  output_widecr_from_longcr <- add_class(output_widecr_from_longcr,
                                         "widecr")

  to_widecr_res <- to_widecr(input_longcr)
  expect_identical(to_widecr_res, output_widecr_from_longcr)
})

test_that("to_widecr.longcr correctly renames many columns", {
  input_many_pairs <- input_longcr
  input_many_pairs <- dplyr::bind_rows(
    input_many_pairs,
    input_many_pairs[rep(1, 10), ]
  )

  output_colnames <-
    c(
      "game",
      paste0(rep(c("player", "score"), times = 12),
             formatC(rep(1:12, each = 2), width = 2, format = "d", flag = "0"))
    )

  expect_identical(
    colnames(to_widecr(input_many_pairs, repair = TRUE)),
    output_colnames
  )
})

test_that("to_widecr.longcr throws error on corrupted longcr object", {
  longcr_corrupt <- input_longcr[, -1]
  class(longcr_corrupt) <- "longcr"
  expect_error(to_widecr(longcr_corrupt),
               "not.*longcr")
})

test_that("to_widecr.longcr preserves column types", {
  input_types <- input_longcr
  output_types <- dplyr::tibble(
    game = 1:10,
    player1 = rep(seq(from = 11L, to = 19L, by = 2L), 2),
    score1 = rep(seq(from = 101L, to = 109L, by = 2L), 2),
    player2 = rep(seq(from = 12L, to = 20L, by = 2L), 2),
    score2 = rep(seq(from = 102L, to = 110L, by = 2L), 2)
  )
  output_types <- add_class(output_types, "widecr")

  input_types1 <- input_types
  output_types1 <- output_types
  input_types1$game <- factor(input_types1$game, levels = 1:10)
  output_types1$game <- factor(output_types1$game, levels = 1:10)
  expect_identical(to_widecr(input_types1, repair = TRUE), output_types1,
                   info = "Factor 'game'")

  input_types2 <- input_types
  output_types2 <- output_types
  input_types2$game <- as.character(input_types2$game)
  output_types2$game <- as.character(output_types2$game)
  output_types2[, ] <- output_types2[order(output_types2$game), ]
  expect_identical(to_widecr(input_types2, repair = TRUE), output_types2,
                   info = "Character 'game'")

  input_types3 <- input_types
  output_types3 <- output_types
  input_types3$player <- factor(input_types3$player, levels = 11:20)
  output_types3$player1 <- factor(output_types3$player1, levels = 11:20)
  output_types3$player2 <- factor(output_types3$player2, levels = 11:20)
  expect_identical(to_widecr(input_types3, repair = TRUE), output_types3,
                   info = "Factor 'player'")

  input_types4 <- input_types
  output_types4 <- output_types
  input_types4$player <- as.character(input_types4$player)
  output_types4$player1 <- as.character(output_types4$player1)
  output_types4$player2 <- as.character(output_types4$player2)
  expect_identical(to_widecr(input_types4, repair = TRUE), output_types4,
                   info = "Character 'player'")

  input_types5 <- input_types
  output_types5 <- output_types
  input_types5$score <- as.character(input_types5$score)
  output_types5$score1 <- as.character(output_types5$score1)
  output_types5$score2 <- as.character(output_types5$score2)
  expect_identical(to_widecr(input_types5, repair = TRUE), output_types5,
                   info = "Character 'score'")

  input_types6 <- input_types
  output_types6 <- output_types
  list_scores <- lapply(1:10, function(i) {
    c(points = 100 + i, type = i %% 2)
  })
  input_types6$score <- rep(list_scores, each = 2)
  output_types6$score1 <- list_scores
  output_types6$score2 <- list_scores
  expect_identical(to_widecr(input_types6, repair = TRUE), output_types6,
                   info = "List-column 'score'")
})


# to_widecr.widecr --------------------------------------------------------
test_that("to_widecr.widecr works", {
  to_widecr_res <- to_widecr(input_1, repair = TRUE)
  expect_identical(to_widecr(to_widecr_res, repair = TRUE), to_widecr_res)

  class(to_widecr_res) <- "widecr"
  expect_error(to_widecr(to_widecr_res, repair = TRUE), "not.*widecr")
})


# print.widecr ------------------------------------------------------------
test_that("print.widecr works", {
  expect_output(print(to_widecr(input_good)),
                "# A widecr object:\n# A tibble")
})
