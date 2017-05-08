context("results-longcr")


# Input data --------------------------------------------------------------
input <- data.frame(
  playerscoregame_ID = rep(1:5, times = 2),
  gameId = rep(1:5, each = 2),
  scoreS = 31:40,
  scoreSP = 41:50
)

input_good <- data.frame(
  game = 1:10,
  player = 11:20,
  score = 101:110
)

input_widecr <- dplyr::tbl_df(data.frame(
  player1 = 11:20,
  score1 = 101:110,
  player2 = 12:21,
  score2 = 102:111,
  game = 2:11
))
input_widecr <- add_class(input_widecr, "widecr")


# is_longcr ---------------------------------------------------------------
test_that("is_longcr works", {
  expect_true(is_longcr(to_longcr(input, repair = TRUE)))
})


# to_longcr.default -------------------------------------------------------
test_that("to_longcr.default handles simple repairing", {
  output_1 <- dplyr::tibble(
    game = input$gameId,
    player = input$playerscoregame_ID,
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_1 <- add_class(output_1, "longcr")

  expect_identical(to_longcr(input, repair = TRUE), output_1)
  expect_identical(to_longcr(unclass(input), repair = TRUE), output_1)
})

test_that("to_longcr.default handles missing columns correctly", {
  output_2 <- dplyr::tibble(
    game = input$gameId,
    player = rep(NA_integer_, nrow(input)),
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_2 <- add_class(output_2, "longcr")

  expect_message(to_longcr(input[, -1], repair = TRUE), "not found.*player")
  expect_identical(suppressMessages(to_longcr(input[, -1], repair = TRUE)),
                   output_2)
})

test_that("to_longcr.default works properly on good inputs", {
  output_good <- dplyr::tbl_df(input_good)
  output_good <- add_class(output_good, "longcr")

  expect_identical(to_longcr(input_good, repair = TRUE), output_good)
  expect_silent(to_longcr(input_good, repair = TRUE))
})

test_that("to_longcr.default removes duplicated 'game'-'player' pairs", {
  input_good_extra_row <- dplyr::bind_rows(input_good, input_good[10, ])

  expect_identical(to_longcr(input_good_extra_row, repair = TRUE),
                   to_longcr(input_good, repair = TRUE))
})

test_that("to_longcr.default preserves column types", {
  input_types <- input
  output_types <- dplyr::tibble(
    game = input$gameId,
    player = input$playerscoregame_ID,
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_types <- add_class(output_types, "longcr")

  input_types1 <- input_types
  output_types1 <- output_types
  input_types1$gameId <- factor(input_types1$gameId)
  output_types1$game <- factor(output_types1$game)
  expect_identical(to_longcr(input_types1, repair = TRUE), output_types1,
                   info = "Factor 'game'")

  input_types2 <- input_types
  output_types2 <- output_types
  input_types2$gameId <- as.character(input_types2$gameId)
  output_types2$game <- as.character(output_types2$game)
  expect_identical(to_longcr(input_types2, repair = TRUE), output_types2,
                   info = "Character 'game'")

  input_types3 <- input_types
  output_types3 <- output_types
  input_types3$playerscoregame_ID  <- factor(input_types3$playerscoregame_ID)
  output_types3$player <- factor(output_types3$player)
  expect_identical(to_longcr(input_types3, repair = TRUE), output_types3,
                   info = "factor 'player'")

  input_types4 <- input_types
  output_types4 <- output_types
  input_types4$playerscoregame_ID  <-
    as.character(input_types4$playerscoregame_ID)
  output_types4$player <- as.character(output_types4$player)
  expect_identical(to_longcr(input_types4, repair = TRUE), output_types4,
                   info = "Character 'player'")

  input_types5 <- input_types
  output_types5 <- output_types
  input_types5$scoreS <- as.character(input_types5$scoreS)
  output_types5$score <- as.character(output_types5$score)
  expect_identical(to_longcr(input_types5, repair = TRUE), output_types5,
                   info = "Character 'score'")

  input_types6 <- input_types
  output_types6 <- output_types
  list_scores <- lapply(1:10, function(i) {
    c(points = 100 + i, type = i %% 2)
  })
  input_types6$scoreS <- I(list_scores)
  class(input_types6$scoreS) <- NULL
  output_types6$score <- list_scores
  expect_identical(to_longcr(input_types6, repair = TRUE), output_types6,
                   info = "List-column 'score'")
})

test_that("to_longcr.default works without repairing", {
  output_3 <- dplyr::tbl_df(input)
  output_3 <- add_class(output_3, "longcr")

  expect_identical(to_longcr(input, repair = FALSE), output_3)
})

test_that("to_longcr.default handles extra arguments", {
  expect_silent(to_longcr(input_good, repair = TRUE, extraArg = 1))
  expect_silent(to_longcr(input_good, repair = FALSE, extraArg = 1))
})


# to_longcr.widecr --------------------------------------------------------
test_that("to_longcr.widecr does simple converting", {
  output_longcr_from_widecr <- dplyr::tbl_df(data.frame(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L)
  ))
  output_longcr_from_widecr <- add_class(output_longcr_from_widecr, "longcr")

  to_longcr_res <- to_longcr(input_widecr)

  expect_identical(to_longcr_res, output_longcr_from_widecr)
})

test_that("to_longcr.widecr preserves column types", {
  input_types <- input_widecr
  output_types <- dplyr::tbl_df(data.frame(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L)
  ))
  output_types <- add_class(output_types, "longcr")

  input_types1 <- input_types
  output_types1 <- output_types
  input_types1$game <- factor(input_types1$game, levels = 2:11)
  output_types1$game <- factor(output_types1$game, levels = 2:11)
  expect_identical(to_longcr(input_types1, repair = TRUE), output_types1,
                   info = "Factor 'game'")

  input_types2 <- input_types
  output_types2 <- output_types
  input_types2$game <- as.character(input_types2$game)
  output_types2$game <- as.character(output_types2$game)
  expect_identical(to_longcr(input_types2, repair = TRUE), output_types2,
                   info = "Character 'game'")

  input_types3 <- input_types
  output_types3 <- output_types
  input_types3$player1 <- factor(input_types3$player1, levels = 11:21)
  input_types3$player2 <- factor(input_types3$player2, levels = 11:21)
  output_types3$player <- factor(output_types3$player, levels = 11:21)
  expect_identical(to_longcr(input_types3, repair = TRUE), output_types3,
                   info = "Factor 'player'")

  input_types4 <- input_types
  output_types4 <- output_types
  input_types4$player1 <- as.character(input_types4$player1)
  input_types4$player2 <- as.character(input_types4$player2)
  output_types4$player <- as.character(output_types4$player)
  expect_identical(to_longcr(input_types4, repair = TRUE), output_types4,
                   info = "Character 'player'")

  input_types5 <- input_types
  output_types5 <- output_types
  input_types5$score1 <- as.character(input_types5$score1)
  input_types5$score2 <- as.character(input_types5$score2)
  output_types5$score <- as.character(output_types5$score)
  expect_identical(to_longcr(input_types5, repair = TRUE), output_types5,
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
  output_types6$score <- I(rep(list_scores, each = 2))
  class(output_types6$score) <- NULL
  expect_identical(to_longcr(input_types6, repair = TRUE), output_types6,
                   info = "List-column 'score'")
})

test_that("to_longcr.widecr works without column 'game'", {
  output_longcr_from_widecr <- dplyr::tbl_df(data.frame(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L)
  ))
  output_longcr_from_widecr <- add_class(output_longcr_from_widecr, "longcr")

  input_widecr_nogame <- input_widecr[, setdiff(colnames(input_widecr), "game")]
  input_widecr_nogame <- add_class(input_widecr_nogame, "widecr")

  output_longcr_from_widecr_nogame <- output_longcr_from_widecr
  output_longcr_from_widecr_nogame$game <- rep(1:10, each = 2)

  expect_identical(to_longcr(input_widecr_nogame),
                   output_longcr_from_widecr_nogame)
})

test_that("to_longcr.widecr throws error on corrupted widecr object", {
  input_widecr_corrupt <- input_widecr[, -1]
  input_widecr_corrupt <- add_class(input_widecr_corrupt, "widecr")

  expect_error(to_longcr(input_widecr_corrupt), "not.*widecr")
})


# to_longcr.longcr --------------------------------------------------------
test_that("to_longcr.longcr works", {
  to_longcr_res <- to_longcr(input_good)
  expect_identical(to_longcr(to_longcr_res, repair = TRUE), to_longcr_res)

  class(to_longcr_res) <- "longcr"
  expect_error(to_longcr(to_longcr_res, repair = TRUE), "not.*longcr")
})
