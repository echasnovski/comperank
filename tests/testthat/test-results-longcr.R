context("results-longcr")

input <- data.frame(
  playerscoregame_ID = rep(1:5, times = 2),
  gameId = rep(1:5, each = 2),
  scoreS = 31:40,
  scoreSP = 41:50
)

test_that("is_longcr works", {
  expect_true(is_longcr(to_longcr(input, repair = TRUE)))
})

test_that("to_longcr.default works", {
  # Test repairing
  output_1 <- dplyr::tibble(
    game = input$gameId,
    player = input$playerscoregame_ID,
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_1 <- add_class(output_1, "longcr")
  expect_identical(to_longcr(input, repair = TRUE), output_1)
  expect_identical(to_longcr(unclass(input), repair = TRUE), output_1)

  expect_error(to_longcr(input[, 1:2], repair = TRUE), "3 columns")

  output_2 <- dplyr::tibble(
    game = input$gameId,
    player = input$scoreSP,
    score = input$scoreS
  )
  output_2 <- add_class(output_2, "longcr")
  expect_warning(to_longcr(input[, -1], repair = TRUE), "not found.*player")
  expect_identical(suppressWarnings(to_longcr(input[, -1], repair = TRUE)),
                   output_2)

  # Test usage without repairing
  output_3 <- dplyr::tbl_df(input)
  output_3 <- add_class(output_3, "longcr")
  expect_identical(to_longcr(input, repair = FALSE), output_3)
})

test_that("to_longcr.widecr works", {
  input_widecr <- dplyr::tbl_df(data.frame(
    player1 = 11:20,
    score1 = 101:110,
    player2 = 12:21,
    score2 = 102:111,
    game = 2:11
  ))
  input_widecr <- add_class(input_widecr, "widecr")

  output_longcr_from_widecr <- dplyr::tbl_df(data.frame(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L)
  ))
  output_longcr_from_widecr <- add_class(output_longcr_from_widecr, "longcr")

  to_longcr_res <- to_longcr(input_widecr)
  expect_identical(to_longcr_res, output_longcr_from_widecr)

  # Using withou game column
  input_widecr_nogame <- input_widecr[, setdiff(colnames(input_widecr), "game")]
  input_widecr_nogame <- add_class(input_widecr_nogame, "widecr")
  output_longcr_from_widecr_nogame <- output_longcr_from_widecr
  output_longcr_from_widecr_nogame$game <- rep(1:10, each = 2)
  expect_identical(to_longcr(input_widecr_nogame),
                   output_longcr_from_widecr_nogame)


  # Converting twice doesn't affect the result
  expect_identical(to_longcr(to_longcr_res), to_longcr_res)

  # Throwing error on corrupted widecr object
  input_widecr_corrupt <- input_widecr[, -1]
  input_widecr_corrupt <- add_class(input_widecr_corrupt, "widecr")
  expect_error(to_longcr(input_widecr_corrupt), "not.*widecr")
})
