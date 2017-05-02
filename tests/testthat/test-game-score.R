context("game-score")


# Input data --------------------------------------------------------------
input <- to_longcr(data.frame(
  game = rep(1:5, each = 2),
  player = rep(1:5, times = 2),
  score = 31:40
))


# get_game_scores ---------------------------------------------------------
test_that("get_game_scores works with NULL score_game", {
  output <- structure(
    list(
      game = 1:5
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -5L),
    .Names = c("game")
  )

  # input_game_scores <- get_game_scores(cr_data = input,
  #                                      score_game = score_game_mean_sd)

  expect_equal(get_game_scores(cr_data = input,
                               score_game = NULL),
               output)
  # expect_identical(class(input_game_scores), class(output))
  # expect_equal(input_game_scores$game, output$game)
  # expect_equal(input_game_scores$meanScore, output$meanScore)
  # expect_equal(input_game_scores$sdScore, output$sdScore)
})

test_that("get_game_scores works with not NULL score_game", {
  output <- structure(
    list(
      game = 1:5,
      meanScore = c(31.5, 33.5, 35.5, 37.5, 39.5),
      sdScore = rep(0.707106781186548, 5)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -5L),
    .Names = c("game", "meanScore", "sdScore")
  )

  input_game_scores <- get_game_scores(cr_data = input,
                                       score_game = score_game_mean_sd)

  expect_identical(class(input_game_scores), class(output))
  expect_equal(input_game_scores$game, output$game)
  expect_equal(input_game_scores$meanScore, output$meanScore)
  expect_equal(input_game_scores$sdScore, output$sdScore)
})


# score_game_mean_sd ------------------------------------------------------
test_that("score_game_mean_sd works", {
  input_game <- input[1:2, ]
  output_game <- c(meanScore = 31.5, sdScore = 0.707106781186548)

  expect_equal(score_game_mean_sd(input_game), output_game)
})
