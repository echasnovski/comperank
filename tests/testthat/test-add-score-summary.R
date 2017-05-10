context("add-score-summary")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = rep(1:20, each = 2),
  player = rep(1:10, times = 4),
  score = 31:70,
  season = rep(1:2, each = 20)
)

input_widecr <- to_widecr(data.frame(
  player1 = 1:10, score1 = 10:19,
  player2 = c(2:10, 1), score2 = 11:20
))

# add_score_summary -------------------------------------------------------
test_that("add_score_summary works for NULL score_fun", {
  expect_identical(add_score_summary(input, item = "game"), input)
  expect_identical(add_score_summary(input, item = "game", score_fun = NULL),
                   input)
})

test_that("add_score_summary works for not NULL score_fun", {
  output <- input
  output$minScore <- rep(c(31L, 51L), each = 20)
  output$maxScore <- rep(c(50L, 70L), each = 20)

  expect_identical(add_score_summary(input, item = "season",
                                     score_fun = score_min_max),
                   output)
})

test_that("add_score_summary handles absence of 'game' column", {
  output <- input_widecr
  output$meanScore <- 10:19 + 0.5
  output$sdScore <- rep(0.707106781186548, times = 10)

  output_res <- add_score_summary(input_widecr, item = "game",
                                  score_fun = score_mean_sd)

  expect_identical(output_res[, -6], output[, -6])
  expect_equal(output_res[[6]], output[[6]])
})

test_that("add_score_summary can add prefix", {
  output_res <- add_score_summary(input_widecr, item = "game",
                                  score_fun = score_mean_sd, prefix = "game_")

  expect_equal(
    colnames(output_res),
    c("player1", "score1", "player2", "score2",
      "game_meanScore", "game_sdScore")
  )
})

test_that("add_score_summary works with extra arguments", {
  expect_silent(add_score_summary(input_widecr, item = "game",
                                  score_fun = score_mean_sd, prefix = "game_",
                                  extraArg = TRUE))
})


# add_game_summary --------------------------------------------------------
test_that("add_game_summary works", {
  expect_silent(add_game_summary(input_widecr, score_fun = score_mean_sd,
                                 prefix = "game_", extraArg = TRUE))
})


# add_player_summary --------------------------------------------------------
test_that("add_player_summary works", {
  expect_silent(add_player_summary(input, score_fun = score_mean_sd,
                                   prefix = "player_", extraArg = TRUE))
})
