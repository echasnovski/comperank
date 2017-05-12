context("item-summary-functions")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = rep(1:20, each = 2),
  player = rep(1:10, times = 4),
  score = 31:70,
  season = rep(1:2, each = 20)
)


# Custom expectations -----------------------------------------------------
expect_summary_fun_works <- function(summary_fun, input_data, output_data) {
  expect_equal(summary_fun(input_data), output_data)
  expect_silent(summary_fun(input_data, extraArg = TRUE))
}

expect_add_prefix <- function(summary_fun, input_data, raw_summary_col_names) {
  output_summary <-
    get_item_summary(
      cr_data = input_data, item = c("season", "player"),
      summary_fun = summary_fun, prefix = "seasonPlayer_"
    )

  expect_equal(
    colnames(output_summary),
    c("season", "player", paste0("seasonPlayer_", raw_summary_col_names))
  )
}

# summary_mean_sd_score ---------------------------------------------------
test_that("summary_mean_sd_score works", {
  input_game <- input[1:2, ]
  output_game <- c(meanScore = 31.5, sdScore = 0.707106781186548)

  expect_summary_fun_works(summary_mean_sd_score, input_game, output_game)
})

test_that("summary_mean_sd_score adds prefix", {
  expect_add_prefix(
    summary_fun = summary_mean_sd_score,
    input_data = input,
    raw_summary_col_names = c("meanScore", "sdScore")
  )
})


# summary_min_max_score ---------------------------------------------------
test_that("summary_min_max_score works", {
  input_game <- input[1:2, ]
  output_game <- c(minScore = 31, maxScore = 32)

  expect_summary_fun_works(summary_min_max_score, input_game, output_game)
})

test_that("summary_min_max_score adds prefix", {
  expect_add_prefix(
    summary_fun = summary_min_max_score,
    input_data = input,
    raw_summary_col_names = c("minScore", "maxScore")
  )
})

# summary_sum_score -------------------------------------------------------
test_that("summary_sum_score works", {
  input_game <- input[1:2, ]
  output_game <- c(sumScore = 63)

  expect_summary_fun_works(summary_sum_score, input_game, output_game)
})

test_that("summary_sum_score adds prefix", {
  expect_add_prefix(
    summary_fun = summary_sum_score,
    input_data = input,
    raw_summary_col_names = "sumScore"
  )
})

# summary_num_games -------------------------------------------------------
test_that("summary_num_games works", {
  output <- c(numGames = 20)
  expect_summary_fun_works(summary_num_games, input, output)
})

test_that("summary_num_games adds prefix", {
  expect_add_prefix(
    summary_fun = summary_num_games,
    input_data = input,
    raw_summary_col_names = "numGames"
  )
})


# summary_num_players -------------------------------------------------------
test_that("summary_num_players works", {
  output <- c(numPlayers = 10)
  expect_summary_fun_works(summary_num_players, input, output)
})

test_that("summary_num_players adds prefix", {
  expect_add_prefix(
    summary_fun = summary_num_players,
    input_data = input,
    raw_summary_col_names = "numPlayers"
  )
})
