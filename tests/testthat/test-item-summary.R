context("item-summary")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = rep(1:20, each = 2),
  player = rep(1:10, times = 4),
  score = 31:70,
  season = rep(1:2, each = 20)
)


# get_item_summary ---------------------------------------------------------
test_that("get_item_summary works with NULL summary_fun", {
  output <- dplyr::tibble(game = 1:20)

  expect_identical(
    get_item_summary(cr_data = input, item = "game",
                     summary_fun = NULL),
    output
  )
})

test_that("get_item_summary works with not NULL summary_fun", {
  output <- structure(
    list(
      game = 1:20,
      meanScore = seq(from = 31.5, to = 69.5, by = 2),
      sdScore = rep(0.707106781186548, 20)
    ),
    class = class(dplyr::tibble()),
    row.names = c(NA, -20L),
    .Names = c("game", "meanScore", "sdScore")
  )

  output_scores <-
    get_item_summary(cr_data = input,
                     item = "game",
                     summary_fun = summary_mean_sd_score)

  expect_identical(class(output_scores), class(output))
  expect_equal(output_scores$game, output$game)
  expect_equal(output_scores$meanScore, output$meanScore)
  expect_equal(output_scores$sdScore, output$sdScore)
})

test_that("get_item_summary works with multiple items", {
  output <- structure(
    list(
      season = rep(1:2, each = 10),
      player = rep(1:10, times = 2),
      meanScore = c(36:45, 56:65),
      sdScore = rep(7.07106781186548, 20)
    ),
    class = class(dplyr::tibble()),
    row.names = c(NA, -20L),
    .Names = c("season", "player", "meanScore", "sdScore")
  )

  output_scores <- get_item_summary(
    cr_data = input, item = c("season", "player"),
    summary_fun = summary_mean_sd_score
  )

  expect_identical(class(output_scores), class(output))
  expect_equal(output_scores$season, output$season)
  expect_equal(output_scores$player, output$player)
  expect_equal(output_scores$meanScore, output$meanScore)
  expect_equal(output_scores$sdScore, output$sdScore)
})

test_that("get_item_summary works with extra arguments", {
  expect_silent(
    get_item_summary(
      cr_data = input, item = c("season", "player"),
      summary_fun = summary_mean_sd_score, prefix = "seasonPlayer_"
    )
  )
})


# get_game_summary --------------------------------------------------------
test_that("get_game_summary works", {
  output <- dplyr::tibble(game = 1:20)

  expect_identical(
    get_game_summary(cr_data = input,
                     summary_fun = NULL),
    output
  )
})

# get_player_summary ------------------------------------------------------
test_that("get_player_summary works", {
  output <- dplyr::tibble(player = 1:10)

  expect_identical(
    get_player_summary(cr_data = input,
                       summary_fun = NULL),
    output
  )
})
