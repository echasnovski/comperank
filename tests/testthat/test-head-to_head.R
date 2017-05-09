context("head-to_head")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game = rep(1:2, each = 2),
  player = rep(1:2, times = 2),
  score = 31:34,
  scoreSP = 44:41
)


# h2h ------------------------------------------------------------
test_that("h2h works", {
  output_1 <- matrix(c(0, -1, 1, 0), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_1) <- c("h2h", "matrix")

  output_res_1 <- h2h(
    cr_data = cr_data, h2h_fun = mean_score_diff,
    score_game = NULL, players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_res_1, output_1)
  expect_equal(class(output_res_1)[1], "h2h")
  expect_true(inherits(output_res_1, "matrix"))

  output_2 <- output_1 * 2
  output_res_2 <- h2h(
    cr_data = cr_data, h2h_fun = sum_score_diff,
    score_game = NULL, players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_res_2, output_2)

  expect_silent(h2h(
    cr_data = cr_data, h2h_fun = mean_score_diff,
    score_game = NULL, players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h,
    extrArg = TRUE
  ))
})

