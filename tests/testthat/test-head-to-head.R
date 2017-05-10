context("head-to-head")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game = rep(1:2, each = 2),
  player = rep(1:2, times = 2),
  score = 31:34,
  scoreSP = 44:41
)


# get_h2h ------------------------------------------------------------
test_that("get_h2h works", {
  output_1 <- matrix(c(0, -1, 1, 0), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_1) <- c("h2h", "matrix")

  output_res_1 <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_res_1, output_1)
  expect_equal(class(output_res_1)[1], "h2h")
  expect_true(inherits(output_res_1, "matrix"))

  output_2 <- output_1 * 2
  output_res_2 <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_sum_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_res_2, output_2)

  expect_silent(get_h2h(
    cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h,
    extrArg = TRUE
  ))
})

