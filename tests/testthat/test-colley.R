context("colley")


# Input and output data ---------------------------------------------------
cr_data <- ncaa2005
output_base <- tibble::tibble(
  player = c("Duke", "Miami", "UNC", "UVA", "VT"),
  rating_colley = c(0.214285714285714, 0.785714285714286, 0.5,
                    0.357142857142857, 0.642857142857143)
)


# rate_colley -------------------------------------------------------------
test_that("rate_colley works", {
  output <- rate_colley(cr_data)

  expect_equal_tbls(output, output_base)
})

test_that("rate_colley works with factor `player`", {
  inds <- c(5, 2, 1, 3, 4)
  players_1 <- output_base$player[inds]
  input_1 <- cr_data
  input_1$player <- factor(input_1$player, levels = players_1)
  output_1 <- rate_colley(input_1)
  output_ref_1 <- output_base
  output_ref_1$player <- factor(output_ref_1$player, levels = players_1)
  expect_equal_tbls(output_1, output_ref_1[inds, ])

  players_2 <- output_base$player[1:3]
  input_2 <- cr_data
  input_2$player <- factor(input_2$player, levels = players_2)
  output_2_x <- rate_colley(input_2)
  output_2_y <- rate_colley(input_2[c(1, 2, 3, 4, 9, 10), ])

  expect_equal_tbls(output_2_x, output_2_y)
})

test_that("rate_colley works with numeric `player`", {
  input <- cr_data
  input$player <- as.integer(as.factor(input$player))
  output <- rate_colley(input)
  output_ref <- output_base
  output_ref$player <- 1:5

  expect_equal_tbls(output, output_ref)
})

test_that("rate_colley works with not all matchups present", {
  input <- cr_data[1:10, ]
  output <- rate_colley(input)
  output_ref <- output_base
  output_ref$rating_colley <- c(
    0.214285714285714, 0.671428571428571, 0.471428571428571,
    0.571428571428571, 0.571428571428571
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_colley throws error on not pair games", {
  input_nonpair <- data.frame(
    game = rep(1, 3),
    player = 1:3,
    score = 10:12,
    extraCol = -(1:3)
  )

  expect_error(rate_colley(input_nonpair), "not.*pairgames")
})


# rank_colley -------------------------------------------------------------
test_that("rank_colley works", {
  output_1 <- rank_colley(cr_data)
  output_ref_1 <- tibble::tibble(
    player = output_base$player,
    ranking_colley = c(5, 1, 3, 4, 2)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rank_colley(cr_data, keep_rating = TRUE)
  output_ref_2 <- output_base
  output_ref_2[["ranking_colley"]] <- output_ref_1$ranking_colley

  expect_equal_tbls(output_2, output_ref_2)
})
