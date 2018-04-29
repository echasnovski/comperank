context("massey")


# Input and output data ---------------------------------------------------
cr_data <- ncaa2005
output_base <- tibble::tibble(
  player = c("Duke", "Miami", "UNC", "UVA", "VT"),
  rating_massey = c(-24.8, 18.2, -8, -3.4, 18)
)


# rate_massey -------------------------------------------------------------
test_that("rate_massey works", {
  expect_equal_tbls(rate_massey(cr_data), output_base)
})

test_that("rate_massey works with factor `player`", {
  inds <- c(5, 2, 1, 3, 4)
  players_1 <- output_base$player[inds]
  input_1 <- cr_data
  input_1$player <- factor(input_1$player, levels = players_1)
  output_1 <- rate_massey(input_1)
  output_ref_1 <- output_base
  output_ref_1$player <- factor(output_ref_1$player, levels = players_1)

  expect_equal_tbls(output_1, output_ref_1[inds, ])

  players_2 <- output_base$player[1:3]
  input_2 <- cr_data
  input_2$player <- factor(input_2$player, levels = players_2)
  output_2_x <- rate_massey(input_2)
  output_2_y <- rate_massey(input_2[c(1, 2, 3, 4, 9, 10), ])

  expect_equal_tbls(output_2_x, output_2_y)
})

test_that("rate_massey works with numeric `player`", {
  input <- cr_data
  input$player <- as.integer(as.factor(input$player))
  output <- rate_massey(input)
  output_ref <- output_base
  output_ref$player <- 1:5

  expect_equal_tbls(output, output_ref)
})

test_that("rate_massey handles players absent in `cr_data`", {
  players <- c(output_base$player, "extra")
  input <- cr_data
  input$player <- factor(input$player, levels = players)

  expect_message(
    expect_error(rate_massey(input)),
    "^rate_massey: .* players .*absent.*  extra"
  )
})

test_that("rate_massey works with not all matchups present", {
  input <- cr_data[1:10, ]
  output <- rate_massey(input)
  output_ref <- output_base
  output_ref$rating_massey <- c(-24.8, 12.2, -13.8, 6.2, 20.2)

  expect_equal_tbls(output, output_ref)
})

test_that("rate_massey throws error on not pair games", {
  input_nonpair <- data.frame(
    game = rep(1, 3),
    player = 1:3,
    score = 10:12,
    extraCol = -(1:3)
  )

  expect_error(rate_massey(input_nonpair), "not.*pairgames")
})


# rank_massey -------------------------------------------------------------
test_that("rank_massey works", {
  output_1 <- rank_massey(cr_data)
  output_ref_1 <- tibble::tibble(
    player = output_base$player,
    ranking_massey = c(5, 1, 4, 3, 2)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rank_massey(cr_data, keep_rating = TRUE)
  output_ref_2 <- output_base
  output_ref_2[["ranking_massey"]] <- output_ref_1$ranking_massey

  expect_equal_tbls(output_2, output_ref_2)
})
