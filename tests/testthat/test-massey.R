context("massey")


# Input and output data ---------------------------------------------------
input <- ncaa2005
output <- c(-24.8, 18.2, -8, -3.4, 18)
names(output) <- c("Duke", "Miami", "UNC", "UVA", "VT")


# rate_massey -------------------------------------------------------------
test_that("rate_massey works", {
  expect_equal(rate_massey(input), output)
})

test_that("rate_massey works with factor `players`", {
  players_1 <- names(output)[c(5, 2, 1, 3, 4)]
  input_1 <- input
  input_1$player <- factor(input_1$player, levels = players_1)
  expect_equal(rate_massey(input_1), output[players_1])

  players_2 <- names(output)[1:3]
  input_2 <- input
  input_2$player <- factor(input_2$player, levels = players_2)
  output_1 <- rate_massey(input_2)
  output_2 <- rate_massey(input[c(1, 2, 3, 4, 9, 10), ])

  expect_identical(output_1, output_2)
})

test_that("rate_massey handles players absent in `cr_data`", {
  players <- c(names(output)[c(5, 2, 1, 3, 4)], "extra")
  input$player <- factor(input$player, levels = players)
  expect_message(
    expect_error(rate_massey(input)),
    "^rate_massey: .* players .*absent.*  extra"
  )
})

test_that("rate_massey works with not all matchups present", {
  input_1 <- input[1:10, ]
  output_1 <- c(-24.8, 12.2, -13.8, 6.2, 20.2)
  names(output_1) <- names(output)

  expect_equal(rate_massey(input_1), output_1)
})

test_that("rate_massey correctly works with not pair games", {
  input_nonpair <- data.frame(
    game = c(rep(1:5, each = 3), 6),
    player = c(rep(1:5, times = 3), 1),
    score = c(110L, 111L, 106L, 113L, 108L, 115L, 102L, 105L, 103L, 116L,
              101L, 114L, 104L, 107L, 109L, 112L),
    extraCol = -(1:16)
  )
  output <- c(-1.87272727272727, 1.38181818181818, -1.45454545454545,
              0.436363636363636, 1.50909090909091)
  names(output) <- 1:5

  expect_equal(rate_massey(input_nonpair), output)
})


# rank_massey -------------------------------------------------------------
test_that("rank_massey works", {
  rank_output <- output
  rank_output[] <- c(5, 1, 4, 3, 2)

  expect_equal(rank_massey(input), rank_output)
})

