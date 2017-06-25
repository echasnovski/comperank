context("colley")


# Input and output data ---------------------------------------------------
input <- ncaa2005
output <- c(0.21, 0.79, 0.5, 0.36, 0.64)
names(output) <- c("Duke", "Miami", "UNC", "UVA", "VT")


# rate_colley -------------------------------------------------------------
test_that("rate_colley works with NULL players", {
  expect_equal(round(rate_colley(input), 2), output)
  expect_equal(round(rate_colley(input, players = NULL), 2), output)
})

test_that("rate_colley works with not NULL players", {
  players <- names(output)[c(5, 2, 1, 3, 4)]
  expect_equal(round(rate_colley(input, players = players), 2),
               output[players])

  output_1 <- c(0.3, 0.7, 0.5)
  names(output_1) <- names(output)[1:3]
  expect_equal(rate_colley(input, players = names(output)[1:3]),
               output_1)
})

test_that("rate_colley works with not all matchups present", {
  input_1 <- input[1:10, ]
  output_1 <- c(0.214285714285714, 0.671428571428571, 0.471428571428571,
                0.571428571428571, 0.571428571428571)
  names(output_1) <- names(output)

  expect_equal(rate_colley(input_1, players = NULL), output_1)
})

test_that("rate_colley correctly works with not pair games", {
  input_nonpair <- data.frame(
    game = c(rep(1:5, each = 3), 6),
    player = c(rep(1:5, times = 3), 1),
    score = c(110L, 111L, 106L, 113L, 108L, 115L, 102L, 105L, 103L, 116L,
              101L, 114L, 104L, 107L, 109L, 112L),
    extraCol = -(1:16)
  )
  output <- c(0.51123595505618, 0.48876404494382, 0.387640449438202,
              0.5, 0.612359550561798)
  names(output) <- 1:5

  expect_equal(rate_colley(input_nonpair), output)
})


# rank_colley -------------------------------------------------------------
test_that("rank_colley works", {
  output_ref <- output
  output_ref[] <- c(5, 1, 3, 4, 2)

  expect_equal(rank_colley(input), output_ref)
})
