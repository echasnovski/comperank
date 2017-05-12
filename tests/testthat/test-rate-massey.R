context("rate-massey")


# Input and output data ---------------------------------------------------
input <- ncaa2005
output <- c(-24.8, 18.2, -8, -3.4, 18)
names(output) <- c("Duke", "Miami", "UNC", "UVA", "VT")


# rate_massey -------------------------------------------------------------
test_that("rate_massey works with NULL 'players'", {
  expect_equal(rate_massey(input), output)
  expect_equal(rate_massey(input, players = NULL), output)
})

test_that("rate_massey works with not NULL 'players'", {
  players <- names(output)[c(5, 2, 1, 3, 4)]
  expect_equal(rate_massey(input, players = players), output[players])

  output_1 <- c(-16, 21, -5)
  names(output_1) <- names(output)[1:3]
  expect_equal(rate_massey(input, players = names(output)[1:3]),
               output_1)
})

test_that("rate_massey handles players absent in 'cr_data'", {
  players <- c(names(output)[c(5, 2, 1, 3, 4)], "extra")
  expect_message(
    expect_error(rate_massey(input, players = players)),
    "^rate_massey: .* players .*absent.*  extra"
  )
})

test_that("rate_massey works with not all matchups present", {
  input_1 <- input[1:10, ]
  output_1 <- c(-24.8, 12.2, -13.8, 6.2, 20.2)
  names(output_1) <- names(output)

  expect_equal(rate_massey(input_1, players = NULL), output_1)
})
