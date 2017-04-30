context("utils")

test_that("get_formatC_width works", {
  expect_equal(get_formatC_width(1:9), 1)
  expect_equal(get_formatC_width(1:10), 2)
  expect_equal(get_formatC_width(1:99), 2)
})

test_that("add_class works", {
  input <- 1:10
  class(input) <- "class1"

  output <- input
  class(output) <- c("class2", "class1")

  expect_identical(add_class(input, class_char = "class2"),
                   output)
})
