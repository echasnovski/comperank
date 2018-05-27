context("utils")


# Input data --------------------------------------------------------------
input_fun <- function(x) {x}


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  input <- 1:10
  class(input) <- "class1"
  output <- input
  class(output) <- c("class2", "class1")

  expect_identical(add_class(input, class_char = "class2"),
                   output)
})


# to_list -----------------------------------------------------------------
test_that("to_list works", {
  input <- 1:2

  expect_identical(to_list(input), list(input))
  expect_identical(to_list(list(input)), list(input))
})


# is_function_list --------------------------------------------------------
test_that("is_function_list works", {
  expect_false(is_function_list(input_fun))
  expect_true(is_function_list(list(input_fun, input_fun)))
  expect_false(is_function_list(list(input_fun, input_fun, "a")))
})


# to_function_list --------------------------------------------------------
test_that("to_function_list works", {
  expect_equal(to_function_list(input_fun), list(input_fun))

  input_fun_list <- list(input_fun, input_fun)
  expect_identical(to_function_list(input_fun_list), input_fun_list)

  input_bad_fun_list <- list(input_fun, "a")
  expect_error(to_function_list(input_bad_fun_list),
               "function")
})


# round_rank ---------------------------------------------------------------
test_that("round_rank works", {
  set.seed(220)
  input <- runif(10)
  output_1 <- round_rank(input, type = "desc")
  output_ref_1 <- c(9, 1, 3, 7, 10, 4, 2, 8, 6, 5)

  expect_equal(output_1, output_ref_1)

  output_2 <- round_rank(input, type = "asc")
  output_ref_2 <- c(2, 10, 8, 4, 1, 7, 9, 3, 5, 6)

  expect_equal(output_2, output_ref_2)

  expect_error(round_rank(input, type = "aesc"))
})

test_that("round_rank rounds", {
  set.seed(515)
  input_1 <- rep(0.2, 5) + 10^(-7) * runif(5)

  expect_equal(round_rank(input_1, round_digits = 6), rep(3, 5))

  set.seed(516)
  input_2 <- c((1:4)/4, 1) + 10^(-7) * runif(5)

  expect_equal(round_rank(input_2, type = "desc", round_digits = 6),
               c(5, 4, 3, 1.5, 1.5))
})


# assert_used_objects -----------------------------------------------------
test_that("assert_used_objects works", {
  used <- c("a", "b", "c")
  original <- c("a", "d", "e", "f")

  expect_message(assert_used_objects(used = used, original = original,
                                     prefix = "prefix: ",
                                     object_name = "some objects",
                                     data_name = "some data"),
                 "^prefix: .* some objects .*absent.*in.* some data:.*  b, c")
  expect_silent(assert_used_objects(used = used[1], original = original,
                                    prefix = "prefix: ",
                                    object_name = "some objects",
                                    data_name = "some data"))
})


# assert_pairgames --------------------------------------------------------
test_that("assert_pairgames works", {
  expect_silent(assert_pairgames(ncaa2005))
  expect_error(assert_pairgames(ncaa2005[-1, ]), "not.*pairgames")
  expect_error(assert_pairgames(ncaa2005[-1, ], "cr"), "cr.*not.*pairgames")
})


# assert_h2h_fun ----------------------------------------------------------
test_that("assert_h2h_fun works", {
  expect_silent(assert_h2h_fun(1))
  expect_error(assert_h2h_fun(), "Head-to-Head")
})


# unique_levels -----------------------------------------------------------
test_that("unique_levels works", {
  num_input <- c(NA, 2, -1, 10, NA, 1, 1, 10)
  expect_identical(unique_levels(num_input), c(-1, 1, 2, 10, NA))
  expect_identical(unique_levels(num_input, na.last = FALSE),
                   c(NA, -1, 1, 2, 10))
  expect_identical(unique_levels(num_input, na.last = NA), c(-1, 1, 2, 10))

  expect_identical(unique_levels(factor(1)), factor(1))
  expect_identical(unique_levels(factor(1, levels = 1:2)), factor(1:2))
  expect_identical(unique_levels(factor(c(1, NA))), factor(1))
  expect_identical(unique_levels(factor(c(1, NA), exclude = NULL)),
                   factor(c(1, NA), exclude = NULL))
})


# enframe_vec -------------------------------------------------------------
test_that("enframe_vec works", {
  input_1 <- c(a = 1L, c = 2L, b = 3L)

  expect_identical(
    enframe_vec(input_1, name = "player", value = "rating"),
    tibble::tibble(player = c("a", "c", "b"), rating = c(1L, 2L, 3L))
  )

  input_2 <- c("1" = "a", "3" = "c", "2" = NA)

  expect_identical(
    enframe_vec(input_2, name = "player", value = "rating"),
    tibble::tibble(player = c("1", "3", "2"), rating = c("a", "c", NA))
  )
  expect_identical(
    enframe_vec(input_2, ref = 1:3, name = "player", value = "rating"),
    tibble::tibble(player = 1:3, rating = c("a", NA, "c"))
  )
  expect_identical(
    enframe_vec(input_2, ref = c(1L, 3L), name = "player", value = "rating"),
    tibble::tibble(player = c(1L, 3L), rating = c("a", "c"))
  )
  expect_identical(
    enframe_vec(input_2, ref = factor(1:2, levels = 1:4),
                name = "player", value = "rating"),
    tibble::tibble(player = factor(1:2, levels = 1:4),
                  rating = c("a", NA))
  )
})

test_that("add_ranking works", {
  input <- data.frame(a = letters[1:5], b = c(3, 1, 1, -1, 4))

  output_1 <- add_ranking(input, "b", "c")
  output_ref_1 <- input[, 1, drop = FALSE]
  output_ref_1[["c"]] <- c(2, 3.5, 3.5, 5, 1)

  expect_equal(output_1, output_ref_1)

  output_2 <- add_ranking(input, "b", "c", keep_rating = TRUE)
  output_ref_2 <- input
  output_ref_2[["c"]] <- c(2, 3.5, 3.5, 5, 1)

  expect_equal(output_2, output_ref_2)

  output_3 <- add_ranking(input, "b", "c", type = "asc")
  output_ref_3 <- input[, 1, drop = FALSE]
  output_ref_3[["c"]] <- c(4, 2.5, 2.5, 1, 5)

  expect_equal(output_3, output_ref_3)

  output_4 <- add_ranking(input, "b", "c", ties = "first")
  output_ref_4 <- input[, 1, drop = FALSE]
  output_ref_4[["c"]] <- c(2, 3, 4, 5, 1)

  expect_equal(output_4, output_ref_4)

  input_5 <- input
  input_5[["b"]][2] <- 1.001
  output_5 <- add_ranking(input_5, "b", "c", round_digits = 2)
  output_ref_5 <- input[, 1, drop = FALSE]
  output_ref_5[["c"]] <- c(2, 3.5, 3.5, 5, 1)

  expect_equal(output_5, output_ref_5)
})


# get_pf_vec --------------------------------------------------------------
test_that("get_pf_vec works", {
  mat_bad <- matrix(c(1, -1, 1, 1), nrow = 2)
  expect_error(get_pf_vec(mat_bad), "wrong.*Perron-Frobenius")

  mat <- matrix(
    c(8.75,   13,    6,  9.5, 11.25,
      1.75, 34.5,    4, 4.25,  1.75,
      5.25,  8.5, 12.5, 1.25,   7.5,
      1.75, 6.25, 1.75, 18.5,    13,
      0,    6.75, 0.75,  3.5,  33.5),
    byrow = TRUE, nrow = 5
  )
  output_ref <- c(0.22469302509596, 0.222939414684547, 0.154163462172164,
                  0.186664590660501, 0.211539507386828)

  expect_equal(get_pf_vec(mat), output_ref)
})
