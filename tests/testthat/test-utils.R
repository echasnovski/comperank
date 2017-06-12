context("utils")

# skip_action -----------------------------------------------------------------
test_that("skip_action works", {
  x <- 1
  expect_identical(skip_action(x), x)

  x <- list(list(1:10), list(list(c("a", "b"))))
  class(x) <- "someclass"
  expect_identical(skip_action(x), x)

  expect_silent(skip_action(x, extraArg = TRUE))
})


# get_formatC_width -----------------------------------------------------
test_that("get_formatC_width works", {
  expect_equal(get_formatC_width(1:9), 1)
  expect_equal(get_formatC_width(1:10), 2)
  expect_equal(get_formatC_width(1:99), 2)
})


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  input <- 1:10
  class(input) <- "class1"
  output <- input
  class(output) <- c("class2", "class1")

  expect_identical(add_class(input, class_char = "class2"),
                   output)
})


# assert_used_names -------------------------------------------------------
test_that("assert_used_names works", {
  info <- data.frame(original = c("gameId", "playerId", "scoreId"),
                     target = c("game", "player", "score"),
                     stringsAsFactors = FALSE)
  expect_message(
    assert_used_names(info, prefix = "prefix: "),
    "prefix: .*not.*matched.*gameId.*game.*playerId.*player.*scoreId.*score"
  )

  info$original <- info$target
  expect_silent(assert_used_names(info))

  info$original[2] <- NA
  expect_message(
    assert_used_names(info, prefix = "prefix: "),
    "prefix: .*not.*found.*NA.*player"
  )
})


# renamecreate_columns ----------------------------------------------------
test_that("renamecreate_columns works", {
  input <- data.frame(x = 1:10, y = 2:11, z = 3:12)
  info <- data.frame(target = c("a", "b", "c"), original = c("x", NA, "y"),
                     stringsAsFactors = FALSE)
  output <- data.frame(a = 1:10, c = 2:11, z = 3:12, b = rep(NA_integer_, 10))

  expect_identical(renamecreate_columns(df = input, info = info,
                                        fill = NA_integer_),
                   output)
})



# reduce_full_join --------------------------------------------------------
test_that("reduce_full_join works", {
  input <- list(
    dplyr::tibble(
      game = 1:10,
      player1 = 11:20,
      score1 = 101:110
    ),
    dplyr::tibble(
      game = 1:5,
      player2 = 12:16,
      score2 = 102:106
    )
  )
  output <- dplyr::tibble(
    game = 1:10,
    player1 = 11:20,
    score1 = 101:110,
    player2 = c(12:16, rep(NA, 5)),
    score2 = c(102:106, rep(NA, 5))
  )

  expect_identical(reduce_full_join(input, by = "game"), output)
  expect_identical(reduce_full_join(input[1], by = "game"), input[[1]])
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


# to_rating_tbl -----------------------------------------------------------
test_that("to_rating_tbl works", {
  input <- c("pl1" = 1, "pl2" = 10, "pl3" = 100)
  output <- dplyr::tibble(
    player = paste0("pl", 1:3),
    rating = 10^(0:2)
  )

  expect_equal(to_rating_tbl(input), output)
})


# to_rating_vec -----------------------------------------------------------
test_that("to_rating_vec works", {
  input <- dplyr::tibble(
    player = paste0("pl", 1:3),
    rating = 10^(0:2),
    stringsAsFactors = FALSE
  )
  output <- c("pl1" = 1, "pl2" = 10, "pl3" = 100)

  expect_equal(to_rating_vec(input), output)
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
