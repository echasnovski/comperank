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
    dplyr::tbl_df(data.frame(
      game = 1:10,
      player1 = 11:20,
      score1 = 101:110
    )),
    dplyr::tbl_df(data.frame(
      game = 1:5,
      player2 = 12:16,
      score2 = 102:106
    ))
  )
  output <- dplyr::tbl_df(data.frame(
    game = 1:10,
    player1 = 11:20,
    score1 = 101:110,
    player2 = c(12:16, rep(NA, 5)),
    score2 = c(102:106, rep(NA, 5))
  ))

  expect_identical(reduce_full_join(input, by = "game"), output)
})
