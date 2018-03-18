# This workaround is currently needed because of these issues:
# https://github.com/r-lib/testthat/issues/593
# https://github.com/tidyverse/tibble/issues/287
# https://github.com/tidyverse/dplyr/issues/2751
expect_equal_tbls <- function(tbl_1, tbl_2) {
  expect_equal(as.data.frame(tbl_1), as.data.frame(tbl_2))
}
