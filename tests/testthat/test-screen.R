test_that("screen works with map", {

  myf <- function(a){
    if (a == 1){
      return(a)
    } else if (a == 2){
      warning("a warning")
      return(a)
    } else if (a == "a"){
      print("I'm here")
      message("This is a message")
      warning("A warning followed by ...")
      stop("an error")
    } else if (a == 4){
      rlang::inform("this is a message")
    } else if (a == 5){
      out <- dplyr::tibble(id = 1:5, letter = letters[1:5])
      return(out)
    } else {
      print("6")
    }
  }

  a = flatten(list(1:2, "a", 4:6))

  output_tbl <- structure(list(
    input_x = c("1", "2", "a", "4", "5", "6"),
    input_x_class = c("integer","integer", "character", "integer", "integer", "integer"),
    result = list(1L, 2L, NULL, NULL, tibble::tibble(id = 1:5, letter = c("a","b", "c", "d", "e")), "6"),
    result_class = list("integer", "integer", "NULL", "NULL", c("tbl_df", "tbl", "data.frame"), "character"),
    error = c(NA, NA, "an error", NA, NA, NA),
    output = c("", "", "[1] \"I'm here\"", "", "", "[1] \"6\""),
    warning = c(NA, "a warning", "A warning followed by ...", NA, NA, NA),
    message = c(NA, NA, "This is a message\n", "this is a message", NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`map(a, myf)`",
  has_fn = TRUE,
  no_err = 1L,
  perc_err = 17,
  no_warn = 2L,
  perc_warn = 33,
  no_of_classes = 4L,
  class_output = "[1] integer, [2] NULL, [3] tbl_df, tbl, data.frame, [4] character"
  )

  expect_equal(output_tbl,
               screen(map(a, myf))
               )
})

test_that("screen works with accumulate", {

  mysum <- function(x, y ){

    res <- x + y

    if (res %% 2) warning("This is a warning")


    if (res %/% 4) print("Result is getting big")

    res
  }

  a = flatten(list(1:2, "a", 4:6))

  output_tbl <- structure(list(
    input_x = c("1", "2", "a", "4", "5", "6"),
    input_x_class = c("integer","integer", "character", "integer", "integer", "integer"),
    result = list(1L, 2L, NULL, NULL, tibble::tibble(id = 1:5, letter = c("a","b", "c", "d", "e")), "6"),
    result_class = list("integer", "integer", "NULL", "NULL", c("tbl_df", "tbl", "data.frame"), "character"),
    error = c(NA, NA, "an error", NA, NA, NA),
    output = c("", "", "[1] \"I'm here\"", "", "", "[1] \"6\""),
    warning = c(NA, "a warning", "A warning followed by ...", NA, NA, NA),
    message = c(NA, NA, "This is a message\n", "this is a message", NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`map(a, myf)`",
  has_fn = TRUE,
  no_err = 1L,
  perc_err = 17,
  no_warn = 2L,
  perc_warn = 33,
  no_of_classes = 4L,
  class_output = "[1] integer, [2] NULL, [3] tbl_df, tbl, data.frame, [4] character"
  )

  expect_equal(output_tbl,
               screen(map(a, myf))
  )
})
