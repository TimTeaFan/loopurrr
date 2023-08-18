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

test_that("screen works with accumulate when no errors / warnings are thrown", {

  output_tbl <- structure(list(
    input_x = c(1L, 1L, 3L, 6L, 10L, 15L),
    input_y = list(NULL, 2L, 3L, 4L, 5L, 6L),
    input_x_class = c("integer", "integer", "integer", "integer", "integer", "integer"),
    input_y_class = c("NULL", "integer", "integer", "integer", "integer", "integer"),
    result = c(1L, 3L, 6L, 10L , 15L, 21L),
    result_class = c("integer", "integer", "integer", "integer", "integer", "integer"),
    error = c(NA, NA, NA, NA, NA, NA),
    output = c(NA, "", "", "", "", ""),
    warning = c(NA, NA, NA, NA, NA, NA),
    message = c(NA, NA, NA, NA, NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`accumulate(1:6, sum)`",
  has_fn = TRUE,
  no_err = 0L,
  perc_err = 0,
  no_warn = 0L,
  perc_warn = 0,
  no_of_classes = 1L,
  class_output = "[1] integer"
  )

  expect_equal(output_tbl,
               screen(accumulate(1:6, sum))
  )
})


test_that("screen works with accumulate backwards  when no errors / warnings are thrown", {

  output_tbl <- structure(list(
    input_x = c(1L, 1L, 3L, 6L, 10L, 15L),
    input_y = list(NULL, 2L, 3L, 4L, 5L, 6L),
    input_x_class = c("integer", "integer", "integer", "integer", "integer", "integer"),
    input_y_class = c("NULL", "integer", "integer", "integer", "integer", "integer"),
    result = c(1L, 3L, 6L, 10L , 15L, 21L),
    result_class = c("integer", "integer", "integer", "integer", "integer", "integer"),
    error = c(NA, NA, NA, NA, NA, NA),
    output = c(NA, "", "", "", "", ""),
    warning = c(NA, NA, NA, NA, NA, NA),
    message = c(NA, NA, NA, NA, NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`accumulate(1:6, sum)`",
  has_fn = TRUE,
  no_err = 0L,
  perc_err = 0,
  no_warn = 0L,
  perc_warn = 0,
  no_of_classes = 1L,
  class_output = "[1] integer"
  )

  expect_equal(output_tbl,
               screen(accumulate(1:6, sum, .dir = "backward"))
  )
})

test_that("screen works with accumulate and error / warnings", {

  mysum <- function(x, y ){

    warning("This is a warning")
    res <- x + y

    if (res %/% 2) print("Result is getting big")

    if (res >= 3) message("Result is really big")

    res
  }

  b = flatten(list(1:2, "a", 4:6))

  output_tbl <- structure(list(
    input_x = c("1", "2", "a", "4", "5", "6"),
    input_y = list(NULL, 1L, 3L, NULL, NULL, NULL),
    input_x_class = c("integer", "integer", "character", "integer", "integer", "integer"),
    input_y_class = c("NULL", "integer", "integer", "NULL", "NULL", "NULL"),
    result = list(1L, 3L, NULL, NULL, NULL, NULL),
    result_class = c("integer", "integer", "NULL", "NULL", "NULL", "NULL"),
    error = c(NA, NA,  "non-numeric argument to binary operator", "non-numeric argument to binary operator",  "non-numeric argument to binary operator", "non-numeric argument to binary operator"),
    output = c(NA, "[1] \"Result is getting big\"", "", "", "", ""),
    warning = c(NA, "This is a warning", "This is a warning", "This is a warning", "This is a warning", "This is a warning"),
    message = c(NA, "Result is really big\n", NA, NA, NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`accumulate(b, mysum)`",
  has_fn = TRUE,
  no_err = 4L,
  perc_err = 67,
  no_warn = 5L,
  perc_warn = 83,
  no_of_classes = 1L,
  class_output = "[1] integer"
  )

  expect_equal(output_tbl,
               screen(accumulate(b, mysum))
  )
})

test_that("screen works with accumulate and `.init`", {


  mysum <- function(x, y ){

    print(paste0("this is x: ", x))
    print(paste0("this is y: ", y))
    x + y

  }

  mysum <- function(x, y ){

    warning("This is a warning")
    res <- x + y

    if (res %/% 2) print("Result is getting big")

    if (res >= 3) message("Result is really big")

    res
  }

  b = flatten(list(1:2, "a", 4:6))

  output_tbl <- structure(list(
    input_x = c("1", "2", "a", "4", "5", "6"),
    input_y = list(NULL, 1L, 3L, NULL, NULL, NULL),
    input_x_class = c("integer", "integer", "character", "integer", "integer", "integer"),
    input_y_class = c("NULL", "integer", "integer", "NULL", "NULL", "NULL"),
    result = list(1L, 3L, NULL, NULL, NULL, NULL),
    result_class = c("integer", "integer", "NULL", "NULL", "NULL", "NULL"),
    error = c(NA, NA,  "non-numeric argument to binary operator", "non-numeric argument to binary operator",  "non-numeric argument to binary operator", "non-numeric argument to binary operator"),
    output = c(NA, "[1] \"Result is getting big\"", "", "", "", ""),
    warning = c(NA, "This is a warning", "This is a warning", "This is a warning", "This is a warning", "This is a warning"),
    message = c(NA, "Result is really big\n", NA, NA, NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`accumulate(b, mysum)`",
  has_fn = TRUE,
  no_err = 4L,
  perc_err = 67,
  no_warn = 5L,
  perc_warn = 83,
  no_of_classes = 1L,
  class_output = "[1] integer"
  )

  expect_equal(output_tbl,
               screen(accumulate(1:10, mysum, .init = 100))
  )
})

test_that("screen works with reduce", {

  mysum <- function(x, y ){

    warning("This is a warning")
    res <- x + y

    if (res %/% 2) print("Result is getting big")

    if (res >= 3) message("Result is really big")

    res
  }

  b = flatten(list(1:2, "a", 4:6))

  output_tbl <- structure(list(
    input_x = c("1", "2", "a", "4", "5", "6"),
    input_y = list(NULL, 1L, 3L, NULL, NULL, NULL),
    input_x_class = c("integer", "integer", "character", "integer", "integer", "integer"),
    input_y_class = c("NULL", "integer", "integer", "NULL", "NULL", "NULL"),
    result = list(1L, 3L, NULL, NULL, NULL, NULL),
    result_class = c("integer", "integer", "NULL", "NULL", "NULL", "NULL"),
    error = c(NA, NA,  "non-numeric argument to binary operator", "non-numeric argument to binary operator",  "non-numeric argument to binary operator", "non-numeric argument to binary operator"),
    output = c(NA, "[1] \"Result is getting big\"", "", "", "", ""),
    warning = c(NA, "This is a warning", "This is a warning", "This is a warning", "This is a warning", "This is a warning"),
    message = c(NA, "Result is really big\n", NA, NA, NA, NA)
  ),
  row.names = c(NA, -6L),
  class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
  call = "`reduce(b, mysum)`",
  has_fn = TRUE,
  no_err = 4L,
  perc_err = 67,
  no_warn = 5L,
  perc_warn = 83,
  no_of_classes = 1L,
  class_output = "[1] integer"
  )

  expect_equal(output_tbl,
               screen(reduce(b, mysum))
  )
})
