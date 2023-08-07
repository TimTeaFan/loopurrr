test_that("screen works with map", {

  myf <- function(a){
    if (a == 1){
      return(a)
    } else if (a == 2){
      warning("a warning")
      return(a)
    } else if (a == 3){
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

  a = 1:5

  output_tbl <- structure(
    list(input = 1:5,
         inp_class = c("integer", "integer", "integer", "integer", "integer"),
         result = c("1", NA, NA, "5", "5"),
         res_class = c("integer", "logical", "logical", "character", "integer"),
         error = c(NA, "an error", NA, NA, NA),
         output = c("", "", "", "[1] \"5\"", ""),
         warning = c(NA, NA, "this is a message", NA, NA),
         message = c("a warning", NA, NA, NA, NA)),
    row.names = c(NA, -5L),
    class = c("screen_tbl", "tbl_df", "tbl", "data.frame"),
    call = "`map(a, myf)`",
    no_err = 1L,
    perc_err = 20,
    no_warn = 1L,
    perc_warn = 20,
    no_of_classes = 2L,
    class_output = "[1] integer, [2] character")

  map(a, myf) %>% screen()

  expect_equal(output_tbl,
               screen(map(a, myf))
              )
})
