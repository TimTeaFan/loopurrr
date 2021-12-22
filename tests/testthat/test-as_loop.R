library(purrr)
test_that("map works with unnamed lists", {
  x <- list(c(1,2), c(2,3), c(3,4))
  expect_equal(map(x, sum),
               as_loop(map(x, sum), action = "eval")
               )
})

