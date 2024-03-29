test_that("as_loop works with map", {

  x <- list(c(1,2), c(2,3), c(3,4))

  expect_equal(map(x, sum),
               as_loop(map(x, sum),
                       return = "eval")
               )


  y <- list(a = 1, b = 2, c =3)

  expect_equal(map(y, paste),
               as_loop(map(y, paste),
                       return = "eval")
  )
})

test_that("as_loop works with different ways of supplying .f", {

  x <- list(c(1,2), c(2,3), c(3,4))

  # purrr style formulas
  expect_equal(map(x, ~ sum(.x)),
               as_loop(map(x, ~ sum(.x)),
                       return = "eval")
  )

  # anonymous functions
  expect_equal(map(x, function(x) sum(x)),
               as_loop(map(x, function(x) sum(x)),
                       return = "eval")
  )

  # argument forwarding in ...

  y <- list(c(NA,2), c(2,NA), c(NA,4))

  expect_equal(map(y, sum, na.rm = TRUE),
               as_loop(map(y, sum, na.rm = TRUE),
                       return = "eval")
  )

  # not supported yet: argument forwarding with lambda functions

  expect_error(as_loop(map(y, ~ sum(.x, na.rm = .y), TRUE),
                       return = "eval"),
               "argument forwarding"
  )

  # not supported yet: lambda functions using the `...`

  expect_error(as_loop(map(y, function(...) sum(..1, na.rm = TRUE)),
                       return = "eval"),
               "does not support anonymous functions"
  )

  # not supported yet: lists, numeric or character vectors in .f as extractor functions:

  l2 <- list(
    list(num = 1:3,     letters[1:3]),
    list(num = 101:103, letters[4:6]),
    list()
  )

  expect_error(as_loop(map(l2, list("num", 3)),
                       return = "eval"),
               "does not yet support lists when supplied as"
  )
})

test_that("as_loop works with magrittr pipe", {

  x <- list(c(1,2), c(2,3), c(3,4))

  exp1 <- x %>% map(sum)
  out1 <- x %>% map(sum) %>% as_loop(., return = "eval")

  expect_equal(exp1, out1)


  exp2 <- c(1:3) %>% map(sum)
  out2 <- c(1:3) %>% map(sum) %>% as_loop(., return = "eval")

  expect_equal(exp2, out2)


  exp3 <- sum %>% map(1:3, .)
  out3 <- sum %>% map(1:3, .) %>% as_loop(., return = "eval")

  expect_equal(exp3, out3)
})

test_that("as_loop works with namespaced map calls", {

  x <- list(c(1,2), c(2,3), c(3,4))

  exp1 <- x %>% purrr::map(sum)
  out1 <- x %>% purrr::map(sum) %>% as_loop(., return = "eval")

  expect_equal(exp1, out1)
})

test_that("as_loop works with typed versions of map", {

  x_int <- list(c(1L, 2L), c(2L, 3L), c(3L, 4L))

  # map_int
  expect_equal(map_int(x_int, sum),
               as_loop(map_int(x_int, sum),
                       return = "eval")
  )


  x <- list(c(1,2), c(2,3), c(3,4))

  # map_dbl
  expect_equal(map_dbl(x, sum),
               as_loop(map_dbl(x, sum),
                       return = "eval")
  )


  y <- list(a = 1, b = 2, c = 3)

  # map_chr
  expect_equal(map_chr(y, paste),
               as_loop(map_chr(y, paste),
                       return = "eval")
  )

  # map_lgl
  expect_equal(map_lgl(x, ~ sum(.x) > 4),
               as_loop(map_lgl(x, ~ sum(.x) > 4),
                       return = "eval")
  )

  # # map_raw
  expect_equal(map_raw(y, ~ as.raw(.x)),
               as_loop(map_raw(y, ~ as.raw(.x)),
                       return = "eval")
  )

  # map_dfc with named vector
  expect_equal(map_dfc(y, ~ c(a = .x, b = 1, c = 2)),
               as_loop(
                 map_dfc(y, ~ c(a = .x, b = 1, c = 2)),
                 return = "eval")
  )

  # map_dfc with unnamed vector
  expect_equal(map_dfc(unname(y), ~ c(.x, 1, 2)),
               as_loop(
                 map_dfc(unname(y), ~ c(.x, 1, 2)),
                 return = "eval")
  )

  # map_dfr with named vector
  expect_equal(map_dfr(y, ~ c(a = .x, b = 1, c = 2)),
               as_loop(
                 map_dfr(y, ~ c(a = .x, b = 1, c = 2)),
                 return = "eval")
  )

  # map_dfc with unnamed vector
  expect_error(as_loop(
                 map_dfr(unname(y), ~ c(.x, 1, 2)),
                 return = "eval"),
               "Argument 1 must have names"
  )

})


test_that("as_loop works with map_at and map_if", {

  x <- list(a = c(1,2), b = c("a","b"), c = c(3,4))

  expect_equal(map_at(x, "a", sum),
               as_loop(map_at(x, "a", sum),
                       return = "eval")
  )

  expect_equal(map_at(x, c(1, 3), sum),
               as_loop(map_at(x, c(1, 3), sum),
                       return = "eval")
  )

  expect_equal(map_if(x, is.numeric, sum),
               as_loop(map_if(x, is.numeric, sum),
                       return = "eval")
  )

  expect_equal(map_if(x, is.numeric, sum, .else = ~ paste(.x, collapse = ", ")),
               as_loop(map_if(x, is.numeric, sum, .else = ~ paste(.x, collapse = ", ")),
                       return = "eval")
  )

})


# walk
test_that("as_loop works with walk", {

  output_asloop <- capture.output(
    as_loop(walk(c(a = 1, b = 2, c = 3), ~ print(.x)),
            return = "eval")
  )

  output_walk <- capture.output(
    walk(c(a = 1, b = 2, c = 3), print)
  )

  expect_equal(output_asloop, output_walk)

  # confirm return value of `walk`

  exp1 <- walk(c(a = 1, b = 2, c = 3), ~ print(.x)) %>%
    `+`(., 1)

  out1 <- walk(c(a = 1, b = 2, c = 3), ~ print(.x)) %>%
    as_loop(., return = "eval")  %>%
    `+`(., 1)

  expect_equal(exp1, out1)

})

# lmap

test_that("as_loop works with lmap", {

  y <- list(a = 1, b = "a", c = 3)

  list_rep <- function(x) {

    out <- rep_len(x, 2)
    if (length(out) > 0) {
      names(out) <- paste0(names(x), seq_len(2))
    }
    out
  }

  test_ls <- as_loop(lmap(y, list_rep), return = "eval")
  expect_ls <- lmap(y, list_rep)

  expect_equal(test_ls, expect_ls)

})


# imap / iwalk
test_that("as_loop works with imap and iwalk", {

  # imap
  expect_equal(imap(c(a = 1, b = 2, c = 3), paste),

               as_loop(imap(c(a = 1, b = 2, c = 3), paste),
                       return = "eval")
  )

  # iwalk
  output_asloop <- capture.output(
    as_loop(iwalk(c(a = 1, b = 2, c = 3), ~ print(c(.x, .y))),
            return = "eval")
  )

  output_iwalk <- capture.output(
    iwalk(c(a = 1, b = 2, c = 3), ~ print(c(.x, .y)))
  )

  expect_equal(output_asloop, output_iwalk)

})

# map2
test_that("as_loop works with map2", {

  a <- 1:10
  b <- 10:1

  expect_equal(map2(a, b, sum),
               as_loop(map2(a, b, sum),
                       return = "eval")
  )

})


# typed version of map2

test_that("as_loop works with typed versions of map2", {

  a <- 1:10
  b <- 10:1

  # map2_chr
  expect_equal(map2_chr(a, b, sum),
               as_loop(map2_chr(a, b, sum),
                       return = "eval")
  )

  # map2_int
  expect_equal(map2_int(a, b, sum),
               as_loop(map2_int(a, b, sum),
                       return = "eval")
  )

  # map2_dbl
  expect_equal(map2_dbl(a, b, sum),
               as_loop(map2_dbl(a, b, sum),
                       return = "eval")
  )

  # map2_lgl
  expect_equal(map2_dbl(a, b, ~ .x == .y),
               as_loop(map2_dbl(a, b, ~ .x == .y),
                       return = "eval")
  )

  # map2_raw
  expect_equal(map2_raw(a, b, ~ as.raw(.x + .y)),
               as_loop(map2_raw(a, b, ~ as.raw(.x + .y)),
                       return = "eval")
  )

  # map2_dfc
  expect_equal(map2_dbl(a, b, sum),
               as_loop(map2_dbl(a, b, sum),
                       return = "eval")
  )

  # map2_dfr
  expect_equal(map2_dbl(a, b, sum),
               as_loop(map2_dbl(a, b, sum),
                       return = "eval")
  )


})

# walk2

test_that("as_loop works with walk2", {

  a <- 1:10
  b <- 10:1

  output_asloop <- capture.output(
    as_loop(walk2(a, b, ~ print(paste0(.x, .y))),
            return = "eval")
  )

  output_walk2 <- capture.output(
    walk2(a, b, ~ print(paste0(.x, .y)))
  )

  expect_equal(output_asloop, output_walk2)

})



# pmap
test_that("as_loop works with pmap", {

  a <- 1:10
  b <- 10:1
  c <- rep(5, 10)

  expect_equal(pmap(list(a, b, c), sum),
               as_loop(pmap(list(a, b, c), sum),
                       return = "eval")
  )

  d <- set_names(1:10, letters[1:10])

  expect_equal(pmap(list(d, b, c), sum),
               as_loop(pmap(list(d, b, c), sum),
                       return = "eval")
  )

})


# pwalk
test_that("as_loop works with pwalk", {

  a <- 1:10
  b <- letters[10:1]
  c <- rep(5, 10)

  output_asloop <- capture.output(
    as_loop(pwalk(list(a, b, c), ~ print(paste(..1, ..2, ..3))),
            return = "eval")
  )

  output_pwalk <- capture.output(
    pwalk(list(a, b, c), ~ print(paste(..1, ..2, ..3)))
  )

  expect_equal(output_asloop, output_pwalk)

})


# lmap_at (lmap_if)

test_that("as_loop works with lmap and lmap_at", {

  list_rep <- function(x) {

    out <- rep_len(x, 2)
    if (length(out) > 0) {
      names(out) <- paste0(names(x), seq_len(2))
    }
    out
  }

  y <- list(a = 1, b = "a", c = 3)

  expect_equal(lmap(y,  list_rep),
               as_loop(lmap(y,  list_rep),
                       return = "eval")
  )

  expect_equal(lmap_at(y, c(1,3), list_rep),
               as_loop(lmap_at(y, c(1,3), list_rep),
                       return = "eval")
  )

})


# modify

test_that("as_loop works with modify", {

  x <- list(a = c(7, 10), b = c(20, 25))
  attr(x, "myatt") <- "this vector has attributes"

  exp1 <- modify(x, sum)
  out1 <- as_loop(modify(x, sum), return = "eval")

  expect_equal(exp1, out1)

})

# modify_at and modify_if

test_that("as_loop works with modify_at and modify_if", {

  x <- list(a = c(7, 10), b = c("a", "b"))
  attr(x, "myatt") <- "this vector has attributes"

  # modify_at by name in function
  exp1 <- modify_at(x, "a", sum)
  out1 <- as_loop(modify_at(x, "a", sum), return = "eval")

  expect_equal(exp1, out1)

  # modify_at by name in external vector
  myvec <- "a"
  exp1b <- modify_at(x, myvec, sum)
  out1b <- as_loop(modify_at(x, myvec, sum), return = "eval")

  expect_equal(exp1b, out1b)


  # modify_at by position
  exp2 <- modify_at(x, 1, sum)
  out2 <- as_loop(modify_at(x, 1, sum), return = "eval")

  expect_equal(exp2, out2)

  # modify_at by position in vector
  myvec2 <- 1
  exp2b <- modify_at(x, myvec2, sum)
  out2b <- as_loop(modify_at(x, myvec2, sum), return = "eval")

  expect_equal(exp2b, out2b)


  # modify_if without else
  exp3 <- modify_if(x, is.numeric, sum)
  out3 <- as_loop(modify_if(x, is.numeric, sum), return = "eval")

  expect_equal(exp3, out3)


  # modify_if without else in custom function
  myfun <- function(x) is.numeric(x)
  exp3b <- modify_if(x, myfun, sum)
  out3b <- as_loop(modify_if(x, myfun, sum), return = "eval")

  expect_equal(exp3b, out3b)


  # modify_if without else in regular anonymous function
  exp3c <- modify_if(x, function(x) is.numeric(x), sum)
  out3c <- as_loop(modify_if(x, function(x) is.numeric(x), sum), return = "eval")

  expect_equal(exp3c, out3c)


  # modify_if with else
  exp4 <- modify_if(x, is.numeric, sum, .else = ~paste(.x, collapse = ","))
  out4 <- as_loop(modify_if(x, is.numeric, sum, .else = ~ paste(.x, collapse = ",")),
                  return = "eval")

  expect_equal(exp4, out4)
})


# modify2
# TODO: add modify2 with .inp objects!

test_that("as_loop works with modify", {

  x <- list(a = 7, b = 25)
  attr(x, "myatt") <- "this vector has attributes"

  y <- list(20, c(100))

  exp1 <- modify2(x, y, sum)
  out1 <- as_loop(modify2(x, y, sum), return = "eval")

  expect_equal(exp1, out1)

})

# accumulate

test_that("as_loop works with accumulate, lambda function", {

  sum_print <- function(x, y) {
    print(paste("x is :", x))
    x + y
  }

  # dir = forward, no init
  exp1 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y))
    )

  out1 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y)) %>% as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y), .init = 100)
  )

  out2 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y), .init = 100) %>% as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)


  # dir = backward, no init
  exp3 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y), .dir = "backward")
  )

  out3 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y), .dir = "backward") %>% as_loop(., return = "eval")
  )

  expect_equal(exp3, out3)


  # dir = backward, with init
  exp4 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y), .init = 100, .dir = "backward")
  )

  out4 <- capture.output(
    accumulate(1:10, ~sum_print(.x, .y), .init = 100, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp4, out4)


})


test_that("as_loop works with accumulate, anonymous function", {

  sum_print <- function(x, y) {
    print(paste("x is :", x))
    x + y
  }

  # dir = forward, no init
  exp1 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y))
  )

  out1 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y)) %>% as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y), .init = 100)
  )

  out2 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y), .init = 100) %>% as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)


  # dir = backward, no init
  exp3 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y), .dir = "backward")
  )

  out3 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y), .dir = "backward") %>% as_loop(., return = "eval")
  )

  expect_equal(exp3, out3)


  # dir = backward, with init
  exp4 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y), .init = 100, .dir = "backward")
  )

  out4 <- capture.output(
    accumulate(1:10, function(.x, .y) sum_print(.x, .y), .init = 100, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp4, out4)


})

test_that("as_loop works with accumulate, named function", {

  sum_print <- function(x, y) {
    print(paste("x is :", x))
    x + y
  }

  # dir = forward, no init
  exp1 <- capture.output(
    accumulate(1:10, sum_print)
  )

  out1 <- capture.output(
    accumulate(1:10, sum_print) %>% as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    accumulate(1:10, sum_print, .init = 100)
  )

  out2 <- capture.output(
    accumulate(1:10, sum_print, .init = 100) %>% as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)


  # dir = backward, no init
  exp3 <- capture.output(
    accumulate(1:10, sum_print, .dir = "backward")
  )

  out3 <- capture.output(
    accumulate(1:10, sum_print, .dir = "backward") %>% as_loop(., return = "eval")
  )

  expect_equal(exp3, out3)


  # dir = backward, with init
  exp4 <- capture.output(
    accumulate(1:10, sum_print, .init = 100, .dir = "backward")
  )

  out4 <- capture.output(
    accumulate(1:10, sum_print, .init = 100, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp4, out4)


})

# accumulate2

test_that("as_loop works with accumulate2, named function", {

  sum_print3 <- function(x, y, z) {
    print(paste("x is :", x))
    print(paste("y is :", y))
    print(paste("z is :", z))
    x + y + z
  }

  # dir = forward, no init
  exp1 <- capture.output(
    accumulate2(1:5, 5:8, sum_print3)
  )

  out1 <- capture.output(
    accumulate2(1:5, 5:8, sum_print3) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    accumulate2(1:5, 5:9, sum_print3, .init = 100)
  )

  out2 <- capture.output(
    accumulate2(1:5, 5:9, sum_print3, .init = 100) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)

})

test_that("as_loop works with accumulate2, anonymous function", {

  sum_print3 <- function(x, y, z) {
    print(paste("x is :", x))
    print(paste("y is :", y))
    print(paste("z is :", z))
    x + y + z
  }

  # dir = forward, no init
  exp1 <- capture.output(
    accumulate2(1:5, 5:8, function(.x, .y, .z) sum_print3(.x, .y, .z))
  )

  out1 <- capture.output(
    accumulate2(1:5, 5:8, function(.x, .y, .z) sum_print3(.x, .y, .z)) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    accumulate2(1:5, 5:9, function(.x, .y, .z) sum_print3(.x, .y, .z), .init = 100)
  )

  out2 <- capture.output(
    accumulate2(1:5, 5:9, function(.x, .y, .z) sum_print3(.x, .y, .z), .init = 100) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)

})

test_that("as_loop works with accumulate2, lambda function", {

  sum_print3 <- function(x, y, z) {
    print(paste("x is :", x))
    print(paste("y is :", y))
    print(paste("z is :", z))
    x + y + z
  }

  # dir = forward, no init
  exp1 <- capture.output(
    accumulate2(1:5, 5:8, ~ sum_print3(.x, .y, ..3))
  )

  out1 <- capture.output(
    accumulate2(1:5, 5:8, ~ sum_print3(.x, .y, ..3)) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    accumulate2(1:5, 5:9, ~ sum_print3(.x, .y, ..3), .init = 100)
  )

  out2 <- capture.output(
    accumulate2(1:5, 5:9, ~ sum_print3(.x, .y, ..3), .init = 100) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)

})


# reduce

test_that("as_loop works with reduce, lambda function", {

  sum_print <- function(x, y) {
    print(paste("x is :", x))
    x + y
  }

  # dir = forward, no init
  exp1 <- capture.output(
    reduce(1:10, ~ sum_print(.x, .y))
  )

  out1 <- capture.output(
    reduce(1:10, ~ sum_print(.x, .y)) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    reduce(1:10, ~sum_print(.x, .y), .init = 100)
  )

  out2 <- capture.output(
    reduce(1:10, ~sum_print(.x, .y), .init = 100) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)


  # dir = backward, no init
  exp3 <- capture.output(
    reduce(1:10, ~sum_print(.x, .y), .dir = "backward")
  )

  out3 <- capture.output(
    reduce(1:10, ~sum_print(.x, .y), .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp3, out3)


  # dir = backward, with init
  exp4 <- capture.output(
    reduce(1:10, ~sum_print(.x, .y), .init = 100, .dir = "backward")
  )

  out4 <- capture.output(
    reduce(1:10, ~sum_print(.x, .y), .init = 100, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp4, out4)

})

test_that("as_loop works with reduce, anonymous function", {

  sum_print <- function(x, y) {
    print(paste("x is :", x))
    x + y
  }

  # dir = forward, no init
  exp1 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y))
  )

  out1 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y)) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y), .init = 100)
  )

  out2 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y), .init = 100) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)


  # dir = backward, no init
  exp3 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y), .dir = "backward")
  )

  out3 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y), .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp3, out3)


  # dir = backward, with init
  exp4 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y), .init = 100, .dir = "backward")
  )

  out4 <- capture.output(
    reduce(1:10, function(.x, .y) sum_print(.x, .y), .init = 100, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp4, out4)

})

test_that("as_loop works with reduce, named function", {

  sum_print <- function(x, y) {
    print(paste("x is :", x))
    x + y
  }

  # dir = forward, no init
  exp1 <- capture.output(
    reduce(1:10, sum_print)
  )

  out1 <- capture.output(
    reduce(1:10, sum_print) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp1, out1)


  # dir = forward, with init
  exp2 <- capture.output(
    reduce(1:10, sum_print, .init = 100)
  )

  out2 <- capture.output(
    reduce(1:10, sum_print, .init = 100) %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp2, out2)


  # dir = backward, no init
  exp3 <- capture.output(
    reduce(1:10, sum_print, .dir = "backward")
  )

  out3 <- capture.output(
    reduce(1:10, sum_print, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp3, out3)


  # dir = backward, with init
  exp4 <- capture.output(
    reduce(1:10, sum_print, .init = 100, .dir = "backward")
  )

  out4 <- capture.output(
    reduce(1:10, sum_print, .init = 100, .dir = "backward") %>%
      as_loop(., return = "eval")
  )

  expect_equal(exp4, out4)

})


# reduce2

test_that("as_loop works with reduce2, named function", {

  foo <- function(x, y, z) {
    (x^2 + y^(1/2)) / z
  }

  # dir = forward, no init
  exp1 <- reduce2(1:3, 4:5, foo)

  out1 <- reduce2(1:3, 4:5, foo) %>%
      as_loop(., return = "eval")

  expect_equal(exp1, out1)


  # dir = forward, no init
  exp2 <- reduce2(1:3, 4:6, foo, .init = 10)

  out2 <- reduce2(1:3, 4:6, foo, .init = 10) %>%
    as_loop(., return = "eval")

  expect_equal(exp2, out2)

})

test_that("as_loop works with reduce2, anonymous function", {

  foo <- function(x, y, z) {
    (x^2 + y^(1/2)) / z
  }

  # dir = forward, no init
  exp1 <- reduce2(1:3, 4:5, function(.x, .y, .z) foo(.x, .y, .z))

  out1 <- reduce2(1:3, 4:5, function(.x, .y, .z) foo(.x, .y, .z)) %>%
    as_loop(., return = "eval")

  expect_equal(exp1, out1)


  # dir = forward, no init
  exp2 <- reduce2(1:3, 4:6, function(.x, .y, .z) foo(.x, .y, .z), .init = 10)

  out2 <- reduce2(1:3, 4:6, function(.x, .y, .z) foo(.x, .y, .z), .init = 10) %>%
    as_loop(., return = "eval")

  expect_equal(exp2, out2)

})

test_that("as_loop works with reduce2, lambda function", {

  foo <- function(x, y, z) {
    (x^2 + y^(1/2)) / z
  }

  # dir = forward, no init
  exp1 <- reduce2(1:3, 4:5, ~ foo(.x, .y, ..3))

  out1 <- reduce2(1:3, 4:5, ~ foo(.x, .y, ..3)) %>%
    as_loop(., return = "eval")

  expect_equal(exp1, out1)


  # dir = forward, no init
  exp2 <- reduce2(1:3, 4:6, ~ foo(.x, .y, ..3), .init = 10)

  out2 <- reduce2(1:3, 4:6, ~ foo(.x, .y, ..3), .init = 10) %>%
    as_loop(., return = "eval")

  expect_equal(exp2, out2)

})




# character and numeric vectors as .f argument

test_that("as_loop throws an error when called with non-purrr or non-supported functions", {

  l2 <- list(
    list(num = 1:3,     letters[1:3]),
    list(num = 101:103, letters[4:6]),
    list()
  )

  exp1 <- map(l2, "num")
  out1 <- as_loop(map(l2, "num"), return = "eval")

  expect_equal(exp1, out1)

  exp2 <- map(l2, c(2, 2))
  out2 <- as_loop(map(l2, c(2, 2)), return = "eval")

  expect_equal(exp2, out2)

})

# support

test_that("as_loop throws an error when called with non-purrr or non-supported functions", {

  x <- list(1, 2, 3)

  tmap <- function(x, f) f(x)

  expect_error(as_loop(tmap(x, sum), return = "eval"),
               "only works with `map` and similar functions from the purrr package"
  )

  expect_error(as_loop(lmap_if(x, sum), return = "eval"),
               "does only support certain"
  )

  expect_error(as_loop(lapply(x, sum), return = "eval"),
               "doesn't support functions from base R's apply family"
  )

})
