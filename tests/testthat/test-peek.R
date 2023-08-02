# add out of bound tests for index_fn and index_all_fn

test_that("peek works with map", {

  a <- list(x = 1, y = 1:2, z = 1:3)

  expect_equal(
    1,
    peek(map(a, sum), 1)
  )

  expect_equal(
    list(x = 1, z = 6),
    peek(map(a, sum), c(1, 3))
  )

  expect_equal(
    list(x = 1),
    peek(map(a, sum), simplify = FALSE)
  )

  expect_equal(
    3,
    peek(map(a, sum), "y")
  )

})

test_that("peek works with the pipe", {

  a <- list(1, 1:2, 1:3)

  expect_equal(
    1,
    map(a, sum) %>% peek(1)
  )

  expect_equal(
    list(1, 6),
    map(a, sum) %>% peek(c(1,3))
  )

  expect_equal(
    list(1),
    map(a, sum) %>% peek(simplify = FALSE)
  )

})

test_that("peek works with map_at", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
  c <- set_names(c, letters[1:10])

  # using idx in .at
  expect_equal(
    list(b = c(1,2), e = 15),
    map_at(c, .at = c(5,10), sum) %>% peek(c(2,5))
  )

  # using name in .at and peek()
  expect_equal(
    list(c = 6, g = 28),
    map_at(c, .at = c("c", "g"), sum) %>% peek(c("c", "g"))
  )

  # using idx in .at and name in peek()
  expect_equal(
    list(c = 1:3, e = 15),
    map_at(c, .at = c(5, 10), sum) %>% peek(c("c", "e"))
  )

  # using name in .at and idx in peek()
  expect_equal(
    list(e = 1:5, f = 1:6, g = 28),
    map_at(c, .at = c("c", "g"), sum) %>% peek(c(5:7))
  )

})


test_that("peek works with map_if", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
  c <- set_names(c, letters[1:10])

  # peeking the transformed element
  expect_equal(
    21,
    map_if(c, .p = ~ any(.x > 5), sum) %>% peek(6)
  )

  # peeking a non-transformed element
  expect_equal(
    1:3,
    map_if(c, .p = ~ any(.x > 5), sum) %>% peek(3)
  )

  # peeking two elements
  expect_equal(
    list(e = 1:5, f = 21),
    map_if(c, .p = ~ any(.x > 5), sum) %>% peek(c(5,6))
  )

  # peeking an else elements

  expect_equal(
    "1 2",
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% peek(2)
  )

  # peeking if and else element

  expect_equal(
    list(e = "1 2 3 4 5",
         f = 21),
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% peek(c(5,6))
  )

  # peeking works with names

  expect_equal(
    list(e = "1 2 3 4 5",
         f = 21),
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% peek(c("e", "f"))
  )


})

test_that("peek works with map2", {

  a <- list(1, 1:2, 1:3)
  b <- list(11, 11:12, 11:13)

  expect_equal(
    12,
    map2(a, b, sum) %>% peek(1)
  )

  expect_equal(
    list(12, 42),
    map2(a, b, sum) %>% peek(c(1,3))
  )

  expect_equal(
    list(12),
    map2(a, b, sum) %>% peek(simplify = FALSE)
  )

  # names work too

  c <- set_names(a, letters[24:26])

  expect_equal(
    26,
    map2(c, b, sum) %>% peek(c("y"))
  )

  expect_equal(
    list(y = 26, z = 42),
    map2(c, b, sum) %>% peek(c("y", "z"))
  )

})


test_that("peek works with pmap", {

  a <- list(1, 1:2, 1:3)
  b <- list(11, 11:12, 11:13)
  c <- list(111, 111:112, 111:113)

  l <- list(a, b, c)

  expect_equal(
    378,
    pmap(l, sum) %>% peek(3)
  )

  expect_equal(
    list(123, 378),
    pmap(l, sum) %>% peek(c(1,3))
  )

  expect_equal(
    list(123),
    pmap(l, sum) %>% peek(simplify = FALSE)
  )

  # names work too

  a2 <- set_names(a, letters[1:3])
  l2 <- list(a2, b, c)

  expect_equal(
    378,
    pmap(l2, sum) %>% peek("c")
  )

  expect_equal(
    list(a = 123, c = 378),
    pmap(l2, sum) %>% peek(c("a","c"))
  )

  expect_equal(
    list(a = 123),
    pmap(l2, sum) %>% peek(simplify = FALSE)
  )

})

test_that("peek works with imap, imodify and iwalk", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)

  expect_equal(
    list("3:6", "9:45"),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% peek(c(3,9))
  )

  expect_equal(
    list(c("1 3 a", "2 3 a", "3 3 a"),
         c("1 9 a", "2 9 a","3 9 a","4 9 a","5 9 a","6 9 a","7 9 a","8 9 a","9 9 a")),
    imap(c, paste, "a") %>% peek(c(3,9))
  )

  # works with negative subsets
  expect_equal(
    list("8:36", "9:45", "10:55"),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% peek(-c(1:7))
  )

  # TODO: make it work with imodidy
  # works with imodfy
  x <- list(a = c(10, 20),
            b = c(20, 40),
            c = c(40, 80),
            d = c(80, 160))

  attr(x, "test") <- "test"

  out <- list(b = "b:60", d = "d:240")
  attr(out, "test") <- "test"

  expect_equal(
    out,
    imodify(x, ~ paste0(.y, ":", sum(.x))) %>% peek(c(2,4))
  )

  # works with iwalk
  d <- 1:4

  custom_iwalk <- function(x) {
    .idx <- seq_along(x)
    for (i in seq_along(x)) {
      if (i %% 2) next
      print(c(x[[i]], .idx[[i]]))
    }
    invisible(x[c(2,4)])
  }

  expect_equal(
    custom_iwalk(d),
    iwalk(d, ~ print(c(.x, .y))) %>% peek(c(2,4))
  )

})

test_that("peek works with lmap", {

  list_rep <- function(x) {

    out <- rep_len(x, 2)
    if (length(out) > 0) {
      names(out) <- paste0(names(x), seq_len(2))
    }
    out
  }

  y <- list(a = 1, b = "a", c = 3)

  expect_equal(
    lmap(y["c"], list_rep),
    lmap(y, list_rep) %>% peek("c")
  )

  expect_equal(
    lmap(y[2], list_rep),
    lmap(y, list_rep) %>% peek(2)
  )

  expect_equal(
    lmap(y[c(2,3)], list_rep),
    lmap(y, list_rep) %>% peek(c(2,3))
  )

  expect_equal(
    lmap(y[c("a","b")], list_rep),
    lmap(y, list_rep) %>% peek(c("a","b"))
  )

})


# reduce
test_that("peek works with reduce and reduce2", {

  # with one i
  expect_equal(
    21,
    reduce(1:10, ~ sum(.x, .y)) %>% peek(5)
  )

  # with several i
  expect_equal(
    c(3, 55),
    reduce(as.list(1:10), ~ sum(.x, .y)) %>% peek(c(1,9))
  )

  # with named i's
  a <- set_names(as.list(1:10), letters[1:10])

  expect_error(
    reduce(a, ~ sum(.x, .y)) %>% peek(c("c", "h")),
    "only accepts numeric vectors for indexing"
  )

  # using .init
  expect_equal(
    c(101, 155),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% peek(c(1, 10))
    )

  # with .dir = "backward"
  expect_equal(
    c(19, 55),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peek(c(1,9))
  )

  expect_equal(
    c(34, 27),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peek(c(3,2))
  )

  # with .init and .dir = "backward"
  expect_equal(
    c(127, 119),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% peek(c(3,2))
  )

  expect_equal(
    c(140, 127),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% peek(c(5,3))
  )

  # reduce2
  expect_equal(
    list("1 2 6 3 7", "1 2 6 3 7 4 8 5 9"),
    reduce2(1:5, 6:9, paste) %>% peek(c(2,4))
  )

  expect_equal(
    list("1 2 6 3 7 4 8", "1 2 6 3 7"),
    reduce2(1:5, 6:9, paste) %>% peek(c(3,2))
  )

  # reduce2 with .init
  expect_equal(
    list("100 1 6 2 7 3 8", "100 1 6 2 7"),
    reduce2(1:4, 6:9, paste, .init = 100) %>% peek(c(3,2))
  )

})


# acumulate

test_that("peek works with accumulate and accumulate2", {

  # with one i
  expect_equal(
    21,
    accumulate(1:10, ~ sum(.x, .y)) %>% peek(5)
  )

  # with several i
  expect_equal(
    c(3, 55),
    accumulate(as.list(1:10), ~ sum(.x, .y)) %>% peek(c(1,9))
  )

  # with named i's
  a <- set_names(as.list(1:10), letters[1:10])

  expect_error(
    accumulate(a, ~ sum(.x, .y)) %>% peek(c("c", "h")),
    "only accepts numeric vectors for indexing"
  )

  # using .init
  expect_equal(
    c(101, 155),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% peek(c(1, 10))
  )

  # with .dir = "backward"
  expect_equal(
    c(19, 55),
    accumulate(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peek(c(1,9))
  )

  expect_equal(
    c(40, 27),
    accumulate(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peek(c(4,2))
  )

  # with .init and .dir = "backward"
  expect_equal(
    c(110, 155),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% peek(c(1,10))
  )

  # accumulate2
  expect_equal(
    list("1 2 6 3 7", "1 2 6 3 7 4 8 5 9"),
    accumulate2(1:5, 6:9, paste) %>% peek(c(2,4))
  )

  # accumulate2 with .init
  expect_equal(
    list("100 1 6 2 7", "100 1 6 2 7 3 8 4 9"),
    accumulate2(1:4, 6:9, paste, .init = 100) %>% peek(c(2,4))
  )
})


# test_that("peek works with map_depth and modify_depth", {
#
#   x <- list(a = list(foo = 1:2, bar = 3:4), b = list(baz = 5:6))
#
#   expect_equal(
#     list(foo = "1/2"),
#     map_depth(x, 2, paste, collapse = "/") %>% peek()
#   )
#
#   expect_equal(
#     list(bar = "3/4"),
#     map_depth(x, 2, paste, collapse = "/") %>% peek(2)
#   )
#
#   expect_equal(
#     list(a = list(bar = "3/4"), b = list(baz = "5/6")),
#     map_depth(x, 2, paste, collapse = "/") %>% peek(2:3)
#   )
#
#   expect_equal(
#     list(a = list(bar = "3/4")),
#     map_depth(x, 2, paste, collapse = "/") %>% peek(2, simplify = FALSE)
#   )
#
#   # TODO: modify_depth
#
# })



# negative tests

# test that apply throws error
test_that("apply family throws error", {

  # with one i
  expect_error(
    lapply(list(1:3, 4:6, 7:9), sum) %>% peek(),
    "doesn't support functions from base R's apply family"
  )
  }
)

# test subsetting
test_that("peek throws subsetting errors", {

  expect_error(
    map(list(1:3, 4:6, 7:9), sum) %>% peek(5),
    "One or more locations don't exist"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peek("d"),
    "One or more named elements don't exist"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peek(c(TRUE, FALSE, TRUE)),
    "only accepts numeric or character vectors for subsetting"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peek(list(1, "c")),
    "only accepts numeric or character vectors for subsetting"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peek(c(-1, 2)),
    "`i` argument must not contain positive and negative subscripts"
  )
}
)















