# add out of bound tests for index_fn and index_all_fn

test_that("ping works with map", {

  a <- list(x = 1, y = 1:2, z = 1:3)

  expect_equal(
    1,
    ping(map(a, sum), 1)
  )

  expect_equal(
    list(x = 1, z = 6),
    ping(map(a, sum), c(1, 3))
  )

  expect_equal(
    list(x = 1),
    ping(map(a, sum), simplify = FALSE)
  )

  expect_equal(
    3,
    ping(map(a, sum), "y")
  )

})

test_that("ping works with the pipe", {

  a <- list(1, 1:2, 1:3)

  expect_equal(
    1,
    map(a, sum) %>% ping(1)
  )

  expect_equal(
    list(1, 6),
    map(a, sum) %>% ping(c(1,3))
  )

  expect_equal(
    list(1),
    map(a, sum) %>% ping(simplify = FALSE)
  )

})

test_that("ping works with map_at", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
  c <- set_names(c, letters[1:10])

  # using idx in .at
  expect_equal(
    list(b = c(1,2), e = 15),
    map_at(c, .at = c(5,10), sum) %>% ping(c(2,5))
  )

  # using name in .at and ping()
  expect_equal(
    list(c = 6, g = 28),
    map_at(c, .at = c("c", "g"), sum) %>% ping(c("c", "g"))
  )

  # using idx in .at and name in ping()
  expect_equal(
    list(c = 1:3, e = 15),
    map_at(c, .at = c(5, 10), sum) %>% ping(c("c", "e"))
  )

  # using name in .at and idx in ping()
  expect_equal(
    list(e = 1:5, f = 1:6, g = 28),
    map_at(c, .at = c("c", "g"), sum) %>% ping(c(5:7))
  )

})


test_that("ping works with map_if", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
  c <- set_names(c, letters[1:10])

  # pinging the transformed element
  expect_equal(
    21,
    map_if(c, .p = ~ any(.x > 5), sum) %>% ping(6)
  )

  # pinging a non-transformed element
  expect_equal(
    1:3,
    map_if(c, .p = ~ any(.x > 5), sum) %>% ping(3)
  )

  # pinging two elements
  expect_equal(
    list(e = 1:5, f = 21),
    map_if(c, .p = ~ any(.x > 5), sum) %>% ping(c(5,6))
  )

  # pinging an else elements

  expect_equal(
    "1 2",
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% ping(2)
  )

  # pinging if and else element

  expect_equal(
    list(e = "1 2 3 4 5",
         f = 21),
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% ping(c(5,6))
  )

  # pinging works with names

  expect_equal(
    list(e = "1 2 3 4 5",
         f = 21),
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% ping(c("e", "f"))
  )


})

test_that("ping works with map2", {

  a <- list(1, 1:2, 1:3)
  b <- list(11, 11:12, 11:13)

  expect_equal(
    12,
    map2(a, b, sum) %>% ping(1)
  )

  expect_equal(
    list(12, 42),
    map2(a, b, sum) %>% ping(c(1,3))
  )

  expect_equal(
    list(12),
    map2(a, b, sum) %>% ping(simplify = FALSE)
  )

  # names work too

  c <- set_names(a, letters[24:26])

  expect_equal(
    26,
    map2(c, b, sum) %>% ping(c("y"))
  )

  expect_equal(
    list(y = 26, z = 42),
    map2(c, b, sum) %>% ping(c("y", "z"))
  )

})


test_that("ping works with pmap", {

  a <- list(1, 1:2, 1:3)
  b <- list(11, 11:12, 11:13)
  c <- list(111, 111:112, 111:113)

  l <- list(a, b, c)

  expect_equal(
    378,
    pmap(l, sum) %>% ping(3)
  )

  expect_equal(
    list(123, 378),
    pmap(l, sum) %>% ping(c(1,3))
  )

  expect_equal(
    list(123),
    pmap(l, sum) %>% ping(simplify = FALSE)
  )

  # names work too

  a2 <- set_names(a, letters[1:3])
  l2 <- list(a2, b, c)

  expect_equal(
    378,
    pmap(l2, sum) %>% ping("c")
  )

  expect_equal(
    list(a = 123, c = 378),
    pmap(l2, sum) %>% ping(c("a","c"))
  )

  expect_equal(
    list(a = 123),
    pmap(l2, sum) %>% ping(simplify = FALSE)
  )

})

test_that("ping works with imap, imodify and iwalk", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)

  expect_equal(
    list("3:6", "9:45"),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% ping(c(3,9))
  )

  expect_equal(
    list(c("1 3 a", "2 3 a", "3 3 a"),
         c("1 9 a", "2 9 a","3 9 a","4 9 a","5 9 a","6 9 a","7 9 a","8 9 a","9 9 a")),
    imap(c, paste, "a") %>% ping(c(3,9))
  )

  # works with negative subsets
  expect_equal(
    list("8:36", "9:45", "10:55"),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% ping(-c(1:7))
  )

  # works with imodfy
  x <- list(c(10, 20),
            c(20, 40),
            c(40, 80),
            c(80, 160))

  expect_equal(
    list("2:60", "4:240"),
    imodify(x, ~ paste0(.y, ":", sum(.x))) %>% ping(c(2,4))
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
    iwalk(d, ~ print(c(.x, .y))) %>% ping(c(2,4))
  )

})

test_that("ping works with lmap", {

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
    lmap(y, list_rep) %>% ping("c")
  )

  expect_equal(
    lmap(y[2], list_rep),
    lmap(y, list_rep) %>% ping(2)
  )

  expect_equal(
    lmap(y[c(2,3)], list_rep),
    lmap(y, list_rep) %>% ping(c(2,3))
  )

  expect_equal(
    lmap(y[c("a","b")], list_rep),
    lmap(y, list_rep) %>% ping(c("a","b"))
  )

})

# reduce
test_that("ping works with reduce and reduce2", {

  # with one i
  expect_equal(
    15,
    reduce(1:10, ~ sum(.x, .y)) %>% ping(5)
  )

  # with several i
  expect_equal(
    c(1, 55),
    reduce(as.list(1:10), ~ sum(.x, .y)) %>% ping(c(1,10))
  )

  # with named i's
  a <- set_names(as.list(1:10), letters[1:10])

  expect_equal(
    c(6, 36),
    reduce(a, ~ sum(.x, .y)) %>% ping(c("c", "h"))
  )

  # using .init
  expect_equal(
    c(100, 155),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% ping(c(1, 11))
    )

  # with .dir = "backward"
  expect_equal(
    c(55, 10),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% ping(c(1,10))
  )

  # with .init and .dir = "backward"
  expect_equal(
    c(155, 100),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% ping(c(1,11))
  )

  # reduce2
  expect_equal(
    list("1 2 6", "1 2 6 3 7 4 8"),
    reduce2(1:5, 6:9, paste) %>% ping(c(2,4))
  )

  # reduce2 with .init
  expect_equal(
    list("100 1 6", "100 1 6 2 7 3 8"),
    reduce2(1:4, 6:9, paste, .init = 100) %>% ping(c(2,4))
  )

})


# acumulate

test_that("ping works with accumulate and accumulate2", {

  # with one i
  expect_equal(
    15,
    accumulate(1:10, ~ sum(.x, .y)) %>% ping(5)
  )

  # with several i
  expect_equal(
    c(1, 55),
    accumulate(as.list(1:10), ~ sum(.x, .y)) %>% ping(c(1,10))
  )

  # with named i's
  a <- set_names(as.list(1:10), letters[1:10])

  expect_equal(
    c(c = 6, h = 36),
    accumulate(a, ~ sum(.x, .y)) %>% ping(c("c", "h"))
  )

  # using .init
  expect_equal(
    c(100, 155),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% ping(c(1, 11))
  )

  # with .dir = "backward"
  expect_equal(
    c(55, 10),
    accumulate(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% ping(c(1,10))
  )

  # with .init and .dir = "backward"
  expect_equal(
    c(155, 100),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% ping(c(1,11))
  )

  # accumulate2
  expect_equal(
    list("1 2 6", "1 2 6 3 7 4 8"),
    accumulate2(1:5, 6:9, paste) %>% ping(c(2,4))
  )

  # accumulate2 with .init
  expect_equal(
    list("100 1 6", "100 1 6 2 7 3 8"),
    accumulate2(1:4, 6:9, paste, .init = 100) %>% ping(c(2,4))
  )
})


# negative tests

# test that apply throws error
test_that("apply family throws error", {

  # with one i
  expect_error(
    lapply(list(1:3, 4:6, 7:9), sum) %>% ping(),
    "doesn't support functions from base R's apply family"
  )
  }
)

# test subsetting
test_that("ping throws subsetting errors", {

  expect_error(
    map(list(1:3, 4:6, 7:9), sum) %>% ping(5),
    "One or more locations don't exist"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% ping("d"),
    "One or more named elements don't exist"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% ping(c(TRUE, FALSE, TRUE)),
    "only accepts numeric or character vectors for subsetting"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% ping(list(1, "c")),
    "only accepts numeric or character vectors for subsetting"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% ping(c(-1, 2)),
    "`i` argument must not contain positive and negative subscripts"
  )
}
)















