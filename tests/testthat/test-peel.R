# add out of bound tests for index_fn and index_all_fn

test_that("peel works with map", {

  a <- list(x = 1, y = 1:2, z = 1:3)

  expect_equal(
    1,
    peel(map(a, sum), 1)
  )

  expect_equal(
    list(1, 1:3),
    peel(map(a, sum), c(1, 3))
  )

  expect_equal(
    list(1),
    peel(map(a, sum), simplify = FALSE)
  )

  expect_equal(
    1:2,
    peel(map(a, sum), "y")
  )

})

test_that("peel works with the pipe", {

  a <- list(1, 1:2, 1:3)

  expect_equal(
    1,
    map(a, sum) %>% peel(1)
  )

  expect_equal(
    list(1, 1:3),
    map(a, sum) %>% peel(c(1,3))
  )

  expect_equal(
    list(1),
    map(a, sum) %>% peel(simplify = FALSE)
  )

})

test_that("peel works with map_at", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
  c <- set_names(c, letters[1:10])

  # using idx in .at
  expect_equal(
    list(c(1,2), c(1:5)),
    map_at(c, .at = c(5,10), sum) %>% peel(c(2,5))
  )

  # using name in .at and peel()
  expect_equal(
    list(1:3, 1:7),
    map_at(c, .at = c("c", "g"), sum) %>% peel(c("c", "g"))
  )

  # using idx in .at and name in peel()
  expect_equal(
    list(1:3, 1:5),
    map_at(c, .at = c(5, 10), sum) %>% peel(c("c", "e"))
  )

  # using name in .at and idx in peel()
  expect_equal(
    list( 1:5, 1:6, 1:7),
    map_at(c, .at = c("c", "g"), sum) %>% peel(c(5:7))
  )

})



test_that("peel works with map_if", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)
  c <- set_names(c, letters[1:10])

  # peeling the transformed element
  expect_equal(
    1:6,
    map_if(c, .p = ~ any(.x > 5), sum) %>% peel(6)
  )

  # peeling a non-transformed element
  expect_equal(
    1:3,
    map_if(c, .p = ~ any(.x > 5), sum) %>% peel(3)
  )

  # peeling two elements
  expect_equal(
    list(1:5, 1:6),
    map_if(c, .p = ~ any(.x > 5), sum) %>% peel(c(5,6))
  )

  # peeling an else elements

  expect_equal(
    1:2,
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% peel(2)
  )

  # peeling if and else element

  expect_equal(
    list(1:5,
         1:6),
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% peel(c(5,6))
  )

  # peeling works with names

  expect_equal(
    list(1:5,
         1:6),
    map_if(c,
           .p = ~ any(.x > 5),
           sum,
           .else = ~ paste(.x, collapse = " ")) %>% peel(c("e", "f"))
  )


})


test_that("peel works with map2", {

  a <- list(1, 1:2, 1:3)
  b <- list(11, 11:12, 11:13)

  expect_equal(
    list(.x = 1, .y = 11),
    map2(a, b, sum) %>% peel(1)
  )

  expect_equal(
    list(list(.x = 1,
              .y = 11),
         list(.x = 1:3,
              .y = 11:13)),
    map2(a, b, sum) %>% peel(c(1,3))
  )

  expect_equal(
    list(list(.x =  1,
              .y = 11)),
    map2(a, b, sum) %>% peel(simplify = FALSE)
  )

  # names work too

  c <- set_names(a, letters[24:26])

  expect_equal(
    list(.x = 1:2,
         .y = 11:12),
    map2(c, b, sum) %>% peel("y")
  )

  expect_equal(
    list(list(.x = 1:2,
              .y = 11:12),
         list(.x = 1:3,
              .y = 11:13)),
    map2(c, b, sum) %>% peel(c("y", "z"))
  )

})

test_that("peel works with pmap", {

  a <- list(1, 1:2, 1:3)
  b <- list(11, 11:12, 11:13)
  c <- list(111, 111:112, 111:113)

  l <- list(a, b, c)

  expect_equal(
    map(l,  ~`[`(.x, c(3))) %>% flatten,
    pmap(l, sum) %>% peel(3)
  )

  expect_equal(
    map(l,  ~`[`(.x, c(1,3))) %>% transpose,
    pmap(l, sum) %>% peel(c(1,3))
  )

  expect_equal(
    map(l,  ~`[`(.x, c(1))),
    pmap(l, sum) %>% peel(simplify = FALSE)
  )

  # names work too but are not displayed as output for first & second lvl

  a2 <- set_names(a, letters[1:3])
  l2 <- list(a2, b, c)

  expect_equal(
    pmap(l2, sum) %>% peel(3),
    pmap(l2, sum) %>% peel("c")
  )

  expect_equal(
    pmap(l, sum) %>% peel(c(1,3)),
    pmap(l2, sum) %>% peel(c("a","c"))
  )

  expect_equal(
    list(a[1], b[1], c[1]),
    pmap(l2, sum) %>% peel(simplify = FALSE)
  )

  # third level names work as intended

  a3 <- map(a2, ~ set_names(.x, letters[seq_along(.x)]))
  l3 <- list(a3, b, c)

  expect_equal(
    list(a3[[3]], b[[3]], c[[3]]),
    pmap(l3, sum) %>% peel("c")
  )

  expect_equal(
    list(unname(a3[3]), b[3], c[3]),
    pmap(l3, sum) %>% peel("c", simplify = FALSE)
  )

})



test_that("peel works with imap, imodify and iwalk", {

  c <- list(1, 1:2, 1:3, 1:4, 1:5, 1:6, 1:7, 1:8, 1:9, 1:10)

  expect_equal(
    list(list(.x = 1:3,
              .y = 3),
         list(.x = 1:9,
              .y = 9)),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% peel(c(3,9))
  )

  expect_equal(
    list(.x = c(1,2), .y = 2),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% peel(2)
  )

  # works with negative subsets
  expect_equal(
    list(list(.x = 1:9,
              .y = 9),
         list(.x = 1:10,
              .y = 10)),
    imap(c, ~ paste0(.y, ":", sum(.x))) %>% peel(-c(1:8))
  )

  # works with imodfy
  x <- list(c(10, 20),
            c(20, 40),
            c(40, 80),
            c(80, 160))

  attr(x, "test") <- "test"

  out <- list(list(.x = c(20, 40),
                   .y = 2),
              list(.x = c(80, 160),
                   .y = 4))

  expect_equal(
    out,
    imodify(x, ~ paste0(.y, ":", sum(.x))) %>% peel(c(2,4))
  )

  # works with iwalk
  d <- 4:1

  expect_equal(
    list(list(".x" = 3,
              ".y" = 2),
         list(".x" = 1,
              ".y" = 4)),
    iwalk(d, ~ print(c(.x, .y))) %>% peel(c(2,4))
  )

})


test_that("peel works with lmap", {

  list_rep <- function(x) {

    out <- rep_len(x, 2)
    if (length(out) > 0) {
      names(out) <- paste0(names(x), seq_len(2))
    }
    out
  }

  y <- list(a = 1, b = "a", c = 3)

  expect_equal(
    list(c = 3),
    lmap(y, list_rep) %>% peel("c")
  )

  expect_equal(
    list(c = 3),
    lmap(y, list_rep) %>% peel("c", simplify = FALSE)
  )

  expect_equal(
    list(b = "a"),
    lmap(y, list_rep) %>% peel(2)
  )

  expect_equal(
    list(b = "a", c = 3),
    lmap(y, list_rep) %>% peel(c(2,3))
  )

  expect_equal(
    list(a = 1, b = "a"),
    lmap(y, list_rep) %>% peel(c("a","b"))
  )

})


# reduce
test_that("peel works with reduce and reduce2", {

  # with one i
  expect_equal(
    list(.x = 15, .y = 6),
    reduce(1:10, ~ sum(.x, .y)) %>% peel(5)
  )

  # with several i
  expect_equal(
    list(
      list(.x = 3 , .y = 3),
      list(.x = 45, .y = 10)
    ),
    reduce(as.list(1:10), ~ sum(.x, .y)) %>% peel(c(2,9))
  )

  expect_equal(
    list(
      list(.x = 10, .y = 5),
      list(.x = 6,  .y = 4)
    ),
    reduce(as.list(1:10), ~ sum(.x, .y)) %>% peel(c(4,3))
  )

  # with named i's
  a <- set_names(as.list(1:10), letters[1:10])

  expect_error(
    reduce(a, ~ sum(.x, .y)) %>% peel(c("c", "h")),
    "only accepts numeric vectors for indexing"
  )

  # using .init

  expect_equal(
    list(.x = 100,
         .y = 1),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% peel(1)
  )

  b <- 100

  expect_equal(
    list(.x = 100,
         .y = 1),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = b) %>% peel(1)
  )

  expect_equal(
    list(list(.x = 103,
              .y = 3),
         list(.x = 101,
              .y = 2)),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% peel(c(3,2))
  )

  expect_equal(
    list(list(.x = 100,
              .y = 1),
         list(.x = 145,
              .y = 10)),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% peel(c(1, 10))
  )

  # with .dir = "backward"
  expect_equal(
    list(.x = 10,
         .y = 9),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(1)
  )

  expect_equal(
    list(list(.x = 10,
              .y = 9),
         list(.x = 19,
              .y = 8)),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(c(1,2))
  )

  expect_equal(
    list(list(.x = 10,
              .y = 9),
         list(.x = 19,
              .y = 8),
         list(.x = 27,
              .y = 7)),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(1:3)
  )

  expect_equal(
    list(list(.x = 19,
              .y = 8),
         list(.x = 27,
              .y = 7)),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(c(2,3))
  )

  expect_equal(
    list(list(.x = 27,
              .y = 7),
         list(.x = 34,
              .y = 6)),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(c(3,4))
  )


  expect_equal(
    list(list(.x = 40,
              .y = 5),
         list(.x = 27,
              .y = 7)),
    reduce(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(c(5,3))
  )

  # with .init and .dir = "backward"
  expect_equal(
    list(list(.x = 100,
              .y = 10),
         list(.x = 154,
              .y = 1)),
    reduce(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% peel(c(1,10))
  )

  # reduce2
  expect_equal(
    list(list(.x = "1 2 6",
              .y = 3,
              .z = 7),
         list(.x = "1 2 6 3 7 4 8",
              .y = 5,
              .z = 9)),
    reduce2(1:5, 6:9, paste) %>% peel(c(2,4))
  )

  # reduce2 with .init
  expect_equal(
    list(list(.x = "100 1 6",
              .y = 2,
              .z = 7),
         list(.x = "100 1 6 2 7 3 8",
              .y = 4,
              .z = 9)),
    reduce2(1:4, 6:9, paste, .init = 100) %>% peel(c(2,4))
  )

})


# acumulate

test_that("peel works with accumulate and accumulate2", {

  # with one i
  expect_equal(
    list(.x = 15,
         .y = 6),
    accumulate(1:10, ~ sum(.x, .y)) %>% peel(5)
  )

  # with several i
  expect_equal(
    list(list(.x = 1,
              .y = 2),
         list(.x = 45,
              .y = 10)),
    accumulate(as.list(1:10), ~ sum(.x, .y)) %>% peel(c(1,9))
  )

  expect_equal(
    list(list(.x = 6,
              .y = 4),
         list(.x = 15,
              .y = 6)),
    accumulate(as.list(1:10), ~ sum(.x, .y)) %>% peel(c(3,5))
  )

  expect_equal(
    list(list(.x = 15,
              .y = 6),
         list(.x = 6,
              .y = 4)),
    accumulate(as.list(1:10), ~ sum(.x, .y)) %>% peel(c(5,3))
  )

  # with named i's
  a <- set_names(as.list(1:10), letters[1:10])

  expect_error(
    accumulate(a, ~ sum(.x, .y)) %>% peel(c("c", "h")),
    "only accepts numeric vectors for indexing"
  )

  # using .init
  expect_equal(
    list(list(.x = 100,
              .y = 1),
         list(.x = 145,
              .y = 10)),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100) %>% peel(c(1, 10))
  )

  # with .dir = "backward"
  expect_equal(
    list(list(.x = 10,
              .y = 9),
         list(.x = 54,
              .y = 1)),
    accumulate(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(c(1,9))
  )

  expect_equal(
    list(list(.x = 40,
              .y = 5),
         list(.x = 27,
              .y = 7)),
    accumulate(as.list(1:10), ~ sum(.x, .y), .dir = "backward") %>% peel(c(5,3))
  )

  # with .init and .dir = "backward"
  expect_equal(
    list(list(.x = 100,
              .y = 10),
         list(.x = 154,
              .y = 1)),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% peel(c(1,10))
  )

  expect_equal(
    list(list(.x = 140,
              .y = 5),
         list(.x = 127,
              .y = 7)),
    accumulate(as.list(1:10), ~ sum(.x, .y), .init = 100, .dir = "backward") %>% peel(c(6,4))
  )

  # accumulate2
  expect_equal(
    list(list(.x = "1 2 6",
              .y = 3,
              .z = 7),
         list(.x = "1 2 6 3 7 4 8",
              .y = 5,
              .z = 9)),
    accumulate2(1:5, 6:9, paste) %>% peel(c(2,4))
  )

  expect_equal(
    list(list(.x = "1 2 6 3 7",
              .y = 4,
              .z = 8),
         list(.x = "1 2 6",
              .y = 3,
              .z = 7)),
    accumulate2(1:5, 6:9, paste) %>% peel(c(3,2))
  )

  # accumulate2 with .init
  expect_equal(
    list(list(.x = "100 1 6",
              .y = 2,
              .z = 7),
         list(.x = "100 1 6 2 7 3 8",
              .y = 4,
              .z = 9)),
    accumulate2(1:4, 6:9, paste, .init = 100) %>% peel(c(2,4))
  )
})

### TODO: UP UNTIL HERE
# negative tests

# test that apply throws error
test_that("apply family throws error", {

  # with one i
  expect_error(
    lapply(list(1:3, 4:6, 7:9), sum) %>% peel(),
    "doesn't support functions from base R's apply family"
  )
}
)

# test subsetting
test_that("peel throws subsetting errors", {

  expect_error(
    map(list(1:3, 4:6, 7:9), sum) %>% peel(5),
    "One or more locations don't exist"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peel("d"),
    "One or more named elements don't exist"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peel(c(TRUE, FALSE, TRUE)),
    "only accepts numeric or character vectors for subsetting"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peel(list(1, "c")),
    "only accepts numeric or character vectors for subsetting"
  )

  expect_error(
    map(set_names(list(1:3, 4:6, 7:9), letters[1:3]),sum) %>% peel(c(-1, 2)),
    "`i` argument must not contain positive and negative subscripts"
  )
}
)
