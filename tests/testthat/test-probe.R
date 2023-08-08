
# probe() works without .f

test_that("without .f: works with map & friends", {

  expect_equal(
    c(1:3),
    map(list(1:3, 4:6, 7:9)) %>% probe()
    )

  expect_equal(
    list(.x = 1,
         .y = 2),
    reduce(1:10) %>% probe()
  )

  expect_equal(
    list(.x = 100,
         .y = 1),
    accumulate(1:10, .init = 100) %>% probe()
  )

})

# probe() works with errors

test_that("with error: works with map", {

  a <- list(1:3, letters[4:6], 7:9)

  expect_equal(
    letters[4:6],
    map(a, sum) %>% probe()
    )

  b <- flatten(list(c(1:5), "a", 6:10))

  expect_equal(
    list(.x = 15,
         .y = "a"),
    accumulate(b, sum) %>% probe()
  )

  expect_equal(
    list(.x = 15,
         .y = "a"),
    reduce(b, sum) %>% probe()
  )

  expect_equal(
    list(.x = 115,
         .y = "a"),
    accumulate(b, sum, .init = 100) %>% probe()
  )

  expect_equal(
    list(.x = 115,
         .y = "a"),
    reduce(b, sum, .init = 100) %>% probe()
  )

  expect_equal(
    list(.x = 140,
         .y = "a"),
    accumulate(b, sum, .init = 100, .dir = "backward") %>% probe()
  )

  expect_equal(
    list(.x = 140,
         .y = "a"),
    reduce(b, sum, .init = 100, .dir = "backward") %>% probe()
  )

})


# probe() works when no error

test_that("without error: works with map", {

  expect_equal(
    6,
    map(list(1:3, 4:6, 7:9), sum) %>% probe()
    )

  expect_equal(
    3,
    reduce(1:10, sum) %>% probe()
  )

  expect_equal(
    101,
    reduce(1:10, sum, .init = 100) %>% probe()
  )
})

test_that("without error and stop_at: works with map", {

  expect_equal(
    7:9,
    map(list(1:3, 4:6, 7:9), sum) %>% probe(stop_at = \(x) x > 20)
  )

  expect_equal(
    list(.x = 15,
         .y = 6),
    reduce(1:10, sum) %>% probe(stop_at = \(x) x > 20)
  )

  expect_equal(
    list(.x = 103,
         .y = 3),
    reduce(1:10, sum, .init = 100) %>% probe(stop_at = \(x) x > 105)
  )
})
