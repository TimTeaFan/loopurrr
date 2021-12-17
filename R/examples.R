library(purrr)

a <- 1:10

b <- set_names(a, letters[a])



my_paste <- function(x) {
  1+1
  10+10
  100+100
  paste(x)
}

list_rep <- function(x) {

  out <- rep_len(x, 2)
  if (length(out) > 0) {
    names(out) <- paste0(names(x), seq_len(2))
  }
  out
}

debugonce(rewrite_fn)



y <- list(a = 1, b = "a", c =3)
debugonce(as_loop)

myvec <- 1:3

lmap(y,  list_rep) |> as_loop()


y %>%
  lmap(list_rep) |> as_loop()

debugonce(check_magrittr_pipe)

# --- convert: `lmap(y, list_rep)` as loop --- #
out <- vector("list", length = length(y))

for (i in seq_along(y)) {
  out[[i]] <- list_rep(y[i])
  stopifnot(is.list(out[[i]]))
}

out <- flatten(out)
# --- end loop --- #


lmap_at(y, c(1,3), list_rep) |> as_loop()

# --- convert: `lmap_at(y, c(1, 3), list_rep)` as loop --- #
.sel <- seq_along(y) %in% c(1, 3)
out <- vector("list", length = length(y))

for (i in seq_along(y)) {
  out[[i]] <- if (.sel[[i]]) {
    list_rep(y[i])
  } else {
    y[i]
  }

  stopifnot(is.list(out[[i]]))
}

out <- flatten(out)
# --- end loop --- #






x <- list(a = 1:4, b = letters[5:7], c = 8:9, d = letters[10])

debugonce(as_loop)
lmap_if(iris, is.numeric, \(x) list(mean(unlist(x))), .else = \(x) list(as.character(unlist(x)))) |> as_loop()
lmap_at(x, c(1,3), list_rep) |> as_loop()
map_if(iris, is.numeric, \(x) mean(unlist(x)), .else = as.character(unlist(x))) |> as_loop()

# --- convert: `lmap_if(iris, is.numeric, function(x) list(me...` as loop --- #
out <- vector("list", length = length(iris))

for (i in seq_along(iris)) {
  if (!is.numeric(iris[[i]])) {
    out[[i]] <- list(as.character(unlist(iris[i])))
    next
  }
  out[[i]] <- list(mean(unlist(iris[i])))
}
stopifnot(is.list(out[[i]]))

out <- dplyr::as_tibble(flatten(out))
# --- end loop --- #



map_if(iris, is.numeric, mean) |> as_loop()

# --- convert: `map_if(iris, is.numeric, mean)` as loop --- #
.sel <- vector("logical", length = length(iris))
out <- vector("list", length = length(iris))

for (i in seq_along(iris)) {
  if (!is.numeric(iris[[i]])) {
    .sel[i] <- TRUE
    next
  }
  out[[i]] <- mean(iris[[i]])
}

out[.sel] <- iris[.sel]

names(out) <- names(iris)
# --- end loop --- #



lmap_if # HERE ------
# --- convert: `map_if(x, is.numeric, list_rep)` as loop --- #
# no selection vector
out <- vector("list", length = length(x))

for (i in seq_along(x)) {
  if (!is.numeric(x[[i]])) {
    out[[i]] <- x[i] # no next  // same as map_if with else
    # no selection update // same as map_if with else
  } else {
    out[[i]] <- list_rep(x[i])
  }
  stopifnot(is.list(out[[i]]))
}
# no .sel // same as map_if with else
out <- flatten(out)
# --- end loop --- #

debugonce(lmap_if)
lmap_if(x, is.numeric, list_rep, .else = ~ list(a = 1)) |> as_loop()
# --- convert: `lmap_if(x, is.numeric, list_rep, .else = func...` as loop --- #
out <- vector("list", length = length(x))

for (i in seq_along(x)) {
  if (!is.numeric(x[[i]])) {
    out[[i]] <- list(a = 1)
    next
  }
  out[[i]] <- list_rep(x[i])
  stopifnot(is.list(out[[i]]))
}

out <- flatten(out)
# --- end loop --- #

map_if(iris, is.factor, as.character, .else = mean) |> as_loop()

# --- convert: `map_if(iris, is.factor, as.character, .else =...` as loop --- #
out <- vector("list", length = length(iris))

for (i in seq_along(iris)) {
  if (!is.factor(iris[[i]])) {
    out[[i]] <- mean(iris[[i]])
    next
  }
  out[[i]] <- as.character(iris[[i]])
}

names(out) <- names(iris)
# --- end loop --- #



out <- vector("list", length = 3L)
for (i in 1:3) {
  out[[i]] <- 1
  stopifnot(is.list(out[[i]]))
}














debugonce(as_loop)
imap(c(a = 1, b = 2, c = 3), paste) |> as_loop()

# --- convert: `imap(c(a = 1, b = 2, c = 3), paste)` as loop --- #
.inp1 <- c(a = 1, b = 2, c = 3)
.idx <- names(.inp1)
out <- vector("list", length = length(.inp1))

for (i in seq_along(.inp1)) {
  out[[i]] <- paste(.inp1[[i]], .idx[[i]])
}

names(out) <- names(.inp1)
# --- end loop --- #

lmap_if


imap(c, ~ paste(.x, .y))

names(c) <- rep("", length(c))




map(a, paste) |> as_loop()


map(b, sum, na.rm = TRUE) |> as_loop(action = "eval")



test <- as_loop(map(b, sum), action = "eval")


debugonce(as_loop)
debugonce(add_selection)
map_if(iris, ~ is.factor(.x), as.character) |> as_loop()

# --- convert: `map_if(iris, ~is.factor(.x), as.character)` as loop --- #
.sel <- vector("logical", length = length(iris))
out <- vector("list", length = length(iris))

for (i in seq_along(iris)) {
  if (!is.factor(iris[[i]])) {
    .sel[i] <- TRUE
    next
  }
  out[[i]] <- as.character(iris[[i]])
}

out[.sel] <- iris[.sel]


names(out) <- names(iris)
# --- end loop --- #


iwalk(b, ~print(c(.x, .y)))
# --- convert: `iwalk(b, ~print(c(.x, .y)))` as loop --- #
.idx <- names(b)
for (i in seq_along(b)) {
  print(c(b[[i]], .idx[[i]]))
}
# --- end loop --- #





debugonce(add_selection)

myvec <- "Species"

iris |> lmap_at(myvec, ~ if(is.factor(unlist(.x))) list() else .x) |> as_loop(idx = "j")

# --- convert: `lmap_at(iris, myvec, ~if (is.factor(unlist(.x...` as loop --- #
.sel <- names(iris) %in% myvec
out <- vector("list", length = length(iris))

for (j in seq_along(iris)) {
  out[[j]] <- if (.sel[[j]]) {
    if (is.factor(unlist(iris[j]))) list() else iris[j]
  } else {
    iris[j]
  }
  stopifnot(is.list(out[[j]]))
}
out <- dplyr::as_tibble(flatten(out))
# --- end loop --- #




map_dfr(a, ~ c(.x, 1, 2))


map(b, ~ data.frame(a = .x, b = 1, c = 2)) -> test
map(b, ~ c(a = .x, b = 1, c = 2)) -> test

map_dfc(b, ~ c(a = .x, b = 1, c = 2)) |> as_loop()

map_dfr(b, ~ c(.x, 1, 2), .id = "test") |> as_loop()

debugonce(bind_rows_cols)
map_dfr(b, ~ c(a = .x, b = 1, c = 2), .id = "test") |> as_loop()

as_loop(map(a, mean))

as_loop(map(a, ~ mean(sum(.x), sum(1:10))))

debugonce(rewrite_fn)
as_loop(map(a, \(x) mean(sum(x), sum(1:10))))

debugonce(as_loop)
as_loop(map(a, \(x) {
  mean(c(sum(x),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10),
         sum(1:10))
  )
}
)
)


