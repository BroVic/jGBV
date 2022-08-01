xopts <- options(jgbv.project.states = c("Taraba", "Kebbi", "Niger"),
        jgbv.multiresponse.regex = "bin")

test_that("input is validated for 'import_data'", {
  db <- 'testdata/testdb.db'
  s <- "Services"
  c <- "Capacity"
  tr <- "Taraba"
  mlist <- list(
    list(
      var = "start",
      func = "str_remove",
      nestfunc = "str_which",
      args = list(pattern = ".")
    ),
    list(
      var = "end",
      func = "str_remove",
      nestfunc = "str_which",
      args = list(pattern = ".")
    )
  )
  expect_error(import_data(db, "list", tr, s))
  expect_error(import_data(db, mlist, "Maryland", s))
  expect_error(import_data(db, mlist, "Imo", s))
  expect_error(import_data(db, mlist, tr))
  expect_error(import_data(db, mlist, tr, NA_character_))
})


test_that("binary values are tranformed to logical T/F", {
  sz <- 50L
  dat <-
    data.frame(
      bin = sample(0:1, sz, TRUE),
      logi = sample(c(T, F), sz, TRUE),
      dbl = runif(sz),
      str = sample(letters, sz, TRUE)
    )

  dtrans <- transform_bool_to_logical(dat)

  expect_message(transform_bool_to_logical(dat), "Modifying bin")
  expect_type(dtrans$bin, "logical")
  expect_type(dtrans$logi, "logical")
  expect_type(dtrans$dbl, "double")
  expect_type(dtrans$str, "character")

})

options(xopts)
