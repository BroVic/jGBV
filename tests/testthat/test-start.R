xopts <- options(
  jgbv.project.states = c("Taraba", "Kebbi", "Niger"),
  jgbv.multiresponse.regex = "bin",
  jgbv.excelfile.regex = c(Services = "services_", Capacity = "capacity_"),
  jgbv.new.varnames = c(
    sn = "serialnum",
    age = "agegroup",
    fake1 = "fake_1",
    rem1 = "rem_1",
    alc = "alcohol_grp",
    fake2 = "fake_2",
    tob = "tobacco_grp",
    rem2 = "rem_2",
    n = "num_cases",
    nc = "num_controls",
    rem3 = "rem3"
  )
)

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



d <- "testdata"
s <- "Taraba"
t <- "Services"
outvars <- c(3, 6)

test_that("Unwanted variables are made missing when reading data", {
  expect_error(suppressWarnings(read_in_excel_data(d, s, t)))
  expect_warning(try(read_in_excel_data(d, s, t), silent = TRUE))
  expect_silent(read_in_excel_data(d, s, t, drop.v = outvars))
})


test_that("Excel files are read and labelled", {
  res <- read_in_excel_data(d, s, t, drop.v = outvars)

  expect_s3_class(res, "data.frame")
  expect_equal(attr(res$alcohol_grp, "label"), "alcgp")
})


test_that("missing values are accurately represented", {
  df <-
    read_in_excel_data(d, s, t, drop.v = outvars, na.strings = c("", "n/a"))
})




options(xopts)
