dbf <- "testdata/testdb.db"

test_that("data can be read from the database", {
  expect_s3_class(read_from_db(dbf, "mtcars"), "data.frame")

  expect_error(read_from_db("fake.db", "mtcars"))
  expect_error(read_from_db(dbf, "nontable"))
  expect_warning(read_from_db(dbf, c("mtcars", "mtcars2")))
  expect_error(read_from_db(dbf, 999))
})


test_that("Project-specific data are read from the database", {

  expect_error(load_data(dbf, "Taraba"))

  # expect_s3_class(db, "data.frame")
  # expect_s3_class(db[[1]], "labelled")
})


test_that("Date objects are made where appropriate", {
  dtstr1 <- "1950-06-11"
  dtstr2 <- "2022-08-10 01:16:36"
  expect_identical(as.character(make_date("18425")), dtstr1)
  expect_identical(as.character(make_date(18425)), dtstr1)
  expect_identical(as.character(make_date('1660090596')), dtstr2)
  expect_identical(as.character(make_date(1660090596)), dtstr2)
  expect_s3_class(make_date("2022-01-01"), "Date")
})
