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
