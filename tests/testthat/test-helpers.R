test_that("data can be read from the database", {
  dbf <- "testdata/testdb.db"

  expect_s3_class(read_from_db(dbf, "mtcars"), "data.frame")

  expect_error(read_from_db("fake.db", "mtcars"))
  expect_error(read_from_db(dbf, "nontable"))
  expect_warning(read_from_db(dbf, c("mtcars", "mtcars2")))
  expect_error(read_from_db(dbf, 999))
})
