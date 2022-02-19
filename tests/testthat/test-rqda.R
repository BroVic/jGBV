test_that("input validation for function that picks up RQDA projects", {
  expect_error(get_rqda_projs(),
               "The directory '.+data/qual/rqda' does not exist")
  expect_error(get_rqda_projs("fake.directory"),
               "The directory 'fake.directory' does not exist")
  expect_warning(get_rqda_projs("."),
                 "No RQDA projects were found in '.'")
})


#
# test_that("input validation when picking up a single RQDA project", {
#   testproj <- here::here('tests/testthat/testdata/test.rqda')
#   ct <- retrieve_codingtable(testproj)
#   expect_error(retrieve_codingtable("nonproject"),
#                "'proj' is not an RQDA project")
#   expect_type(ct, "list")
#   # expect_s3_class(retrieve_codingtable(testproj))
# })



test_that("create a master-table for all available codings", {
  proj <- "testdata/test.rqda"
  ct <- get_codings_master_df(proj)
  expect_s3_class(ct, 'data.frame')
  expect_length(ct, 10L)
  expect_equal(nrow(ct), 9L)
  expect_identical(
    names(ct),
    c(
      "rowid",
      "cid",
      "fid",
      "codename",
      "filename",
      "index1",
      "index2",
      "CodingLength",
      "codecat",
      "coder"
    )
  )
})


test_that("Input is validated before fetching quotes", {
  tproj <- "testdata/test.rqda"
  cdf <- get_codings_master_df(tproj)
  expect_error(get_quotes(cdf, tproj, 99),
               "'code' must be of type character and length 1")
})
