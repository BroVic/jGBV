test_that("input validation for function that picks up RQDA projects", {
  expect_error(get_rqda_projs(),
               "The directory '.+data/qual/rqda' does not exist")
  expect_error(get_rqda_projs("fake.directory"),
               "The directory 'fake.directory' does not exist")
  expect_warning(get_rqda_projs("."),
                 "No RQDA projects were found in '.'")
})



# test_that("input validation when picking up a single RQDA project", {
  testproj <- here::here('tests/testthat/testdata/test.rqda')
  ct <- retrieve_codingtable(testproj)
  expect_error(retrieve_codingtable("nonproject"),
               "'proj' is not an RQDA project")
  expect_type(ct, "list")
  # expect_s3_class(retrieve_codingtable(testproj))
# })
