test_that("input validation for function that picks up RQDA projects", {
  expect_error(get_rqda_projs(),
               "The directory '.+data/qual/rqda' does not exist")
  expect_error(get_rqda_projs("fake.directory"),
               "The directory 'fake.directory' does not exist")
  expect_warning(get_rqda_projs("."),
                 "No RQDA projects were found in '.'")
})
