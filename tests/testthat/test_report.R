test_that("Input is validated before building a report", {
  errmsg <- "state %in% raampStates is not TRUE"

  expect_error(build_report(), "argument \"state\" is missing")
  expect_error(build_report(".", "NotAState"), errmsg)
  expect_error(build_report(".", "Borno"), errmsg)
})
