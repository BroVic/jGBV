test_that("Input is validated before building a report", {
  errmsg <- "state %in% raampStates is not TRUE"

  expect_error(build_report(), "argument \"state\" is missing")
  expect_error(build_report(".", "NotAState"), errmsg)
  expect_error(build_report(".", "Borno"), errmsg)
  expect_error(build_report(".", "Akwa-Ibom"), errmsg)
})


test_that("check input for template cloning", {
  expect_error(clone_template(999))
  expect_error(clone_template("some string"))
  expect_error(clone_template("Borno"))

  curwd <- getwd()
  setwd(tempdir())
  expect_warning(clone_template())
  expect_null(clone_template())
  setwd(curwd)
})
