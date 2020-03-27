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

  cwd <- getwd()
  setwd(tempdir())
  expect_warning(clone_template())
  expect_null(suppressWarnings(clone_template()))
  setwd(cwd)
})

# TODO: Add tests to check that the templates code chunks have the
# appropriate labels. Use regex e.g. (plot|table)_[:alpha:]+\\,

