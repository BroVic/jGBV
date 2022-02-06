# Reporting Templates
test_that("templates are created", {
  expect_error(open_template("foobar"))
  expect_error(open_template(type = "foobar"))
})
