test_that("load_shiny_data: Input validation is correct", {
  expect_error(load_shiny_data(), "Argument 'state' was not supplied with no default")
  expect_error(load_shiny_data("tell"),
               paste(sQuote("tell"), "is not a Nigerian State"),
               fixed = TRUE)
  expect_error(load_shiny_data("Borno"),
               "is_project_state(state) is not TRUE",
               fixed = TRUE)
})

