test_that("outputs for commencement dates are generated", {
  df <- readRDS("testdata/commence.rds")

  res <- outputs_commencement(df, "opened", "gbvstart")

  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$plot, "ggplot")
})
