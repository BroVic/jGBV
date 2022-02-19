test_that("a table and plot are produced for multiresponse variables", {
  op <- options(use.regex = FALSE)

  df <- readRDS('testdata/dft-out.rds')
  out <- dual_multiopts(df, 2:4)

  expect_s3_class(out$table, "flextable")
  expect_s3_class(out$plot, "ggplot")

  options(op)
})
