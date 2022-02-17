
test_that("plots for the report are made", {
  op <- options(data.on.redcap = FALSE, use.regex = FALSE)

  file <-"testdata/dft-out.rds"
  data <- readRDS(file)

  x <- show_output(data, index = 2:4)

  expect_s3_class(x, "ggplot")

  options(op)
})
