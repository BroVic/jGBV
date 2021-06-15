test_that("plots for the report are made", {
  file <-"testdata/dft-out.rds"
  try(data <- readRDS(file))
  x <- show_output(data, index = 2:4)

  expect_s3_class(x, "ggplot")
})
