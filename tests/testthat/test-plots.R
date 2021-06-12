test_that("plots for the report are made", {
  file <-"testdata/dft-out.rds"
  try(dat <- readRDS(file))
  x <-
    show_output(table_multiopt(dat, indices = 2:4, use.regex = FALSE))

  expect_s3_class(x, "flextable")
})
