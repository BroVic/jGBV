 test_that("multi-response table is created successfully", {
   dat <- readRDS(here::here("tests/testthat/testdata/dft-out.rds"))
   ccl <- quote(table_multiopt(dat, indices = 2:4, use.regex = FALSE))
   ft <- eval(ccl)
   ccl$data.only <- TRUE
   df <- eval(ccl)

   expect_s3_class(ft, 'flextable')
   expect_s3_class(df, 'data.frame')
   expect_identical(colnames(df)[1], "Option")
   expect_gt(nrow(df), 0L)
   expect_false(all(df$Frequency == nrow(dat)))
})
