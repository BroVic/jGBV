 test_that("multi-response table is created successfully", {
   dat <- readRDS("testdata/dft-out.rds")
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



test_that("Labels of multi-response questions are stripped of prefixes", {
   expect_identical(.abridgeOptions("Type of Services/Option_1"), "Option_1")
   expect_identical(.abridgeOptions("Type of Services / Option_2"), "Option_2")
})
