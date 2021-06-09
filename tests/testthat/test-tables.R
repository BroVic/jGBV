 test_that("multi-response table is created successfully", {
   res <- c(1, 0)
   num <- 482L
   fx <- quote(sample(res, num, replace = TRUE))
   set.seed(15)
   dat <-
     data.frame(
       x  = rep(NA, num),
       a = eval(fx),
       b = eval(fx),
       c = eval(fx),
       y = runif(num)
     )
   dat <-
      labelled::set_variable_labels(dat, .labels = c("X", "A", "B", "C", "Y"))
   ccl <- quote(table_multiopt(dat, indices = 2:4, use.regex = FALSE))
   ft <- eval(ccl)
   ccl$data.only <- TRUE
   df <- eval(ccl)

   expect_s3_class(ft, 'flextable')
   expect_s3_class(df, 'data.frame')
   expect_identical(colnames(df)[1], "Option")
   expect_gt(nrow(df), 0L)
   expect_false(all(df$Frequency == num))
})
