 test_that("multi-response table is created successfully", {
   res <- c(1, 0)
   num <- 50L
   fx <- quote(sample(res, num, replace = TRUE))
   dat <-
     data.frame(
       x  = rep(NA, num),
       a = eval(fx),
       b = eval(fx),
       c = eval(fx),
       y = runif(num)
     )

   ccl <- quote(table_multiopt(dat, indices = 2:4, use.regex = FALSE))
   ft <- eval(ccl)
   ccl$data.only <- TRUE
   df <- eval(ccl)

   expect_s3_class(ft, 'flextable')
   expect_s3_class(df, 'data.frame')
})
