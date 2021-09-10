dat <- readRDS("testdata/dft-out.rds")

test_that("multi-response table is created successfully", {
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


test_that("When digits argument is set, table is successfully created", {
   options(data.on.redcap = FALSE)
   ft <- table_multiopt(dat, indices = 2:4, use.regex = FALSE, digits = 1)

   expect_s3_class(ft, 'flextable')
})

test_that("Labels of multi-response questions are stripped of prefixes", {
   opts <-
      c(
         "Type Of Services Provided/Option 1",
         "Type Of Services Provided/Option Legal Aid",
         "Type Of Services Provided/Psychosocial Support",
         "Type Of Services Provided/Security Police",
         "Type Of Services Provided/Temporary Accommodation   Refuge",
         "Type Of Services Provided/Economic Empowerment Livelihoods"
      )
   nn.out <-
      c(
         'Option 1',
         'Option Legal Aid',
         'Psychosocial Support',
         'Security Police',
         'Temporary Accommodation Refuge',
         'Economic Empowerment Livelihoods'
      )
   expect_identical(.abridgeOptions("Type of Services/Option_1"), "Option_1")
   expect_identical(.abridgeOptions("Type of Services / Option_2"), "Option_2")
   expect_identical(.abridgeOptions(opts, FALSE), nn.out)

})


test_that("Labels of a data frame are retrieved", {
   dd <- readRDS("testdata/dft-out.rds")

   expect_identical(get_var_labels(dd), c("X", "A", "B", "C", "Y"))
   expect_identical(get_var_labels(dd, 1), "X")
   expect_identical(get_var_labels(dd, 5), "Y")
   expect_identical(get_var_labels(dd, 3), "B")
   expect_identical(get_var_labels(dd, 3:4), c("B", "C"))
   expect_error(get_var_labels(pi), "'data' should be of class data.frame")
   expect_error(get_var_labels(dd, letters[1:2]),
                "'ind' should be a numeric vector")
   expect_error(get_var_labels(dd, 5:6),
                "Out-of-bounds or missing index in 'ind'")
})
