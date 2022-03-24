op <- options(use.regex = FALSE)


df <- readRDS('testdata/dft-out.rds')
ft <- "flextable"
gg <- "ggplot"


test_that("a table and plot are produced for multiresponse variables", {
  out <- dual_multiopts(df, 2:4)

  expect_s3_class(out$table, ft)
  expect_s3_class(out$plot, gg)

})


test_that("a table and plot are produced for cross-sectional bivariate data", {
  out.s <- dual_singleopts(df, 2)
  expect_s3_class(out.s$table, ft)
  expect_s3_class(out.s$plot, gg)

  out.b <- dual_singleopts(df, 2, "y")
  expect_s3_class(out.s$table, ft)
  expect_s3_class(out.s$plot, gg)

  expect_type(dual_singleopts(df, "a", "y"), 'list')
  expect_error(dual_singleopts(df, TRUE))
  expect_warning(dual_singleopts(df, c("a", "b")))

  obj1 <- suppressWarnings(dual_singleopts(df, c("a", "b")))
  obj2 <- dual_singleopts(df, "a")
  expect_type(obj1, "list")
  expect_equal(obj1, obj2)   # Why are they not identical?


})





test_that("Data are filtered by service types", {
  df <-
    data.frame(
      first = letters[1:3],
      `A/a` = c(1, 0, 1),
      `A/b` = c(0, 1, 0),
      last = LETTERS[1:3],
      check.names = FALSE
    )
  dlogi <- data.frame(
    first = letters[1:3],
    `A/a` = c(T, F, T),
    `A/b` = c(F, T, F),
    last = LETTERS[1:3],
    check.names = FALSE
  )

  expect_true(nrow(filterData(df, srvtype = "b", group.regex = "A/")) == 1)
  expect_true(nrow(filterData(df, srvtype = "a", group.regex = "A/")) == 2)

  expect_true(nrow(filterData(dlogi, srvtype = "b", group.regex = "A/")) == 1)
  expect_true(nrow(filterData(dlogi, srvtype = "a", group.regex = "A/")) == 2)
})


# Cleanup
rm(df)

options(op)
