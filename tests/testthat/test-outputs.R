op <- options(use.regex = FALSE)


df <- readRDS('testdata/dft-out.rds')
ft <- "flextable"
gg <- "ggplot"


test_that("a table and plot are produced for multiresponse variables", {
  priv.out <- dual_multiopts(df, 2:4)

  expect_s3_class(priv.out$table, ft)
  expect_s3_class(priv.out$plot, gg)

})


test_that("a table and plot are produced for cross-sectional bivariate data",
          {
            out.s <- dual_singleopts(df, 2)
            expect_s3_class(out.s$table, ft)
            expect_s3_class(out.s$plot, gg)

            out.b <- dual_singleopts(df, 2, "y")
            expect_s3_class(out.s$table, ft)
            expect_s3_class(out.s$plot, gg)

            expect_type(dual_singleopts(df, "m_a", "y"), 'list')
            expect_error(dual_singleopts(df, TRUE))
            expect_warning(dual_singleopts(df, c("m_a", "m_b")))

            obj1 <- suppressWarnings(dual_singleopts(df, c("m_a", "m_b")))
            obj2 <- dual_singleopts(df, "m_a")
            expect_type(obj1, "list")
            expect_equal(obj1, obj2)   # Why are they not identical?

            obj3 <- dual_singleopts(df, 'yn', "f1", use.table = T)
            expect_type(obj3, 'list')
            expect_s3_class(obj3$table, 'flextable')
            expect_s3_class(obj3$plot , 'ggplot')
            expect_s3_class(obj3$plot, 'gg')
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

  expect_true(nrow(filterData(
    df, srvtype = "b", group.regex = "A/"
  )) == 1)
  expect_true(nrow(filterData(
    df, srvtype = "a", group.regex = "A/"
  )) == 2)

  expect_true(nrow(filterData(
    dlogi, srvtype = "b", group.regex = "A/"
  )) == 1)
  expect_true(nrow(filterData(
    dlogi, srvtype = "a", group.regex = "A/"
  )) == 2)
})


# Cleanup
rm(df)




test_that("Multiple-choice variable is disaggregated by LGA", {
  num <- 100L
  ops <- quote(sample(0:1, num, TRUE))
  lgasYobe <- naijR::lgas("Yobe")

  dd <- data.frame(
    LGA = sample(lgasYobe, num, TRUE),
    A = eval(ops),
    B = eval(ops),
    C = eval(ops),
    D = eval(ops),
    X = sample(LETTERS[1:5], num, TRUE)
  )

  res <- multiresponse_by_lga(dd, 2:5)

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) <= length(lgasYobe))

  pp <-
    plot_services_by_lga(res, c("apple", "berry", "cabbage", "donut"))
  expect_s3_class(pp, "ggplot")
})




test_that("Bivariate output beyond 'Y/N' are produced", {
  num <- 50L
  ynd <-
    data.frame(private = sample(
                 c("Yes", "No", "Sometimes", "Don't know"),
                 num,
                 replace = TRUE),
               public = sample(c("Yes", "No"), num, replace = TRUE),
               protected = sample(paste0("resp", 1:3), num, TRUE),
               service = sample(
                 c('Health', "Economic", "Police", "Shelter", "Legal", "Social"),
                 num,
                 TRUE
               ))

  priv.out <- dual_singleopts(ynd, 'private', 'service', use.table = TRUE)
  priv.data <- priv.out$table$body$dataset

  expect_length(priv.out, 2L)
  expect_length(priv.data, 6L)

  hdr <- names(priv.data)
  vals <- c("Variable", "Yes", "No", "Don't Know", "Sometimes", "Total")

  for (i in vals)
    expect_true(i %in% hdr)


  pub.out <- dual_singleopts(ynd, 'public', 'service', use.table = TRUE)
  pub.data <- pub.out$table$body$dataset

  expect_length(pub.data, 4L)
  expect_true("Percent" %in% names(pub.data))


  prot.out <- dual_singleopts(ynd, 'protected', 'service', use.table = TRUE)
  prot.data <- prot.out$table$body$dataset
  cnames <- names(prot.data)

  # expect_true("Percent" %in% cnames)
  expect_true("resp1" %in% cnames)
  expect_true("resp3" %in% cnames)
  expect_true("Total" %in% cnames)
})



options(op)
