
cap <- readRDS("testdata/capdata.rds")
hlth <- filterAndSelect(cap, "health")

test_that("capacity data are filtered and appropriate columns selected", {
  expect_true(ncol(cap) > ncol(hlth))
  expect_true(ncol(hlth) == 28L)
  expect_true(nrow(cap) > nrow(hlth))
  expect_s3_class(hlth, 'data.frame')
})



test_that("Plot or tabulation is created", {
  lbls <- c(
    "GBV Training",
    "CMR (Adults)",
    "CMR (Child)",
    "Pyschosocial support",
    "Medico-legal report",
    "Forensics",
    "Privacy",
    "Referral/Follow-up",
    "Documentation",
    "HIV/STI testing",
    "Mgt of children",
    "Court testimony",
    "Contraceptives",
    "Communication"
  )
  p <- makePlot(hlth, lbls, "blue")

  expect_s3_class(p, 'ggplot')
  expect_s3_class(p, 'gg')
  expect_s3_class(makePlot(hlth, lbls, 'blue', table = TRUE), "data.frame")
})


test_that("Main table is generated", {
  expect_s3_class(makeTable(hlth), "flextable")
})
