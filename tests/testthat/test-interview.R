test_that("Regular expression is created", {
  ob <- interview(c("What are the issue(s)?", "foo[bar]{baz}, boy!"))

  expect_length(ob, 2)
  expect_identical(names(ob), c("Q1", "Q2"))
})
