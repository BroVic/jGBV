test_that("Objects have been properly created", {
  expect_identical(raampStates, c("Abia", "Akwa Ibom", "Ogun", "Bauchi"))
  expect_type(tool.sectors, "character")
  expect_is(tool.sectors, "Tools")
  expect_is(print(tool.sectors), "character")
})
