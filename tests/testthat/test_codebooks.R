library(raampGBV)

test_that("input can be validated", {
  testOutput <- .getCodebookFileName("Abia", "Health")

  expect_error(.getCodebookFileName("Borno", "Health"))
  expect_error(.getCodebookFileName("Abia", "NotSector"))
  expect_identical(testOutput, "codebook_Abia_Health.html")
  expect_false(identical(testOutput, "codebook_Health_Abia.html"))
})

test_that("States with hyphens in names will not be processed", {
  expect_error(build_codebook("Akwa-Ibom", "Health"),
               paste(sQuote("Akwa-Ibom"), "is not a Nigerian State"),
               fixed = TRUE)
})
