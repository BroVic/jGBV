str <- quote("c(\"no.space\", \"has. space\")")
v <- extract_vector(str)

test_that("'extract_vector()` validates input", {
  expect_type(v, "character")
  expect_is(v, "character")
  expect_error(extract_vector())
  expect_error(extract_vector(999))
  expect_error(extract_vector(TRUE))
  expect_error(extract_vector(NA))
  expect_error(extract_vector(NA_character_))
})

test_that("extracted vector has no spaces", {
  expect_false(any(grepl("\\s+", v)))
  expect_length(v, 2L)
})
