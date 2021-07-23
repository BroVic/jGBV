# test-strings.R

test_that("Underscores are replaced with spaces and text is made title case", {
  expect_identical(spaceAndTitle("a_boy"), "A Boy")
  expect_identical(spaceAndTitle("god_is_good"), "God Is Good")
  expect_error(spaceAndTitle(999))
  expect_error(spaceAndTitle(NULL))
  expect_error(spaceAndTitle(TRUE))
  expect_false(identical(spaceAndTitle("hold_the_line_"), "Hold The Line "))
})
