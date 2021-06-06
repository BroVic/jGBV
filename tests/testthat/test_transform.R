library(naijR)
library(raampGBV)

age.range <- c("< 5 yrs", "5-9 yrs", "10-14 yrs", "15-19 yrs", "> 20 yrs")
# bad.range <- c(age.range[-5], "20+ yrs")
range <- 1:5
# opts <- LETTERS[range]
opts <- age.range

no <- nrow(iris)
nums <- rep(NA, no)

for (i in seq_len(no)) {
  v <- sample(range, 1)
  cats <- sample(opts, v)
  nums[i] <- paste0(cats, collapse = "_")
}
testdf <- cbind(iris, nums)
testdf$lgas <- sample(lgas("Kogi"), no, replace = TRUE)

test_that("transformed column with relevant inputs is created", {
  df <- prepare_extended_col(testdf, "nums", opts, multisectoral = FALSE)

  expect_match(as.character(df$name), "20", all = FALSE)
  expect_match(as.character(df$name), "15\\.19", all = FALSE)
  expect_match(as.character(df$name), "10\\.14", all = FALSE)
  expect_match(as.character(df$name), "5\\.9", all = FALSE)
  expect_match(as.character(df$name), "5", all = FALSE)
  expect_match(as.character(df$name), "\\.\\.5", all = FALSE)
  expect_match(as.character(df$name), "\\.\\.20", all = FALSE)
  expect_equal(length(levels(df$name)), 5L)
  expect_equal(length(unique(as.character(df$name))), 5L)
})

