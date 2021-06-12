library(here)

test_that("Odd and even values are properly set", {
  ten <- 1:10
  od <- odd_even_seq(ten, "odd")
  ev <- odd_even_seq(ten, "even")

  expect_false(all(od %in% ev))
  expect_false(any(od %in% ev))
  expect_equal(od[1], 1L)
  expect_equal(od[2], 3L)
  expect_equal(od[3], 5L)
  expect_equal(od[4], 7L)
  expect_equal(od[5], 9L)
  expect_equal(ev[1], 2L)
  expect_equal(ev[2], 4L)
  expect_equal(ev[3], 6L)
  expect_equal(ev[4], 8L)
  expect_equal(ev[5], 10L)
  expect_length(od, length(ten) / 2)
})




test_that("variable is appropriately labelled", {
  word_word <-
    sample(
      c("a", "c", "e", "g", "i", "k", "m", "o", "q", "s", "u", "w", "y"),
      size = 10,
      replace = TRUE
    )
  num_word <- sample(c(0, 1), 10, replace = TRUE)
  val.lab <- paste(c(0, 1), c("Yes", "No"), collapse = " ")
  x <- suppressWarnings(
    create_value_label_pairs(word_word, paste(letters, collapse = " "))
    )
  y <- suppressWarnings(
    create_value_label_pairs(num_word, val.lab)
    )

  ## Tests
  expect_true(is.atomic(x))
  expect_is(x, "haven_labelled")
  expect_is(y, "haven_labelled")
})

test_that("'create_value_label_pairs' works with labelled vectors", {
  labl.letters5 <- letters5 <- sample(1:5, 15, replace = TRUE)
  Hmisc::label(labl.letters5) <- "Lower case English 5"
  val_labs <- paste(1:5, letters[1:5], collapse = " ")

  expect_warning(
    create_value_label_pairs(letters5, val_labs),
    "Expected an object of class 'labelled'"
  )
})

test_that("A named vector is returned", {
  v <- create_named_vector(paste(letters, collapse = " "))

  expect_vector(v)
  expect_length(v, length(letters) / 2)
  expect_false(is.null(names(v)))
  expect_error(create_named_vector(letters),
               "Object of length < 2 cannot produce a label-value pair")
  errmsg <- "is.character\\(vec.list\\) is not TRUE"
  expect_error(create_named_vector(data.frame(letters)), errmsg)
  expect_error(create_named_vector(list(paste(letters, LETTERS))), errmsg)
})



test_that("knitr_kable object is created", {
  kable_class <- "knitr_kable"
  ReferralStr <- "Referral"
  colLabPattern <- "\\(N\\/%\\)"
  foo <- c("Black", ReferralStr, "Sun")
  tool.sectors <-
    c("Health", "Social", "Judicial", "Security", "Temporary", "Legal", "Referral")
  k <- make_kable(letters[1:5], foo)
  k2 <- make_kable(tool.sectors, LETTERS[1:3])

  expect_match(tool.sectors, ReferralStr, all = FALSE)
  expect_match(k, ReferralStr, all = FALSE)
  expect_true(any(grepl(ReferralStr, k2)))
  expect_is(k, kable_class)
  expect_is(k2, kable_class)
  expect_false(any(grepl(colLabPattern, k2)))
})



vec <- readRDS(here("tests/testthat", "testdata/samplecol.rds"))
test_that("labelled column can be converted to a factor", {
  f <- transform_to_factor(vec)
  sngl <- readRDS(here("tests/testthat", "testdata/singleval.rds"))

  expect_is(f, "factor")
  expect_type(f, "integer")
  expect_is(transform_to_factor(sngl), "factor")
})

test_that("not_multichoice detects appropriate variables", {
  chkd <- readRDS(here("tests/testthat", "testdata/checked.rds"))

  expect_true(not_multichoice(vec))
  expect_false(not_multichoice(chkd))
})

test_that("A single list colum is unnested", {
  dtest <- readRDS(here("tests/testthat", "testdata/list_col_test.rds"))

  dOut <- extend_single_listcol(dtest, new1, "Test")
  dOut2 <- extend_single_listcol(dtest, new1, c("Test1", "Test2"))
  expect_is(dOut, "data.frame")
  expect_is(dOut, "tbl")
  expect_equal(ncol(dOut), 3L)
  expect_type(dOut$Test, "logical")

})

test_that("Columns are transformed", {
  dat <- readRDS(here("tests/testthat", "testdata/pickout.rds"))
  colCheckObj <- readRDS(here("tests/testthat", "testdata/colCheckObj.rds"))

  newdat <- pickout_cols_and_transform(dat, colCheckObj)

  expect_lt(ncol(newdat), ncol(dat))

})


test_that("`compute_percent()` returns correct values", {
  expect_type(compute_percent(.05666), "double")
  expect_equal(compute_percent(.042223), 4.22)
  expect_equal(compute_percent(0), 0)
  expect_equal(compute_percent(1), 100)
  expect_false(identical(compute_percent(1), 100L))
})

test_that("Input is validated for 'compute_percent'", {
  expect_error(compute_percent(TRUE))
  expect_error(compute_percent("a"),
               "is.numeric\\(num\\) is not TRUE")
})



test_that("RAAMP States can be checked", {
  nochrErr <- "is.character\\(str\\) is not TRUE"
  nostateErr <- "is not a Nigerian State"

  expect_false(is_project_state("Borno"))
  expect_error(is_project_state("NoExist"), nostateErr)
  expect_error(is_project_state(NULL), nochrErr)
  expect_error(is_project_state(999), nochrErr)
  expect_error(is_project_state(NA), nochrErr)
  expect_error(is_project_state(TRUE), nochrErr)
  expect_error(is_project_state(FALSE), nochrErr)
  expect_true(is_project_state("Abia"))
  expect_false(suppressWarnings(is_project_state("abia")))
  expect_warning(is_project_state("abia"),
                 "Possible typo. Did you mean to type")

})




test_that("show_codebook() validates input", {
  statesErr <- "is.character\\(state\\) is not TRUE"

  expect_error(show_codebook(),
               "argument \"state\" is missing, with no default")
  expect_error(show_codebook(999), statesErr)
  expect_error(show_codebook(NA), statesErr)
  expect_error(show_codebook("Borno", "Health"),
               "is_project_state\\(state\\) is not TRUE")
  expect_error(show_codebook("Abia", "NonSector"),
               "sector %in% tool.sectors is not TRUE")
})




test_that("templates can be retrieved", {
  errmsg <- "is.character\\(dir\\) is not TRUE"
  errmsg2 <- "Codebook template not found"
  expect_error(.retrieveDocumentTemplate(999), errmsg)
  expect_error(.retrieveDocumentTemplate(NULL), errmsg)
  expect_error(.retrieveDocumentTemplate(NA), errmsg)
  expect_error(.retrieveDocumentTemplate(NA_character_), errmsg2)
  expect_error(.retrieveDocumentTemplate(TRUE), errmsg)
  expect_error(.retrieveDocumentTemplate("fake-directory"), errmsg2)
})
