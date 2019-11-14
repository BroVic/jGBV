library(raampGBV)
library(here)

test_that("Proper type of file naming input as provided when saving RDS", {
  x <- 1
  expect_error(save_as_rds(x),
               "argument \"state\" is missing, with no default")
})



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


test_that("'fetch_all_filepaths_named()' receives correct input", {
  dir <- here("tests/testthat/ex")
  dir.create(dir, showWarnings = FALSE)

  cat("#Dummy script", file = file.path(dir, "file.R"))
  cat("This is a text file", file = file.path(dir, "file.txt"))

  created.r.files <- fetch_all_filepaths_named(dir)
  created.txt.files <- fetch_all_filepaths_named(dir, file.ext = "txt")

  expect_error(fetch_all_filepaths_named(1L),
               "invalid filename argument",
               fixed = TRUE)
  expect_error(fetch_all_filepaths_named(dir, 42L),
               "is.character(file.ext) is not TRUE",
               fixed = TRUE)
  expect_warning(fetch_all_filepaths_named(dir, file.ext = "noext"),
                 "No filepaths returned")
  expect_length(created.r.files, 1L)
  expect_length(created.txt.files, 1L)
  expect_length(suppressWarnings(fetch_all_filepaths_named(dir, "csv")), 0L)
  # ---
  unlink(dir, recursive = TRUE)
})

test_that("Check 'select_file()' fails when more than one path is selected", {
  dir <- here("tests")
  sapply(1:4, function(x) {
    file <- tempfile(pattern = "file",
                     tmpdir = dir,
                     fileext = ".R")
    cat("#File", x, file = file)
  })
  files <- list.files(dir, '\\.R$', full.names = TRUE)

  expect_error(select_file(files, "file"),
               "Expected to select only 1 file, but now 4 were selected")
  expect_error(select_file(c('test.csv', 'test_helpers.R'), "test"),
               "One or more paths do not exist")

  sapply(files, file.remove)
})

test_that("'import_redcap_csv()' collects correct input", {
  test.path <-  here("tests/test.csv")
  write.csv(data.frame(small = letters, large = LETTERS),
            file = test.path)
  dat <-
    import_redcap_csv(dir(dirname(test.path), full.names = TRUE), pattern = 'test')

  expect_error(
    import_redcap_csv(path.list = 1L, pattern = "file"),
    "is.character(path.list) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    import_redcap_csv(path.list = dir("tests"), pattern = 42L),
    "is.character(pattern) is not TRUE",
    fixed = TRUE
  )
  expect_condition(import_redcap_csv(dir(dirname(test.path)), "test"))
  expect_type(dat, "list")
  expect_is(dat, "data.frame")

  file.remove(test.path)
})

test_that("Input for 'save_as_rds()' is checked", {
  object <- pi

  expect_error(save_as_rds(), "argument \"dir\" is missing, with no default")
  expect_error(save_as_rds(state = "Bauchi"),
               "argument \"dir\" is missing, with no default")
  expect_error(save_as_rds(filelabel = "label", state = "Bauchi"),
               "argument \"dir\" is missing, with no default")
})

test_that("list_files_pattern's input is validated", {
  expect_error(list_files_pattern(42L), "argument \"pattern\" is missing, with no default")
  expect_error(list_files_pattern(42L, "pattern"), "invalid 'path' argument")
})

test_that("files are properly located", {
  dir <- here("tests")
  list <- list_files_pattern(dir, "testthat")

  expect_match(list, "that")
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


test_that("Word documents are converted to text files", {
  docfile <- here("tests/testthat", "testdoc/test.docx")

  expect_message(convert_word_to_text(docfile))
  expect_error(convert_word_to_text("not/real/path.docx"),
               "file.exists(docpath) is not TRUE",
               fixed = TRUE)

  txtfile <- here("tests/testthat", "testdoc/test.txt")
  if(file.exists(txtfile))
    file.remove(txtfile)
})

test_that("Only docx files are returned", {
  f <- fetch_only_word_files("testdoc")

  expect_true(all(endsWith(f, ".docx")))
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
