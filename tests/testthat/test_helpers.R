library(testthat)
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
  x <-
    create_value_label_pairs(word_word, paste(letters, collapse = " "))
  y <- create_value_label_pairs(num_word, val.lab)
  
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
               "is.list\\(vec.list\\) is not TRUE")
  expect_error(create_named_vector(data.frame(letters)),
               "is.character\\(vec.list\\) is not TRUE")
  expect_error(create_named_vector(list(paste(letters, LETTERS))),
               "Expected a vector of length 1L as input")
})




test_that("'fetch_all_files_named' receives correct input", {
  tst <- "tests"
  dir <- "ex"
  f <- "fake"
  dir.create(dir, showWarnings = FALSE)
  rfile <- file.path(dir, "file.R")
  txtfile <- file.path(dir, "file.txt")
  cat("#Dummy script", file = rfile)
  cat("This is a text file", file = txtfile)
  
  testfiles <- fetch_all_files_named(dir)
  created.r.files <- fetch_all_files_named(dir)
  created.txt.files <- fetch_all_files_named(dir, file.ext = "txt")
  
  msg <- "all(sapply(args, is.character)) is not TRUE"
  expect_error(fetch_all_files_named(1L), msg, fixed = TRUE)
  expect_error(fetch_all_files_named(tst, 42L), msg, fixed = TRUE)
  expect_error(fetch_all_files_named(tst, f), "The directory does not exist")
  expect_warning(fetch_all_files_named(tst, file.ext = "noext"),
                 "No filepaths returned")
  
  unlink(dir, recursive = TRUE)
})





test_that("Check 'select_file' fails when more than one path is selected", {
  sapply(1:4, function(x) {
    file <- tempfile(pattern = "file",
                     tmpdir = "tests",
                     fileext = ".txt")
    cat("File", x, file = file)
  })
  files <- fetch_all_files_named("tests", file.ext = 'txt')
  
  expect_error(select_file(files, "file"),
               "length(path) == 1L is not TRUE",
               fixed = TRUE)
  expect_error(select_file(c('test.csv', 'test_helpers.R'), "test"),
               "One or more paths do not exist")
  
  sapply(files, file.remove)
})


test_that("'import_redcap_csv' collects correct input", {
  test.path <-  "tests/test.csv"
  write.csv(data.frame(small = letters, large = LETTERS),
            file = test.path)
  dat <-
    import_redcap_csv(dir("tests", full.names = TRUE), pattern = 'test')
  
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
  expect_condition(import_redcap_csv(dir("tests"), "test"))
  expect_type(dat, "list")
  expect_is(dat, "data.frame")
  
  file.remove(test.path)
})


test_that("Input for 'save_as_rds()' is checked", {
  object <- pi
  
  expect_error(save_as_rds(), "argument \"state\" is missing, with no default")
  expect_error(save_as_rds(state = "Bauchi"),
               "argument \"filelabel\" is missing, with no default")
  expect_error(
    save_as_rds(filelabel = "label", state = "Bauchi"),
    "argument \"obj\" is missing, with no default"
  )
  
})


test_that("list_files_pattern's input is validated", {
  expect_error(list_files_pattern(42L), "argument \"pattern\" is missing, with no default")
  expect_error(list_files_pattern(42L, "pattern"), "invalid 'path' argument")
})

test_that("files are properly located", {
  dir <- here::here("data", "Akwa Ibom")
  list <- list_files_pattern(dir, "metadata")
  
  expect_match(list, "metadata")
})





test_that("knitr_kable object is created", {
  kable_class <- "knitr_kable"
  ReferralStr <- "Referral"
  colLabPattern <- "\\(N\\/%\\)"
  foo <- c("Black", ReferralStr, "Sun")
  k <- make_kable(letters[1:5], foo)
  k2 <- make_kable(tool.sectors, LETTERS[1:3])
  k3 <- make_kable(tool.sectors, LETTERS[1:3], num.percent = FALSE)
  # browser()
  expect_match(tool.sectors, ReferralStr, all = FALSE)
  expect_match(k, ReferralStr, all = FALSE)
  expect_false(any(grepl(ReferralStr, k2)))
  expect_is(k, kable_class)
  expect_is(k2, kable_class)
  expect_true(any(grepl(colLabPattern, k2)))
  expect_false(all(grepl(colLabPattern, k3)))
})








test_that("Zipfile is extracted and list is returned", {
  zipdir <- here("tests", "zipdir")
  dir.create(zipdir)
  numfiles <- 5L
  sapply(seq_len(numfiles), function(x) {
    f <- file.path(zipdir, paste0("Test", x, ".txt"))
    file.create(f)
    cat(paste("Test", x), file = f)
  })
  #zipfile <- file.path(, "ziptest.zip")
  files <- list.files(zipdir, full.names = TRUE)
  zipfile <- "ziptest.zip"
  zip(zipfile, files = files)
  
  # Tests
  fs <- extract_zipfile(zipfile)
  
  expect_is(fs, "data.frame")
  expect_equal(nrow(fs), numfiles)
  
  file.remove(zipfile)
  unlink(zipdir, recursive = TRUE, force = TRUE)
})





convert_word_to_text <- function(docpath) {
  require(stringr)
  message("Converting ", basename(docpath))
  txtpath <- docpath %>%
    str_replace("(.+)(\\.docx|\\.doc)$", "\\1\\.txt")
  tryCatch({
    opts <- c("-f", "docx", "-t", "markdown", "-o")
    system2(command = "pandoc", 
            args = c(shQuote(docpath), opts, shQuote(txtpath)))
  },
  error = function(e) {
    e
    readtextObj <- readtext::readtext(docpath)
    cat(readtextObj$text, file = txtpath)
  },
  warning = function(w) w)
  cat("Done\n")
}


test_that("Word documents are converted to text files", {
  docfile <- "testdoc/test.docx"
  
  expect_message(convert_word_to_text(docfile))
  expect_error(convert_word_to_text("not/real/path.docx"),
               "file.exists(docpath) is not TRUE",
               fixed = TRUE)
  
  txtfile <- "testdoc/test.txt"
  if(file.exists(txtfile))
    file.remove(txtfile)
})





test_that("Only docx files are returned", {
  f <- fetch_only_word_files("testdoc")
  
  expect_true(all(endsWith(f, ".docx")))
})



# Tests linked to approaches for collaborating on QDA coding changes
toAdd <- c("1Code", "2Code")
withAdded <- add_rqdacodes(toAdd)
test_that("Codes can be added to existing ones", {
  expect_is(withAdded, "LocalCodes")
  expect_gt(length(withAdded), length(toAdd))
})


test_that("All codes are stored and can be viewed", {
  cf <- ".CODES"
  cdt <- "CODES.rds"
  
  expect_error(store_rqdacodes(LETTERS, dir = "."), 
               "inherits(codes, \"LocalCodes\") is not TRUE",
               fixed = TRUE)
  expect_null(store_rqdacodes(withAdded, dir = "."))
  expect_true(file.exists(cf))
  expect_true(file.exists(cdt))
  
  expect_output(view_rqdacodes(dir = "."))
  
  
  file.remove(cf)
  file.remove(cdt)
})




vec <- readRDS("testdata/samplecol.rds")
test_that("labelled column can be converted to a factor", {
  f <- transform_to_factor(vec)
  sngl <- readRDS("testdata/singleval.rds")
  
  expect_is(f, "factor")
  expect_type(f, "integer")
  expect_is(transform_to_factor(sngl), "factor")
})




test_that("not_multichoice detects appropriate variables", {
  chkd <- readRDS("testdata/checked.rds")
  
  expect_true(not_multichoice(vec))
  expect_false(not_multichoice(chkd))
})





test_that("Labels are applied to variable values", {
  
})






test_that("A single list colum is unnested", {
  dtest <- readRDS("testdata/list_col_test.rds")
  
  dOut <- extend_single_listcol(dtest, new1, "Test")
  dOut2 <- extend_single_listcol(dtest, new1, c("Test1", "Test2"))
  expect_is(dOut, "data.frame")
  expect_is(dOut, "tbl")
  expect_equal(ncol(dOut), 3L)
  expect_type(dOut$Test, "logical")
  
})








test_that("Columns are transformed", {
  dat <- readRDS("tests/testdata/pickout.rds")
  colCheckObj <- readRDS("tests/testdata/colCheckObj.rds")
  
  newdat <- pickout_cols_and_transform(dat, colCheckObj)
  
  expect_lt(ncol(newdat), ncol(dat))
  
})





test_that("Questions existing across sectors are found", {
  txt <- "What forms of GBV does this facility|Agency|organization address?"
  result <- question_in_all_sectors(txt, labelList)
  
  expect_is(result, "logical")
  expect_true(all(result))
})





test_that("RQDA coding table is extracted from a zipped archive/folder", {
  notproj <- "collabo/Zipped ZimProject files.zip"
  bad <- tryCatch(
    obtain_rqda_codes(notproj),
    error = function(e) e,
    warning = function(w) w)
  # expect_is(cd, "data.frame")
  expect_is(bad, "simpleWarning")
  expect_is(bad, "warning")
  expect_is(bad, "condition")
  expect_match(bad$message, "Created extraction directory")
  expect_match(bad$message, "was removed")
  expect_type(bad$call, "language")
  expect_error(obtain_rqda_codes(notproj), "RQDA project file not found in")
})






test_that("compute_percent returns correct values", {
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
