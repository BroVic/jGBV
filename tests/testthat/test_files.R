test_that("Project directory tree is established", {
  expect_error(make_dir_tree(),
               "You are not in the 'RAAP_GBV' working directory or repository")
})



"argument \"state\" is missing, with no default"

test_that("Proper type of file naming input as provided when saving RDS", {
  x <- "No-exist"
  expect_error(save_as_rds(1), "invalid filename argument")
  expect_error(save_as_rds(x), "No directory called")
  expect_error(save_as_rds(".", 42, "testdata", state = "Borno"),
               "is_project_state\\(state\\) is not TRUE")
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





test_that("spaces are removed from filenames", {
  output <- .removeSpaceForFilepath("Akwa Ibom")
  expect_match(output, "-")
  expect_false(grepl("\\s", output))
})
