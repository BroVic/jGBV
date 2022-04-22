test_that("Data are prepared for deveoping referra directory Exce sheet", {
  inds <-
    c(21, 23, 24, 25, 57, 58, 59, 61, 62, 26, 117, 141, 142, 143, 144, 145, 146,
      147, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 213, 214, 215,
      216, 217, 218, 219, 235, 236, 237, 238, 239, 259, 260, 261, 292, 293, 294,
      295, 296, 297, 298, 299, 50, 51, 52, 53, 54, 55, 56)

  srv.inds <-
    c(152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 213, 214, 215, 216,
      217, 218, 219, 235, 236, 237, 238, 239, 259, 260, 261, 292, 293, 294, 295,
      296, 297, 298, 299)

  srv.rgx <- "^srvtype_"
  dow.rgx <- "_(mon|tue|wed|thu|fri|sat|sun)$"
  main.nms <-
    list(orgname = "org_name", orgphone = "org_phone", gbvcontact = "fp_contact",
         openaccess = "open_247", lga = "LGA", ward = "ward")

  dat <- readRDS("testdata/srvdata.rds")

  ref.df <-
    prep_ref_directory(dat, inds, srv.inds, srv.rgx, dow.rgx, main.nms)

  expect_length(ref.df, 15L)
  expect_true("num.intervention" %in% names(ref.df))
})
