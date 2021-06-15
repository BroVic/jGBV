# Create an example data frame for use in testing writing
# tables and plots

res <- c(1, 0)
num <- 482L
fx <- quote(sample(res, num, replace = TRUE))
set.seed(15)
dat <-
  data.frame(
    x  = rep(NA, num),
    a = eval(fx),
    b = eval(fx),
    c = eval(fx),
    y = runif(num)
  )
dat <-
  labelled::set_variable_labels(dat, .labels = c("X", "A", "B", "C", "Y"))
saveRDS(dat, here::here("tests/testthat/testdata/dft-out.rds"))
