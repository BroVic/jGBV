# ---- setup ----
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, error = TRUE, message = FALSE)

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jGBV)

# Get the index of the column for 'Type of Service' and that of the first
# column of the changeable portion of the respective tables
tableBookmarks <- function(data) {
  stopifnot(is.data.frame(data))
  cols <- names(data)
  c(type.index = grep("type", cols, ignore.case = TRUE)[1])
}

# Draws the plot for a given service category; thus, 'data' is
# a prefiltered data frame
makePlot <- function(data, labs, colour, annot = waiver()) {
  ## last column of the fixed part of the table
  fixed <- tableBookmarks(data)["type.index"]
  d <- data %>%
    select(-c(seq_len(fixed))) %>%
    select(seq_len(length(.) - 3)) %>%
    mutate(across(everything(), ~ ifelse(.x == "-", NA_character_, .x))) %>%
    pivot_longer(everything()) %>%
    mutate(value = ifelse(value == "No", 0L, 1L))


  ii <- getTrainingsIndex(data)
  d %>%
    mutate(name = factor(name)) %>%
    filter(value != 0) %>%
    mutate(name = forcats::fct_infreq(name)) %>%
    group_by(name) %>%
    summarise(n = n()) %>%
    mutate(perc = n / nrow(data) * 100) %>%
    ggplot(aes(name, perc)) +
    geom_col(fill = colour) +
    labs(
      x = "Trainings Done",
      y = sprintf("Respondents (N = %d)", nrow(data)),
      caption = annot
    ) +
    scale_x_discrete(breaks = names(data)[ii], labels = labs) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    coord_flip() +
    ggthemes::theme_gdocs() +
    theme(axis.title = element_text(face = "bold.italic"),
          axis.title.x = element_text(vjust = 0),
          axis.title.y = element_text(vjust = 1),
          plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
          plot.caption = element_text(face = 'italic', size = 7))
}

makeTable <- function(service, caption = NULL) {
  stopifnot(is.character(service))
  require(magrittr)
  require(flextable)
  # 'allcap' is in the parent environment
  dt <- allcap[[service]]
  train.cols <- getTrainingsIndex(dt)
  scales <-
    scales::col_factor(palette = c("darkgreen", "red"),
                       levels = c("Yes", "No"))
  dt %>%
    select(-10) %>%   # remove the 'type of service' column
    flextable %>%
    add_header_row(
      values = c("Facility Info", "Areas of Training", "Coordination"),
      colwidths = c(tableBookmarks(dt) - 1L, length(train.cols), 3L)
    ) %>%
    theme_box %>%
    set_caption(caption) %>%
    align(1L, align = 'center', part = 'header') %>%
    bold(1L, part = 'header') %>%
    fontsize(size = 7, part= "all") %>%
    color(color = scales, j = train.cols - 1L) %>% # needed to shift indices by 1
    bold(j = train.cols) %>%
    set_formatter_type
}

getTrainingsIndex <- function(dt) {
  stopifnot(is.data.frame(dt))
  sentinel <- tableBookmarks(dt)
  len <- ncol(dt) - (sentinel + 3L)
  seq(sentinel + 1L, length.out = len)
}


filterAndSelect <- function(df, service) {
  stopifnot(is.data.frame(df), is.character(service))
  require(stringr)
  cols <- names(df)
  t <- tableBookmarks(df)['type.index']
  df <- df %>%
    filter(grepl(service, .data[[cols[[t]]]], ignore.case = TRUE)) %>%
    select(!where( ~ all(.x == "-")))   # NAs were earlier converted to '-'

  # Carry out this step here because when the entire complement of
  # columns are available, transformation is impossible due to
  # the duplication of column names.
  colnames(df) %<>%    # magrittr pipe assignment
    {
      .[[1]] <- "LGA"
      .[[2]] <- "Name of Facility"
      .[[4]] <- "GBV Focal Person?"
      .[[5]] <- "Designation"
      .[[6]] <- "Age Range"
      .[[8]] <- "Qualifications"
      .[[9]] <- "Phone"
      .[[10]] <- "Type of Organization"
      .
    } %>%
    str_remove("\\.+\\d{1,2}$") %>%
    str_remove("^Q\\d\\.") %>%
    str_remove("^Responding/") %>%
    str_remove("approach$") %>%
    str_replace("Sex/Gender", "Gender") %>%
    str_trim %>%
    str_squish
  df
}


statGBV <- function(df, stat = c("sum", "perc", 'total'), var = character()) {
  stopifnot(is.data.frame(df))
  stat <- match.arg(stat)
  i <- if (is.character(var)) {
    var <- match.arg(var, c('gbv', 'gender'))
    grep(var, names(df), ignore.case = TRUE)[1]
  } else
    var
  stopifnot(is.numeric(i))
  if (length(i) == 0L)
    stop("grep: No index found", call. = TRUE)
  col <- df[[i]]
  v <- tolower(col) == "yes"
  if (var == 'gender')
    v <- col == "Female"

  switch(stat,
         sum = sum(v),
         perc = paste0(round(mean(v) * 100, 1), '%'),
         total = nrow(df))
}

fixOffset <- function(x, ...)
  UseMethod("fixOffset")

fixOffset.numeric <- function(x) {
  tableBookmarks(capdata)['type.index'] + x
}

fixOffset.character <- function(x, df) {
  stopifnot(is.data.frame(df))
  grep(x, names(df), ignore.case = TRUE)
}

fixOffset.default <- function(x) "Unsupported type"

serviceAreas <- getOption("jgb.service.areas")

excfilepath <- here("out", generate_filename(params$state, "capneed"))

if (!file.exists(excfilepath))
  stop(sprintf("The Excel file %s does not exist", excfilepath))
capdata <- readxl::read_xlsx(excfilepath, sheet = SheetName("cap"))

allcap <- sapply(
  c("health", "law", "legal", "psycho", "shelter"),
  \(x) filterAndSelect(capdata, x),
  USE.NAMES = TRUE
)


# ---- health-chart ----

lbl <- c(
  "GBV Training",
  "CMR (Adults)",
  "CMR (Child)",
  "Pyschosocial support",
  "Medico-legal report",
  "Forensics",
  "Privacy",
  "Referral/Follow-up",
  "Documentation",
  "HIV/STI testing",
  "Mgt of children",
  "Court testimony",
  "Contraceptives",
  "Communication"
)
makePlot(
  allcap$health, lbl, 'orange', annot = '*CMR - Clinial management of rape'
)

# ---- lawenf-chart ----
lbl <-
  c(
    "Psychosocial support",
    "Medico-legal report",
    "Forensics",
    "Privacy",
    "Court testimony",
    "Safety planning",
    "Communication"
  )
makePlot(allcap$law, lbl, "blue")

# ---- legal-chart ----
lbl <-
  c(
    "GBV training",
    "GBV laws/policies",
    "Psychosocial support",
    "Forensics",
    "Referral/Follow-up",
    "Documentation",
    "Court testimony",
    "Safety planning",
    "Privacy",
    "Communication"
  )
makePlot(allcap$legal, lbl, "brown")

# ---- psycho-chart ----
lbl <-
  c(
    "Gender Mainstreaming",
    "Prevention &",
    "Survivor-centered",
    "Referral pathways",
    "Principles of care",
    "Case management",
    "Sexual exploitation",
    "Mental health",
    "Communication"
  )
makePlot(allcap$psycho, lbl, "green3")

# ---- shelter-chart ----
lbl <- c(
  "Safety & dignity",
  "Provision of Care",
  "Survivor-centered",
  "Referral pathways",
  "Principles of care",
  "Case management",
  "Preventing SE*",
  "Mental health",
  "Communication"
)
makePlot(allcap$shelter, lbl, "violet", annot = "*SE - Sexual exploitation")

# ---- health-table
makeTable("health", serviceAreas[1])




# ---- lawenf-table ----
makeTable('law', serviceAreas[2])




# ---- legal-table ----
makeTable('legal', serviceAreas[3])




# ---- psycho-table ----
makeTable('psycho', serviceAreas[4])




# ---- shelter-table ----
makeTable('shelter', serviceAreas[5])
