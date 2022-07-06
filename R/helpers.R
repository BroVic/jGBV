# Source file: helpers.R
#
# MIT License
#
# Copyright (c) 2019 Victor Ordu

globalVariables(c(".", "name", "value", "new"))

# The last name change caused a lot a problems with the code
thisPkg <- function()
{
  "jGBV"
}





#' Creates value-label pairing for a given vector
#'
#' @param variable The data frame variable
#' @param val.lab.obj The object with the label:value pairs
#'
#' @importFrom labelled var_label
#' @importFrom labelled val_labels
#'
#' @export
create_value_label_pairs <- function(variable, val.lab.obj)
{
  stopifnot(is.atomic(variable))
  stopifnot(is.character(val.lab.obj))

  if (!inherits(variable, "labelled"))
    warning("Expected an object of class 'labelled'")

  if (inherits(variable, "integer"))
    class(variable) <- "numeric"  # preserve 'label' attribute vs. as.numeric()

  vals <- create_named_vector(val.lab.obj)

  # Take care of instances where the number of values supplied
  # by the imported script exceeds those in the actual dataset
  cats <- unique(variable)
  if (!identical(length(cats), length(vals))) {
    isPresent <- vals %in% cats
    vals <- vals[isPresent]
  }

  val_labels(variable) <- vals
  variable
}





#' Creates sequences of odd or even value
#'
#' @param vec The vector to be checked
#' @param type Whether to follow an odd or even sequence
#'
#' @export
odd_even_seq <- function(vec, type = c("odd", "even")) {
  stopifnot(is.vector(vec))

  type <- match.arg(type)
  from <- switch(type,
                 odd = 1L,
                 even = 2L)
  seq(from, length(vec), by = 2)
}





#' creates a named vector from single string
#'
#' @param vec.list A vector that contains a list of items for creating
#' a named vector
#'
#' @import stringr
#'
#' @export
create_named_vector <- function(vec.list) {
  stopifnot(is.character(vec.list))

  vec <- vec.list[[1]]
  if (length(vec) > 1L)
    stop("Expected a vector of length 1L as input")

  if (str_detect(vec, "^\\d{1}\\s+")) {
    ## We use this construct in this branch because when we try to split up the
    ## object, some of the would-be labels have multiple words and thus cannot
    ## be implemented by purely splitting on the basis of whitespace.
    vals <- vec %>%
      str_extract_all("\\d+") %>%
      unlist %>%
      as.numeric

    lbls <- vec %>%
      str_replace_all("\\d+\\s", ",") %>%
      str_replace(",", "") %>%
      str_split(" ,") %>%
      unlist %>%
      str_squish
  }
  else if (str_detect(vec, "^\\w")) {
    vectorVersion <- vec %>%
      str_split(" ") %>%
      unlist
    if (length(vectorVersion) < 2)
      stop("Object of length < 2 cannot produce a label-value pair")
    odd.num <- odd_even_seq(vectorVersion, 'odd')
    even.num <- odd_even_seq(vectorVersion, 'even')

    vals <- vectorVersion[odd.num]
    lbls <- vectorVersion[even.num]
  }

  if (!identical(length(vals), length(lbls)))
    stop("Length of values vs labels are unequal")

  names(vals) <- lbls
  vals
}









#' Imports CSV downloaded from REDCap into R session
#'
#' @param path.list A character vector of filepaths
#' @param pattern A regular expression for searching for a given CSV file
#'
#' @importFrom utils read.csv
#'
#' @return A dataframe; \code{stringsAsFactors} is set to \code{FALSE}
#'
#' @export
import_redcap_csv <- function(path.list, pattern) {
  stopifnot(exprs = {
    is.character(path.list)
    is.character(pattern)
  })
  path <- select_file(path.list, pattern, "csv")
  read.csv(path, stringsAsFactors = FALSE)
}








#' Check which columns are multiple choice
#'
#' @param x A column, represented as a vector
#'
#' @import stringr
#' @importFrom labelled val_labels
#' @importFrom stats na.omit
#'
#' @export
not_multichoice <- function(x) {
  x <- names(val_labels(x))
  str_detect(x, "^Checked|Unchecked$", negate = TRUE) %>%
    na.omit %>%
    all
}




#' Create Object Of Class colCheck
#'
#' Constructor for colCheck objects - a class that conveys information on
#' different multiple choice variables
#'
#' @param regex A regular expression used for identifying the columns
#' @param opts TA list of character vectors, each of which are options for the
#' for the given question.
#'
#' @export
# TODO: Write unit tests
colCheck <- function(regex, opts) {
  structure(
    list(regex = regex,
         opts = opts),
    class = "colCheck"
  )
}





#' Unnest a List Column
#'
#' 'Open up list columns containing multiple choice responses by creating
#' a column for each option
#'
#' @param data The data frame
#' @param var The variable to be expanded
#' @param chr A character vector, which is the label of the value and which
#' will also be used to develop a new name for the column
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#'
#' @return A modified data frame
#'
#' @export
extend_single_listcol <- function(data, var, chr) {
 stopifnot(any(grepl(deparse(substitute(var)), colnames(data))))  # TODO: Test!

  var <- enquo(var)
  colname <- chr[1] %>% make.names %>% quo_name

  data %>%
    mutate(
      !!colname := map(!!var, str_detect, chr) %>%
        map_lgl(any)
    )
}







#' Get a variable with it's question
#'
#' @param data The data frame
#' @param ques The question found within the variable label
#'
#' @importFrom labelled var_label
#' @importFrom stringr str_which
#'
#' @export
# TODO: Unit tests and roxygen2 commenting
find_var_with_ques <- function(data, ques) {
  x <- var_label(data)
  str_which(x, ques)
}






#' Get a Question from the Variable name
#'
#' @param data The data frame
#' @param varname The variable name
#'
#' @importFrom labelled var_label
#'
#' @export
find_ques_with_var <- function(data, varname) {
  stopifnot(is.character(varname))
  names <- colnames(data)
  ind <- match(varname, names)
  if (length(ind) > 1L)
    stop("Expected to have unique column name")
  var_label(data)[ind]
}





#' Get Column from a question
#'
#' @param data The data frame
#' @param ques The question found in the variable label
#'
#' @export
column_from_question <- function(data, ques) {
  ind <- find_var_with_ques(data, ques)
  colnames(data)[ind]
}





#' Get Data on Services Offered
#'
#' @param data A data frame
#' @param ques The question (found in the variable label itself)
#' @param options The service options for that given question and tool
#'
#' @export
get_service_data <- function(data, ques, options) {
  col <- column_from_question(data, ques)
  prepare_extended_col(data, col, options, multisectoral = FALSE)
}



#' Check A Question Across Datasets
#'
#' Uses a question, held in the label to identify variables across different
#' data frames.
#'
#' @param data.list A list of data frames
#' @param ques The label containing the actual question
#'
#' @importFrom labelled var_label
#' @importFrom purrr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#'
#' @return A list of variables from each dataset containing the question.
#' @export
find_var_across_datasets <- function(data.list, ques) {
  stopifnot(is.list(data.list), is.character(ques))

  map(data.list, find_var_with_ques, ques = ques) %>%
    map2(data.list, function(ind, df)
    var_label(df)[ind])
}





# view_labels <- function(data, columnames) {
#   var_label(data)[colnames(data)]
# }


#' Convert a number to percentage
#'
#' @param num The number to be converted
#'
#' @export
compute_percent <- function(num) {
  stopifnot(is.numeric(num))
  round(num, 4) * 100
}






#' Test For RAAMP-GBV Project State
#'
#' Checks whether a string represents a RAAMP-GBV project state
#'
#' @importFrom naijR states
#'
#' @param str A character vector of length 1.
#'
#' @return A boolean value \code{TRUE} or \code{FALSE}
#'
#' @export
is_project_state <- function(str)
{
  stopifnot(is.character(str))
  allStates <- naijR::states()
  if (!str %in% allStates) {
    if (str %in% tolower(allStates)) {
      warning(sprintf(
        "Possible typo. Did you mean to type %s?",
        sQuote(tools::toTitleCase(str))
      ))
      return(FALSE)
    }
    else
      stop(sQuote(str), " is not a Nigerian State")
  }
  if (!str %in% raampStates)
    return(FALSE)

  TRUE
}





#' Change Variable Names
#'
#' Modifies the variable names and returns a list with both the old and new
#'
#' @param as_list Whether to present the results as a list. Otherwise as an
#' atomic vector
#'
#' @export
modify_varnames <- function(as_list = FALSE) {
  old <-
    c(
      "start",
      "end",
      "today",
      "username",
      "simserial",
      "deviceid",
      "phonenumber",
      "Preliminary Guidance",
      "GPS location of the organization/facility",
      "_GPS location of the organization/facility_latitude",
      "_GPS location of the organization/facility_longitude",
      "_GPS location of the organization/facility_altitude",
      "_GPS location of the organization/facility_precision",
      "Does the service provider have an office and basic infrastructure to provide services?",
      "Is the service provider reachable by phone?",
      "Given that no answers regarding essential capacities to provide services were YES, it is not necessary to include this organization in the mapping. Please complete the remaining questions, only basic information will be collected",
      "Continue with mapping of this organization",
      "Obtaining Consent",
      "Did the respondent agree with or sign the consent form?",
      "Briefly explain or comment why the respondent does not agree with the consent form",
      "Name of organization / facility:",
      "stateorigin",
      "lgaorigin",
      "Ward",
      "Address of the organization",
      "Telephone number of the organization",
      "Organization email address",
      "Name of interviewer",
      "Contact details of interviewer",
      "List of World Bank Projects",
      "Name of the World Bank Project",
      "Type of organization",
      "Governmental - Please specify ministry or service",
      "Other - Please describe the type of the organization",
      "Opening date of the organization/facility",
      "Date the organization/facility started providing GBV services",
      "Sources of funding(select all that apply)",
      "Sources of funding(select all that apply)/Nigerian Government",
      "Sources of funding(select all that apply)/Foreign Governments",
      "Sources of funding(select all that apply)/International Organizations",
      "Sources of funding(select all that apply)/Private donations",
      "Sources of funding(select all that apply)/Fee for service",
      "Sources of funding(select all that apply)/Other (describe)",
      "Other funding - Please describe the source",
      "Number of full-time staff",
      "Number of part-time staff",
      "Number of female staff",
      "Age groups that the facility serves",
      "Opening days",
      "Opening days/Sunday",
      "Opening days/Monday",
      "Opening days/Tuesday",
      "Opening days/Wednesday",
      "Opening days/Thursday",
      "Opening days/Friday",
      "Opening days/Saturday",
      "Is the facility open and accessible 24 hours a day?",
      "At what time does the establishment open each day?",
      "At what time does the establishment close each day?",
      "Information about the respondent/interviewee",
      "First and last name",
      "Title/role with the organization",
      "Which forms of GBV does this facility address?",
      "Which forms of GBV does this facility address?/Sexual violence (rape, sexual assault)",
      "Which forms of GBV does this facility address?/Physical assault",
      "Which forms of GBV does this facility address?/Intimate partner violence",
      "Which forms of GBV does this facility address?/Violence against children (including physical or sexual abuse of children)",
      "Which forms of GBV does this facility address?/Early marriage",
      "Which forms of GBV does this facility address?/Early child-bearing",
      "Which forms of GBV does this facility address?/Female genital mutilation",
      "Which forms of GBV does this facility address?/Other (please describe)",
      "Other to describe",
      "Ask to see copies of any existing policies, protocols or standard operating procedures related to GBV and take a photo of them, if possible.",
      "Are there policies, protocols or standard operating procedures that your staff follows to provide services to survivors of specific forms of GBV?",
      "Can these be shown?",
      "Photo of the protocols",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :/Sexual violence (rape, sexual assault, etc.)",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :/Intimate partner violence/domestic violence",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :/Early marriage",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :/Early pregnancy",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :/Female genital mutilation",
      "Please select the specific forms of GBV for which there are policies, protocols or standard operating procedures that your staff follows to provide services to survivors, such as :/Other (please describe)",
      "Other (please describe)",
      "If not, what is the process for responding to cases of GBV?",
      "Are there specific policies and protocols for dealing with cases of GBV affecting children?",
      "Are standardized forms used to collect patient information?",
      "How are data processed and stored?",
      "Is all physical data stored in a secure and locked location?",
      "Are all electronic files stored on a password protected computer?",
      "Do survivors have the right to choose their treatment and / or to refuse to be treated?",
      "Are your staff ever required to contact the authorities in the event of GBV?",
      "Please explain why only sometimes",
      "If yes or sometimes, please specify for which types of cases to contact the authorities",
      "If yes or sometimes, please specify the authorities to be contacted",
      "Does the facility have sufficient space to ensure that the survivor's privacy is respected when speaking with survivors and/or providing services?",
      "Does the room where survivors are met prevent them from being seen and heard from outside?",
      "Do you ever ask questions about GBV in the presence of another person?",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?/Staff",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?/The qualification / capacity of the staff",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?/Equipment (to describe)",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?/Space",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?/Other",
      "Is the facility missing anything that is necessary to provide quality care to survivors of GBV?/Nothing, facility is well equipped",
      "Equipment missing",
      "Other - what else is missing",
      "Does the centre serve persons with disabilities?",
      "Do they provide specialised services designed/adapted for persons with disabilities?",
      "Please describe these services",
      "Have all staff signed codes of conduct e.g. professional standards defined by HR?",
      "Is there a copy of the code of conduct present?",
      "Photo of the code of conduct",
      "Do codes of conduct contain clauses on patient confidentiality?",
      "Do codes of conduct contain clauses on non-discrimination and treating all clients regardless of sexual orientation, religion, ethnic group, gender, disability etc. with equal rights, dignity and respect?",
      "Is there a GBV focal person at the facility?",
      "Contact info for the GBV focal person",
      "Does this facility have staff trained in GBV?",
      "Please indicate the number of staff trained in GBV?",
      "Please indicate which staff members have received training.",
      "Please briefly describe the person / entity that provided the training and the duration of the training",
      "Do you have a referral directory?",
      "Please take a photo of the reference directory",
      "Referral to specific services",
      "Refer to Health structures",
      "Refer to Psychosocial Services",
      "Refer to Police or other law enforcement",
      "Refer to Legal aid services",
      "Refer to Shelter / temporary accommodation",
      "Refer to Economic reintegration services",
      "Refer to Other services (to describe)",
      "Please specify which other services",
      "How often is this directory updated?",
      "Who should be contacted if a GBV survivor is referred to your facility?",
      "Does the survivor have the right to choose the services to which he / she wishes to be referred?",
      "Please explain why not",
      "Does anyone from your organization participate in national or regional GBV Subclusters, Working Groups or other coordination mechanisms with other GBV service providers or stakeholders?",
      "please specify which ones",
      "Other comments or observations:",
      "Please select the type of services provided by this organization:",
      "Please select the type of services provided by this organization:/Health",
      "Please select the type of services provided by this organization:/Legal aid",
      "Please select the type of services provided by this organization:/Psychosocial support",
      "Please select the type of services provided by this organization:/Security/Police",
      "Please select the type of services provided by this organization:/Temporary accommodation / refuge",
      "Please select the type of services provided by this organization:/Economic Empowerment/Livelihoods",
      "Please select the type of services provided by this organization:/Other (specify)",
      "Other services (specify)",
      "Types of health facility",
      "Other health facility",
      "What services do you provide to survivors of GBV at this facility?...151",
      "What services do you provide to survivors of GBV at this facility?/Clinical management of rape and sexual assault cases",
      "What services do you provide to survivors of GBV at this facility?/Treatment of acute injuries",
      "What services do you provide to survivors of GBV at this facility?/Post-exposure prophylaxis for HIV",
      "What services do you provide to survivors of GBV at this facility?/Emergency contraception",
      "What services do you provide to survivors of GBV at this facility?/HIV testing, counseling and links to treatment",
      "What services do you provide to survivors of GBV at this facility?/Screening and treatment of STIs",
      "What services do you provide to survivors of GBV at this facility?/Forensic examination (including completion of  medical certificates or other medico-legal forms)",
      "What services do you provide to survivors of GBV at this facility?/Specialized psychotherapy for trauma cases",
      "What services do you provide to survivors of GBV at this facility?/Case management (referral to other structures)...160",
      "What services do you provide to survivors of GBV at this facility?/Basic health needs (i.e. first aid kits, feminine hygiene products)...161",
      "What services do you provide to survivors of GBV at this facility?/Other (please describe)...162",
      "Other services...163",
      "Approximately how many survivors have benefited from these services during the past 6 months?...164",
      "Is your facility able to ensure that post-exposure prophylaxis (PEP) is available and administered within 72 hours of an emergency?",
      "please explain why not...166",
      "Can your facility ensure that emergency contraception is offered within 120 hours of a rape case?",
      "please explain why not...168",
      "The services offered are",
      "Do women and girls need to meet any special conditions to access them?",
      "what is the price of the Clinical management of rape and sexual assault cases service in NGN?",
      "what is the price of the Treatment of acute injuries service in NGN?",
      "what is the price of the Post-exposure prophylaxis for HIV service in NGN?",
      "what is the price of the Emergency contraception service in NGN?",
      "what is the price of the HIV testing, counseling and links to treatment service in NGN?",
      "what is the price of the Screening and treatment of STIs service in NGN?",
      "what is the price of the Forensic examination (including completion of  medical certificates or other medico-legal forms) service in NGN?",
      "what is the price of the Specialized psychotherapy for trauma cases service in NGN?",
      "what is the price of the Case management (referral to other structures) service in NGN?",
      "what is the price of the Basic health needs (i.e. first aid kits, feminine hygiene products) service in NGN?",
      "what is the price of the other service in NGN?...181",
      "Do you have standard health forms?",
      "Which type of standard health forms?",
      "Which type of standard health forms?/Forensic forms / forensic examination forms for rape cases",
      "Which type of standard health forms?/Consent form for examination and treatment of rape",
      "Which type of standard health forms?/Consent form for HIV testing",
      "Which type of standard health forms?/Safety assessment forms",
      "Which type of standard health forms?/Patient admission / assessment forms",
      "Are the following elements available?",
      "Are the following elements available?/Private consultation / examination room",
      "Are the following elements available?/Waiting room (i.e. private or hidden from other rooms / areas)",
      "Are the following elements available?/Lockable cabinet or drawer for temporary storage of forensic evidence",
      "Comments on available items",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/HIV testing kits",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Pregnancy test",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Emergency contraception pills / IUD",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Post-exposure prophylaxis (PEP) for HIV",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Medicines for the treatment of sexually transmitted infections",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Medicines for pain relief, especially paracetamol",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Local anesthetic for sutures",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Broad-spectrum antibiotics for the treatment of wounds",
      "Are the following MEDICINES AND ESSENTIAL SUPPLIES  available?/Tetanus vaccine",
      "Comments on available medicines",
      "Does this facility have staff who are specifically trained and experienced in",
      "Does this facility have staff who are specifically trained and experienced in/Providing medical care to GBV survivors",
      "Does this facility have staff who are specifically trained and experienced in/Clinical management of rape and sexual assault",
      "Does this facility have staff who are specifically trained and experienced in/Psycho-social support, psychological first aid or case management",
      "Does this facility have staff who are specifically trained and experienced in/Other forms of care (please describe)",
      "Other forms of care (please describe)",
      "Please identify qualified staff and briefly describe the nature of their training and experience...211",
      "What services do you provide to survivors of GBV at this facility?...212",
      "What services do you provide to survivors of GBV at this facility?/Legal consultation/counseling on legal rights, processes and potential outcomes",
      "What services do you provide to survivors of GBV at this facility?/Legal representation",
      "What services do you provide to survivors of GBV at this facility?/Accompaniment and support in court",
      "What services do you provide to survivors of GBV at this facility?/Mediation",
      "What services do you provide to survivors of GBV at this facility?/Security/safety planning",
      "What services do you provide to survivors of GBV at this facility?/Basic psychosocial counseling",
      "What services do you provide to survivors of GBV at this facility?/Other (please describe)...219",
      "Other services (please describe)",
      "Approximately how many survivors have benefited from these services during the past 6 months?...221",
      "Is the service free or paid?...222",
      "Are there any special conditions to be met to access your services?...223",
      "What is the price of the Legal consultation/counseling on legal rights, processes and potential outcomes service in NGN?",
      "What is the price of the Legal representation service in NGN?",
      "What is the price of the Accompaniment and support in court service in NGN?",
      "What is the price of the Mediation service in NGN?",
      "What is the price of the Security/safety planning service in NGN?",
      "What is the price of the Basic psychosocial counseling service in NGN?",
      "What is the price of the other service in NGN?",
      "Do you offer funds or other material assistance to survivors and witnesses for their appearance in court (meals, transport, overnight accommodation, etc.).",
      "What happens when there are not enough resources to investigate or follow up on the case?",
      "Other - please specify what happens when there are not enough resources to investigate?",
      "What services do you provide to survivors of GBV at this facility?...234",
      "What services do you provide to survivors of GBV at this facility?/Counseling",
      "What services do you provide to survivors of GBV at this facility?/Case management",
      "What services do you provide to survivors of GBV at this facility?/Psychotherapy for trauma cases",
      "What services do you provide to survivors of GBV at this facility?/Safety planning...238",
      "What services do you provide to survivors of GBV at this facility?/Other (please describe)...239",
      "Other psychosocial services (please describe)",
      "Approximately how many survivors have benefited from these services during the past 6 months?...241",
      "Is the service free or paid?...242",
      "Are there any special conditions to be met to access your services?...243",
      "what is the price of the Counseling service in NGN?",
      "what is the price of the Case management service in NGN?",
      "what is the price of the Psychotherapy for trauma cases service in NGN?",
      "what is the price of the Safety planning service in NGN?",
      "what is the price of the other service in NGN?...248",
      "Does this facility have staff who are trained and experienced in:...249",
      "Does this facility have staff who are trained and experienced in:/Safety planning",
      "Does this facility have staff who are trained and experienced in:/Specific treatment for child survivors of sexual assault",
      "Does this facility have staff who are trained and experienced in:/Counseling",
      "Does this facility have staff who are trained and experienced in:/Specialized psychotherapy for trauma cases",
      "Does this facility have staff who are trained and experienced in:/Case management",
      "Does this facility have staff who are trained and experienced in:/Other services (please specify)",
      "Other services staff are trained in (please specify)",
      "Please identify qualified staff and briefly describe the nature of their training and experience...257",
      "What services do you provide to survivors of GBV at this facility?...258",
      "What services do you provide to survivors of GBV at this facility?/Case investigation",
      "What services do you provide to survivors of GBV at this facility?/Safety/security planning and enforcement",
      "What services do you provide to survivors of GBV at this facility?/Other (please specify)",
      "Other security services (please specify)",
      "Are there specialized GBV police officers and/or units at this facility (e.g. Gender Desk Unit, Family Unit)?",
      "Please state the officers? title, unit and how many are present at the facility",
      "Do you refer survivors to other police locations?",
      "Does this facility have staff who are trained and experienced in:...266",
      "Does this facility have staff who are trained and experienced in:/Working with survivors of GBV (survivor-centered approaches and trauma-informed interview skills)",
      "Does this facility have staff who are trained and experienced in:/Working with children",
      "Does this facility have staff who are trained and experienced in:/Investigation of GBV cases",
      "Please identify qualified staff and briefly describe the nature of their training and experience...270",
      "Now we'll record how many cases were reported in the last 6 months for key GBV types",
      "How many cases were reported in the last 6 months for rape or sexual assault?",
      "How many cases were reported in the last 6 months for IPV?",
      "How many cases were reported in the last 6 months for Child sexual abuse?",
      "How many cases were reported in the last 6 months for FGM?",
      "How many cases were reported in the last 6 months for Other (to specify)?",
      "Please specify for which other GBV types these cases were reported in the last 6 months",
      "Are there any costs for survivors to receive services?",
      "Are there any special conditions to be met to access your services?...279",
      "what is the price of the Case investigation service in NGN?",
      "what is the price of the Safety/security planning and enforcement service in NGN?",
      "what is the price of the other service in NGN?...282",
      "What resources are available for investigation and follow-up?",
      "What resources are available for investigation and follow-up?/Vehicles",
      "What resources are available for investigation and follow-up?/Motorbikes",
      "What resources are available for investigation and follow-up?/Fuel",
      "What resources are available for investigation and follow-up?/Other (please specify)",
      "Other resources (please specify)",
      "Do you make contact with a survivor to ensure their well-being and follow-up?",
      "How is this done in complete security and confidentiality?",
      "What services do you provide to survivors of GBV at this facility?...291",
      "What services do you provide to survivors of GBV at this facility?/Shelter/housing  (Please specify the length of time survivors are allowed to stay)",
      "What services do you provide to survivors of GBV at this facility?/Basic psychosocial support/counseling",
      "What services do you provide to survivors of GBV at this facility?/Safety planning...294",
      "What services do you provide to survivors of GBV at this facility?/Case management (referral to other structures)...295",
      "What services do you provide to survivors of GBV at this facility?/Basic health needs (i.e. first aid kits, feminine hygiene products)...296",
      "What services do you provide to survivors of GBV at this facility?/Other health services (please specify)",
      "What services do you provide to survivors of GBV at this facility?/Other (please describe)...298",
      "Other health services",
      "Other services...300",
      "Are survivors allowed to bring their families?",
      "Are there child friendly spaces in the facility?",
      "What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy?",
      "What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy?/Security walls",
      "What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy?/Security guards",
      "What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy?/Video surveillance",
      "What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy?/Rooms with locks on the door",
      "What precautions are taken to ensure the safety of survivors of GBV and to protect their privacy?/Other precautions (to specify)",
      "Other precautions (please specify):",
      "Does this organization offer the following amenities?",
      "Does this organization offer the following amenities?/Food",
      "Does this organization offer the following amenities?/Clothing",
      "Does this organization offer the following amenities?/Sanitary / Showers",
      "Does this organization offer the following amenities?/Others",
      "Other amenities",
      "Does your establishment have electricity or water?",
      "Approximately how many women have benefited from this shelter during the last six months?",
      "Approximately how many men have benefited from this shelter during the last 6 months?",
      "Does your organization specifically support women and girls who are at risk of GBV?",
      "Do your programs have the capacity to accept new women / girls who are looking for support?",
      "What services are provided?",
      "What services are provided?/Agriculture and livestock programs",
      "What services are provided?/Training and apprenticeship / internship programs",
      "What services are provided?/Income generating activities",
      "What services are provided?/Village savings and loan associations",
      "What services are provided?/Cash or in-kind assistance (funds for school fees, food, etc.)",
      "What services are provided?/Access to land and / or land ownership",
      "What services are provided?/Other services (specify):",
      "Other economic services (specify):",
      "Approximately how many women have benefited from these activities during the last six months?",
      "Do your programs identify safe and hazardous areas for women and girls in the local environment for livelihood activities?",
      "Have you had to turn away women and girls because of the lack of opportunities offered by the programs during the past year?",
      "Please click \"OK\" to confirm you have answered all questions and completed the questionnaire",
      "Thank you for collecting this information.",
      "__version__",
      "_version_",
      "_version__001",
      "Has the service provider been operating for more than a year?",
      "_id",
      "_uuid",
      "_submission_time",
      "_validation_status",
      "_notes",
      "_status",
      "_submitted_by",
      "_tags",
      "_index"
    )


  # When editing this vector, care should be taken before altering
  # characters preceding an underscore, as they are used for pattern-
  # matching elsewhere in the project workflow. Any edit at that
  # level should be followed by scrutiny of the script used for
  # data cleaning and initial storage of the data.
  mod <- c(
    start = "int_start",
    end = "int_end",
    today = "today",
    "username",
    "simserial",
    device.id = "device_id",
    phonenum = "phonenumber",
    "prelim_guid",
    gps.loc = "gps_loc",
    gps.lat = "gps_lat",
    gps.long = "gps_long",
    gps.alt = "gps_alt",
    gps.prec = "gps_prec",
    "has_office",
    "has_phone",
    "only_basic_info",
    "continue_mapping",
    "get_consent",
    "respondent_consent",
    "reason_no_consent",
    orgname = "org_name",
    state = "stateorigin",
    lga = "LGA",
    ward = "ward",
    address = "address",
    phone = "org_phone",
    email = "org_email",
    interviewer = "interviewer_name",
    interviewer.contact = "interviewer_contact",
    "wb_proj_list",
    proj.name = "wb_proj_name",
    org.type = "org_type",
    gov.spec = "govt_spec",
    "other_org_type",
    opstart = "started_ops",
    gbvstart = "started_gbv",
    "funding",
    "funding_nig_govt",
    "funding_foreign_govt",
    "funding_intl_org",
    "funding_pvt_donor",
    "funding_fees",
    "funding_other",
    "describe_other_funding",
    fulltime.staff = "num_fulltime",
    partime.staff = "num_parttime",
    female.staff = "num_female",
    age = "age_grp_served",
    "openday",
    "openday_sun",
    "openday_mon",
    "openday_tue",
    "openday_wed",
    "openday_thu",
    "openday_fri",
    "openday_sat",
    open.247 = "open_247",
    open.time = "open_time",
    close.time = "close_time",
    "respondent_info",
    staffname = "respondent_names",
    title = "respondent_title",
    "gbvaddr",
    "gbvaddr_sexviol",
    "gbvaddr_physasslt",
    "gbvaddr_ipv",
    "gbvaddr_csa",
    "gbvaddr_earlymar",
    "gbvaddr_earlypreg",
    "gbvaddr_fgm",
    "gbvaddr_other",
    "describe_other_gbv",
    "doc_sight",
    "doc_areused",
    showed.docs = "doc_showable",
    "doc_photo",
    "docused",
    "docused_sexviol",
    "docused_ipv",
    "docused_earlymar",
    "docused_earlypreg",
    "docused_fgm",
    "docused_other",
    "describe_other_docused",
    "nodoc_process",
    child.docs = "docs_child",
    standard.forms = "standard_forms",
    how.data = "data_storage",
    data.is.stored = "secure_physical",
    computer.secured = "secure_computer",
    choose.treatment = "choice_treat",
    contact.authority = "contact_auth",
    "contact_whysome",
    "contact_casetyp",
    "contact_authtyp",
    "privacy",
    "private_room",
    private.ques = "private_questions",
    "facmiss",
    "facmiss_staff",
    "facmiss_cap",
    "facmiss_equip",
    "facmiss_space",
    "facmiss_other",
    "facmiss_nothing",
    no.equipment = "missing_equip",
    details.miss.equip = "describe_other_facmiss",
    serve.disabled = "disabled_srv",
    disabled.special = "disabled_spec",
    "describe_other_disabled",
    coc.signed = "coc_signed",
    coc.copies = "coc_copy",
    coc.photo = "coc_pic",
    coc.confidentiality = "coc_conf",
    coc.equity = "coc_equity",
    has.focalperson = "gbv_focal",
    focalperson.contact = "fp_contact",
    has.gbv.trained = "gbvtrain",
    num.gbv.trained = "gbvtrain_num",
    "gbvtrain_which",
    "gbvtrain_who",
    has.refdir = "refdir",
    "refdir_pic",
    "refto",
    "refto_health",
    "refto_psych",
    "refto_police",
    "refto_legal",
    "refto_shelt",
    "refto_econ",
    "refto_other",
    "describe_other_refto",
    update.refdir = "refdir_update",
    "gbvcase_contact",
    choose.referral = "choice_ref",
    "choice_ref_whynot",
    coordination = "coord",
    "coord_which",
    "coord_comments",
    service.types = "srvtype",
    service.health = "srvtype_health",
    service.legal = "srvtype_legal",
    service.psychosocial = "srvtype_psych",
    service.police = "srvtype_police",
    service.shelter = "srvtype_shelt",
    service.economic = "srvtype_econ",
    service.others = "srvtype_other",
    service.othersdetail = "describe_other_srvtype",
    hf.type = "hf_type",
    hf.type.others = "hf_other",
    "srvhealth",
    "srvhealth_clinrape",
    "srvhealth_injuries",
    "srvhealth_pep",
    "srvhealth_contra",
    "srvhealth_hiv",
    "srvhealth_sti",
    "srvhealth_foren",
    "srvhealth_psycho",
    "srvhealth_case",
    "srvhealth_basic",
    "srvhealth_other",
    "describe_other_srvhealth",
    total.health = "health_num",
    has.pep = "pep_72hr",
    has.no.pep = "pep_72hr_not",
    has.contracep = "contra_120",
    has.no.contracep = "contra_120_not",
    health.paid = "health_fees",
    "srv_access",
    "healthfee_clin",
    "healthfee_inj",
    "healthfee_pep",
    "healthfee_contra",
    "healthfee_hiv",
    "healthfee_sti",
    "healthfee_foren",
    "healthfee_psych",
    "healthfee_case",
    "healthfee_basic",
    "healthfee_other",
    "forms_yes",
    "formtyp",
    "formtyp_foren",
    "formtyp_consentrape",
    "formtyp_consenthiv",
    "formtyp_safety",
    "formtyp_patient",
    "elem",
    "elem_consroom",
    "elem_waitroom",
    "elem_cabinet",
    "comment_elem",
    "suppl",
    "suppl_hivkit",
    "suppl_pregkit",
    "suppl_contra",
    "suppl_pep",
    "suppl_sti",
    "suppl_analg",
    "suppl_anaes",
    "suppl_antibio",
    "suppl_tt",
    "comment_suppl",
    "hlthtrain",
    "hlthtrain_medicare",
    "hlthtrain_clinrape",
    "hlthtrain_psycho",
    "hlthtrain_other",
    "describe_other_hlthtrain",
    "qual_staff",
    "srvleg",
    "srvleg_consult",
    "srvleg_rep",
    "srvleg_court",
    "srvleg_med",
    "srvleg_secur",
    "srvleg_counsel",
    "srvleg_other",
    "describe_other_srvleg",
    total.legal = "legal_num",
    legal.paid = "legal_fees",
    "legal_access",
    "legalfee_consult",
    "legalfee_rep",
    "legalfee_court",
    "legalfee_med",
    "legalfee_secur",
    "legalfee_counsel",
    "legalfee_other",
    support.for.court = "support_court",
    no.resources1 = "noresource1",
    no.resources2 = "other_action",
    "srvpsy",
    "srvpsy_counsel",
    "srvpsy_case",
    "srvpsy_therapy",
    "srvpsy_safety",
    "srvpsy_other",
    "describe_other_srvpsy",
    total.psychosocial = "psych_num",
    psych.paid = "psych_fees",
    "psych_access",
    "psychfee_counsel",
    "psychfee_case",
    "psychfee_therapy",
    "psychfee_safety",
    "psychfee_other",
    "psytrain",
    "psytrain_safety",
    "psytrain_child",
    "psytrain_counsel",
    "psytrain_therapy",
    "psytrain_case",
    "psytrain_other",
    "describe_other_psytrain",
    "id_qualstaff",
    "srvpol",
    "srvpol_caseinv",
    "srvpol_safety",
    "srvpol_other",
    "describe_other_srvpol",
    "gbvpolice",
    "gbvpolice_who",
    refer.otherpolice = "ref_otherpol",
    "poltrain",
    "poltrain_surv",
    "poltrain_child",
    "poltrain_inv",
    "id_trainedpolice",
    "polnum",
    total.police.rape = "polnum_rape",
    total.police.ipv = "polnum_ipv",
    total.police.csa = "polnum_csa",
    total.police.fgm = "polnum_fgm",
    total.police.oth = "polnum_other",
    "describe_other_polnum",
    police.fees = "police_fees",
    "pol_access",
    "policefee_case",
    "policefee_safety",
    "policefee_other",
    "polresrc",
    "polresrc_vehcl",
    "polresrc_bike",
    "polresrc_fuel",
    "polresrc_other",
    "describe_other_polresrc",
    police.followup = "pol_ffup",
    police.confidential = "pol_ffup_conf",
    "srvshelt",
    "srvshelt_hous",
    "srvshelt_psych",
    "srvshelt_safety",
    "srvshelt_case",
    "srvshelt_basic",
    "srvshelt_health",
    "srvshelt_other",
    "descr_health_srvshelt",
    "descr_oth_srvshelt",
    shelter.famfriendly = "shelt_fam",
    shelter.kidfriendly = "shelt_childfrndly",
    "sheltpriv",
    "sheltpriv_wall",
    "sheltpriv_guard",
    "sheltpriv_cctv",
    "sheltpriv_locks",
    "sheltpriv_other",
    "describe_other_sheltpriv",
    "sheltamen",
    "sheltamen_food",
    "sheltamen_cloth",
    "sheltamen_shwr",
    "sheltamen_other",
    "describe_other_sheltamen",
    electricwater = "shelt_elecwtr",
    total.shelter.f = "shelt_num_f",
    total.shelter.m = "shelt_num_m",
    shelter.support = "shelt_givsupp",
    shelter.new.support = "shelt_capsupp",
    "srvecon",
    "srvecon_agric",
    "srvecon_train",
    "srvecon_incomegen",
    "srvecon_loans",
    "srvecon_cash",
    "srvecon_land",
    "srvecon_other",
    "describe_other_srvecon",
    total.economic = "econ_num",
    "econ_areas",
    "econ_reject",
    "complete",
    "thanks",
    "version1",
    "version2",
    "version_001",
    "op1year",
    "id",
    "uuid",
    "submission_time",
    "validation_status",
    "notes",
    "status",
    "submitted_by",
    "tags",
    "index"
  )
  if (!identical(length(old), length(mod)))
    stop( )
  if (as_list)
    mod <- as.list(mod)
  list(modified = mod, old = old)
}
