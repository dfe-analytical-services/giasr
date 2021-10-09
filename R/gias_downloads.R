#' Download GIAS Establishment Fields Data
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd

download.gias.estab.fields <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # create a temporary directory
  tmp_dir <- tempdir()

  # define file path for GIAS data
  gias_dir <- file.path(tmp_dir, "gias")

  # create the directory for GIAS data if it doesn't already exist
  if(!dir.exists(gias_dir)){
    dir.create(gias_dir)
  }

  # define GIAS URL using the specified GIAS date or 1st of the month if not specified
  gias_url <- paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/edubasealldata",gsub("-","",gias_date),".csv")

  # specify GIAS download filepath
  gias_download <- file.path(gias_dir, basename(gias_url))

  # download GIAS data if not already available
  if(!file.exists(gias_download)){
    utils::download.file(gias_url, mode = "wb", method = "libcurl", destfile = gias_download)
  }

  gias_download
}


#' Import GIAS Data
#'
#' `gias.estab.fields` Imports GIAS Establishment Fields Data from https://www.get-information-schools.service.gov.uk/Downloads.
#' Standardises variable names to be unique and consist only of the _ character, numbers, and letters and sets all date and numeric variables to relevant classes.
#' `gias.links.data` Imports GIAS Links Data.
#'
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered.
#'
#' @usage gias.estab.fields(gias_date)
#'
#' @return Dataframe of GIAS establishment fields data with standardised names and variable class assigned.
#'
#' @examples gias.estab.fields("2021-06-01")
#'
gias.estab.fields <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  gias_download <- download.gias.estab.fields(gias_date)

  # define NA strings
  na_strings <- c("NA", "NULL", "", "-", "Not applicable", "Does not apply", " ", "None", "..")

  gias_establishment_data <- readr::read_csv(gias_download,
                                             na = na_strings,
                                             col_types = readr::cols(.default = "c"))

  gias_establishment_data_clean <- janitor::clean_names(gias_establishment_data)

  gias_establishment_data_clean <- dplyr::mutate(gias_establishment_data_clean,
                                                 open_date = as.Date(open_date, "%d-%m-%Y"),
                                                 close_date = as.Date(close_date, "%d-%m-%Y"),
                                                 statutory_low_age = as.numeric(statutory_low_age),
                                                 statutory_high_age = as.numeric(statutory_high_age),
                                                 school_capacity = as.numeric(school_capacity),
                                                 census_date = as.Date(census_date, "%d-%m-%Y"),
                                                 number_of_pupils = as.numeric(number_of_pupils),
                                                 number_of_boys = as.numeric(number_of_boys),
                                                 number_of_girls = as.numeric(number_of_girls),
                                                 percentage_fsm = as.numeric(percentage_fsm),
                                                 ofsted_last_insp = as.Date(ofsted_last_insp, "%d-%m-%Y"),
                                                 last_changed_date = as.Date(last_changed_date, "%d-%m-%Y"),
                                                 date_of_last_inspection_visit = as.Date(date_of_last_inspection_visit, "%d-%m-%Y"),
                                                 next_inspection_visit = as.Date(next_inspection_visit, "%d-%m-%Y"),
                                                 teen_moth_places = as.numeric(teen_moth_places),
                                                 places_pru = as.numeric(places_pru),
                                                 resourced_provision_on_roll = as.numeric(resourced_provision_on_roll),
                                                 resourced_provision_capacity = as.numeric(resourced_provision_capacity),
                                                 sen_unit_on_roll = as.numeric(sen_unit_on_roll),
                                                 sen_unit_capacity = as.numeric(sen_unit_capacity),
                                                 sen_stat = as.numeric(sen_stat),
                                                 sen_no_stat = as.numeric(sen_no_stat))
}

#' Download GIAS Establishment Links Data
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd
download.gias.links.data <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # create a temporary directory
  tmp_dir <- tempdir()

  # define file path for GIAS data
  gias_dir <- file.path(tmp_dir, "gias")

  # create the directory for GIAS data if it doesn't already exist
  if(!dir.exists(gias_dir)){
    dir.create(gias_dir)
  }

  # define GIAS Links URL using the specified GIAS date or 1st of the month if not specified
  gias_links_url <- paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/links_edubasealldata",gsub("-","",gias_date),".csv")

  # specify GIAS download filepath
  gias_links_download <- file.path(gias_dir, basename(gias_links_url))

  # download GIAS data if not already available
  if(!file.exists(gias_links_download)){
    utils::download.file(gias_links_url, mode = "wb", method = "libcurl", destfile = gias_links_download)
  }

  gias_links_download
}

#' Prepare GIAS Establishment Links Data
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd
prep.gias.links.data <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # define NA strings
  na_strings <- c("NA", "NULL", "", "-", "Not applicable", "Does not apply", " ", "None", "..")

  gias_links_download <- download.gias.links.data(gias_date)

  gias_links_data <- readr::read_csv(gias_links_download,
                                     na = na_strings,
                                     col_types = readr::cols(.default = "c"))

  gias_links_data <-janitor::clean_names(gias_links_data)

  gias_links_data <- dplyr::mutate(gias_links_data,
                                   link_established_date = as.Date(link_established_date, "%d-%m-%Y"))
}


#' Clean GIAS Establishment Links Data
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd
clean.gias.links.data <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  state_schools_w_status <- state.schools.data(gias_date)

  links_data <- prep.gias.links.data(gias_date)

  successor_link <- dplyr::left_join(state_schools_w_status, links_data, by = "urn")

  # remove entries with no links
  successor_link <- dplyr::filter(successor_link, !is.na(link_urn))

  # remove links explicitly flagged predecessors or sixth form centres
  successor_link <- dplyr::filter(successor_link, grepl('Pred|Sixth', link_type))

  # remove duplicates
  successor_link <- dplyr::distinct(successor_link, urn, link_urn, .keep_all = TRUE)

  # make explicit that the establishment data relates to the predecessor URN (URN)
  add_predecessor_urn_info <- dplyr::transmute(successor_link,
                                           urn,
                                           link_urn,
                                           link_name,
                                           link_established_date,
                                           urn_establishment_status = establishment_status_name,
                                           urn_open_date = open_date,
                                           urn_close_date = close_date)

  add_successor_urn_info <- dplyr::left_join(add_predecessor_urn_info,
                                             establishment_status_data,
                                             by = c("link_urn" = "urn"))

  urn_info <- dplyr::transmute(add_successor_urn_info,
                               urn,
                               link_urn,
                               link_name,
                               link_established_date,
                               urn_establishment_status,
                               urn_open_date,
                               urn_close_date,
                               link_establishment_status = establishment_status,
                               link_open_date = open_date,
                               link_close_date = close_date)

  # remove proposed to open successors - they aren't yet linked and the URN often changes
  opened_successors <- dplyr::filter(urn_info, link_establishment_status != "Proposed to open")


}
