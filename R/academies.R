#' Import Academy Pipeline Data
#'
#' `academy.pipeline()` returns a df containing basic data on the converter and/or sponsored academy pipelines from the latest pipeline MI: https://www.gov.uk/government/publications/open-academies-and-academy-projects-in-development
#'
#' @usage academy.pipeline(converter = TRUE, sponsored = TRUE)
#'
#' @param converter TRUE includes academy converter data in the output df
#' @param sponsored TRUE includes sponsored academy data in the output df
#'
#' @details Returns the following variables in a df:
#'
#' * `urn` the URN of the school as listed in the pipeline MI datacurrent_pipeline_sponsored,
#' * `application_date` for sponsored academies this variable reflects the data the project was approved (dAO issue date), for academy converters this reflects the date the application was approved.
#' * `name_of_matched_sponsor` populated only for sponsored academies,
#' * `proposed_opening_date` proposed date for the academy to open,
#' * `academy_type` indicates whether the academy is a sponsored academy or academy converter
#'
#' @return Dataframe of pipeline data.
#'
#' @examples academy.pipeline(converter = TRUE, sponsored = FALSE)
#'
#' @export
#'
academy.pipeline <- function(converter = TRUE, sponsored = TRUE){
  # Set NA strings if not already defined
  if(!exists("na_strings")){
    na_strings <- c("NA", "NULL", "", "-", "Not applicable", "Does not apply", " ", "None", "..")
    }

  # Create temp directory ---------------------------------------------------
  tmp_dir <- tempdir()

  pipeline_dir <- file.path(tmp_dir, "pipeline")

  if (!dir.exists(pipeline_dir)){
    dir.create(pipeline_dir)
    }

  # Set the link to the pipeline data ---------------------------------------
  pipeline_url <- "https://www.gov.uk/government/publications/open-academies-and-academy-projects-in-development"

  # get page attributes
  pg <- rvest::read_html(pipeline_url)

  # extract link for current MI data
  current_pipeline_url <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href")

  current_pipeline_url <- tibble::as_tibble(current_pipeline_url)

  current_pipeline_url <- dplyr::filter(current_pipeline_url, grepl("https://assets.publishing.service.gov.uk/", .data$value) & grepl(".xlsx", .data$value))

  current_pipeline_url <- dplyr::slice_head(current_pipeline_url) # leaves only the top link (usually the most recent)

  current_pipeline_url <- current_pipeline_url$value

  # Download the file to the temp directory ------------------------------
  pipeline_file <- file.path(pipeline_dir, basename(current_pipeline_url))

  if(!file.exists(pipeline_file)){
    utils::download.file(current_pipeline_url, mode = "wb", method = "libcurl", destfile = file.path(pipeline_dir, basename(current_pipeline_url)))
    }

  # Read data from sponsored and converter sheets ------------------------
  if(sponsored == TRUE){
    current_pipeline_sponsored <- readxl::read_xlsx(pipeline_file, skip = 6, na = na_strings, sheet = "Sponsor Pipeline")
    current_pipeline_sponsored <- janitor::clean_names(current_pipeline_sponsored)
    current_pipeline_sponsored <- dplyr::mutate(current_pipeline_sponsored,
                                                project_approval_month = as.Date(.data$project_approval_month, "%Y-%m-%d"),
                                                proposed_opening_date = as.Date(.data$proposed_opening_date, "%Y-%m-%d"))

    current_sponsored_pipeline_info <- dplyr::transmute(current_pipeline_sponsored,
                                                        .data$urn,
                                                        application_date = .data$project_approval_month,
                                                        .data$name_of_matched_sponsor,
                                                        .data$proposed_opening_date,
                                                        academy_type = "sponsored")

    pipeline_info <- current_sponsored_pipeline_info
  }

  if(converter == TRUE){
    current_pipeline_converter <- readxl::read_xlsx(pipeline_file, skip = 7, na = na_strings, sheet = "Converter Pipeline")
    current_pipeline_converter <- janitor::clean_names(current_pipeline_converter)
    current_pipeline_converter <- dplyr::mutate(current_pipeline_converter,
                                                application_date = as.Date(.data$application_date, "%Y-%m-%d"),
                                                application_approved_date = as.Date(.data$application_approved_date, "%Y-%m-%d"))

    current_converter_pipeline_info <- dplyr::transmute(current_pipeline_converter,
                                                        .data$urn,
                                                        .data$application_date,
                                                        approval_date = .data$application_approved_date,
                                                        academy_type = "converter")

    pipeline_info <- current_converter_pipeline_info
  }

# Consolidate lists ----------------------------------------------------

  if(converter == TRUE & sponsored == TRUE){
    consolidated_current_pipeline_info <- dplyr::full_join(current_sponsored_pipeline_info,
                                                           current_converter_pipeline_info,
                                                           by = "urn")

    pipeline_info <- consolidated_current_pipeline_info
  }

  pipeline_info
}
