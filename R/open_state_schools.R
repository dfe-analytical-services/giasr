#' Identify State Funded Establishments with Additional Data
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd

state.schools.data <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  gias_download <- download.gias.estab.fields(gias_date)

  # define NA strings
  na_strings <- c("NA", "NULL", "", "-", "Not applicable", "Does not apply", " ", "None", "..")

  # get only data needed from GIAS dataset
  all_schools_type_status <- readr::read_csv(file = gias_download,
                                             na = na_strings,
                                             col_types = readr::cols(.default = "c"))

  all_schools_type_status <- janitor::clean_names(all_schools_type_status)

  all_schools_type_status <- dplyr::transmute(all_schools_type_status,
                                              .data$urn,
                                              .data$type_of_establishment_name,
                                              .data$phase_of_education_name,
                                              open_date = as.Date(.data$open_date, "%d-%m-%Y"),
                                              close_date = as.Date(.data$close_date, "%d-%m-%Y"),
                                              .data$establishment_status_name)

  # list all non-special state funded school types
  state_funded_list <- c("Academy 16 to 19 sponsor led",
                         "Academy 16-19 converter",
                         "Academy converter",
                         "Academy sponsor led",
                         "City technology college",
                         "Community school",
                         "Foundation school",
                         "Free schools 16 to 19",
                         "Free schools",
                         "Studio schools",
                         "University technical college",
                         "Voluntary aided school",
                         "Voluntary controlled school")

  academy_list <- c("Academy 16-19 converter",
                    "Academy 16 to 19 sponsor led",
                    "Academy alternative provision converter",
                    "Academy alternative provision sponsor led",
                    "Academy converter",
                    "Academy special converter",
                    "Academy special sponsor led",
                    "Academy sponsor led",
                    "City technology college",
                    "Free schools 16 to 19",
                    "Free schools alternative provision",
                    "Free schools special",
                    "Free schools",
                    "Studio schools",
                    "University technical college")

  la_maintained_list <- c("Community school",
                          "Community special school",
                          "Foundation school",
                          "Foundation special school",
                          "Local authority nursery school",
                          "Pupil referral unit",
                          "Voluntary aided school",
                          "Voluntary controlled school")


  # create new variable to flag state funded establishments
  state_school_flag <- dplyr::mutate(all_schools_type_status,
                                     phase_type_grouping = dplyr::case_when(.data$phase_of_education_name == "Nursery" ~ "State-funded Nursery",
                                                                            .data$phase_of_education_name %in% c("Primary","Middle deemed primary") & .data$type_of_establishment_name %in% state_funded_list ~ "State-funded Primary",
                                                                            (.data$phase_of_education_name %in% c("Secondary", "Middle deemed secondary", "16 plus", "Not applicable", "All-through") & .data$type_of_establishment_name %in% state_funded_list) | .data$type_of_establishment_name == "City technology college" ~ "State-funded Secondary",
                                                                            .data$type_of_establishment_name %in% c("Foundation special school", "Community special school", "Academy special converter", "Academy special sponsor led", "Free schools special") ~ "State-funded Special school",
                                                                            .data$type_of_establishment_name == "Non-maintained special school" ~ "Non-maintained special school",
                                                                            .data$type_of_establishment_name %in% c("Pupil referral unit", "Academy alternative provision sponsor led", "Free schools alternative provision", "Academy alternative provision converter") ~ "Pupil referral unit",
                                                                            .data$type_of_establishment_name %in% c("Other independent school", "Other independent special school") ~ "Independent school",
                                                                            TRUE ~ "Other non-school institution"),
                                     general_type_group = dplyr::case_when(.data$type_of_establishment_name %in% academy_list ~ "Academies",
                                                                           .data$type_of_establishment_name %in% la_maintained_list ~ "Local authority maintained schools"))
  state_schools_data <- dplyr::filter(state_school_flag,
                                 .data$phase_type_grouping %in% c("State-funded Nursery", "State-funded Primary", "State-funded Secondary", "State-funded Special school", "Pupil referral unit"))

  state_schools_data
}


#' Identify State Funded Establishments
#'
#' The `state.schools()` function is used to identify a list of all schools that are state funded establishments in the GIAS all establishment data released on the selected date.
#' While `current.state.schools()` identifies all currently open state funded establishments as at a specified date, using the GIAS all establishment data released on the selected date.
#'
#' @usage state.schools(gias_date)
#'
#' @param gias_date default is to get GIAS data from the 1st of the current month. However, to reproduce historic analysis or to use more recent data the date can be set using the "%Y-%m-%d" format.
#' @param open_date default is to use the 1st of the current month for open schools. However, to reproduce historic analysis or identify more recently open schools the date can be set using the "%Y-%m-%d" format.
#'
#' @details Schools identified as state funded establishments align with those in external reporting. These are:
#'
#' * Academy 16 to 19 sponsor led
#' * Academy 16-19 converter
#' * Academy alternative provision converter
#' * Academy alternative provision sponsor led
#' * Academy converter
#' * Academy special converter
#' * Academy special sponsor led
#' * Academy sponsor led
#' * City technology college
#' * Community school
#' * Community special school
#' * Foundation school
#' * Foundation special school
#' * Free schools
#' * Free schools 16 to 19
#' * Free schools alternative provision
#' * Free schools special
#' * Local authority nursery school
#' * Pupil referral unit
#' * Studio schools
#' * University technical college
#' * Voluntary aided school
#' * Voluntary controlled school
#'
#' @return A list of state funded establishment URNs along with
#' * `phase_type_grouping`: State-funded Nursery, State-funded Primary, State-funded Secondary, State-funded Special school, Pupil referral unit
#' * `general_type_group`: Academies, Local Authority Maintained
#' * `type_of_establishment_name`: GIAS fields
#' * `phase_of_education_name`: GIAS fields
#'
#' @examples current.state.schools("2021-06-01", "2021-06-01")
#' @examples state.schools("2021-06-01")
#'
#' @describeIn state.schools Identifies all state funded establishments (historc and currently open) as at a specified date, using the GIAS all establishment data released on the selected date.


state.schools <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  all_state_schools <- state.schools.data(gias_date)

  state_schools <- dplyr::select(all_state_schools,
                                 urn = .data$urn,
                                 .data$phase_type_grouping,
                                 .data$general_type_group,
                                 .data$type_of_establishment_name,
                                 .data$phase_of_education_name)

  state_schools
}


#' @describeIn state.schools Identifies all currently open state funded establishments as at a specified date, using the GIAS all establishment data released on the selected date.

current.state.schools <- function(gias_date, open_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # set open_date to start of month if not provided
  if(missing(open_date)){
    open_date <- as.Date(cut(Sys.Date(), "month"))
  }


  all_state_schools <- state.schools.data(gias_date)

  open_state_schools <- dplyr::filter(all_state_schools,
                                      .data$establishment_status_name %in% c("Open", "Open, but proposed to close") |
                                        .data$close_date > open_date &
                                        (is.na(.data$open_date) |
                                           .data$open_date <= open_date))

  open_state_schools <- dplyr::select(open_state_schools,
                                      urn = .data$urn,
                                      .data$phase_type_grouping,
                                      .data$general_type_group,
                                      .data$type_of_establishment_name,
                                      .data$phase_of_education_name)

  open_state_schools
}
