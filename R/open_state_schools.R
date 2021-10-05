state_schools <- function(gias_date = Sys.Date()){
  # define NA strings
  na_strings <- c("NA", "NULL", "", "-", "Not applicable", "Does not apply", " ", "None", "..")

  # create a temporary directory
  tmp_dir <- tempdir()

  # define file path for GIAS data
  gias_dir <- file.path(tmp_dir, "gias")

  # create the directory for GIAS data
  dir.create(gias_dir)

  # define GIAS URL using the specified GIAS date or today if not specified
  gias_url <- paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/edubasealldata",gsub("-","",gias_date),".csv")

  # specify GIAS download filepath
  gias_download <- file.path(gias_dir, basename(gias_url))

  # download GIAS data
  download.file(gias_url, mode = "wb", method = "libcurl", destfile = gias_download)

  # get only data needed from GIAS dataset
  all_schools_type_status <- readr::read_csv(file = gias_download,
                                             na = na_strings,
                                             col_types = readr::cols(.default = "c"))

  all_schools_type_status <- janitor::clean_names(all_schools_type_status)

  all_schools_type_status <- dplyr::transmute(all_schools_type_status,
                                              urn,
                                              type_of_establishment_name,
                                              phase_of_education_name,
                                              open_date = as.Date(open_date, "%d-%m-%Y"),
                                              close_date = as.Date(close_date, "%d-%m-%Y"),
                                              establishment_status_name)

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
                                     phase_type_grouping = dplyr::case_when(phase_of_education_name == "Nursery" ~ "State-funded Nursery",
                                                                            phase_of_education_name %in% c("Primary","Middle deemed primary") & type_of_establishment_name %in% state_funded_list ~ "State-funded Primary",
                                                                            (phase_of_education_name %in% c("Secondary", "Middle deemed secondary", "16 plus", "Not applicable", "All-through") & type_of_establishment_name %in% state_funded_list) | type_of_establishment_name == "City technology college" ~ "State-funded Secondary",
                                                                            type_of_establishment_name %in% c("Foundation special school", "Community special school", "Academy special converter", "Academy special sponsor led", "Free schools special") ~ "State-funded Special school",
                                                                            type_of_establishment_name == "Non-maintained special school" ~ "Non-maintained special school",
                                                                            type_of_establishment_name %in% c("Pupil referral unit", "Academy alternative provision sponsor led", "Free schools alternative provision", "Academy alternative provision converter") ~ "Pupil referral unit",
                                                                            type_of_establishment_name %in% c("Other independent school", "Other independent special school") ~ "Independent school",
                                                                            TRUE ~ "Other non-school institution"),
                                     general_type_group = dplyr::case_when(type_of_establishment_name %in% academy_list ~ "Academies",
                                                                           type_of_establishment_name %in% la_maintained_list ~ "Local authority maintained schools"))
  state_schools <- dplyr::filter(state_school_flag,
                                 phase_type_grouping %in% c("State-funded Nursery", "State-funded Primary", "State-funded Secondary", "State-funded Special school", "Pupil referral unit"))
}

open_state_schools <- function(cut_date = Sys.Date()){
  open_state_schools <- dplyr::filter(state_schools,
                                      establishment_status_name %in% c("Open", "Open, but proposed to close") |
                                        close_date > cut_date &
                                        (is.na(open_date) |
                                        open_date <= cut_date))

  open_state_schools <- dplyr::select(open_state_schools,
                                      urn = urn,
                                      phase_type_grouping,
                                      general_type_group)

  open_state_schools
}
