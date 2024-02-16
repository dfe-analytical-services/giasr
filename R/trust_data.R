#' Download GIAS Academies Trust Membership History File
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd
download.gias.trust.history <- function(gias_date){
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

  # define GIAS academies history URL using the specified GIAS date or 1st of the month if not specified
  trust_url <- paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/academiesmatmembership",gsub("-","",gias_date),".csv")

  # specify download filepath
  trust_history_download <- file.path(gias_dir, basename(trust_url))

  options(timeout = max(300, getOption("timeout")))

  # download GIAS data if not already available
  if(!file.exists(trust_history_download)){
    utils::download.file(trust_url, mode = "wb", method = "libcurl", destfile = trust_history_download)
  }

  trust_history_download

}

#' Import GIAS Academies Trust Membership History File as DF
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd
import.gias.trust.data <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  trust_history_download <- download.gias.trust.history(gias_date)

  # define NA strings
  na_strings <- c("NA", "NULL", "", "-", "Not applicable", "Does not apply", " ", "None", "..")

  academy_info <- readr::read_csv(trust_history_download,
                                  na = na_strings,
                                  col_types = readr::cols(.default = "c"),
                                  locale = readr::locale(encoding = "latin1"))

  academy_info_clean <- janitor::clean_names(academy_info)

  academy_info_clean <- dplyr::mutate(academy_info_clean,
                                      date_joined_group = as.Date(date_joined_group),
                                      date_left_group = as.Date(date_left_group),
                                      establishment_close_date = as.Date(establishment_close_date),
                                      establishment_open_date = as.Date(establishment_open_date),
                                      group_open_date = as.Date(group_open_date),
                                      group_closed_date = as.Date(group_closed_date))

  academy_info_clean <- dplyr::filter(academy_info_clean,
                                      !is.na(urn))

  academy_info_clean
}

#' Import and Sequence Academy Trust History Data
#'
#' `academies.trust.data()` imports GIAS Academies Trust Membership History Data from https://www.get-information-schools.service.gov.uk/Downloads. Standardises variable names to be unique and consist only of characters, numbers, and letters and sets all date and numeric variables to relevant classes. Removes duplicates and adds a history_order variable to show 1 = current trust.
#'
#' @usage academies.trust.data(gias_date, cut_off_date, urn_link_type = "ofsted")
#'
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered.
#' @param cut_off_date the cut off date for open schools, defaults to first day of current month if not entered.
#' @param urn_link_type defaults to Ofsted URN linking methodology, use "all" if merges are to be included. Note this will likely flag a warning for multiple join dates.
#'
#' @details Returns the following variables in a df:
#'
#' * `current_urn` The current URN of the schools using either the Ofsted methodology or links including all merges (no splits)
#' * `group_id` The Group ID of the Trust
#' * `type_of_establishment_name` Shows whether the school is sponsored academy or an academy converter according to the status when the school first academised
#' * `date_joined_group`
#' * `date_left_group`
#' * `history_order` The order in which the school joined the trust with 1 = most recent/current
#' * `n_trusts` The number of trusts the school has been a member of
#' * `group_type` Whether the trust is a Single or Multi-Academy Trust (SAT or MAT)
#'
#' @return Dataframe of academies and trust history.
#'
#' @examples academies.trust.data("2021-06-01", "2021-06-01", urn_link_type = "all")
#'
#' @export
#'
academies.trust.data <- function(gias_date, cut_off_date, urn_link_type = "ofsted"){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # set cut_off_date to start of month if not provided
  if(missing(cut_off_date)){
    cut_off_date <- as.Date(cut(Sys.Date(), "month"))
  }

  academy_info_clean <- import.gias.trust.data(gias_date)

  # use Ofsted as default reflecting the Ofsted URN linking methodology, use "all" in arguments to use all predecessors (but ignoring splits)
  if(urn_link_type == "ofsted"){
    urn_links <- ofsted.urn.links(gias_date, cut_off_date)
  }
  else {
    if(urn_link_type == "all"){
      urn_links <- urn.links.no.splits(gias_date, cut_off_date)
    }
  }

  # add chosen urn_linking in order to link trust history over time
  linked_academy_info <- dplyr::left_join(academy_info_clean,
                                          urn_links,
                                          by = "urn")

  linked_academy_info <- dplyr::filter(linked_academy_info,
                                       !is.na(current_urn))

  linked_academy_info <- dplyr::filter(linked_academy_info,
                                       !grepl(".old", group_id))


  # the data is very messy with multiple entries per trust/school combination
  # These next steps clean up the data to leave just one entry with pragmatic dates
  trust_history <- dplyr::group_by(linked_academy_info, current_urn, urn, group_id, type_of_establishment_name, phase_of_education_name)

  # filter out links to trusts "joined" for 2 months or fewer
  trust_history_remove_short_stay <- dplyr::filter(trust_history,
                                             is.na(date_left_group) | date_left_group > date_joined_group + 62)

  # filter out schools that closed 2 months or less after joining a group
  trust_history_remove_short_stay <- dplyr::filter(trust_history_remove_short_stay,
                                             is.na(establishment_close_date) | (establishment_close_date > date_joined_group + 62))



  # extract the earliest join dates and latest leaving dates for each school by trust
  trust_history_clean_dates <- dplyr::summarise(trust_history_remove_short_stay,
                                                date_joined_group = min(date_joined_group),
                                                date_left_group = max(date_left_group),
                                                establishment_close_date = min(establishment_close_date),
                                                .groups = "drop")

  # remove observations with no group_id
  trust_history_no_group_id_removed <- dplyr::filter(trust_history_clean_dates, !is.na(group_id))

  # get a list of all trusts
  trust_list <- dplyr::select(linked_academy_info,
                              group_id,
                              group_type,
                              group_name,
                              group_status)

  trust_list <- dplyr::group_by(trust_list,
                                group_id)


  trust_list <- dplyr::distinct(trust_list)

  trust_list <- dplyr::mutate(trust_list,
                              n = dplyr::n())

  trust_list <- dplyr::ungroup(trust_list)

  # get rid of the SATs that "closed" to become MATs
  trust_list <- dplyr::filter(trust_list,
                              n == 1 | (n > 1 & group_status == "Open"))

  trust_list <- dplyr::select(trust_list, -n)



  # remove and update individual incorrect instances - see QA doc for info
  trust_history_non_existent_trusts <- dplyr::filter(trust_history_no_group_id_removed,
                                                     # trusts that don't seem to exist
                                                     !group_id %in% c("TR00670", "TR00340", "TR02585", "TR02314", "TR03305.old", "TR03461"))

  trust_history_remove_wrong_trust <- dplyr::filter(trust_history_non_existent_trusts,
                                                    # remove instances of schools registered with the wrong trust
                                                    !(current_urn == "142285" & group_id == "TR02253") &
                                                      !(current_urn %in% c("144067", "144068", "145233") & group_id == "TR02659") &
                                                      !(current_urn == "145482" & group_id == "TR01769") &
                                                      !(current_urn == "147177" & group_id == "TR00674") &
                                                      !(current_urn == "147531" & group_id == "TR02582") &
                                                      !(current_urn == "144753" & group_id == "TR01336") &
                                                      !(current_urn == "140027" & group_id == "TR02012") &
                                                      !(current_urn == "139299" & group_id == "TR00195") &
                                                      !(current_urn == "138845" & group_id == "TR02666"))

  # schools noted as joining/leaving on incorrect (and conflicting) dates
  trust_history_correct_dates <- dplyr::mutate(trust_history_remove_wrong_trust,
                                               date_joined_group = dplyr::case_when(current_urn == "136789" & group_id == "TR01513" ~ as.Date("2015-10-01"),
                                                                                    current_urn == "137667" & group_id == "TR00395" ~ as.Date("2015-09-01"),
                                                                                    current_urn == "138130" & group_id == "TR01550" ~ as.Date("2015-07-01"),
                                                                                    current_urn == "139320" & group_id == "TR00920" ~ as.Date("2015-09-08"),
                                                                                    current_urn == "140260" & group_id == "TR00456" ~ as.Date("2016-10-01"),
                                                                                    current_urn == "140482" & group_id == "TR01384" ~ as.Date("2016-09-01"),
                                                                                    current_urn == "140533" & group_id == "TR00996" ~ as.Date("2016-09-01"),
                                                                                    current_urn == "140919" & group_id == "TR02666" ~ as.Date("2016-10-01"),
                                                                                    current_urn == "141965" & group_id == "TR03322" ~ as.Date("2016-04-01"),
                                                                                    current_urn == "140027" & group_id == "TR01854" ~ as.Date("2016-05-01"),
                                                                                    current_urn == "136549" & group_id == "TR00174" ~ as.Date("2011-04-01"),
                                                                                    current_urn == "136549" & group_id == "TR00174" ~ as.Date("2011-04-01"),
                                                                                    current_urn == "136549" & group_id == "TR03120" ~ as.Date("2018-03-01"),
                                                                                    current_urn == "139828" & group_id == "TR00307" ~ as.Date("2016-02-01"),
                                                                                    current_urn == "136665" & group_id == "TR00691" ~ as.Date("2011-04-01"),
                                                                                    current_urn == "137156" & group_id == "TR01307" ~ as.Date("2011-08-01"),
                                                                                    TRUE ~ date_joined_group),
                                               date_left_group = dplyr::case_when(current_urn == "140533" & group_id == "TR00411" ~ as.Date("2016-08-31"),
                                                                                  current_urn == "140919" & group_id == "TR02665" ~ as.Date("2016-09-30"),
                                                                                  current_urn == "141965" & group_id == "TR03322" ~ as.Date("2016-03-31"),
                                                                                  current_urn == "136549" & group_id == "TR00174" ~ as.Date("2018-03-01"),
                                                                                  current_urn == "139828" & group_id == "TR00415" ~ as.Date("2016-02-01"),
                                                                                  current_urn == "143424" & group_id == "TR00984" ~ as.Date("2016-08-31"),
                                                                                  current_urn == "143426" & group_id == "TR01685" ~ as.Date("2016-08-31"),
                                                                                  current_urn == "143427" & group_id == "TR01685" ~ as.Date("2016-08-31"),
                                                                                  current_urn == "143428" & group_id == "TR00432" ~ as.Date("2016-08-31"),
                                                                                  current_urn == "142762" & group_id == "TR01385" ~ as.Date("2016-03-01"),
                                                                                  TRUE ~ date_left_group))

  # correct issues where school is noted as both converter and sponsor
  trust_history_correct_converter_sponsor_labels <- dplyr::filter(trust_history_correct_dates,
                                                                  !(current_urn == "145421" & group_id == "TR01013" & type_of_establishment_name == "Academy converter") &
                                                                    !(current_urn == "145474" & group_id == "TR02034" & type_of_establishment_name == "Academy converter"))

  # add a history order variable where 1 = first trust
  trust_history_ordered <- dplyr::group_by(trust_history_correct_converter_sponsor_labels,
                                           current_urn)

  trust_history_ordered <- dplyr::mutate(trust_history_ordered,
                                         history_order = rank(as.numeric(date_joined_group)),
                                         n_trusts = dplyr::n())

  trust_history_ordered <- dplyr::ungroup(trust_history_ordered)

  check_date_duplicates <- dplyr::group_by(trust_history_ordered,
                                           current_urn, date_joined_group)

  check_date_duplicates <- dplyr::mutate(check_date_duplicates,
                                         date_repeats = dplyr::n())

  check_date_duplicates <- dplyr::ungroup(check_date_duplicates)

  check_date_duplicates <- dplyr::filter(check_date_duplicates,
                                         date_repeats > 1)

  check_date_duplicates <- dplyr::left_join(check_date_duplicates,
                                            trust_list,
                                            by = "group_id")

  if(nrow(check_date_duplicates)>0){
    warning("GIAS trust history data includes duplicated entries for trust join date by current URN.")
  }

  # these steps ensure that the school is correctly shown as a sponsored academy if the original academisation was as a sponsored academy
  update_academy_type <- dplyr::filter(trust_history_ordered,
                                       history_order == 1)

  update_academy_type <- dplyr::select(update_academy_type,
                                       current_urn,
                                       type_1 = type_of_establishment_name)

  update_academy_type <- dplyr::left_join(trust_history_ordered,
                                          update_academy_type,
                                          by = "current_urn")

  update_academy_type <- dplyr::mutate(update_academy_type,
                                       type_of_establishment_name = dplyr::case_when(grepl("sponsor", type_1) & grepl("converter", type_of_establishment_name) ~ type_1,
                                                                                     TRUE ~ type_of_establishment_name))

  update_academy_type <- dplyr::select(update_academy_type,
                                       -type_1)

  # renumber history order so that 1 = most recent
  academy_trust_history <- dplyr::group_by(update_academy_type,
                                           current_urn)

  academy_trust_history <- dplyr::mutate(academy_trust_history,
                                         history_order = rank(-as.numeric(date_joined_group)))

  academy_trust_history <- dplyr::ungroup(academy_trust_history)

  academy_trust_history <- dplyr::left_join(academy_trust_history,
                                            dplyr::select(trust_list, group_id, group_name, group_type),
                                            by = "group_id")

  academy_trust_history
}
