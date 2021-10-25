#' Clean links data
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not provided
#' @noRd
links.data.clean.links <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  urn_info <- links.data.add.info(gias_date)

  # remove proposed to open successors - they aren't yet linked and the URN may change prior to opening
  opened_successors <- dplyr::filter(urn_info, link_establishment_status != "Proposed to open")

  # remove links where the successor and predecessor are both open. These are either splits or the data is incorrect
  ambiguous_links_removed <- dplyr::filter(opened_successors, !(grepl("Open", urn_establishment_status) & grepl("Open", link_establishment_status)))

  # create a flag for instances where the close date of the successor is more than a month before the predecessor closed (a short delay is normal)
  ambiguous_link_flag <- dplyr::mutate(ambiguous_links_removed,
                                       flipped = dplyr::case_when(((urn_close_date - link_close_date) > 30) | (urn_establishment_status == "Open" & link_establishment_status == "Closed") ~ 1,
                                                                  TRUE ~ 0), # could these be splits?
                                       split = dplyr::case_when(urn_establishment_status == "Open" & link_establishment_status == "Open" ~ 1,
                                                                TRUE ~ 0))
  # select just those links flagged as "flipped"
  flipped_links <- dplyr::filter(ambiguous_link_flag, flipped == 1)

  # correct variable names to reflect urn <-> link_urn and info
  corrected_flipped_links <- dplyr::select(flipped_links,
                                           predecessor_urn = link_urn,
                                           predecessor_close_date = link_close_date,
                                           link_established_date,
                                           predecessor_establishment_status = link_establishment_status,
                                           predecessor_name = link_name,
                                           predecessor_open_date = link_open_date,
                                           predecessor_phase_of_education = link_phase_of_education,
                                           predecessor_reason_establishment_opened = link_reason_establishment_opened,
                                           predecessor_statutory_high_age = link_statutory_high_age,
                                           predecessor_statutory_low_age = link_statutory_low_age,
                                           predecessor_link = link_urn,
                                           link_link_type = predecessor_link_type,
                                           predecessor_link_type = successor_link_type,
                                           link_urn = urn,
                                           link_close_date = urn_close_date,
                                           link_establishment_status = urn_establishment_status,
                                           link_name = urn_name,
                                           link_open_date = urn_open_date,
                                           link_phase_of_education = urn_phase_of_education,
                                           link_reason_establishment_closed = urn_reason_establishment_closed,
                                           link_statutory_high_age = urn_statutory_high_age,
                                           link_statutory_low_age = urn_statutory_low_age)

  corrected_flipped_links <- dplyr::select(corrected_flipped_links,
                                           urn = predecessor_urn,
                                           urn_close_date = predecessor_close_date,
                                           link_established_date,
                                           urn_establishment_status = predecessor_establishment_status,
                                           urn_name = predecessor_name,
                                           urn_open_date = predecessor_open_date,
                                           urn_phase_of_education = predecessor_phase_of_education,
                                           urn_reason_establishment_opened = predecessor_reason_establishment_opened,
                                           urn_statutory_high_age = predecessor_statutory_high_age,
                                           urn_statutory_low_age = predecessor_statutory_low_age,
                                           link_urn,
                                           successor_link_type = link_link_type,
                                           predecessor_link_type,
                                           link_urn,
                                           link_close_date,
                                           link_establishment_status,
                                           link_name,
                                           link_open_date,
                                           link_phase_of_education,
                                           link_reason_establishment_closed,
                                           link_statutory_high_age,
                                           link_statutory_low_age)

  # remove those links flagged as ambiguous
  no_ambiguous_links <- dplyr::filter(ambiguous_link_flag,
                                      flipped != 1 & split != 1)

  # merge the corrected_flipped_links with the non-ambiguous links
  corrected_links <- dplyr::bind_rows(no_ambiguous_links, corrected_flipped_links)

  corrected_links <- dplyr::distinct(corrected_links,
                                     urn, link_urn,
                                     .keep_all = TRUE)

  identify_circular_links <- dplyr::left_join(dplyr::select(corrected_links, urn, link_urn),
                                              dplyr::select(corrected_links, urn, link_urn),
                                              by = c("urn" = "link_urn"))

  identify_circular_links <- dplyr::mutate(identify_circular_links,
                                           same_urn = dplyr::case_when(is.na(urn.y) ~ 0,
                                                                       link_urn == urn.y ~ 1,
                                                                       TRUE ~ 0))

  non_circular_links <- dplyr::left_join(corrected_links,
                                         dplyr::select(identify_circular_links, urn, link_urn, same_urn),
                                         by = c("urn", "link_urn"))


  non_circular_links <- dplyr::filter(non_circular_links, same_urn != 1)

  identify_splits <- dplyr::group_by(non_circular_links, urn)

  identify_splits <- dplyr::mutate(identify_splits,
                                   n_schools = dplyr::n())

  splits_removed <- dplyr::filter(identify_splits, n_schools == 1)

  final_links_clean <- dplyr::select(splits_removed,
                                     -n_schools, -flipped, -split, -same_urn,
                                     -urn_open_date, -link_close_date,
                                     -urn_reason_establishment_opened, -link_reason_establishment_closed)

  final_links_clean
}


#' Intermediary function for iteratively joining the urn link_urn data to identify all links to the current_urn
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @param cut_off_date the cut off date for current school opening, defaults to first day of current month if not entered
#' @noRd
identify.links.iteratively <- function(urn_list, gias_date, cut_off_date){
  establishment_status_data <- state.schools.data(gias_date)

  # Create base table for looping
  current_urn <- dplyr::filter(establishment_status_data,
                               (is.na(open_date) | open_date <= as.Date(cut_off_date)))

  current_urn <- dplyr::transmute(current_urn,
                                  urn = urn,
                                  current_urn = urn,
                                  n_links = 0)

  # cut links data by cut off date
  final_links_cut_off <- dplyr::filter(urn_list,
                                       (is.na(link_open_date) | link_open_date <= as.Date(cut_off_date))  &
                                         urn_close_date <= as.Date(cut_off_date))

  final_links_cut_off <- dplyr::select(urn_list,
                                       urn,
                                       link_urn)


  # While loop join successors
  while (TRUE) {
    current_urn <- dplyr::left_join(current_urn,
                                    final_links_cut_off,
                                    by = c("current_urn" = "urn"))

    if (all(is.na(current_urn$link_urn))) {
      current_urn <- dplyr::select(current_urn,
                                   -link_urn)
      break
    }

    current_urn <- dplyr::mutate(current_urn,
                                 n_links = dplyr::if_else(is.na(link_urn), n_links, n_links + 1),
                                 current_urn = dplyr::if_else(is.na(link_urn), current_urn, link_urn))

    current_urn <- dplyr::select(current_urn,
                                 -link_urn)

    current_urn <- dplyr::distinct(current_urn)
  }

  all_urn_links <- dplyr::select(current_urn,
                                 urn,
                                 current_urn)

  all_urn_links <- dplyr::mutate(all_urn_links,
                                 urn = as.character(urn),
                                 current_urn = as.character(current_urn)) # for easier matching consistency

  all_urn_links
}


#' Prepare Links Data for Ofsted Methodology Linking
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @noRd
prep.ofsted.links.data <- function(gias_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  final_links_clean <- links.data.clean.links(gias_date)

  # use link_type from links data to remove links flagged as amalgamations, mergers, or splits
  flag_amalgamations_mergers_splits <- dplyr::mutate(final_links_clean,
                                                     discounted_predecessor = dplyr::case_when(grepl('amal|merg|split', .data$successor_link_type , ignore.case = TRUE) ~ 1,
                                                                                               grepl('amal|merg|split', .data$predecessor_link_type , ignore.case = TRUE) ~ 1,
                                                                                               is.na(urn_close_date) | is.na(link_open_date) ~ 0, # flag as explicitly not amalgamations so they're not picked up by the next step
                                                                                               urn_close_date > (link_open_date + 365) ~ 1, # mark as amalgamation by date where school closed more than a year after open date
                                                                                               TRUE ~ 0))

  amalgamations_mergers_splits_removed_by_tag <- dplyr::filter(flag_amalgamations_mergers_splits, discounted_predecessor != 1)

  # use GIAS data on reasons for closure and opening to remove amalgamations, merges, and splits
  amalgamations_mergers_splits_removed_by_gias <- dplyr::filter(amalgamations_mergers_splits_removed_by_tag,
                                                                (is.na(urn_reason_establishment_closed) | urn_reason_establishment_closed != 1) &
                                                                  (is.na(link_reason_establishment_opened) | link_reason_establishment_opened != 1))

  # use the number of predecessors to a single link URN to remove any remaining merges
  merges_removed_by_n_predecessors <- dplyr::group_by(amalgamations_mergers_splits_removed_by_gias,
                                                      link_urn)

  merges_removed_by_n_predecessors <- dplyr::mutate(merges_removed_by_n_predecessors,
                                                    n_predecessors = dplyr::n())

  merges_removed_by_n_predecessors <- dplyr::ungroup(merges_removed_by_n_predecessors)

  merges_removed_by_n_predecessors <- dplyr::filter(merges_removed_by_n_predecessors,
                                                    n_predecessors == 1)

  # use school phase and high and low age changes to remove instances where an infant or junior school has become a primary
  infant_junior_to_primary_flag <- dplyr::mutate(merges_removed_by_n_predecessors,
                                                 infant_to_primary_age = dplyr::case_when(urn_phase_of_education == "Primary" & urn_statutory_high_age <= 9 & link_statutory_high_age > 9 ~ 1,
                                                                                          TRUE ~ 0),
                                                 junior_to_primary_age = dplyr::case_when(link_phase_of_education == "Primary" & urn_statutory_low_age >= 7 & link_statutory_low_age < 7 ~ 1,
                                                                                          TRUE ~ 0),
                                                 one_urn_diff = dplyr::case_when((as.numeric(link_urn) == as.numeric(urn) + 1 | as.numeric(urn) == as.numeric(link_urn) + 1) &
                                                                                   (is.na(link_open_date) | link_open_date < urn_close_date) ~ 1,
                                                                                 TRUE ~ 0))

  # use school names to flag instances where an infant/first or middle/junior has become a primary
  infant_junior_to_primary_flag <- dplyr::mutate(infant_junior_to_primary_flag,
                                                 # flag predecessors that are specifically infant or junior
                                                 infant_junior = xor(grepl("infant|first|lower", urn_name, ignore.case = TRUE),
                                                                     grepl("junior|\\bmiddle\\b|upper", urn_name, ignore.case = TRUE)),
                                                 # flag predecessors that don't have the word "primary" in their names
                                                 infant_junior_specific = dplyr::if_else(!grepl("\\bprimary\\b", urn_name, ignore.case = TRUE), "TRUE", "FALSE"),
                                                 # flag successors that have the word "primary" in their names
                                                 primary = dplyr::if_else(grepl("\\bprimary\\b", link_name, ignore.case = TRUE), "TRUE", "FALSE"),
                                                 # flag successors that are not specifically infant or junior
                                                 primary_specific = dplyr::if_else(!grepl("infant|first|junior", link_name, ignore.case = TRUE), "TRUE", "FALSE"),
                                                 primary_merge = dplyr::if_else(infant_junior == "TRUE" & infant_junior_specific == "TRUE" & primary == "TRUE" & primary_specific == "TRUE", 1, 0))

  # use flags to eliminate infant/first/lower or middle/junior/upper to primary
  no_infant_junior_to_primary <- dplyr::filter(infant_junior_to_primary_flag,
                                               infant_to_primary_age != 1 &
                                                 junior_to_primary_age != 1 &
                                                 one_urn_diff != 1 &
                                                 primary_merge != 1)


  urn_list_with_dates <- dplyr::select(no_infant_junior_to_primary,
                                       urn,
                                       link_urn,
                                       link_open_date,
                                       urn_close_date)

  urn_list_with_dates
}


#' Identify URN Links
#'
#' The `all.predecessors.no.splits()` function is used to identify all state funded establishments that link to the most recent single URN for a state funded establishment, excluding splits. \cr
#' While `ofsted.urn.links()` function identifies only those links for state funded establishments that would be considered links using the Ofsted methodology.
#'
#' @usage all.predecessors.no.splits(gias_date, open_date)
#'
#' @param gias_date default is to get GIAS data from the 1st of the current month. However, to reproduce historic analysis or to use more recent data the date can be set using the "%Y-%m-%d" format.
#' @param cut_off_date default is to use the 1st of the current month for open schools. However, to reproduce historic analysis or identify more recently open schools the date can be set using the "%Y-%m-%d" format.
#'
#' @details The resulting df for `all.predecessors.no.splits` can be used to link and aggregate data from other sources (e.g. school census, performance data). While the resulting df from `ofsted.urn.links` should be used primarily to link Ofsted data to the current school.
#'
#' @return A df of `urn` and `current_urn` identifiers. This can be used to link data from other sources (e.g. school census, performance data)
#' * `urn`: All past and present URNs
#' * `current_urn`: The most recent URN relating to the URN shown in the `urn` variable. This may be the same as the `urn`, or a successor. `current_urn` establishments may be closed if there has been no successor.
#'
#' @examples all.predecessors.no.splits("2021-06-01", "2021-06-01")
#' @examples ofsted.urn.links("2021-06-01", "2021-06-01")
#'
#' @describeIn all.predecessors.no.splits Identifies all predecessor-successor links as at a specified date, using the GIAS all establishment data released on the date provided and open on the date provided.

all.predecessors.no.splits <- function(gias_date, cut_off_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # set cut_off_date to start of month if not provided
  if(missing(cut_off_date)){
    cut_off_date <- as.Date(cut(Sys.Date(), "month"))
  }

  urn_list <- links.data.clean.links(gias_date)

  all_urn_links <- identify.links.iteratively(urn_list, gias_date, cut_off_date)

  all_urn_links
}


#' @describeIn all.predecessors.no.splits Identifies predecessor-successor links following the Ofsted methodology as at a specified date, using the GIAS all establishment data released on the date provided and open on the date provided.

ofsted.urn.links <- function(gias_date, cut_off_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # set cut_off_date to start of month if not provided
  if(missing(cut_off_date)){
    cut_off_date <- as.Date(cut(Sys.Date(), "month"))
  }

  urn_list <- prep.ofsted.links.data(gias_date)

  ofsted_urn_links <- identify.links.iteratively(urn_list, gias_date, cut_off_date)

  ofsted_urn_links
}


