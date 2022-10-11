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
  opened_successors <- dplyr::filter(urn_info, .data$link_establishment_status != "Proposed to open")

  # remove links where the successor and predecessor are both open. These are either splits or the data is incorrect
  ambiguous_links_removed <- dplyr::filter(opened_successors, !(grepl("Open", .data$urn_establishment_status) & grepl("Open", .data$link_establishment_status)))

  # create a flag for instances where the close date of the successor is more than a month before the predecessor closed (a short delay is normal)
  ambiguous_link_flag <- dplyr::mutate(ambiguous_links_removed,
                                       flipped = dplyr::case_when(((.data$urn_close_date - .data$link_close_date) > 30) | (.data$urn_establishment_status == "Open" & .data$link_establishment_status == "Closed") ~ 1,
                                                                  TRUE ~ 0), # could these be splits?
                                       split = dplyr::case_when(.data$urn_establishment_status == "Open" & .data$link_establishment_status == "Open" ~ 1,
                                                                TRUE ~ 0))
  # select just those links flagged as "flipped"
  flipped_links <- dplyr::filter(ambiguous_link_flag, .data$flipped == 1)

  # correct variable names to reflect urn <-> link_urn and info
  corrected_flipped_links <- dplyr::select(flipped_links,
                                           predecessor_urn = .data$link_urn,
                                           predecessor_close_date = .data$link_close_date,
                                           .data$link_established_date,
                                           predecessor_establishment_status = .data$link_establishment_status,
                                           predecessor_name = .data$link_name,
                                           predecessor_open_date = .data$link_open_date,
                                           predecessor_phase_of_education = .data$link_phase_of_education,
                                           predecessor_reason_establishment_opened = .data$link_reason_establishment_opened,
                                           predecessor_statutory_high_age = .data$link_statutory_high_age,
                                           predecessor_statutory_low_age = .data$link_statutory_low_age,
                                           predecessor_link = .data$link_urn,
                                           link_link_type = .data$predecessor_link_type,
                                           predecessor_link_type = .data$successor_link_type,
                                           link_urn = .data$urn,
                                           link_close_date = .data$urn_close_date,
                                           link_establishment_status = .data$urn_establishment_status,
                                           link_name = .data$urn_name,
                                           link_open_date = .data$urn_open_date,
                                           link_phase_of_education = .data$urn_phase_of_education,
                                           link_reason_establishment_closed = .data$urn_reason_establishment_closed,
                                           link_statutory_high_age = .data$urn_statutory_high_age,
                                           link_statutory_low_age = .data$urn_statutory_low_age)

  corrected_flipped_links <- dplyr::select(corrected_flipped_links,
                                           urn = .data$predecessor_urn,
                                           urn_close_date = .data$predecessor_close_date,
                                           .data$link_established_date,
                                           urn_establishment_status = .data$predecessor_establishment_status,
                                           urn_name = .data$predecessor_name,
                                           urn_open_date = .data$predecessor_open_date,
                                           urn_phase_of_education = .data$predecessor_phase_of_education,
                                           urn_reason_establishment_opened = .data$predecessor_reason_establishment_opened,
                                           urn_statutory_high_age = .data$predecessor_statutory_high_age,
                                           urn_statutory_low_age = .data$predecessor_statutory_low_age,
                                           .data$link_urn,
                                           successor_link_type = .data$link_link_type,
                                           .data$predecessor_link_type,
                                           .data$link_urn,
                                           .data$link_close_date,
                                           .data$link_establishment_status,
                                           .data$link_name,
                                           .data$link_open_date,
                                           .data$link_phase_of_education,
                                           .data$link_reason_establishment_closed,
                                           .data$link_statutory_high_age,
                                           .data$link_statutory_low_age)

  # remove those links flagged as ambiguous
  no_ambiguous_links <- dplyr::filter(ambiguous_link_flag,
                                      .data$flipped != 1 & .data$split != 1)

  # merge the corrected_flipped_links with the non-ambiguous links
  corrected_links <- dplyr::bind_rows(no_ambiguous_links, corrected_flipped_links)

  corrected_links <- dplyr::distinct(corrected_links,
                                     .data$urn, .data$link_urn,
                                     .keep_all = TRUE)

  identify_circular_links <- dplyr::left_join(dplyr::select(corrected_links, .data$urn, .data$link_urn),
                                              dplyr::select(corrected_links, .data$urn, .data$link_urn),
                                              by = c("urn" = "link_urn"))

  identify_circular_links <- dplyr::mutate(identify_circular_links,
                                           same_urn = dplyr::case_when(is.na(.data$urn.y) ~ 0,
                                                                       link_urn == .data$urn.y ~ 1,
                                                                       TRUE ~ 0))

  non_circular_links <- dplyr::left_join(corrected_links,
                                         dplyr::select(identify_circular_links, .data$urn, .data$link_urn, .data$same_urn),
                                         by = c("urn", "link_urn"))


  non_circular_links <- dplyr::filter(non_circular_links, .data$same_urn != 1)

  non_circular_links <- dplyr::distinct(non_circular_links)

  final_links_clean <- dplyr::select(non_circular_links,
                                     -.data$flipped, -.data$split, -.data$same_urn,
                                     -.data$urn_open_date, -.data$link_close_date,
                                     -.data$urn_reason_establishment_opened, -.data$link_reason_establishment_closed)

  final_links_clean
}


#' Intermediary function for iteratively joining the urn link_urn data to identify all links to the current_urn
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered
#' @param cut_off_date the cut off date for current school opening, defaults to first day of current month if not entered
#' @noRd
identify.links.iteratively <- function(urn_list, gias_date, cut_off_date){
  gias_establishment_data <- gias.estab.fields(gias_date)

  establishment_status_data <- dplyr::transmute(gias_establishment_data,
                                                .data$urn,
                                                .data$establishment_name,
                                                .data$type_of_establishment_name,
                                                .data$phase_of_education_name,
                                                open_date = as.Date(.data$open_date, "%d-%m-%Y"),
                                                close_date = as.Date(.data$close_date, "%d-%m-%Y"),
                                                reason_establishment_closed = .data$reason_establishment_closed_code,
                                                reason_establishment_opened = .data$reason_establishment_opened_code,
                                                establishment_status = .data$establishment_status_name,
                                                .data$statutory_low_age,
                                                .data$statutory_high_age)

  # Create base table for looping
  current_urn <- dplyr::filter(establishment_status_data,
                               (is.na(.data$open_date) | .data$open_date <= as.Date(cut_off_date)))

  current_urn <- dplyr::transmute(current_urn,
                                  urn = .data$urn,
                                  current_urn = .data$urn,
                                  n_links = 0)

  # cut links data by cut off date
  final_links_cut_off <- dplyr::filter(urn_list,
                                       (is.na(.data$link_open_date) | .data$link_open_date <= as.Date(cut_off_date))  &
                                         .data$urn_close_date <= as.Date(cut_off_date))

  final_links_cut_off <- dplyr::select(urn_list,
                                       .data$urn,
                                       .data$link_urn)


  # While loop join successors
  while (TRUE) {
    current_urn <- dplyr::left_join(current_urn,
                                    final_links_cut_off,
                                    by = c("current_urn" = "urn"))

    if (all(is.na(current_urn$link_urn))) {
      current_urn <- dplyr::select(current_urn,
                                   -.data$link_urn)
      break
    }

    current_urn <- dplyr::mutate(current_urn,
                                 n_links = dplyr::if_else(is.na(.data$link_urn), .data$n_links, .data$n_links + 1),
                                 current_urn = dplyr::if_else(is.na(.data$link_urn), .data$current_urn, .data$link_urn))

    current_urn <- dplyr::select(current_urn,
                                 -.data$link_urn)

    current_urn <- dplyr::distinct(current_urn)
  }

  all_urn_links <- dplyr::select(current_urn,
                                 .data$urn,
                                 .data$current_urn)

  all_urn_links <- dplyr::mutate(all_urn_links,
                                 urn = as.character(.data$urn),
                                 current_urn = as.character(.data$current_urn)) # for easier matching consistency

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
                                                                                               is.na(.data$urn_close_date) | is.na(.data$link_open_date) ~ 0, # flag as explicitly not amalgamations so they're not picked up by the next step
                                                                                               .data$urn_close_date > (.data$link_open_date + 365) ~ 1, # mark as amalgamation by date where school closed more than a year after open date
                                                                                               TRUE ~ 0))

  amalgamations_mergers_splits_removed_by_tag <- dplyr::filter(flag_amalgamations_mergers_splits,
                                                               .data$discounted_predecessor != 1)

  # use GIAS data on reasons for closure and opening to remove amalgamations, merges, and splits
  amalgamations_mergers_splits_removed_by_gias <- dplyr::filter(amalgamations_mergers_splits_removed_by_tag,
                                                                (is.na(.data$urn_reason_establishment_closed) | .data$urn_reason_establishment_closed != 1) &
                                                                  (is.na(.data$link_reason_establishment_opened) | .data$link_reason_establishment_opened != 1))

  # use the number of successors to a single predecessor URN to remove any remaining splits
  identify_splits <- dplyr::group_by(amalgamations_mergers_splits_removed_by_gias, .data$urn)

  identify_splits <- dplyr::mutate(identify_splits,
                                   n_schools = dplyr::n())

  splits_removed <- dplyr::filter(identify_splits, .data$n_schools == 1)

  # use the number of predecessors to a single link URN to remove any remaining merges
  merges_removed_by_n_predecessors <- dplyr::group_by(splits_removed,
                                                      .data$link_urn)

  merges_removed_by_n_predecessors <- dplyr::mutate(merges_removed_by_n_predecessors,
                                                    n_predecessors = dplyr::n())

  merges_removed_by_n_predecessors <- dplyr::ungroup(merges_removed_by_n_predecessors)

  merges_removed_by_n_predecessors <- dplyr::filter(merges_removed_by_n_predecessors,
                                                    .data$n_predecessors == 1)

  # use school phase and high and low age changes to remove instances where an infant or junior school has become a primary
  infant_junior_to_primary_flag <- dplyr::mutate(merges_removed_by_n_predecessors,
                                                 infant_to_primary_age = dplyr::case_when(.data$urn_phase_of_education == "Primary" & .data$urn_statutory_high_age <= 9 & .data$link_statutory_high_age > 9 ~ 1,
                                                                                          TRUE ~ 0),
                                                 junior_to_primary_age = dplyr::case_when(.data$link_phase_of_education == "Primary" & .data$urn_statutory_low_age >= 7 & .data$link_statutory_low_age < 7 ~ 1,
                                                                                          TRUE ~ 0),
                                                 one_urn_diff = dplyr::case_when((as.numeric(.data$link_urn) == as.numeric(.data$urn) + 1 | as.numeric(.data$urn) == as.numeric(.data$link_urn) + 1) &
                                                                                   (is.na(.data$link_open_date) | .data$link_open_date < .data$urn_close_date) ~ 1,
                                                                                 TRUE ~ 0))

  # use school names to flag instances where an infant/first or middle/junior has become a primary
  infant_junior_to_primary_flag <- dplyr::mutate(infant_junior_to_primary_flag,
                                                 # flag predecessors that are specifically infant or junior
                                                 infant_junior = xor(grepl("infant|first|lower", .data$urn_name, ignore.case = TRUE),
                                                                     grepl("junior|\\bmiddle\\b|upper", .data$urn_name, ignore.case = TRUE)),
                                                 # flag predecessors that don't have the word "primary" in their names
                                                 infant_junior_specific = dplyr::if_else(!grepl("\\bprimary\\b", .data$urn_name, ignore.case = TRUE), "TRUE", "FALSE"),
                                                 # flag successors that have the word "primary" in their names
                                                 primary = dplyr::if_else(grepl("\\bprimary\\b", .data$link_name, ignore.case = TRUE), "TRUE", "FALSE"),
                                                 # flag successors that are not specifically infant or junior
                                                 primary_specific = dplyr::if_else(!grepl("infant|first|junior", .data$link_name, ignore.case = TRUE), "TRUE", "FALSE"),
                                                 primary_merge = dplyr::if_else(.data$infant_junior == "TRUE" & .data$infant_junior_specific == "TRUE" & .data$primary == "TRUE" & .data$primary_specific == "TRUE", 1, 0))

  # use flags to eliminate infant/first/lower or middle/junior/upper to primary
  no_infant_junior_to_primary <- dplyr::filter(infant_junior_to_primary_flag,
                                               .data$infant_to_primary_age != 1 &
                                                 .data$junior_to_primary_age != 1 &
                                                 .data$one_urn_diff != 1 &
                                                 .data$primary_merge != 1)


  urn_list_with_dates <- dplyr::select(no_infant_junior_to_primary,
                                       .data$urn,
                                       .data$link_urn,
                                       .data$link_open_date,
                                       .data$urn_close_date)

  urn_list_with_dates
}


#' Identify URN Links
#'
#' The `urn.links.no.splits()` function is used to identify all state funded establishments that link to the most recent single URN for a state funded establishment, excluding splits. \cr
#' While `ofsted.urn.links()` function identifies only those links for state funded establishments that would be considered links using the Ofsted methodology.
#'
#' @usage urn.links.no.splits(gias_date, cut_off_date)
#' @usage ofsted.urn.links(gias_date, cut_off_date)
#'
#' @param gias_date default is to get GIAS data from the 1st of the current month. However, to reproduce historic analysis or to use more recent data the date can be set using the "%Y-%m-%d" format.
#' @param cut_off_date default is to use the 1st of the current month for open schools. However, to reproduce historic analysis or identify more recently open schools the date can be set using the "%Y-%m-%d" format.
#'
#' @details The resulting df for `urn.links.no.splits` can be used to link and aggregate data from other sources (e.g. school census, performance data). While the resulting df from `ofsted.urn.links` should be used primarily to link Ofsted data to the current school.
#'
#' @return A df of `urn` and `current_urn` identifiers. This can be used to link data from other sources (e.g. school census, performance data)
#' * `urn`: All past and present URNs
#' * `current_urn`: The most recent URN relating to the URN shown in the `urn` variable. This may be the same as the `urn`, or a successor. `current_urn` establishments may be closed.
#'
#' @examples urn.links.no.splits("2021-06-01", "2021-06-01")
#' @examples ofsted.urn.links("2021-06-01", "2021-06-01")
#'
#' @export

urn.links.no.splits <- function(gias_date, cut_off_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # set cut_off_date to start of month if not provided
  if(missing(cut_off_date)){
    cut_off_date <- as.Date(cut(Sys.Date(), "month"))
  }

  urn_list <- links.data.clean.links(gias_date)

  # use the number of successors to a single predecessor URN to remove any remaining splits
  identify_splits <- dplyr::group_by(urn_list, .data$urn)

  identify_splits <- dplyr::mutate(identify_splits,
                                   n_schools = dplyr::n())

  splits_removed <- dplyr::filter(identify_splits, .data$n_schools == 1)

  all_urn_links <- identify.links.iteratively(identify_splits, gias_date, cut_off_date)

  all_urn_links
}


#' @describeIn urn.links.no.splits Identifies predecessor-successor links following the Ofsted methodology as at a specified date, using the GIAS all establishment data released on the date provided and open on the date provided.
#'
#' @export

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

  # ADD BACK IN EXCEPTIONS---
  # add predecessor for 146701 - patch prior to fixing code if the cut date is after 31 December 2018 (link date)
  if(cut_off_date >= as.Date("2018-12-31")) {
    #ofsted_urn_links <- rbind(ofsted_urn_links, c(103232, 146701))
    ofsted_urn_links <- dplyr::mutate(ofsted_urn_links,
                                      current_urn = dplyr::case_when(urn == "103232" ~ "146701",
                                                                     TRUE ~ current_urn))
  }


  # add new current URN 146372 to replace 144772 - patch prior to fixing code
  if(cut_off_date >= as.Date("2018-08-31")) {
    ofsted_urn_links <- dplyr::mutate(ofsted_urn_links,
                                      current_urn = dplyr::case_when(current_urn == "144772" ~ "146372",
                                                                     TRUE ~ current_urn))
  }

  if(cut_off_date >= as.Date("2020-10-01")) {
    #ofsted_urn_links <- rbind(ofsted_urn_links, c(125975, 148184))
    ofsted_urn_links <- dplyr::mutate(ofsted_urn_links,
                                      current_urn = dplyr::case_when(urn == "125975" ~ "148184",
                                                                     TRUE ~ current_urn))
  }

  ofsted_urn_links
}


