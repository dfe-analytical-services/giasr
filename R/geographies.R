#' Get Establishment Location Data
#'
#' `lad.lookup` uses eastings and northings data from GIAS and LAD boundary data from the ONS to identify historic LAD locations of schools from 2018.
#' `priority.areas` uses eastings and northings data from GIAS and LAD LAD boundary data from the ONS to identify schools in target areas for ONE and OA place-based programmes.
#'
#' @param gias_date the file date of the GIAS cut, defaults to first day of current month if not entered.
#' @param schools_open_date the date on which the schools were open, defaults to returning data for all establishments when not provided
#'
#' @details NOTE: due to the size of the datasets and amount of processing required to output the dataframe this function may take some time to run.
#'
#' @usage lad.lookup(gias_date, schools_open_date)
#'
#' @return Dataframe of schools with 2018, 2019, and 2020 LAD locations.
#' @return `priority.areas` includes OA and ONE LADs where applicable
#'
#' @examples lad.lookup("2021-06-01", "2021-06-01")
#' @examples priority.areas("2021-06-01", "2021-06-01")
#'
#' @export

lad.lookup <- function(gias_date, schools_open_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  # create a temporary directory
  tmp_dir <- tempdir()

  gias_data <- gias.estab.fields(gias_date)

  state_schools <- state.schools.data(gias_date)

  state_schools <- dplyr::select(state_schools,
                                 .data$urn,
                                 .data$open_date,
                                 .data$close_date)

  gias_location_data <- dplyr::select(gias_data,
                                      .data$urn,
                                      .data$easting,
                                      .data$northing,
                                      .data$district_administrative_name,
                                      .data$district_administrative_code)


  gias_location_data <- dplyr::left_join(state_schools,
                                         gias_location_data,
                                         by = "urn")

  # get open schools list if open_date provided
  if(!missing(schools_open_date)){
    gias_location_data <- dplyr::filter(gias_location_data,
                                        (is.na(.data$open_date) | .data$open_date <= schools_open_date) &
                                          (is.na(.data$close_date) | .data$close_date > schools_open_date))
  }

  # Create a list of LADs that have changed between 2018 and 2021 (LADs listed in GIAS)
  lad_changes <- c("Bournemouth, Christchurch and Poole",
                   "Somerset West and Taunton",
                   "West Suffolk",
                   "East Suffolk",
                   "Dorset",
                   "Buckinghamshire",
                   "North Northamptonshire",
                   "West Northamptonshire")

  # get LAD info for those which have unchanged
  school_location_data <- dplyr::mutate(gias_location_data,
                                        lad_2018 = dplyr::case_when(!(.data$district_administrative_name %in% lad_changes) ~ .data$district_administrative_name))

  schools_w_2018_lads <- dplyr::filter(school_location_data,
                                       !is.na(.data$lad_2018))

  schools_w_2018_lads <- dplyr::select(schools_w_2018_lads,
                                       .data$urn,
                                       .data$lad_2018,
                                       lad_2018cd = .data$district_administrative_code)

  #Commented out lines 81-94 as this was leading to errors
  ## GET LAD SPATIAL DATA
  ## NOTE: This is to the full resolution file as the lower resolution data omits schools that are on the boundaries cut out when generalised this is therefore a large download
  #geojson_url <- "http://geoportal1-ons.opendata.arcgis.com/datasets/26d3055941bb4434acad33cc13cd1bb0_0.geojson"
  #
  ## alternative link if the above fails
  ##geojson_url <- "https://opendata.arcgis.com/datasets/c8165cd6d0e7486699dccaa92b421469_0.geojson" # NB this is UK boundaries
  #
  ## create path for geojson data
  #file_path_lads_2018 <- file.path(tmp_dir, "location_data_2018.geojson")
  #
  ## download from the URL to the filepath specified
  #if(!file.exists(file_path_lads_2018)){
  #  utils::download.file(geojson_url, mode = "wb", method = "libcurl", destfile = file_path_lads_2018)
  #}

  #Hot fix that should work for all users, assuming they have synced up to our onedrive - should be updated to be more general
  User<-Sys.info()[["user"]]
  lads_2018 <- sf::read_sf(Paste0("C:/",User,"/rdrake/OneDrive - Department for Education/Documents - Infrastructure & Funding Analysis/School_Educational_Performance/School_Improvement/place-based-analyses/Data/lad_2018.geojson"))

  no_2018_lad <- dplyr::filter(school_location_data,
                               is.na(.data$lad_2018))

  no_2018_lad <- dplyr::filter(no_2018_lad,
                               !is.na(.data$easting) & .data$easting != "0")

  no_2018_lad <- dplyr::transmute(no_2018_lad,
                                  .data$urn,
                                  longitude = as.numeric(.data$easting),
                                  latitude = as.numeric(.data$northing))

  # convert eastings and northings to long lat
  no_2018_lad <- sf::st_as_sf(no_2018_lad,
                              coords = c("longitude", "latitude"),
                              crs = sf::st_crs(27700))

  no_2018_lad <- sf::st_transform(no_2018_lad,
                                  crs = sf::st_crs(4326))

  no_2018_lad$intersection <- as.integer(sf::st_intersects(no_2018_lad,lads_2018))

  no_2018_lad$lad_2018 <- lads_2018$lad18nm[no_2018_lad$intersection]

  no_2018_lad$lad_2018cd <- lads_2018$lad18cd[no_2018_lad$intersection]

  schools_w_e_n <- as.data.frame(no_2018_lad)

  schools_w_e_n <- dplyr::select(schools_w_e_n,
                                 -.data$intersection,
                                 -.data$geometry)

  # get schools that have LAD info but no eastings/northings data
  schools_no_e_n <- dplyr::filter(school_location_data,
                                  is.na(.data$lad_2018))

  schools_no_e_n <- dplyr::filter(schools_no_e_n,
                                  is.na(.data$easting) | .data$easting == 0)


  schools_no_e_n <- dplyr::transmute(schools_no_e_n,
                                     .data$urn,
                                     lad_2018 = dplyr::case_when(is.na(.data$district_administrative_name) ~ "no data",
                                                                 .data$district_administrative_name %in% lad_changes ~ paste0(.data$district_administrative_name, "*"),
                                                                 TRUE ~ .data$district_administrative_name),
                                     lad_2018cd = dplyr::case_when(is.na(.data$district_administrative_code) ~ "",
                                                                   .data$district_administrative_name %in% lad_changes ~ paste0(.data$district_administrative_code, "*"),
                                                                   TRUE ~ .data$district_administrative_code))

  school_lads_2018 <- dplyr::bind_rows(schools_w_2018_lads,
                                       schools_w_e_n,
                                       schools_no_e_n)

  school_lads <- dplyr::transmute(school_lads_2018,
                                  .data$urn,
                                  .data$lad_2018,
                                  lad_2019 = dplyr::case_when(.data$lad_2018 %in% c("Bournemouth", "Poole", "Christchurch") ~ "Bournemouth, Christchurch and Poole",
                                                              .data$lad_2018 %in% c("East Dorset","North Dorset", "Purbeck", "West Dorset", "Weymouth and Portland") ~ "Dorset",
                                                              .data$lad_2018 %in% c("Suffolk Coastal", "Waveney") ~ "East Suffolk",
                                                              .data$lad_2018 %in% c("Taunton Deane", "West Somerset") ~ "Somerset West and Taunton",
                                                              .data$lad_2018 %in% c("Forest Heath", "St Edmundsbury") ~ "West Suffolk",
                                                              TRUE ~ .data$lad_2018),
                                  lad_2020 = dplyr::case_when(.data$lad_2019 %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe") ~ "Buckinghamshire",
                                                              TRUE ~ .data$lad_2019),
                                  .data$lad_2018cd)

  school_lads
}

#' @describeIn lad.lookup provides a df of all state funded establishments with 2018, 2019, and 2020 LAD locations and OA and/or ONE area membership.
#' @export
priority.areas <- function(gias_date, schools_open_date){
  # set gias_date to start of month if not provided
  if(missing(gias_date)){
    gias_date <- as.Date(cut(Sys.Date(), "month"))
  }

  lad_lookup_table <- lad.lookup(gias_date, schools_open_date)

  # create list of ONE LAD codes
  ONE_LADs <- c("Darlington",
                "Durham",
                "Gateshead",
                "Hartlepool",
                "Middlesbrough",
                "Newcastle upon Tyne",
                "North Tyneside",
                "Northumberland",
                "Redcar and Cleveland",
                "Stockton-on-Tees",
                "Sunderland")

  # create list of OA lad codes
  oa_lad_code <- c("E06000009",
                   "E08000004",
                   "E07000168",
                   "E06000015",
                   "E07000148",
                   "E07000191",
                   "E08000032",
                   "E08000017",
                   "E06000021",
                   "E07000009",
                   "E07000010",
                   "E07000202",
                   "E07000062",
                   "E10000027")

  ONE_OAs_flagged <- dplyr::mutate(lad_lookup_table,
                                   ONE = dplyr::case_when(.data$lad_2018 %in% ONE_LADs ~ .data$lad_2018),
                                   OA = dplyr::case_when(.data$lad_2018cd %in% oa_lad_code ~ .data$lad_2018))
}
