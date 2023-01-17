#' 2018 LADs Geospatial data - sf
#'
#' Digital vector boundaries for Local Authority Districts, in Great Britain, as at December 2018.
#' The BFE boundaries are full resolution – extent of the realm (usually this is the Mean Low Water
#' mark but in some cases boundaries extend beyond this to include off shore islands).
#' Contains both Ordnance Survey and ONS Intellectual Property Rights. Used in giasr for historic
#' analysis where LAD a school is in is important and where 2018 LADs are still used for
#' program eligibility
#'
#' @format ## `who`
#' sf type data with 391 rows and 11 columns:
#' \describe{
#'   \item{objectid}{Row number}
#'   \item{lad18cd}{LAD 2018 code}
#'   \item{lad18nm}{LAD 2018 name}
#'   \item{lad18nmw}{empty}
#'   \item{bng_e}{British National Grid - Eastings}
#'   \item{bng_n}{British National Grid - Northings}
#'   \item{long}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{st_areashape}{polygonal geometry information}
#'   \item{st_lengthshape}{polygonal geometry information}
#'   \item{geometry}{polygonal geometry information}
#'   ...
#' }
#' @source <https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2018-full-extent-boundaries-gb/about>
"lad_2018"
