#' Local Authority Districts - Boundaries
#'
#' The England subset of Full Extent of the Realm LAD Boundaries UK (December 2018)
#'
#'
#' @format ## `lads_2018`
#' sf with 326 obs. of 11 variables
#' \describe{
#'   \item{objcid}{Row number}
#'   \item{lad18cd}{8 digit LAD code preceded by "E" for England}
#'   \item{lad18nm}{LAD Name}
#'   \item{bng_e}{Easting}
#'   \item{bng_n}{Northing}
#'   \item{long}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{st_areashape}{Polygon area}
#'   \item{st_lengthshape}{Polygon length}
#'   \item{geometry}{Polygon geometry}
#'   ...
#' }
#' @source <https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_Dec_2018_Boundaries_UK_BFE_2022/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json>
"lads_2018"
