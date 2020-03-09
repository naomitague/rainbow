#' Climate Data from Santa Barbara
#'
#' Data from SB-LTER met station, daily meterology
#'
#' @format A data frame with 3287 rows and 11 colums
#' \describe{
#' \item{date}{Year-Month-Day}
#' \item{day}{(1-31)}
#' \item{month}{(1-12)}
#' \item{year}{ (integer)}
#' \item{wy}{year starting Oct 1 (integer) }
#' \item{wyd}{day of water year  (integer) }
#' \item{tavg}{air temperature (degrees C) }
#' \item{vpg}{vapor pressure deficit (MPa) }
#' \item{rnet}{net radiation (kJ/day) }
#' \item{preip}{precipitation (mm/day) }
#' }
#' @source \url{http://sbc.lternet.edu/data/}
#'
"sbdata"
