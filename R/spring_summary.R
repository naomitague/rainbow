#' Summary information about spring climate
#'
#' computes summary information about spring temperature and precipitation
#' @param clim_data  data frame with columns tavg (C)
#'	rain (precip in mm), year, month (integer), day
#' @param months (as integer) to include in spring; default 4,5,6
#' @param spring_out (default FALSE) set to TRUE to output spring precip and temperature for all years
#' @return returns a list containing,
#' \describe{
#'  \item{mean_springT}{mean_springT mean spring temperature (C)}
#'  \item{coldest_spring_yr}{ year with lowest spring temperature  (year)}
#'  \item{warmest_spring_yr}{ year with highest spring temperature  (year)}
#'  \item{coldest_springT}{  lowest spring temperature  (C)}
#'  \item{warmest_springT}{  highest spring temperature  (C)}
#'  \item{mean_springP}{ mean spring precipitation  (mm)}
#'  \item{wettest_spring_yr}{ spring (as year) with highest precip (year)}
#'  \item{wettest_springP}{  highest precip (mm)}
#'  \item{driest_spring_yr}{ spring (as year) with highest precip (year)}
#'  \item{driest_springP}{  highest precip (mm)}
#'  and array of means for all years if spring_out set to TRUE
#'  }

spring_summary = function(clim_data, spring_months = c(4:6), spring_out=FALSE) {

  library(tidyverse)
  spring = clim_data %>% subset(month %in% spring_months)

  S_means_all = spring %>% group_by(year) %>% select(year, tavg, precip) %>% summarize_all(list(mean=mean, sum=sum))


  S_extremes = S_means_all %>% summarize( Tavg_spring = mean(tavg_mean), Pavg_spring=mean(precip_sum),
                                          Tmax_spring = max(tavg_mean), Tmin_spring=min(tavg_mean),
                                          Pmax_spring = max(precip_sum), Pmin_spring=min(precip_sum),
                                          warmest_spring = year[which.max(tavg_mean)],
                                          coldest_spring = year[which.min(tavg_mean)],
                                          wettest_spring = year[which.max(precip_sum)],
                                          driest_spring = year[which.min(precip_sum)])


  if (spring_out)
  return(list(as.list(S_extremes), S_means_all[,c("year","tavg_mean","precip_sum")]))
  else
  return(as.list(S_extremes))

}
