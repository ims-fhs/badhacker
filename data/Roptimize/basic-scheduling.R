#' Calculate whether times belong to certain week days
#'
#' @param t An integer, seconds since origin date
#' @param origin_date A POSIXct, the origin date
#' @param wdays An array of integers (1 - 7, 1 = Monday)
#'
#' @return An array of 0 and 1
in_wdays <- function(t, origin_date, wdays) {
  wdays <- as.integer(format(origin_date + t, "%u"))
  return(as.integer(wdays %in% wdays))
}

#' Calculate whether times belong to certain months
#'
#' @param t An integer, seconds since origin date
#' @param origin_date A POSIXct, the origin date
#' @param months An array of integers (1 - 12)
#'
#' @return An array of 0 and 1
in_months <- function(t, origin_date, months) {
  y <- as.integer(format(origin_date + t, "%m"))
  return(as.integer(y %in% months))
}

#' Calculate whether times belong to a certain day of the year
#'
#' @param t An integer, seconds since origin date
#' @param origin_date A POSIXct, the origin date
#' @param day_of_year An array of integers (1 - 366)
#'
#' @return An array of 0 and 1
in_day_of_year <- function(t, origin_date, day_of_year) {
  y <- as.integer(format(origin_date + t, "%j"))
  return(as.integer(y %in% day_of_year))
}

#' Calculate times "on duty"
#'
#' Vectorized version, needed in planned_labor_vehicle_id()
#'
#' @param sim_vehicle A sim_vehicle
#' @param t An integer, the time since origin date. At present, minutes are used.
#' @param origin_date A POSIXct, the origin date
#'
#' @return An array of integers (0 or 1)
id_on_duty <- function(sim_vehicle, t, origin_date) {
  assertthat::assert_that(nrow(sim_vehicle) == 1)

  cond1 <- sim_vehicle$shift_from_simdate <= simtimer::sim_date(t) &
    sim_vehicle$shift_to_simdate >= simtimer::sim_date(t) &
    sim_vehicle$shift_from_simtime <= simtimer::sim_time(t) &
    sim_vehicle$shift_to_simtime > simtimer::sim_time(t)
  wdays <- as.integer(format(origin_date + t, "%u"))
  wdays_schedule <- as.integer(unlist(strsplit(sim_vehicle$shift_weekday, ",")))
  cond2 <- wdays %in% wdays_schedule
  cond <- cond1 & cond2 # correct => the change from 0 => 1 or 1 => 0
  return(as.integer(cond))
}

