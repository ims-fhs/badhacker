#' Planned labor from tmin to tmax as step function
#'
#' @param vehicle A sim_vehicle
#' @param id An integer, the vehicle ID
#' @param year An integer, the year
#' @param tmin An integer, seconds from origin
#' @param tmax An integer, seconds from origin
#' @param origin_date A POSIXct, the origin date
#'
#' @return A stepfun, the planned labor
planned_labor_vehicle <- function(vehicle, year, tmin, tmax, origin_date) {
  assertthat::assert_that(nrow(vehicle) == 1)
  vehicle <- sim911:::update_vehicles(year = year,
                                       origin_date = origin_date,
                                       vehicles = vehicle)

  t_min <- seq(tmin, tmax, by = 60)
  cond_on_duty <- id_on_duty(sim_vehicle = vehicle, t = t_min,
                             origin_date = origin_date)
  if (all(cond_on_duty == 1)) {
    # 24h shift stops at 24h although it might continue the next day
    cond_on_duty[length(cond_on_duty)] <- 0
  }

  f_on_duty <- stepfun(x = t_min, y = c(0, cond_on_duty)) # first value always 0
  # => Question: last value always 0
  return(prune_stepfun(f_on_duty))
}
