#' Calculate all times per day and per vehicle id
#'
#' night shifts have to be handled via "name"
#'
#' @param missions A data.frame, the sim_missions
#' @param vehicles A data.frame, the sim_vehicles
#' @param vehicle_id An integer, the vehicle ID
#' @param origin_date A POSIXct, the origin date
#' @param wdays An array of weekdays (1-7)
#' @param months An array of months (1-12)
#' @param idleovertime2overtime A boolean, whether idletime outside planned labor
#' should be added to overtime or not (then it is added to idle time)
#'
#' @return A list of stepfun, the sum_planned_labor, sum_missions_labor,
#' sum_labor, sum_overtime, sum_idletime, and the sum_idle_overtime
calculate_times_per_day_id <- function(missions, vehicles, vehicle_id,
                                       origin_date, wdays, months,
                                       idleovertime2overtime) {
  # number of missions depends on vehicle
  # => Bug in calculate_times_id(), see (***) below!!!
  # Difference between different scenarios may occur in planned labor as
  # calculate_times_id() below calculates labor.
  # Solution:
  # =========
  # * calculate_times_id() takes planned labor as input (stepfun)
  # * planned labor needs only take time span of missions but nothing else.
  #   New function call
  #   planned_labor_vehicle(vehicle, # or vehicles + ID
  #     year = year, # only one year => needs iteration over year
  #                                     (& vehicle name / ID& ??)
  #     min_sec = min_sec, # from missions
  #     max_sec = max_sec, # from missions
  #     origin_date = origin_date) # from missions
  m <- missions[missions$vehicle_id == vehicle_id, ]
  if (nrow(m) > 0) {
    # (***) e.g. in case of hist_scenario
    min_sec <- 3600*24*floor(min(m$t_alarm_sec)/3600/24)
    max_sec <- 3600*24*ceiling(max(
      m$t_alarm_sec + m$dt_to_launch + m$dt_to_poa + m$dt_to_completion)/3600/24)
  } else {
    # (***) e.g. in case of sim_scenario_hist
    min_sec <- 0
    max_sec <- 24*3600
  }

  # The following function also calculates planned labor. See below
  # times$planned_labor...
  # Bug: planned labor has to be independent of missions! Only the time span of
  # missions should be of interest!
  times <- calculate_times_id( # ........................... Bug planned_labor!!!
    missions = missions,
    vehicles = vehicles,
    vehicle_id = vehicle_id,
    origin_date = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), # Why fixed date?
    idleovertime2overtime = idleovertime2overtime)

  tvals <- seq(min_sec, max_sec, by = 24*3600) # in steps of days in seconds
  if (!is.null(wdays)) {
    cond_in_wdays <- in_wdays(tvals, origin_date, wdays)
    f_in_wdays <- stepfun(x = tvals, y = c(0, cond_in_wdays))
  } else {
    f_in_wdays <- stepfun(x = 0, y = c(1, 1))
  }
  if (!is.null(months)) {
    cond_in_months <- in_months(tvals, origin_date, months)
    f_in_months <- stepfun(x = tvals, y = c(0, cond_in_months))
  } else {
    f_in_months <- stepfun(x = 0, y = c(1, 1))
  }

  available <- prune_stepfun(f_in_wdays * f_in_months)
  planned_labor <- prune_stepfun(times$planned_labor * available)
  labor <- prune_stepfun(times$labor * available)
  overtime <- prune_stepfun(times$overtime * available)
  idletime <- prune_stepfun(times$idletime * available)
  # if (idle2overtimes) {
  #   f <- times$idletime + times$idle_overtime
  #   idletime <- prune_stepfun(f * available)
  # } else {
  #   idletime <- prune_stepfun(times$idletime * available)
  # }

  # browser()
  # all_days <- as.integer(format(origin_date + tvals, "%j")) # not used
  # in_day_of_year(tvals, origin_date, all_days[1])
  sum_planned_labor <- planned_labor * stepfun(x = tvals[1:2], y = c(0, 1, 0))
  sum_labor <- labor * stepfun(x = tvals[1:2], y = c(0, 1, 0))
  sum_overtime <- overtime * stepfun(x = tvals[1:2], y = c(0, 1, 0))
  sum_idletime <- idletime * stepfun(x = tvals[1:2], y = c(0, 1, 0))
  if (length(tvals) > 2) {
    for (i in 2:(length(tvals)-1)) {
      f <- shift_stepfun(planned_labor * stepfun(x = tvals[i:(i+1)],
                                                 y = c(0, 1, 0)),
                         dx = -tvals[i])
      sum_planned_labor <- prune_stepfun(sum_planned_labor + f)

      f <- shift_stepfun(labor * stepfun(x = tvals[i:(i+1)], y = c(0, 1, 0)),
                         dx = -tvals[i])
      sum_labor <- prune_stepfun(sum_labor + f)

      f <- shift_stepfun(overtime * stepfun(x = tvals[i:(i+1)], y = c(0, 1, 0)),
                         dx = -tvals[i])
      sum_overtime <- prune_stepfun(sum_overtime + f)

      f <- shift_stepfun(idletime * stepfun(x = tvals[i:(i+1)], y = c(0, 1, 0)),
                         dx = -tvals[i])
      sum_idletime <- prune_stepfun(sum_idletime + f)
    }
  } else {
    emin <- 3600*24*floor(min(missions$t_alarm_sec)/3600/24)
    emax <- 3600*24*ceiling(max(missions$t_alarm_sec +
                                  missions$dt_to_launch +
                                  missions$dt_to_poa +
                                  missions$dt_to_completion)/3600/24)
    etvals <- seq(emin, emax, by = 24*3600) # in steps of days
    if (length(etvals) > 2) {
      for (i in 2:(length(etvals)-1)) {
        f <- shift_stepfun(planned_labor * stepfun(x = etvals[i:(i+1)],
                                                   y = c(0, 1, 0)),
                           dx = -etvals[i])
        sum_planned_labor <- prune_stepfun(sum_planned_labor + f)
      }
    }
  }

  return(list(sum_planned_labor = sum_planned_labor,
              sum_labor = sum_labor,
              sum_overtime = sum_overtime,
              sum_idletime = sum_idletime))
}

calculate_times_per_day_name <- function(missions, vehicles, vehicle_name,
                                       origin_date, wdays, months,
                                       idleovertime2overtime = TRUE) {
  ids <- vehicles$id[vehicles$name == vehicle_name]

  sum_planned_labor <- stepfun(x = 0, y = c(0, 0))
  sum_labor <- stepfun(x = 0, y = c(0, 0))
  sum_overtime <- stepfun(x = 0, y = c(0, 0))
  sum_idletime <- stepfun(x = 0, y = c(0, 0))
  for (i in ids) {
    lf <- calculate_times_per_day_id(missions, vehicles, i,
                                     origin_date, wdays, months,
                                     idleovertime2overtime)
    sum_planned_labor <- prune_stepfun(sum_planned_labor + lf$sum_planned_labor)
    sum_labor <- prune_stepfun(sum_labor + lf$sum_labor)
    sum_overtime <- prune_stepfun(sum_overtime + lf$sum_overtime)
    sum_idletime <- prune_stepfun(sum_idletime + lf$sum_idletime)
  }

  return(list(sum_planned_labor = sum_planned_labor,
              sum_labor = sum_labor,
              sum_overtime = sum_overtime,
              sum_idletime = sum_idletime))
}
