#' Calculate idle time outside planned labor
#'
#' Can be attributed to idle time or overtime.
#' Does not work!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#'
#' @param planned_labor A stepfun, the planned labor
#' @param missions_labor A stepfun, all labor in missions
#' @param idletime A stepfun, the idle time
#'
#' @return A stepfun, the idle-overtime
idle_overtime <- function(planned_labor, missions_labor, idletime) {
  plot(missions_labor, col = "blue")
  plot(0.66*planned_labor, add = TRUE, col = "black")
  plot(idletime, add = TRUE, col = "red")
  # browser()

  x <- get("x", envir = environment(missions_labor))
  y <- get("y", envir = environment(missions_labor))
  ind <- which(y == 1)
  if (length(ind) >2) {
    # Find unrealistic idle times with delta > 12 h
    ind1 <- which(y == 1)
    diff_xind1 <- diff(x[ind1])
    critical_xval <- x[ind1][which(diff_xind1 >= 12*3600)]
    if (length(critical_xval) > 0) {
      ind_remove_x <- which(x == critical_xval)
      ind_remove_y <- ind1[which(diff_xind1 >= 12*3600)]
      t <- x[-c(ind_remove_x, ind_remove_x+1)]
      y <- y[-ind_remove_y]
    } else {
      ind_keep <- ind[c(1, length(ind))]
      t <- x[ind_keep]
      y <- c(0, 1, 0)
    }
    # plot(stepfun(x = t, y= y))
    overtime_in_day <- stepfun(x = t, y = y)
    res <- ((1 - missions_labor) * (1 - planned_labor)) * overtime_in_day
  } else {
    res <- stepfun(x = 0, y = c(0, 0))
  }
  return(prune_stepfun(res))
}

#' Calculate all times for one vehicle ID
#'
#' This function is only valid for missions data within one year. In case of more
#' than one year, iterate over years.
#'
#' * planned_labor = planned_labor_vehicle() is the tricky part using schedules
#' * missions_labor = all labor is extracted from missions.
#'
#' Then labor within working hours
#'
#' * labor = missions_labor * planned_labor (as stepfun)
#' * overtime = missions_labor - missions_labor * planned_labor (as stepfun)
#' * idletime = planned_labor - missions_labor * planned_labor (as stepfun)
#' * idle_overtime = (1 - missions_labor) * (1 - planned_labor) * overtime_in_day
#'
#' where overtime_in_day assumes that idle times larger 12 hours is free time.
#'
#' @param missions The missions
#' @param vehicles The vehicles
#' @param vehicle_id An integer, the vehicle ID
#' @param origin_date A POSIXct, the origin date
#'
#' @return A list of stepfun, the planned_labor, missions_labor, labor, overtime,
#' idletime, and the idle-overtime
calculate_times_id <- function(missions, vehicles, vehicle_id, origin_date,
                               idleovertime2overtime) {

  # Get min- and max times from missions
  m <- missions[missions$vehicle_id == vehicle_id, ]
  v <- vehicles[vehicles$id == vehicle_id, ]
  assertthat::assert_that(nrow(v) == 1)
  if (nrow(m) > 0) {
    # See comments calculate_times_per_day_id()
    # (***) e.g. in case of hist_scenario
    min_sec <- 3600*24*floor(min(m$t_alarm_sec)/3600/24)
    max_sec <- 3600*24*ceiling(max(
      m$t_alarm_sec + m$dt_to_launch + m$dt_to_poa + m$dt_to_completion)/3600/24)
    year <- as.integer(format(origin_date + min_sec, "%Y"))
    year_end <- as.integer(format(origin_date + max_sec - 1, "%Y"))
    assertthat::assert_that(year == year_end)
  } else {
    # See comments calculate_times_per_day_id()
    # (***) e.g. in case of hist_scenario
    min_sec <- 0
    max_sec <- 24*3600
    year <- as.integer(format(origin_date + min_sec, "%Y"))
    # year_end <- as.integer(format(origin_date + max_sec - 1, "%Y")) # useless
  }

  # planned labor => Bug, depends on scenario = combination of missions + vehicles!!!
  # Hack !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  planned_labor <- planned_labor_vehicle(
    vehicle = v, # vehicles[vehicles$id == vehicle_id, ],
    year = year,
    tmin = 600*24*floor(min(missions$t_alarm_sec)/3600/24), #     only tmin, tmax needed???
    tmax = 3600*24*ceiling(max(missions$t_alarm_sec +
                                    missions$dt_to_launch +
                                    missions$dt_to_poa +
                                    missions$dt_to_completion)/3600/24),
    origin_date = origin_date)

  if (nrow(m) > 0) {
    # total labor - no matter what it is
    m_start <- m$t_alarm_sec
    m_stop <- m$t_alarm_sec + m$dt_to_launch + m$dt_to_poa + m$dt_to_completion # dt_free??
    assertthat::assert_that(all(m_start < m_stop))
    t <- c(min_sec, sort(c(m_start, m_stop)), max_sec)
    y <- c(0, rep(c(0, 1), length(m_start)), 0, 0)
    missions_labor <- stepfun(x = t, y = y)
    missions_labor <- prune_stepfun(missions_labor)

    # labor within working hours
    labor <- missions_labor * planned_labor
    labor <- prune_stepfun(labor)

    # overtime
    overtime <- missions_labor - missions_labor * planned_labor
    overtime <- prune_stepfun(overtime)

    # idletime
    idletime <- planned_labor - missions_labor * planned_labor
    idletime <- prune_stepfun(idletime)

    # For tests at the moment
    # idle_overtime <- idle_overtime(planned_labor, missions_labor, idletime)
    idle_overtime <- stepfun(x = 0, y = c(0, 0))
    # idle_overtime <- (1 - planned_labor) * (1 - idletime) ???????????????????

    if (idleovertime2overtime) {
      overtime <- prune_stepfun(overtime + idle_overtime)
    } else {
      idletime <- prune_stepfun(idletime + idle_overtime)
    }
  } else {
    missions_labor <- stepfun(x = 0, y = c(0, 0))
    labor <- stepfun(x = 0, y = c(0, 0))
    overtime <- stepfun(x = 0, y = c(0, 0))
    idletime <- stepfun(x = 0, y = c(0, 0))
  }

  # Should idle_overtime be returned --> Do everything here --> local!
  return(list(planned_labor = planned_labor,
              missions_labor = missions_labor,
              labor = labor,
              overtime = overtime,
              idletime = idletime))
}

