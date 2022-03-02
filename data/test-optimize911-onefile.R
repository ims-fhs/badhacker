#' Move all vehicles from one base to another address
#'
#' Vehicles at old address are set to 00:00--00:00 (not available)
#'
#' @param old_community A character, the name of the old community
#' @param old_address A character, the address of the old community
#' @param new_community A character, the name of the new community
#' @param new_address A character, the address of the new community
#' @param sim_vehicles sim_vehicles
#' @param new_bases A data.frame
#'
#' @return
move_base <- function(old_community, old_address,
                      new_community, new_address,
                      sim_vehicles, new_bases) {
  # Move location of base or delete a base
  cond1 <- old_community == sim_vehicles$community
  cond2 <- old_address == sim_vehicles$address
  assertthat::assert_that(all(cond1 == cond2))
  cond3 <- new_community == new_bases$community
  cond4 <- new_address == new_bases$address
  assertthat::assert_that(all(cond3 == cond4))

  # build vehicles at new base
  cond_old_community <- sim_vehicles$community == old_community &
    sim_vehicles$address == old_address
  vehicles_temp <- sim_vehicles[cond_old_community, ]
  old_ids <- vehicles_temp$id

  nr_vehicles_temp <- nrow(vehicles_temp)
  max_id <- max(sim_vehicles$id)
  temp_id <- c((max_id + 1):(max_id + nr_vehicles_temp))
  # Changes for new vehicles
  vehicles_temp$id <- temp_id
  vehicles_temp$community <- new_community
  vehicles_temp$address <- new_address

  # rbind old and new vehicles
  sim_vehicles <- rbind(sim_vehicles, vehicles_temp)
  cond_old <- sim_vehicles$id %in% old_ids
  sim_vehicles$name[cond_old] <- paste0("X ", sim_vehicles$name[cond_old])
  sim_vehicles$schedule[cond_old] <- paste0(
    stringr::str_split_fixed(string = sim_vehicles$schedule[cond_old],
                             pattern = "\\|",
                             n = 2)[, 1],
    "|00:00--00:00")
  sim_vehicles$shift_from_simtime <- 0L
  sim_vehicles$shift_to_simtime <- 0L

  # new base coordinates
  cond_new_community <- new_bases$community == new_community &
    new_bases$address == new_address
  assertthat::assert_that(sum(cond_new_community) == 1)
  new_base_lat <- new_bases$lat[cond_new_community]
  new_base_lng <- new_bases$lng[cond_new_community]

  # replacement of base_lat and lat
  sim_vehicles$base_lat[sim_vehicles$id %in% temp_id] <- new_base_lat
  sim_vehicles$base_lng[sim_vehicles$id %in% temp_id] <- new_base_lng
  sim_vehicles$lat[sim_vehicles$id %in% temp_id] <- new_base_lat
  sim_vehicles$lng[sim_vehicles$id %in% temp_id] <- new_base_lng

  message(paste0("Move base from ",
                 paste(old_community, old_address, collapse = ", "),
                 " to ", paste(new_community, new_address, collapse = ", ")))
  return(sim_vehicles)
}

#' Move one vehicle (id) from one address (base) to another (new base) by vehicle_id
#'
#' The correct coordinates are retrieved from look-up in "new_bases".
#' Vehicles at old address are set to 00:00--00:00 (not available)
#'
#' If you want to move a vehicle by name, first search all vehicle ID via
#' `vehicle_ids <- vehicle$id[which(v$name == "My great name")]`
#'
#' @param sim_vehicles sim_vehicles
#' @param vehicle_id An integer, the vehicle_id of the vehicle to be moved
#' @param new_community A character, the new community of the vehicle
#' @param new_address A character, the new address of the vehicle
#' @param new_bases A data.frame, the new bases
#'
#' @return sim_vehicles
move_vehicle_id <- function(sim_vehicles, vehicle_id,
                            new_community, new_address, new_bases) {
  cond1 <- new_community == new_bases$community
  cond2 <- new_address == new_bases$address
  assertthat::assert_that(all(cond1 == cond2))
  assertthat::assert_that(length(vehicle_id) == 1)
  cond <- sim_vehicles$id == vehicle_id
  message(paste0("Move: ", sim_vehicles$name[cond], ", id ", vehicle_id, ": ",
                 paste(sim_vehicles[cond, c(5, 9, 10)], collapse = "  ")))

  max_id <- max(sim_vehicles$id)
  vehicle <- sim_vehicles[cond,]
  # replace parameters
  cond_replace <- new_bases$community == new_community &
    new_bases$address == new_address
  assertthat::assert_that(sum(cond_replace) == 1)
  vehicle$lat <- new_bases$lat[cond_replace]
  vehicle$lng <- new_bases$lng[cond_replace]
  vehicle$base_lat <- new_bases$lat[cond_replace]
  vehicle$base_lng <- new_bases$lng[cond_replace]
  vehicle$id <- max_id + 1
  vehicle$community <- new_community
  vehicle$address <- new_address
  vehicle$name <- paste0("M ", vehicle$name)

  sim_vehicles$schedule[cond] <- paste0(
    stringr::str_split_fixed(string = sim_vehicles$schedule[cond],
                             pattern = "\\|",
                             n = 2)[, 1],
    "|00:00--00:00")
  sim_vehicles$shift_from_simtime <- 0L
  sim_vehicles$shift_to_simtime <- 0L
  sim_vehicles$name[cond] <- paste0("X ", sim_vehicles$name[cond])

  # rbind
  sim_vehicles <- rbind(sim_vehicles, vehicle)
  message(paste0("To: ", vehicle$address, " in ", vehicle$community))
  return(sim_vehicles)
}

#' Add new vehicle name to sim_vehicles
#'
#' A "N" for New is added to the name as prefix. new_bases needed to look up
#' coordinates of the (new) base.
#' If the new_schedule corresponds to a night shift, the vehicle ID is duplicate
#' automatically.
#'
#' @param sim_vehicles the sim_vehicles
#' @param origin_date A POSIXct, the origin date
#' @param new_name A character, the new name of the team
#' @param new_organisation A character, the new organisation
#' @param new_type A character, the new type, e.g. RTW, NEF, ...
#' @param new_schedule A character, the new teams' schedule
#' @param new_wdays A character, the weekdays for the new vehicle in the form
#' "1,2,3,4,5,6,7" where 1 = Monday, 7 = Sunday.
#' @param new_community A character, the new community
#' @param new_zipcode A character, the zip code of the community
#' @param new_address A character, the new address
#' @param new_bases A data.frame, the lookup-table. Either an existing one or
#' a list of new bases.
#'
#' @return sim_vehicles
add_vehicle_name <- function(sim_vehicles, origin_date, new_name, new_organisation,
                             new_type, new_schedule, new_wdays, new_community,
                             new_zipcode, new_address, new_bases) {
  dups_ind <- which(sim_vehicles$name == new_name |
                      sim_vehicles$name == paste0("N ", new_name))
  if (any(dups_ind)) {
    warning("New sim_vehicle name already exists")
    message("Old names show ", length(sim_vehicles$name[dups_ind]),
            " duplicated names ", new_name)
    message(paste0("Old: \"", sim_vehicles$name[dups_ind[1]], "\", id ",
                   sim_vehicles$id[dups_ind[1]], ": ",
                   paste(sim_vehicles[dups_ind[1], c(5, 9, 10, 8, 6)],
                         collapse = "  ")))
  }

  new_vehicle <- sim_vehicles[FALSE, ]
  new_vehicle[1, ] <- 0
  new_vehicle$id <- 1 # set in remove_24h_crossings_by_id_duplication() below
  new_vehicle$name <- paste0("N ", new_name)
  new_vehicle$organisation <- new_organisation
  new_vehicle$type <- new_type
  new_vehicle$schedule <- new_schedule
  new_vehicle$shift_weekday <- new_wdays
  cond_replace <- new_bases$community == new_community &
    new_bases$address == new_address
  if (sum(cond_replace) != 1) {
    stop("community and address not found in new_bases.")
  }
  new_vehicle$lat <- new_bases$lat[cond_replace]
  new_vehicle$lng <- new_bases$lng[cond_replace]
  new_vehicle$base_lat <- new_bases$lat[cond_replace]
  new_vehicle$base_lng <- new_bases$lng[cond_replace]
  new_vehicle$community <- new_community
  new_vehicle$zipcode <- new_zipcode
  new_vehicle$address <- new_address

  new_vehicle <- data911::remove_24h_crossings_by_id_duplication(
    vehicles = new_vehicle)
  if (nrow(new_vehicle) == 1) {
    new_vehicle$id <- max(sim_vehicles$id) + 1
    message(paste0("New: \"", new_vehicle$name, "\", id ", new_vehicle$id, ": ",
                   paste(new_vehicle[1, c(5, 9, 10, 8, 6)], collapse = "  ")))
  } else {
    new_vehicle$id <- max(sim_vehicles$id) + c(1:2)
    message(paste0("New: \"", new_vehicle$name[1], "\", id ", new_vehicle$id[1], ": ",
                   paste(new_vehicle[1, c(5, 9, 10, 8, 6)], collapse = "  ")))
    message(paste0("New: \"", new_vehicle$name[2], "\", id ", new_vehicle$id[2], ": ",
                   paste(new_vehicle[2, c(5, 9, 10, 8, 6)], collapse = "  ")))
  }

  sim_vehicles <- rbind(sim_vehicles, new_vehicle)
  sim_vehicles <- data911::update_vehicles(year = as.numeric(format(origin_date, "%Y")),
                                           origin_date = origin_date,
                                           vehicles = sim_vehicles)
  return(sim_vehicles)
}

#' Remove one vehicle ID
#'
#' Disabled via schedule and run data911::update_vehicles()
#'
#' @param sim_vehicles sim_vehicles
#' @param vehicle_id An integer, the vehicle ID
#' @param origin_date A POSIXct, the origin date of the secneario
#'
#' @return sim_vehicles
remove_vehicle_id <- function(sim_vehicles, vehicle_id, origin_date) {
  assertthat::assert_that(length(vehicle_id) == 1)
  cond <- sim_vehicles$id == vehicle_id
  message(paste0("Removed: ", sim_vehicles$name[cond], ", id ", vehicle_id, ": ",
                 paste(sim_vehicles[cond, c(5, 9, 10)], collapse = "  ")))

  sim_vehicles$schedule[cond] <- paste0(
    stringr::str_split_fixed(string = sim_vehicles$schedule[cond],
                             pattern = "\\|",
                             n = 2)[, 1],
    "|00:00--00:00")
  sim_vehicles$shift_from_simtime <- 0L
  sim_vehicles$shift_to_simtime <- 0L

  sim_vehicles$name[cond] <- paste0("X ", sim_vehicles$name[cond])
  return(sim_vehicles)
}

#' Edit schedule of one vehicle ID
#'
#' update via data911::update_vehicles due to changes in "schedule".
#'
#' @param sim_vehicles sim_vehicles
#' @param vehicle_id An integer, the vehicle ID
#' @param origin_date A POSIXct, the origin date of the secneario
#' @param new_schedule A character of the form "Jan-01--Dez-31|07:00--17:00"
#' @param new_wdays A character of the form "1,2,3,4,5,6,7".
#' 1 = Monday, ... 7 = Sunday.
#'
#' @return sim_vehicles
edit_vehicle_schedule <- function(sim_vehicles, vehicle_id, origin_date,
                                  new_schedule = NULL,
                                  new_wdays = NULL) {
  assertthat::assert_that(is.null(new_schedule) + is.null(new_wdays) < 2)
  assertthat::assert_that(length(vehicle_id) == 1)
  assertthat::assert_that(all(as.integer(unlist(
    strsplit(x = new_wdays, split = ","))) %in% c(1:7)))
  cond <- sim_vehicles$id == vehicle_id
  message(paste0("Old: ", sim_vehicles$name[cond], ", id ", vehicle_id, ": ",
                 paste(sim_vehicles[cond, c(5, 9, 10)], collapse = "  ")))

  if (!is.null(new_schedule)) {
    sim_vehicles$schedule[cond] <- new_schedule
  }
  if (!is.null(new_wdays)) {
    sim_vehicles$shift_weekday[cond] <- new_wdays
  }
  sim_vehicles <- data911::update_vehicles(as.numeric(format(origin_date, "%Y")),
                                           origin_date, sim_vehicles)
  message(paste0("Changed to:            ",
                 paste(sim_vehicles[cond, c(5, 9, 10)], collapse = "  ")))
  return(sim_vehicles)
}

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

#' Add two step functions
#'
#' One of f1 or f2 might be numeric
#'
#' @param f1 A step function or a numeric
#' @param f2 A step function or a numeric
#'
#' @return A stepfun
#' @export
'+.stepfun' <- function(f1, f2) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))

  if (is.stepfun(f1) & is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x2 <- get("x", envir = environment(f2))
    x <- sort(unique(c(x1, x2)))
    y <- f1(c(x[1] - 1, x)) + f2(c(x[1] - 1, x))
  } else if (is.stepfun(f1) & !is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x <- x1
    y <- f1(c(x[1] - 1, x)) + f2
  } else if (!is.stepfun(f1) & is.stepfun(f2)) {
    x2 <- get("x", envir = environment(f2))
    x <- x2
    y <- f1 + f2(c(x[1] - 1, x))
  }
  return(stepfun(x = x, y = y))
}

#' Multiply two step functions
#'
#' One of f1 or f2 might be numeric
#'
#' @param f1 A step function or a numeric
#' @param f2 A step function or a numeric
#'
#' @return A stepfun
#' @export
'*.stepfun' <- function(f1, f2) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))

  if (is.stepfun(f1) & is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x2 <- get("x", envir = environment(f2))
    x <- sort(unique(c(x1, x2)))
    y <- f1(c(x[1] - 1, x)) * f2(c(x[1] - 1, x))
  } else if (!is.stepfun(f1) & is.stepfun(f2)) {
    x <- get("x", envir = environment(f2))
    y <- f1 * f2(c(x[1] - 1, x))
  } else if (is.stepfun(f1) & !is.stepfun(f2)) {
    x <- get("x", envir = environment(f1))
    y <- f1(c(x[1] - 1, x)) * f2
  }
  return(stepfun(x = x, y = y))
}

#' Subtract two step functions
#'
#' One of f1 or f2 might be numeric
#'
#' @param f1 A step function or a numeric
#' @param f2 A step function or a numeric
#'
#' @return
#' @export
'-.stepfun' <- function(f1, f2) {
  y <- f1 + (-1)*f2
  return(y)
}

#' Prune step functions
#'
#' Remove x- and y-values from step functions, where diff(y) does not change
#'
#' @param f A stepfun
#'
#' @return A stepfun
#' @export
prune_stepfun <- function(f) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f)))

  y <- c(get("yleft", envir = environment(f)),
         get("y", envir = environment(f)))
  ind_dups <- which(diff(y) == 0)
  if (length(ind_dups) > 0) {
    x <- get("x", envir = environment(f))
    x <- x[-ind_dups]
    if (length(x) == 0) {
      res <- stepfun(x = 0, y = c(0, 0))
    } else {
      y <- y[-(ind_dups+1)]
      res <- stepfun(x = x, y = y)
    }
  } else {
    res <- f
  }
  return(res)
}

#' Shift stepfun by dx in x direction
#'
#' @param f A stepfun
#' @param dx A numeric
#'
#' @return The shifted stepfun
shift_stepfun <- function(f, dx) {
  x <- get("x", envir = environment(f))
  y <- c(get("yleft", envir = environment(f)),
         get("y", envir = environment(f)))
  xnew <- x + dx
  return(stepfun(x = xnew, y = y))
}

'/.stepfun' <- function(f1, f2) {
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))
  assertthat::assert_that(any(c("numeric", "stepfun") %in% class(f1)))

  if (is.stepfun(f1) & is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x2 <- get("x", envir = environment(f2))
    x <- sort(unique(c(x1, x2)))
    y <- f1(c(x[1] - 1, x)) / f2(c(x[1] - 1, x))
  } else if (is.stepfun(f1) & !is.stepfun(f2)) {
    x1 <- get("x", envir = environment(f1))
    x <- x1
    y <- f1(c(x[1] - 1, x)) / f2
  } else if (!is.stepfun(f1) & is.stepfun(f2)) {
    x2 <- get("x", envir = environment(f2))
    x <- x2
    y <- f1 / f2(c(x[1] - 1, x))
  }
  return(stepfun(x = x, y = y))
}

