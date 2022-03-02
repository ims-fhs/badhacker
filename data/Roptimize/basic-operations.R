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

