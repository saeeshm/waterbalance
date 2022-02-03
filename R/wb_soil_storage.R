#' Soil storage
#'
#' Calculates the amount of soil storage available at the end of the month.
#'
#' This function calculates soil storage using the Mather 1978 approach, which
#' employs a daily time-step for calculating soil storage. Daily precipitation
#' and p-pet values are estimated by dividing the total monthly values by the
#' number of days in the month. Due to the increased number of calculations,
#' soil storage functions may run considerably slower than other functions in
#' this package, particularly for large areas or datasets.
#'
#' Passing a raster layer containing storage in the previous month is required
#' for accurate storage calcuations. However, if you are assuming that storage
#' is full at the start of this month's calculations, you can simply pass the
#' `awc` layer to the `prev_storage` argument to indicate that storage is full
#' (i.e is equal to the soil's available water capacity)
#'
#' @param awc Gridded average soil water holding capacity (millimeters - mm).
#' @param precip Gridded total monthly radiation for this month (Watt-hours per
#'   meter squared - Wh/m2).
#' @param p_pet Gridded difference between precipitation and potential
#'   evapo-transporation for this month (millimeters - mm).
#' @param prev_storage Gridded soil storage layer for the month directly prior
#'   (mm). See Details.
#' @return A `terra SpatRaster` giving soil water storage (millimeters - mm)
#' @export
#' @examples
#' # Working on it
wb_soil_storage <- function(awc, precip, p_pet, prev_storage){

  # mindex and dindex imported from inst/extdata/internal_data.R

  # Was the previous month's storage full?
  prvWasFull <- (terra::mean(terra::values(prev_storage == awc), na.rm = T) == 1)
  # Was the previous month's precip minus pet all positive or 0
  pPetAllPositive <- (terra::mean(terra::values(p_pet_list[[.y]] > 0), na.rm = T) == 1)

  # If storage was full previously and p-pet is all positive, there will be no
  # change in storage, so we can return this month's storage as awc
  if( prvWasFull & pPetAllPositive ){
    return(awc)
  }

  # Getting average daily level p-pet (dividing monthly total by number of days)
  dppe <- p_pet[[.y]]/dindex[.y]
  # Saving the current storage state, which at the start of the month is just
  # the previous storage state
  currStorage <- prev_storage

  # For each day in this month
  for (i in 1:dindex[.y]){
    # Calculating available ppe affecting storage on this day. Where ppe is
    # negative, storage is reduced by the amount of soil moisture utilization
    # (smu) by flora, which is computed as a decreasing linear function of
    # storage size (Dyer 2019,Mather 1978). Where ppe is positive, storage is
    # just increased by the surplus precipitation so ppe available is just equal
    # to dppe.
    ppeAvl <- dppe * (storageState/awc) * (dppe < 0) + dppe * (dppe >= 0)
    # New storage
    currStorage <- (currStorage + ppeAvl)
    # Where storage exceeds awc, resetting to awc (storage exceeding awc just
    # becomes runoff).
    currStorage <- (storage * (storage < awc)) + (storage * (storage >= awc))
    # Where storage is less than 0, resetting to 0
    currStorage <- (storage * (storage > 0))
  }

  # Returning the final storage state at the end of the month as this month's
  # storage raster
  return(currStorage)
}

#' Soil storage for an annual stack of data
#'
#' For a raster data "stack" containing 12 layers of input data (1 for each
#' month), this function calculates soil storage in a sequential fashion, using
#' the previous month's storage as the starting point for the next month's
#' calculations. This is to ease the process of calculating a year-long water
#' budget, and is particularly useful when using input data that is meant to
#' represent climatic "normals".
#'
#' It is recommended that input lists are named by month, using 3-letter
#' abbreviated month names (see `month.abb`), and they are ordered in sequence,
#' with the first element of the list being the month in storage calculations
#' should start. If the lists are not named, they are assumed to be ordered from
#' January to December and the calculation begins from January.
#'
#' Input lists less than 12 elements in length are not accepted, as this
#' function is explicitly aimed toward annual calculations. You can iteratively
#' use the `wb_soil_storage` function for smaller time frames. Input lists
#' greater than 12 months are also not accepted. For such multi-year
#' calculations, it is recommended that you apply this function iteratively to
#' each year, using the last month's storage as the starting point for the next
#' year's calculation.
#'
#' When `wrap` is TRUE, the annual calculations begins by assuming storage is
#' full in the starting month. However, if storage calculations are not full at
#' the end of the year, the calculation will "wrap" back to the starting month,
#' using the last month's storage as the new initial state. This approach is
#' likely to be appropriate if modelling "typical" conditions using "normal"
#' data, since in most cases you will be choosing a starting month based on a
#' reasonable assumption of which month will have full soil storage based on
#' rainfall patterns. Using `wrap` allows you to check that assumption during
#' the calculation.
#'
#' If no `prev_storage` layer is provided, the function assumes that storage for
#' the first month of calculation is full (i.e is equal to soil AWC). Note that
#' `prev_storage` and `wrap` cannot be provided together, since if the previous
#' storage state is known there is no need to wrap the calculation. In case both
#' are passed `prev_storage` takes precedence.
#'
#' @param awc Gridded average soil water holding capacity (mm).
#' @param precip_list List of 12 raster layers giving monthly total
#'   precipitation (mm) for each month in the year. The list must be ordered
#'   from January to December.
#' @param p_pet_list List of 12 raster layers giving the difference between
#'   monthly total precipitation and potential evapo-transpiration (mm) for each
#'   month in the year. The list must be ordered from January to December.
#' @param prev_storage A raster layer defining soil storage in the month prior
#'   to the starting month (mm). See Details.
#' @param wrap Should the soil storage calculation "wrap" around after
#'   calculating storage for the last month? Defaults to TRUE. See Details.
#' @param fileOrderWarning Should the user be warned about the implicit ordering
#'   assumption of input layers stacks.
#' @return A list of 12 `terra SpatRaster` layer giving soil storage for each
#'   month (mm)
#' @export
#' @examples
#' # Working on it
wb_soil_storage_annual <- function(awc, precip_list, p_pet_list,
                                   prev_storage = NULL, wrap = T,
                                   fileOrderWarning = T){
  if(fileOrderWarning){
    warning('Function assumes layer lists are ordered from January to December.')
  }

  if (!is.null(prev_storage) & wrap){
    warning('Cannot wrap beacause a previous storage layer was provided. Resetting to FALSE...')
    wrap <- F
  }

  # If there is no previous storage layer, assuming previous storage is full for
  # Jan
  if(is.null(prev_storage)) prev_storage <- awc

  # Calculating storage for each month by iterating over each stack
  storage_stack <- purrr::pmap(awc, precip_list, p_pet_list, ~{
    currStorage <- wb_soil_storage_month(..1, ..2, ..3, prev_storage)
    prev_storage <<- currStorage
    return(currStorage)
  })

  # If wrapping and the last month's storage is not full, we need to recalculate
  # with a new initial storage state
  if (wrap){
    if(storage_stack[[12]] == awc){
      message('Storage year ended full, no need to wrap. Skipping wrap...')
      return(currStorage)
    }else{
      # Getting Dec storage as the previous state
      prev_storage <- storage_stack[[12]]
      storage_stack <- NULL
      # Recalculating storage for each month using the new initial state
      storage_stack <- purrr::pmap(awc, precip_list, p_pet_list, ~{
        currStorage <- wb_soil_storage_month(..1, ..2, ..3, prev_storage)
        prev_storage <<- currStorage
        return(currStorage)
      })
    }
  }
  return(storage_stack)
}

#' Calculate change in storage between months
#'
#'
#'
#' @return A `terra SpatRaster` giving the change in soil storage between the
#'   current month and the previous month.
#' @export
wb_delta_storage <- function(storage_start, storage_end){
  return(storage_start - storage_end)
}

#' Calculate change in storage for a 12-month data stack
#'
#' @return A `terra` raster layer
#' @export
wb_delta_storage_stack <- function(storage_stack){
  if(fileOrderWarning){
    warning('Function assumes that the input layer list is ordered from January to December.')
  }
  names(storage_stack) <- names(mindex)
  delta_storage_stack <- imap(storage, ~{
    # If the month is January (number 1), setting previous month to december
    if(.y == 'Jan') {
      prvName = 'Dec'
      # Otherwise calculating it by looking at the previous one
    }else{
      prvName <- names(mindex[(mindex[.y] - 1)])
    }
    wb_delta_storage(storage[[.y]], storage[[prvName]])
  })
  return(delta_storage_stack)
}

#' Calculating boolean grids indicating whether storage in any month is full
#'
#' @return A `terra` raster layer
#' @export
wb_storage_is_full <- function(awc, storage){
  return(storage == awc)
}

#' Calculating boolean grids indicating whether storage in any month is full
#'
#' @return A `terra` raster layer
#' @export
wb_storage_is_full_stack <- function(awc, storage_stack){
  return(map(storage_stack, ~{.x == awc}))
}

