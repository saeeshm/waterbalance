#' Calculating actual evapo-transpiration
#'
#' @return A `terra` raster layer
#' @export
wb_aet <- function(p_pet, pet, precip, delta_storage){
  # If P-PET is negative, AET is Precipitation minus change in Storage. If P-PET
  # is positive, AET is equal to PET
  aet <- ((p_pet < 0) * (precip - delta_storage)) + ((p_pet >=0) * pet)
  # Wherever AET is results as being higher than PET, resetting to PET (since
  # AET cannot by definition be higher than PET)
  aet <- (aet <= pet) * aet + (aet > pet) * pet
  return(aet)
}

#' Calculating actual evapo-transpiration
#'
#' @return A `terra` raster layer
#' @export
wb_aet_stack <- function(p_pet_stack, pet_stack, precip_stack, delta_storage_stack){
  aet_stack <- pmap(p_pet_stack, pet_stack, precip_stack, delta_storage_stack, wb_aet)
  return(aet_stack)
}

#' Calculating deficit
#'
#' @return A `terra` raster layer
#' @export
wb_deficit <- function(pet, aet){
  # Deficit is the difference between potential and actual evapo-transpiration
  deficit <- pet - aet
  # Where the calculation returns deficits that are negetive, resetting to 0
  deficit <- (deficit > 0.0001) * deficit
  return(deficit)
}

#' Calculating deficit stack
#'
#' @return A `terra` raster layer
#' @export
wb_deficit_stack <- function(pet_stack, aet_stack){
  deficit_stack <- map2(pet_stack, aet_stack, wb_deficit)
  return(deficit_stack)
}

#' Calculating surplus
#'
#' @return A `terra` raster layer
#' @export
wb_surplus <- function(awc, storage, precip, aet, delta_storage){
  # Where storage is less than awc (given a margin of calculation error),
  # surplus is 0. Otherwise, surplus is precip minus aet plus change in storage
  # for that month
  surplus <- (abs(storage - awc) < 0.05) * (precip - (aet + delta_storage))
}

#' Calculating surplus stack
#'
#' @return A `terra` raster layer
#' @export
wb_surplus_stack <- function(awc, storage_stack, precip_stack, aet_stack, delta_storage_stack){
  pmap(awc, storage_stack, precip_stack, aet_stack, delta_storage_stack, wb_surplus)
}
