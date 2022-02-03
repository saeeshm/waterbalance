#' Calculate Potential Evapo-Transpiration
#'
#' Given a gridded radiation layer and a monthly average temperature layer, this
#' function calcualtes the Potential Evapotranspiration for each grid cell in
#' the study region. Input rasters must match in resolution, extent and CRS.
#'
#' @param solar Gridded total monthly radiation in Wh/m2 units.
#' @param temp Gridded average monthly temperature in degrees Celsius units
#' @param month The abbreviated name of the month associated with these raster
#'   data. Valid abbreviations must be three letters, and match those in
#'   `month.abb`
#' @param humid Logical variable indicating whether the study region is in a
#'   humid climate or not. Defaults to true, since the Turc method is designed
#'   to be run for humid climates. If set to FALSE, the `rel_hum` argument must
#'   be provided.
#' @param rel_hum Gridded average monthly relative humidity layer, used to apply
#'   the correction factor for calculating PET in non-humid climates.
#' @param pet_adjst T/F indicating whether a PET adjustment factor should be
#'   applied to the calculated PET. If set to TRUE then the `pet_adjst_coefs`
#'   argument must be provided.
#' @param pet_adjst_coefs Gridded adjustment coefficients to be applied to the
#'   calculated PET layer.
#'
#' @return A `terra SpatRaster` giving potential-evapotranspiration (millimeters - mm)
#' @export
#' @examples
#' # Working on it
wb_pet <- function(solar, temp, month, humid = T, rel_hum = NULL, pet_adjst = T, pet_adjst_coefs = NULL){
  # If avg temp is <= 0, pet is 0, so overwriting any values in the temp layer
  # less than 0 to 0 (ensuring that the pet computation returns 0)
  temp <- terra::classify(temp, matrix(c(-Inf, 0, 0), nrow = 1), right = NA)
  # Converting radiation from WH/m2 to cal/cm2
  solar <- solar*0.08598
  # Calculating PET using the Turc method
  if(humid){
    pet <- (solar+50) * (temp/(temp+15)) * 0.013
  # Adding the relative humidity adjustment factor for non-humid climates
  }else{
    pet <- (solar+50) * (temp/(temp+15)) * 0.013 * (1 + ((50-rel_hum)/70))
  }

  # If the optional pet_adjustment coefficient is required, applying it now
  if(pet_adjst){
    print(paste('Adding adjustment coeficients for month', month))
    pet <- pet*pet_adjst_coef
  }
}

#' Calculate Precipitation Minus Potential Evapo-Transpiration
#'
#' Given a precipitation raster layer and a PET rast layer, this function
#' calculates the difference.
#'
#' @param precip Gridded total monthly precipitation (mm)
#' @param pet Gridded total monthly potential evapo-transpiration (mm)
#'
#' @return A `terra SpatRaster` giving the diference between Precipitation and PET (millimeters - mm)
#' @export
#' @examples
#' # Working on it
wb_precip_minus_pet <- function(precip, pet) {precip - pet}

