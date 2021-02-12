
#' @name createClimateFile
#' @title createClimateFile
#'
#' @description This function takes in the relevant parameters needed for making API calls and proceeds to
#' create the 4 files necessary for use in the AquaCrop software. The 4 files will be saved to the current
#' R working directory, after which they may be copied into the DATA folder within your AquaCrop installation location.
#' From there you should now be able to run the AquaCrop software and select the climate file by finding the placename, specified in this
#' function, in the list of climate files within the program.
#'
#' @export
#' @param latitude Latitude for modeling location
#' @param longitude Longitude for modeling location
#' @param start_date Starting date for model run
#' @param end_date Ending date for model run
#' @param placename Text string for model location, used in file naming
createClimateFile <- function(latitude, longitude, start_date, end_date, placename) {

  if(substr(Sys.getenv("OS"),1,7) == "Windows") {
    # set Windows newline
    newLine <- "\r\n"
  }
  else {
    # set non-Windows newline
    newLine <- "\r\n"
  }

  if(!exists("awhereEnv75247")) {
    stop("This function needs access to aWhere's API. Please request a token using your key and secret and retry.")
  }

  obs <- aWhereAPI::daily_observed_latlng(latitude, longitude, start_date, end_date)
  ag <- aWhereAPI::agronomic_values_latlng(latitude, longitude, start_date, end_date)

  writeLines(createTMPFile(obs, placename), con = paste0(placename, ".TMP"))
  writeLines(createPLUFile(obs, placename), con = paste0(placename, ".PLU"))
  writeLines(createEToFile(ag, placename),  con = paste0(placename, ".ETo"))

  text = paste0(placename, " strategy", newLine,
                    " 4.0   : AquaCrop Version (January 2012)", newLine,
                    placename, ".TMP", newLine,
                    placename, ".ETo", newLine,
                    placename, ".PLU", newLine,
                    "MaunaLoa.CO2")
  writeLines(text, con = paste0(placename, ".CLI"))
}
