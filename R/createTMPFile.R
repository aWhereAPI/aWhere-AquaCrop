
#' @export
#' @import data.table
createTMPFile <- function(dat, placename) {

  if(substr(Sys.getenv("OS"),1,7) == "Windows") {
    # set Windows newline
    newLine <- "\r\n"
  }
  else {
    # set non-Windows newline
    newLine <- "\r\n"
  }

  dat$temperatures.max <- format(round(dat$temperatures.max, 1), nsmall = 1)
  dat$temperatures.min <- format(round(dat$temperatures.min, 1), nsmall = 1)

  dat$min_temp_text <- NA_character_
  dat$max_temp_text <- NA_character_

  dat$max_temp_text <- stringi::stri_pad_left(dat$temperatures.max, width = 10)
  dat$min_temp_text <- stringi::stri_pad_left(dat$temperatures.min, width = 10)

  text = paste0(
    placename, newLine,
    "     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)", newLine,
    stringi::stri_pad_left(lubridate::day(min(dat$date)), width = 6), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)", newLine,
    stringi::stri_pad_left(lubridate::month(min(dat$date)), width = 6), "  : First month of record", newLine,
    "  ", lubridate::year(min(dat$date)), "  : First year of record (1901 if not linked to a specific year)", newLine,
    "", newLine,
    "  Tmin (C)   Tmax (C)", newLine,
    "========================", newLine,
    paste0(paste0(dat$min_temp_text, dat$max_temp_text), collapse = newLine)
  )

  return(text)

}
