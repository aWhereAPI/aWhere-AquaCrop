
#' @export
#' @import data.table
createEToFile <- function(dat, placename) {

  if(substr(Sys.getenv("OS"),1,7) == "Windows") {
    # set Windows newline
    newLine <- "\r"
  }
  else {
    # set non-Windows newline
    newLine <- "\n"
  }

  dat$pet.amount <- format(round(dat$pet.amount, 1), nsmall = 1)

  dat$pet_text <- NA_character_

  dat$pet_text <- stringi::stri_pad_left(dat$pet.amount, width = 10)

  text = paste0(
    placename, newLine,
    "     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)", newLine,
    stringi::stri_pad_left(lubridate::day(min(dat$date)), width = 6), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)", newLine,
    stringi::stri_pad_left(lubridate::month(min(dat$date)), width = 6), "  : First month of record", newLine,
    "  ", lubridate::year(min(dat$date)), "  : First year of record (1901 if not linked to a specific year)", newLine,
    "", newLine,
    "  Average ETo (mm/day)", newLine,
    "=======================", newLine,
    paste0(dat$pet_text, collapse = newLine)
  )

  return(text)
}
