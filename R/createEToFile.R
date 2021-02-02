
#' @export
#' @import data.table
createEToFile <- function(dat, placename) {

  dat$pet.amount <- format(round(dat$pet.amount, 1), nsmall = 1)

  dat$pet_text <- NA_character_

  dat$pet_text <- stringi::stri_pad_left(dat$pet.amount, width = 10)

  text = paste0(
    placename, "\n",
    "     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)\n",
    stringi::stri_pad_left(lubridate::day(min(dat$date)), width = 6), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)\n",
    stringi::stri_pad_left(lubridate::month(min(dat$date)), width = 6), "  : First month of record\n",
    "  ", lubridate::year(min(dat$date)), "  : First year of record (1901 if not linked to a specific year)\n",
    "\n",
    "  Average ETo (mm/day)\n",
    "=======================\n",
    paste0(dat$pet_text, collapse = "\n")
  )

  return(text)
}
