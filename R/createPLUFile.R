
#' @export
#' @import data.table
createPLUFile <- function(dat, placename) {

  dat$precipitation.amount <- format(round(dat$precipitation.amount, 1), nsmall = 1)

  dat$precip_text <- NA_character_

  dat$precip_text <- stringi::stri_pad_left(dat$precipitation.amount, width = 10)

  text = paste0(
    placename, "\n",
    "     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)\n",
    stringi::stri_pad_left(lubridate::day(min(dat$date)), width = 6), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)\n",
    stringi::stri_pad_left(lubridate::month(min(dat$date)), width = 6), "  : First month of record\n",
    "  ", lubridate::year(min(dat$date)), "  : First year of record (1901 if not linked to a specific year)\n",
    "\n",
    "  Total Rain (mm)\n",
    "=======================\n",
    paste0(dat$precip_text, collapse = "\n")
  )

  return(text)
}
