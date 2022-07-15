#' Warning! This is an internal command!
#' Users should not use this!
#' Creates files for HKWeather.
#'
#' @param DTime xxx
#' @param DDays xxx
#' @param URL xxx
#' @param DIR xxx
#' @param Time xxx
#' @param listfail xxx
#'
#' @return
#' @export
#'
#' @examples hkw_fil.cre(DTime = DTime, DDays = DDays, URL = "1", DIR = "1.png", Time = 1, listfail = F)
hkw_fil.cre = function(DTime = DTime, DDays = DDays, URL, DIR, Time, listfail){
  #Check if URL, DIR, and Time are the same in length
  if(length(URL) != length(DIR)){
    message("Error: URL and DIR list length are not the same")
    return()
  }
  if(length(URL) != length(Time)){
    message("Error: URL and Time list length are not the same")
    return()
  }
  #Set additional variables
  ApptN = 0
  FailN = 0
  FailL = list()
  #Download start!
  message(      "..........Initiate download process..........")
  message(      "Download process info")
  message(paste("     Starting Time:", lubridate::with_tz(Sys.time(), tz = "HongKong"), paste = ""))
  message(      "Download info")
  message(paste("    First file:", basename(DIR[length(DIR)])))
  message(paste("     Last file:", basename(DIR[1])))
  message(paste("      Duration:", DDays, "days", paste = ""))

  defaultW <- getOption("warn")
  options(warn = -1)
  DIR_P = NA

  for(i in length(URL):1){
    URL_C = URL[i]
    DIR_C = DIR[i]
    if(basename(dirname(DIR_C)) != DIR_P | is.na(basename(dirname(DIR_C)) != DIR_P)){
      if(!is.na(DIR_P)){
        message(paste("All in", DIR_P, "is downloaded!"))
      }
      DIR_P = basename(dirname(DIR_C))
    }
    if(!file.exists(DIR_C)){
      ApptN = ApptN + 1
      tryCatch(download.file(URL_C, DIR_C, mode = "wb", quiet = T),
               error = function(e){
                 FailL <<- append(FailL, format(Time[i], "%Y%m%d-%H%M"))
                 FailN <<- FailN + 1})
    }
  }
  message(paste("All in", DIR_P, "is downloaded!"))
  options(warn = defaultW)
  message(       "--------------Download complete--------------")
  message("Download process info")
  message(paste0("    Ending Time: ", lubridate::with_tz(Sys.time(), tz = "HongKong")))
  message(paste0("Download info"))
  message(paste0(" Download attempts: ", ApptN))
  message(paste0("  Failed downloads: ", FailN))
  if(listfail){
    message(     "Failed date/time (HKT) as follow:")
    message(paste(shQuote(FailL), collapse=", "))
}
}
