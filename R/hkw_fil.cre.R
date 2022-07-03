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
  message(".........Initiate download process.........")
  message(paste("Starting Date/Time:", DTime, "HKT", paste = ""))
  message(paste("  Duration of time:", DDays, "days", paste = ""))
  defaultW <- getOption("warn")
  options(warn = -1)
  for(i in length(URL):1){
    URL_C = URL[i]
    DIR_C = DIR[i]
    if(!file.exists(DIR_C)){
      ApptN = ApptN + 1
      tryCatch(download.file(URL_C, DIR_C, mode = "wb", quiet = T),
               error = function(e){
                 FailL <<- append(FailL, format(Time[i], "%Y%m%d-%H%M"))
                 FailN <<- FailN + 1})
    }
  }
  options(warn = defaultW)
  message("-------------Download complete-------------")
  message(paste(" Download attempts: ", ApptN, sep = ""))
  message(paste("  Failed downloads: ", FailN, sep = ""))
  if(listfail){
    message("Failed date/time (HKT) as follow:")
    message(paste(shQuote(FailL), collapse=", "))
}
}
