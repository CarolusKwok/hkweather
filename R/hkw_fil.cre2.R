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
#' @examples hkw_fil.cre2(URL = "1", DIR = "1.png", Time = 1, listfail = F)
hkw_fil.cre2 = function(URL, DIR, Time, listfail){
  hkw_lib()

  #Check if URL, DIR, and Time are the same in length
  if(length(URL) != length(DIR)){
    return(message("Error: URL and DIR list length are not the same"))
  }
  if(length(URL) != length(Time)){
    return(message("Error: URL and Time list length are not the same"))
  }
  if(length(DIR) != length(Time)){
    return(message("Error: DIR and Time list length are not the same"))
  }
  #Set additional variables
  ApptN = 0
  FailN = 0
  FailL = list()

  df = data.frame(URL = URL,
                  DIR = DIR,
                  Time = Time) %>%
    mutate(FileP = file.exists(DIR)) %>%
    filter(FileP == F) %>%
    arrange(Time)

  if(nrow(df) > 0){
    DDays = round(as.numeric(difftime(max(df$Time), min(df$Time), units = "days")), digits = 3)
  } else {
    DDays = 0
  }

  #Download start!
  message(      "..........Initiate download process..........")
  message(      "Download process info")
  message(paste("     Starting Time:", with_tz(Sys.time(), tz = "HongKong"), paste = ""))
  message(      "Download info")
  message(paste("    First file:", basename(df$DIR[       1])))
  message(paste("     Last file:", basename(df$DIR[nrow(df)])))
  message(paste("      Duration:", DDays, "days", paste = ""))

  if(nrow(df) > 0){
    defaultW <- getOption("warn")
    options(warn = -1)

    safe_download <- safely(~ download.file(.x , .y, mode = "wb", quiet = T))
    walk2(df$URL, df$DIR, safe_download)

    options(warn = defaultW)
  }

  message(       "--------------Download complete--------------")
  message(       "Download process info")
  message(paste0("    Ending Time: ", with_tz(Sys.time(), tz = "HongKong")))
  message(       "Download info")
  message(paste0("  Download attempts: ", nrow(df)))
  df = df %>%
    mutate(success = file.exists(DIR)) %>%
    filter(success == F)

  message(paste0("   Failed downloads: ", nrow(df)))

  if(listfail){
    message(     "Failed date/time (HKT) as follow:")
    df = df %>%
      select(Time) %>%
      mutate(Year  = sprintf("%02d", year(Time)),
             Month = sprintf("%02d",month(Time)),
             Day   = sprintf("%02d",day(Time)),
             Hour  = sprintf("%02d",hour(Time)),
             Minute= sprintf("%02d",minute(Time))) %>%
      mutate(Time_N = paste0(Year, Month, Day,"-",Hour,Minute))
    print(df$Time_N)
  }
}
