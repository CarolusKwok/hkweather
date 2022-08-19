#' Warning! This is an internal command!
#' Users should not use this!
#' Creates files for HKWeather.
#'
#' @param URL xxx
#' @param DIR xxx
#' @param Time xxx
#' @param listfail xxx
#' @param dit xxx
#'
#' @return
#' @export
#'
#' @examples hkw_fil.cre2(URL = "1", DIR = "1.png", Time = 1, listfail = F)
hkw_fil.cre4 = function(URL, DIR, Time, listfail, dit = 10){
  hkweather::hkw_lib()

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
  message(paste("      Attempts:", nrow(df)))

  attp = nrow(df)

  if(nrow(df) > 0){
    defaultW <- getOption("warn")
    options(warn = -1)

    base = "hello world"

    passenger = function(url, time, dir, dit){
      tryCatch(download.file(url = url, destfile = dir, mode = "wb", quiet = T),
               error = function(e){})
      success = (file.exists(dir) & file.info(dir)$size > 0)


      if(success == F){
        org_url = url
        org_time = time
        org_dir = dir

        for(j in list(seq(1, dit-1, 1), -1, -2, -3, -4, -5)){
          attp = attp + 1
          new_time = time + minutes(j)
          new_url = paste0(substr(url, 1, nchar(url) - 13),
                           sprintf("%04d",   year(new_time)),
                           sprintf("%02d",  month(new_time)),
                           sprintf("%02d",    day(new_time)),
                           "-",
                           sprintf("%02d", hour(new_time)),
                           sprintf("%02d", minute(new_time)))
          tryCatch(download.file(url = new_url, destfile = dir, mode = "wb", quiet = T),
                   error = function(e){})
          success = (file.exists(dir) & file.info(dir)$size > 0)

          if(success == T){
            break
          }
        }
      }

    }

    options(warn = defaultW)
  }

  message(       "--------------Download complete--------------")
  message(       "Download process info")
  message(paste0("    Ending Time: ", with_tz(Sys.time(), tz = "HongKong")))
  message(       "Download info")
  message(paste0("  Attempts: ", attp))
  suc = df %>%
    mutate(success = file.exists(DIR)) %>%
    filter(success == T)

  message(paste0("   Success: ", nrow(suc)))
  message(paste0("   Failure: ", attp - nrow(suc)))

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
