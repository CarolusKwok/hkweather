#' Download hourly rainfall images
#'
#' Download from HKO.
#' Stores them in the working directory orderly according to date.
#'
#' @param ETime The newest png to be downloaded, starting from the latest time. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param STime The oldest png to be downloaded, ending at the earliest time. Only accepts POSIXct (tip: create POSIXct via ISOdatetime)
#' @param lan Language of the descriptive text
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_rain_hr()
load_rain_hr = function(ETime = Sys.time(), DDays = 7, STime = NA, lan = "en", listfail = F){
  hkweather::hkw_lib()
  #Addtional variables
  dit = 15
  if(lan == "en"){
    nlan = "e"
  }
  if(lan == "tc"){
    nlan = "c"
  }
  if(lan == "sc"){
    lan = "tc"
    nlan = "c"
  }

  #For ETime and STime to be HK Time
  ETime = with_tz(ETime, tzone = "HongKong")
  if(!is.na(STime)){
    STime = with_tz(STime, tzone = "HongKong")
  }

  #Check if ETime < STime and Redefine DDays if STime exist
  if(!is.na(STime) & ETime < STime){
    TTime = ETime
    ETime = STime
    STime = TTime
  }
  if(!is.na(STime)){
    DDays = as.double(difftime(ETime, STime, units = "days"))
  }

  #Find the latest available time
  LTime = ISOdatetime(year  =   year(ETime - minutes(minute(ETime) %% dit + dit)),
                      month =  month(ETime - minutes(minute(ETime) %% dit + dit)),
                      day   =    day(ETime - minutes(minute(ETime) %% dit + dit)),
                      hour  =   hour(ETime - minutes(minute(ETime) %% dit + dit)),
                      min   = minute(ETime - minutes(minute(ETime) %% dit + dit)),
                      sec   = 0,
                      tz = "HongKong")

  #Starting to download
  URL              = data.frame(Num = seq(1, 96*DDays-1, 1))
  URL$Time         = LTime - minutes((URL$Num - 1) * dit)
  URL$Date_p       = paste0(sprintf("%04d",   year(URL$Time)),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Time_p       = paste0(sprintf("%02d",   hour(URL$Time)),
                            sprintf("%02d", minute(URL$Time)))
  URL$URL          = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap",
                            URL$Date_p, URL$Time_p, nlan, ".png")
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "RAIN",
                            "/", "RAIN(png)-hr-", lan,
                            "/", substr(URL$Date_p, 1, 4),
                            "/", substr(URL$Date_p, 1, 6),
                            "/", URL$Date_p,
                            "/", "RAIN-hr-", toupper(lan), "-", URL$Date_p, "-", URL$Time_p, ".png")

  #"https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap202208061415e.png"#
  #"https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap202208061415c.png"#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}