#' Download Wind and Gust data from HKO in CSV format
#'
#' Downloads and stores them in the working directory orderly according to date.
#'
#' @param ETime The newest csv to be downloaded, starting from the latest time. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param STime The oldest csv to be downloaded, ending at the earliest time. Only accepts POSIXct (tip: create POSIXct via ISOdatetime)
#' @param lan Language of the descriptive text
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_wind_csv()
load_wind_csv = function(ETime = Sys.time(), DDays = 7, STime = NA, lan = "en", listfail = F){
  hkweather::hkw_lib()
  #Test input
  #Addtional variables
  dit = 10
  if(lan == "en"){
    nlan = ""
  }
  if(lan == "tc"){
    nlan = "_uc"
  }
  if(lan == "sc"){
    nlan = "_sc"
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
  URL              = data.frame(Num = seq(1, 144*DDays-1, 1))
  URL$Time         = LTime - minutes((URL$Num - 1) * dit)
  URL$Date_p       = paste0(sprintf("%04d",   year(URL$Time)),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Time_p       = paste0(sprintf("%02d",   hour(URL$Time)),
                            sprintf("%02d", minute(URL$Time)))
  URL$Time_n       = URL$Time - minutes(dit)
  URL$Date_n_p     = paste0(sprintf("%04d",   year(URL$Time_n)),
                            sprintf("%02d",  month(URL$Time_n)),
                            sprintf("%02d",    day(URL$Time_n)))
  URL$Time_n_p     = paste0(sprintf("%02d",   hour(URL$Time_n)),
                            sprintf("%02d", minute(URL$Time_n)))
  URL$URL          = paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_10min_wind",
                            nlan, ".csv&time=", URL$Date_p, "-",URL$Time_p)
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "WIND",
                            "/", "WIND(csv)-", lan,
                            "/", substr(URL$Date_p, 1, 4),
                            "/", substr(URL$Date_p, 1, 6),
                            "/", URL$Date_p,
                            "/", "WIND-CSV", toupper(lan), "-", URL$Date_n_p, "-", URL$Time_n_p, ".csv")

  #"https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_10min_wind.csv&time=20220805-0000"#
  #"https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_10min_wind_uc.csv&time=20220805-0000"#
  #"https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_10min_wind_sc.csv&time=20220805-0000"#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}
