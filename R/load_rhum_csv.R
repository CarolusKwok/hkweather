#' Download HKO rainfall nowcast information
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
#' @examples load_rhum_csv()
load_rhum_csv = function(ETime = Sys.time(), DDays = 7, STime = NA, lan = "en", listfail = F){
  hkweather::hkw_lib()
  #Test input
  flag_ETime = !is.POSIXct(ETime)
  flag_DDays = !is.numeric(DDays)
  flag_STime = ifelse(!is.na(STime), !is.POSIXct(STime), F)
  flag_lan   = ifelse((lan == "en" | lan == "tc" | lan == "sc"), F, T)
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_ETime + flag_DDays + flag_STime + flag_lan + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_ETime){message("Variable ETime is wrong! (POSIXct date/time only)")}
    if(flag_DDays){message("Variable DDays is wrong! (numeric values only)")}
    if(flag_STime){message("Variable STime is wrong! (POSIXct date/time only)")}
    if(flag_lan){message("Variable lan is wrong! (en/ tc/ sc as char only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return(message("---Download Failed---"))
  }
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

  URL$Time_n       = URL$Time - minutes(10)
  URL$Date_n_p     = paste0(sprintf("%04d",   year(URL$Time_n)),
                            sprintf("%02d",  month(URL$Time_n)),
                            sprintf("%02d",    day(URL$Time_n)))
  URL$Time_n_p     = paste0(sprintf("%02d",   hour(URL$Time_n)),
                            sprintf("%02d", minute(URL$Time_n)))

  URL$URL          = paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_1min_humidity",
                            nlan, ".csv&time=", URL$Date_p, "-", URL$Time_p)
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "RHUM",
                            "/", "RHUM(csv)-", lan,
                            "/", substr(URL$Date_n_p, 1, 4),
                            "/", substr(URL$Date_n_p, 1, 6),
                            "/", URL$Date_n_p,
                            "/", "RHUM-CSV", lan, "-", URL$Date_n_p, "-", URL$Time_n_p, ".csv")

  #https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_1min_humidity.csv&time=20220808-0000#
  #https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_1min_humidity_uc.csv&time=20220808-0000#
  #https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_1min_humidity_sc.csv&time=20220808-0000#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre3(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}
