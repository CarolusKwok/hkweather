#' Download wind data from HKO and stores them in the working directory orderly according to date.
#'
#' @param type Type of wind. Only accepts "gust"/ "wind" as char.
#' @param lan Language of descriptive text within the pictures. Only accepts "e"/ "c" as char.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples HKOwind()
HKOwind = function(type = "wind", lan = "e", listfail = F){
  #Check the input data
  flag_type = ifelse((type == "gust" | type == "wind"), F, T)
  flag_lan = ifelse((lan == "e" | lan == "c"), F, T)
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_type + flag_lan + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_type){message("Variable type is wrong! (gust/ wind as char only)")}
    if(flag_lan) {message("Variable lan is wrong! (e/ c as char only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    message("---Download Failed---")
    return(message("---------------------\n"))
  }
  #Define additional variables
  URL_s = ifelse(type == "gust", "https://www.hko.gov.hk/wxinfo/ts/windgust/", "https://www.hko.gov.hk/wxinfo/ts/")
  type_casted = ifelse(type == "gust", "Gust", "Wind")
  lan_casted = toupper(lan)
  DTime = Sys.time()
  DTime_HKT = lubridate::with_tz(DTime, tz = "HongKong")
  #Find the current time and the ending time of the duration, and download!
  Time_Spl = data.frame(Time  = c("Now", "Wind"),
                        Year  = lubridate::year(DTime_HKT),
                        Month = lubridate::month(DTime_HKT),
                        Day   = lubridate::day(DTime_HKT),
                        Hour  = lubridate::hour(DTime_HKT),
                        Min   = c(lubridate::minute(DTime_HKT), lubridate::minute(DTime_HKT)-lubridate::minute(DTime_HKT)%%10))
  Wind_URL = data.frame(Num = 1:144)
  Wind_URL$Time = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], Time_Spl$Hour[2], Time_Spl$Min[2], 0, tz = "") - lubridate::minutes((Wind_URL$Num - 1)*10)
  Wind_URL$Date = paste(sprintf("%04d", lubridate::year( Wind_URL$Time)),
                        sprintf("%02d", lubridate::month(Wind_URL$Time)),
                        sprintf("%02d", lubridate::day(  Wind_URL$Time)),
                        sep = "")
  Wind_URL$URL = paste(URL_s, type, lan, "hk_",
                       sprintf("%02d", lubridate::hour(  Wind_URL$Time)),
                       sprintf("%02d", lubridate::minute(Wind_URL$Time)), ".png",
                       sep = "")
  Wind_URL$DIR = paste(getwd(),"/",type_casted, lan_casted,"/",Wind_URL$Date,"/",
                       type_casted, lan_casted, "-", Wind_URL$Date,"-",
                       sprintf("%02d", lubridate::hour(Wind_URL$Time)),
                       sprintf("%02d", lubridate::minute(Wind_URL$Time)), ".png",
                       sep = "")
  hkw_dir.cre(pri = c(type_casted, lan_casted), sec = unique(Wind_URL$Date))
  hkw_fil.cre(DTime = DTime, DDays = 1,
              URL = Wind_URL$URL, DIR = Wind_URL$DIR, Time = Wind_URL$Time,
              listfail = listfail)
}
