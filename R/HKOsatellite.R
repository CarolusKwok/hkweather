#' Download satellite images (imaging over East Asia) from HKO and stores them in the working directory orderly according to date.
#'
#' @param magn The satellite image magnification. Only accepts 2/ 4/ 8
#' @param type The type of satellite image, which includes "True color" (tc), "Infrared" (ir), and "Deep convection" (dc). Only accepts "tc", "ir", or "dc".
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param DTime The starting time of images to be downloaded, starting from the latest time. Only accepts POSIXct (tip: create POSIXct via base::ISOdate).
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples HKOsatellite()
HKOsatellite = function(magn = 8, type = "dc", DDays = 3.5, DTime = Sys.time(), listfail = F){
  #Checks the input data
  flag_magn = ifelse((magn == 8 | magn == 4 | magn == 2),F,T)
  flag_type  = ifelse((type == "dc"| type == "tc" | type == "ir"),F,T)
  flag_DDays = !is.numeric(DDays)
  flag_DTime = !lubridate::is.POSIXct(DTime)
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_magn + flag_type + flag_DDays + flag_DTime + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_magn){message("Variable magn is wrong! (2/ 4/ 8 as numeric only)")}
    if(flag_type){message("Variable type is wrong! (dc/ tc/ ir as char only)")}
    if(flag_DDays){message("Variable DDays is wrong! (numeric values only)")}
    if(flag_DTime){message("Variable DTime is wrong! (POSIXct date/time only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return("---Download Failed---")
  }
  #Define additional variables
  DTime = lubridate::with_tz(DTime, tz = "HongKong")
  #Find the current time and the ending time of the duration, and download!
  Time_Spl = data.frame(Time  = c("Now", "Sat"),
                        Year  = lubridate::year(DTime),
                        Month = lubridate::month(DTime),
                        Day   = lubridate::day(DTime),
                        Hour  = lubridate::hour(DTime),
                        Min   = c(lubridate::minute(DTime), lubridate::minute(DTime)-lubridate::minute(DTime)%%10))
  Sat_URL          = data.frame(Num = seq(1:(144*DDays)))
  Sat_URL$Time     = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], Time_Spl$Hour[2], Time_Spl$Min[2], 0, tz = "") - lubridate::minutes((Sat_URL$Num - 1)*10)
  Sat_URL$Time_UTC = lubridate::with_tz(Sat_URL$Time, tzone = "UTC")
  Sat_URL$Date     = paste(sprintf("%04d", lubridate::year( Sat_URL$Time)),
                           sprintf("%02d", lubridate::month(Sat_URL$Time)),
                           sprintf("%02d", lubridate::day(  Sat_URL$Time)),
                           sep = "")
  Sat_URL$URL      = paste("https://www.hko.gov.hk/wxinfo/intersat/satellite/image/images/h8L_",
                           type, "_x", magn, "M_",
                           sprintf("%04d", lubridate::year(  Sat_URL$Time_UTC)),
                           sprintf("%02d", lubridate::month( Sat_URL$Time_UTC)),
                           sprintf("%02d", lubridate::day(   Sat_URL$Time_UTC)),
                           sprintf("%02d", lubridate::hour(  Sat_URL$Time_UTC)),
                           sprintf("%02d", lubridate::minute(Sat_URL$Time_UTC)),
                           "00.png", sep = "")
  Sat_URL$DIR      = paste(getwd(), "/Sat", sprintf("%02d", magn), type, "/", Sat_URL$Date, "/",
                           magn, type, "-",
                           sprintf("%04d", lubridate::year(  Sat_URL$Time)),
                           sprintf("%02d", lubridate::month( Sat_URL$Time)),
                           sprintf("%02d", lubridate::day(   Sat_URL$Time)), "-",
                           sprintf("%02d", lubridate::hour(  Sat_URL$Time)),
                           sprintf("%02d", lubridate::minute(Sat_URL$Time)),
                           ".png", sep = "")
  hkw_dir.cre(pri = c("/Sat", sprintf("%02d", magn), type), sec = unique(Sat_URL$Date))
  hkw_fil.cre(DTime = DTime, DDays = DDays,
              URL = Sat_URL$URL, DIR = Sat_URL$DIR, Time = Sat_URL$Time,
              listfail = listfail)
}
