#' Download lighting and radar images
#'
#' Download from HKO and stores them in the working directory orderly according to date.
#'
#' @param range The radar image range in kilometers. Only accepts values 64 or 256.
#' @param type The type of lighting data, which includes "cloud to cloud"(CC) and "cloud to ground"(CG). Only accepts CC and CG
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param DTime The starting time of images to be downloaded, starting from the latest time. Only accepts POSIXct (tip: create POSIXct via ISOdate).
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples HKOlighting()
HKOlighting = function(range = 64, type = "CC", DDays = 4.5, DTime = Sys.time(), listfail = F){
  hkw_lib()
  #Checks the input data
  flag_range = ifelse((range == 64 | range == 256),F,T)
  flag_type  = ifelse((type == "CC"| type == "CG"),F,T)
  flag_DDays = !is.numeric(DDays)
  flag_DTime = !is.POSIXct(DTime)
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_range + flag_type + flag_DDays + flag_DTime + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_range){message("Variable range is wrong! (64/ 256 as numeric only)")}
    if(flag_type){message("Variable type is wrong! (CC/ CG as char only)")}
    if(flag_DDays){message("Variable DDays is wrong! (numeric values only)")}
    if(flag_DTime){message("Variable DTime is wrong! (POSIXct date/time only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return("---Download Failed---")
  }
  #Define additional variables
  DTime = with_tz(DTime, tz = "HongKong")
  updt = ifelse(range == 64, 6, 12)
  dit = ifelse(range == 256, 6, 0)
  #Find the current time and the ending time of the duration, and download!
  Time_Spl = data.frame(Time  = c("Now", "Lighting"),
                        Year  = year(DTime),
                        Month = month(DTime),
                        Day   = day(DTime),
                        Hour  = hour(DTime),
                        Min   = c(minute(DTime), minute(DTime)-minute(DTime)%%updt + dit))
  Lig_URL = data.frame(Num = seq(1:(1440/updt*DDays)))
  Lig_URL$Time = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], Time_Spl$Hour[2], Time_Spl$Min[2], 0, tz = "") - minutes((Lig_URL$Num - 1)*updt)
  Lig_URL$Date = paste(sprintf("%04d", year( Lig_URL$Time)),
                       sprintf("%02d", month(Lig_URL$Time)),
                       sprintf("%02d", day(  Lig_URL$Time)),
                       sep="")
  Lig_URL$URL  = paste("https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_", range, type, "_",
                       sprintf("%04d",   year(Lig_URL$Time)),
                       sprintf("%02d",  month(Lig_URL$Time)),
                       sprintf("%02d",    day(Lig_URL$Time)),
                       sprintf("%02d",   hour(Lig_URL$Time)),
                       sprintf("%02d", minute(Lig_URL$Time)),
                       ".png", sep="")
  Lig_URL$DIR  = paste(getwd(), "/Lighting", sprintf("%03d", range), type, "/", Lig_URL$Date, "/",
                       sprintf("%03d", range),  type,"-",
                       sprintf("%04d",   year(Lig_URL$Time)),
                       sprintf("%02d",  month(Lig_URL$Time)),
                       sprintf("%02d",    day(Lig_URL$Time)),"-",
                       sprintf("%02d",   hour(Lig_URL$Time)),
                       sprintf("%02d", minute(Lig_URL$Time)),
                       ".png", sep="")
  hkw_dir.cre(pri = c("/Lighting", sprintf("%03d", range), type), sec = unique(Lig_URL$Date))
  hkw_fil.cre(DTime = DTime, DDays = DDays,
              URL = Lig_URL$URL, DIR = Lig_URL$DIR, Time = Lig_URL$Time,
              listfail = listfail)
}
