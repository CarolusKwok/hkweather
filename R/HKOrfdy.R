#' Download daily rainfall images
#'
#' Download from HKO.
#' Stores them in the working directory orderly according to year.
#'
#' @param ETime The latest/ending time of images to be downloaded, at the latest time. Only accepts POSIXct (tip: create POSIXct via ISOdate).
#' @param lan Language of descriptive text within the pictures. Only accepts "e"/ "c" as char.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples HKOrfdy()
HKOrfdy = function(ETime = Sys.time(), lan = "e", listfail = F){
  hkw_lib()
  #Checks if input is normal
  flag_ETime = !is.POSIXct(ETime)
  flag_lan = ifelse(lan == "e" | lan == "c", 0, 1)
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_ETime + flag_lan + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input.")
    if(flag_ETime){message("Variable ETime is wrong! (POSIXct only)")}
    if(flag_lan){message("Variable lan is wrong! (c/e as char only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return("---Download Failed---")
  }
  #Start!
  Time_Spl = data.frame(Time  = c("Now", "Rfhr"),
                        Year  = year(ETime),
                        Month = month(ETime),
                        Day   = day(ETime))
  Rfdy_URL = data.frame(Num = 1:366)
  Rfdy_URL$Time = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], 12, 0, 0, tz = "HongKong") - days(Rfdy_URL$Num)
  for(i in year(ETime):0){
    if(leap_year(i)){
      leap = i
      break
    }
  }
  Rfdy_URL$Time[366] = ISOdate(leap, 2, 29, 12, 0, 0, tz = "HongKong")
  Rfdy_URL$Date = paste0(sprintf("%04d", year(Rfdy_URL$Time)),
                         sprintf("%02d", month(Rfdy_URL$Time)),
                         sprintf("%02d",   day(Rfdy_URL$Time)))
  Rfdy_URL$Year = sprintf("%04d", year(Rfdy_URL$Time))
  Rfdy_URL$URL = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap24hrs",
                        sprintf("%02d", month(Rfdy_URL$Time)),
                        sprintf("%02d",   day(Rfdy_URL$Time)),
                        "0000",lan,".png")
  Rfdy_URL$DIR = paste0(getwd(), "/Rfdy", toupper(lan), "/", Rfdy_URL$Year, "/",
                        "Rfdy", toupper(lan), "-", Rfdy_URL$Date, ".png")
  hkw_dir.cre(pri = c("/Rfdy", toupper(lan)), sec = Rfdy_URL$Year)
  hkw_fil.cre(DTime = ETime, DDays = 365,
              URL = Rfdy_URL$URL, DIR = Rfdy_URL$DIR, Time = Rfdy_URL$Time,
              listfail = listfail)
}
