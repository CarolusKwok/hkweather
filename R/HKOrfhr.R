#' Download hourly rainfall images
#'
#' Download from HKO.
#' Stores them in the working directory orderly according to date.
#'
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param DTime The starting time of images to be downloaded, at the latest time. Only accepts POSIXct (tip: create POSIXct via ISOdate).
#' @param STime The ending time of images to be downloaded, at the earliest time. Only accepts POSIXct (tip: create POSIXct via ISOdate).
#' @param lan Language of descriptive text within the pictures. Only accepts "e"/ "c" as char.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples HKOrfhr()
HKOrfhr = function(DDays = 7, DTime = Sys.time(), STime = NA, lan = "e", listfail = F){
  hkw_lib()
  #Check input data
  flag_DDays = !is.numeric(DDays)
  flag_DTime = !is.POSIXct(DTime)
  flag_lan = ifelse(lan == "e" | lan == "c", 0, 1)
  flag_STime = !(is.POSIXct(STime)|is.na(STime))
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_DDays + flag_DTime + flag_lan + flag_STime + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input.")
    if(flag_DDays){message("Variable DDays is wrong! (numeric only)")}
    if(flag_DTime){message("Variable DTime is wrong! (POSIXct only)")}
    if(flag_lan){message("Variable lan is wrong! (c/e as char only)")}
    if(flag_STime){message("Variable STime is wrong! (POSIXct only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return("---Download Failed---")
  }
  #Define global variables
  DTime = with_tz(DTime, tzone = "hongkong")
  if(!is.na(STime)){
    STime = with_tz(STime, tzone = "HongKong")
  }
  #Convert STime to DDays
  if(!is.na(STime)){
    message("STime variable is found!")
    message("Variable DDays is override")
    STime = STime - minute(STime)%%15
    if(STime > DTime){
      message("STime is larger than DTime!")
      TempTime = STime
      STime = DTime
      DTime = TempTime
      message("STime and DTime are flipped!")
    }
    DDays = as.double(difftime(DTime, STime, units = "days"))
  }
  #List URL and DIR
  Time_Spl = data.frame(Time  = c("Now", "Rfhr"),
                        Year  = year(DTime),
                        Month = month(DTime),
                        Day   = day(DTime),
                        Hour  = hour(DTime),
                        Min   = c(minute(DTime), minute(DTime)-minute(DTime)%%15))
  Rfhr_URL      = data.frame(Num = 1:(96*DDays))
  Rfhr_URL$Time = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], Time_Spl$Hour[2], Time_Spl$Min[2], 0, tz = "") - minutes((Rfhr_URL$Num - 1)*15)
  Rfhr_URL$Date = paste(sprintf("%04d",  year(Rfhr_URL$Time)),
                        sprintf("%02d", month(Rfhr_URL$Time)),
                        sprintf("%02d",   day(Rfhr_URL$Time)),
                        sep = "")
  Rfhr_URL$URL  = paste("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap",
                        Rfhr_URL$Date,
                        sprintf("%02d", hour(Rfhr_URL$Time)),
                        sprintf("%02d", minute(Rfhr_URL$Time)), lan, ".png",
                        sep = "")
  Rfhr_URL$DIR  = paste(getwd(), "/Rfhr", toupper(lan), "/", Rfhr_URL$Date, "/",
                        "Rfhr", toupper(lan), "-", Rfhr_URL$Date, "-",
                        sprintf("%02d", hour(Rfhr_URL$Time)),
                        sprintf("%02d", minute(Rfhr_URL$Time)), ".png",
                        sep = "")
  hkw_dir.cre(pri = c("/Rfhr", toupper(lan)), sec = Rfhr_URL$Date)
  hkw_fil.cre(DTime = DTime, DDays = DDays,
              URL = Rfhr_URL$URL, DIR = Rfhr_URL$DIR, Time = Rfhr_URL$Time,
              listfail = listfail)
}
