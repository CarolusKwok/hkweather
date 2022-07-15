#' Download tidal data from MD
#'
#' Downloads and stores them in the working directory orderly according to date.
#' This is a Level 3 code.
#'
#' @param ETime Ending Time. Only accepts POSIXct date/time only.
#' @param DDays Duration of Time between Ending Time and Starting Time (in days). Defaults as 7 days. Only accepts numerical values.
#' @param STime Starting Time. Only accepts POSIXct date/time only.
#' @param lan Language of descriptive text within the pictures. Only accepts "e"/ "c" as char.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples MDtide()
MDtide = function(ETime = Sys.time(), DDays = 7, STime = NA, lan = "e", listfail = F){
  hkw_lib()
  #Checks the input data
  flag_ETime = !is.POSIXct(ETime)
  flag_DDays = !is.numeric(DDays)
  flag_STime = !(is.POSIXct(STime) | is.na(STime))
  flag_lan = !(lan == "e" | lan == "c")
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all      = sum(flag_ETime, flag_DDays, flag_STime, flag_listfail)
  if(flag_all > 0){
    message("Warning! Something is wrong in the input.")
    if(flag_ETime)   {message("Variable ETime is wrong! (POSIXct only)")}
    if(flag_DDays)   {message("Variable DDays is wrong! (numeric only)")}
    if(flag_STime)   {message("Variable STime is wrong! (POSIXct only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return("---Download Failed---")
  }
  #Force ETime and STime to be HongKong Standard Time
  ETime = with_tz(ETime, tzone = "HongKong")
  if(!is.na(STime)){
    STime = with_tz(STime, tzone = "HongKong")
  }
  #Check if STime > ETime
  if(!is.na(STime) & STime > ETime){
    TempTime = STime
    STime = ETime
    ETime = STime
  }
  #Convert lan into HKG standard
  lan_HKG = ifelse(lan == "e", "en", "tc")
  #Find time
  TempTime = minute(ETime) %% 10
  Time_Spl = data.frame(Time  = c("Now", "ETime"),
                        Year  = c(year(ETime)  ,  year(ETime - minutes(TempTime))),
                        Month = c(month(ETime) , month(ETime - minutes(TempTime))),
                        Day   = c(day(ETime)   ,   day(ETime - minutes(TempTime))),
                        Hour  = c(hour(ETime)  ,  hour(ETime - minutes(TempTime))),
                        Min   = c(minute(ETime),minute(ETime - minutes(TempTime))))
  #Create STime
  if(!is.na(STime)){
    TempTime = minute(STime) %% 10
    Time_Spl = rbind(Time_Spl, data.frame(Time = "STime",
                                          Year =  year(STime - minutes(TempTime)),
                                          Month =month(STime - minutes(TempTime)),
                                          Day =    day(STime - minutes(TempTime)),
                                          Hour =  hour(STime - minutes(TempTime)),
                                          Min = minute(STime - minutes(TempTime))))
  }
  #Create POSITX
  Time_Spl$ISO = ISOdatetime(Time_Spl$Year, Time_Spl$Month, Time_Spl$Day, Time_Spl$Hour, Time_Spl$Min, 0, tz = "HongKong")
  #Override DDays if STime present
  if(!is.na(STime)){
    DDays = as.double(difftime(Time_Spl$ISO[2], Time_Spl$ISO[3], units = "days"))+1
  }
  #List URL and DIR
  Tide_URL = data.frame(Num = 1:(144*DDays))
  Tide_URL = subset(Tide_URL, Num != 0)
  Tide_URL$Time = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], Time_Spl$Hour[2], Time_Spl$Min[2], 0, tz = "HongKong") - minutes((Tide_URL$Num - 1)*10)
  Tide_URL$Time_N = Tide_URL$Time - minutes(10)
  Tide_URL$Date = paste0(sprintf("%04d",  year(Tide_URL$Time)),
                         sprintf("%02d", month(Tide_URL$Time)),
                         sprintf("%02d",   day(Tide_URL$Time)))
  Tide_URL$Date_N = paste0(sprintf("%04d",  year(Tide_URL$Time_N)),
                           sprintf("%02d", month(Tide_URL$Time_N)),
                           sprintf("%02d",   day(Tide_URL$Time_N)))
  Tide_URL$Hour = paste0(sprintf("%02d",  hour(Tide_URL$Time)),
                         sprintf("%02d",minute(Tide_URL$Time)))
  Tide_URL$Hour_N = paste0(sprintf("%02d",  hour(Tide_URL$Time_N)),
                           sprintf("%02d",minute(Tide_URL$Time_N)))
  Tide_URL$URL  = paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Ftide1.hydro.gov.hk%2Fhotide%2FOpenData%2FAll_",
                         lan_HKG, ".csv&time=", Tide_URL$Date,"-", Tide_URL$Hour)
  Tide_URL$DIR  = paste0(getwd(), "/Tide(MD)", toupper(lan), "/", Tide_URL$Date_N, "/",
                         "Tide(MD)", toupper(lan), "-", Tide_URL$Date_N, "-", Tide_URL$Hour_N, ".csv")
  hkw_dir.cre(pri = c("/Tide(MD)", toupper(lan)), sec = Tide_URL$Date_N)
  hkw_fil.cre(DTime = ETime, DDays = DDays,
              URL = Tide_URL$URL, DIR = Tide_URL$DIR, Time = Tide_URL$Time_N,
              listfail = listfail)
}
