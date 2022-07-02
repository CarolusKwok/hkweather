#' Download lighting And radar images from HKO and stores them in the working directory orderly according to date.
#'
#' @param range The radar image range in kilometers. Only accepts values 64 or 256.
#' @param type The type of lighting data, which includes "cloud to cloud"(CC) and "cloud to ground"(CG). Only accepts CC and CG
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param DTime The starting time of images to be downloaded, starting from the latest time. Only accepts POSIXct.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0
#'
#' @return
#' @export
#'
#' @examples HKOlighting()
HKOlighting = function(range = 64, type = "CC", DDays = 4.5, DTime = Sys.time(), listfail = F){
  #Checks the input data
  flag_range = ifelse((range == 64 | range == 256),F,T)
  flag_type  = ifelse((type == "CC"| type == "CG"),F,T)
  flag_DDays = !is.numeric(DDays)
  flag_DTime = !lubridate::is.POSIXct(DTime)
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
  updt = ifelse(range == 64, 6, 12)
  dit = ifelse(range == 256, 6, 0)
  FailN = 0
  Fail = list()
  #Find the current time and the ending time of the duration
  Time_Spl = data.frame(Time  = c("Now", "Lighting"),
                        Year  = lubridate::year(DTime),
                        Month = lubridate::month(DTime),
                        Day   = lubridate::day(DTime),
                        Hour  = lubridate::hour(DTime),
                        Min   = c(lubridate::minute(DTime), lubridate::minute(DTime)-lubridate::minute(DTime)%%updt + dit))
  Lig_URL = data.frame(Num = seq(1:(1440/updt*DDays)))
  Lig_URL$Time = ISOdate(Time_Spl$Year[2], Time_Spl$Month[2], Time_Spl$Day[2], Time_Spl$Hour[2], Time_Spl$Min[2], 0, tz = "") - lubridate::minutes((Lig_URL$Num - 1)*updt)
  Lig_URL$Date = paste(sprintf("%02d", lubridate::month(Lig_URL$Time)), sprintf("%02d", lubridate::day(Lig_URL$Time)), sep="")
  Lig_URL$URL  = paste("https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_", range, type, "_",
                       sprintf("%04d",   lubridate::year(Lig_URL$Time)),
                       sprintf("%02d",  lubridate::month(Lig_URL$Time)),
                       sprintf("%02d",    lubridate::day(Lig_URL$Time)),
                       sprintf("%02d",   lubridate::hour(Lig_URL$Time)),
                       sprintf("%02d", lubridate::minute(Lig_URL$Time)),
                       ".png", sep="")
  Lig_URL$DIR  = paste(getwd(), "/Lighting", sprintf("%03d", range), type, "/", Lig_URL$Date, "/",
                       sprintf("%03d", range),  type,"-",
                       sprintf("%04d",   lubridate::year(Lig_URL$Time)),
                       sprintf("%02d",  lubridate::month(Lig_URL$Time)),
                       sprintf("%02d",    lubridate::day(Lig_URL$Time)),"-",
                       sprintf("%02d",   lubridate::hour(Lig_URL$Time)),
                       sprintf("%02d", lubridate::minute(Lig_URL$Time)),
                       ".png", sep="")
  Fld_Dis = data.frame(Date = unique(Lig_URL$Date))
  dir.create(paste(getwd(), "/Lighting", sprintf("%03d", range), type, sep=""), showWarnings = F)
  for(i in 1:nrow(Fld_Dis)){
    dir.create(paste(getwd(), "/Lighting", sprintf("%03d", range), type, "/", Fld_Dis$Date[i], sep=""), showWarnings = F)
  }
  message("...Initiate download process...")
  for(i in nrow(Lig_URL):1){
    URL = Lig_URL$URL[i]
    DIR = Lig_URL$DIR[i]
    if(!file.exists(DIR)){
      tryCatch(download.file(URL, DIR, mode = "wb", quiet = T),
               error = function(e){
                 Fail <<- append(Fail, format(Lig_URL$Time[i], "%Y%m%d %H:%M"))
                 FailN <<- FailN + 1})
    }
  }
  message("---Download complete---")
  message(paste("Number of failed downloads:", FailN))
  message(paste("Starting Date/Time: "), DTime)
  message(paste("Duration of time:", DDays, "days"))
  if(listfail){
    message("Failed date/time as follow:")
    message(paste(shQuote(Fail), collapse=", "))
  }
}
