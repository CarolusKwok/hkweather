#' Download lighting and radar images
#'
#' Download from HKO and stores them in the working directory orderly according to date.
#'
#' @param ETime The time of the lastest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param STime The time of the earliest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param type The type of lighting data, which includes "cloud to cloud"(CC) and "cloud to ground"(CG)
#' @param range The radar image range in kilometers. Only accepts values 64 or 256.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_ltng()
load_ltng = function(ETime = Sys.time(), DDays = 4.5, STime = NA, type = "cc", range = 64, listfail = F){
  hkweather::hkw_lib()
  #Test input

  #Addtional variables
  dit  = ifelse(range == 64, 6, 12)
  push = ifelse(range == 64, 0,  6)

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
  LTime = ISOdatetime(year  =   year(ETime - minutes(minute(ETime) %% dit + push + dit)),
                      month =  month(ETime - minutes(minute(ETime) %% dit + push + dit)),
                      day   =    day(ETime - minutes(minute(ETime) %% dit + push + dit)),
                      hour  =   hour(ETime - minutes(minute(ETime) %% dit + push + dit)),
                      min   = minute(ETime - minutes(minute(ETime) %% dit + push + dit)),
                      sec   = 0,
                      tz = "HongKong")

  #Starting to download
  URL              = data.frame(Num = seq(1, (1440/dit*DDays-1), 1))
  URL$Time         = LTime - minutes((URL$Num - 1) * dit)
  URL$Date_p       = paste0(sprintf("%04d",   year(URL$Time)),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Time_p       = paste0(sprintf("%02d",   hour(URL$Time)),
                            sprintf("%02d", minute(URL$Time)))
  URL$URL          = paste0("https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_",
                            range, toupper(type),"_", URL$Date_p, URL$Time_p,".png")
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "LTNG",
                            "/", "LTNG-", sprintf("%03d", range), type,
                            "/", substr(URL$Date_p, 1, 4),
                            "/", substr(URL$Date_p, 1, 6),
                            "/", URL$Date_p,
                            "/", sprintf("%03d", range), type, "-", URL$Date_p, "-", URL$Time_p, ".png")

  #https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_256CG_202208051730.png#
  #https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_64CG_202208051736.png#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}
