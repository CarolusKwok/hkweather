#' Downloads satellite images (v2)
#'
#' @param ETime The time of the lastest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param STime The time of the earliest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#'
#'
#'
#'
#' @param magn The satellite image magnification. Only accepts 2/ 4/ 8
#' @param type The type of satellite image, which includes "True color" ("tc"), "Infrared" ("ir"), and "Deep convection" ("dc").
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_satl()
load_satl = function(ETime = Sys.time(), DDays = 3.5, STime = NA, magn = 8, type = "dc", listfail = F){
  hkweather::hkw_lib()
  #Test input
  flag_ETime = !is.POSIXct(ETime)
  flag_DDays = !is.numeric(DDays)
  flag_STime = ifelse(!is.na(STime), !is.POSIXct(STime), F)
  flag_magn = ifelse((magn ==   8 | magn ==   4 | magn ==   2 ), F, T)
  flag_type = ifelse((type == "dc"| type == "tc"| type == "ir"), F, T)
  flag_listfail = !(is.numeric(listfail) | is.logical(listfail))
  flag_all   = flag_ETime + flag_DDays + flag_STime + flag_magn + flag_type + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_ETime){message("Variable ETime is wrong! (POSIXct date/time only)")}
    if(flag_DDays){message("Variable DDays is wrong! (numeric values only)")}
    if(flag_STime){message("Variable STime is wrong! (POSIXct date/time only)")}
    if(flag_magn){message("Variable magn is wrong! (2/ 4/ 8 as numeric only)")}
    if(flag_type){message("Variable type is wrong! (dc/ tc/ ir as char only)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return(message("---Download Failed---"))
  }

  #Addtional variables
  dit = 10

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
  URL$Time_UTC     = with_tz(URL$Time, tzone = "UTC")
  URL$Date_UTC_p   = paste0(sprintf("%04d",   year(URL$Time_UTC)),
                            sprintf("%02d",  month(URL$Time_UTC)),
                            sprintf("%02d",    day(URL$Time_UTC)))
  URL$Time_UTC_p   = paste0(sprintf("%02d",   hour(URL$Time_UTC)),
                            sprintf("%02d", minute(URL$Time_UTC)),
                            "00")
  URL$URL          = paste0("https://www.hko.gov.hk/wxinfo/intersat/satellite/image/images/h8L_",
                            type,"_x",magn,"M_",URL$Date_UTC_p, URL$Time_UTC_p,".png")
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "SATL",
                            "/", "SATL", sprintf("%02d", magn), type,
                            "/", substr(URL$Date_p, 1, 4),
                            "/", substr(URL$Date_p, 1, 6),
                            "/", URL$Date_p,
                            "/", "SATL", sprintf("%02d", magn), type, "_", URL$Date_p, "_", URL$Time_p, ".png")

  #https://www.hko.gov.hk/wxinfo/intersat/satellite/image/images/h8L_dc_x8M_20220802082000.png#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}
