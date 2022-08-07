#' Download wind data as image
#'
#' @param type Type of wind. Only accepts "gust"/ "wind" as char.
#' @param lan Language of the descriptive text
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_wind()
load_wind = function(type = "wind", lan = "en", listfail = F){
  hkweather::hkw_lib()
  #Addtional variables
  dit = 10
  if(type == "wind"){
    url_s = "https://www.hko.gov.hk/wxinfo/ts/wind"
  }
  if(type == "gust"){
    url_s = "https://www.hko.gov.hk/wxinfo/ts/windgust/gust"
  }

  if(lan == "en"){
    nlan = "e"
  }
  if(lan == "tc"){
    nlan = "c"
  }
  if(lan == "sc"){
    nlan = "c"
    lan = "tc"
  }

  #For ETime to be HK Time
  ETime = Sys.time()
  ETime = with_tz(ETime, tzone = "HongKong")

  #Find the latest available time
  LTime = ISOdatetime(year  =   year(ETime - minutes(minute(ETime) %% dit + dit)),
                      month =  month(ETime - minutes(minute(ETime) %% dit + dit)),
                      day   =    day(ETime - minutes(minute(ETime) %% dit + dit)),
                      hour  =   hour(ETime - minutes(minute(ETime) %% dit + dit)),
                      min   = minute(ETime - minutes(minute(ETime) %% dit + dit)),
                      sec   = 0,
                      tz = "HongKong")

  #Starting to download
  URL              = data.frame(Num = seq(1, 143, 1))
  URL$Time         = LTime - minutes((URL$Num - 1) * dit)
  URL$Date_p       = paste0(sprintf("%04d",   year(URL$Time)),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Time_p       = paste0(sprintf("%02d",   hour(URL$Time)),
                            sprintf("%02d", minute(URL$Time)))
  URL$URL          = paste0(url_s, nlan, "hk_", URL$Time_p, ".png")
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "WIND",
                            "/", "WIND-", type, "-", lan,
                            "/", substr(URL$Date_p, 1, 4),
                            "/", substr(URL$Date_p, 1, 6),
                            "/", URL$Date_p,
                            "/", "WIND-", type, "-", lan, "-", URL$Date_p, "-", URL$Time_p, ".png")

  #         https://www.hko.gov.hk/wxinfo/ts/windehk_0000.png#
  #https://www.hko.gov.hk/wxinfo/ts/windgust/gustehk_1930.png#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}
