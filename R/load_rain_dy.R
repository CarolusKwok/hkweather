#' Download daily rainfall images
#'
#' Download from HKO.
#' Stores them in the working directory orderly according to year.
#'
#' @param lan Language of descriptive text within the pictures. Only accepts "e"/ "c" as char.
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_rain_dy()
load_rain_dy = function(lan = "en", listfail = F){
  hkweather::hkw_lib()
  #Test input
  #Addtional variables
  ETime = Sys.time()
  dit = 1
  if(lan == "en"){
    nlan = "e"
  }
  if(lan == "tc"){
    nlan = "c"
  }
  if(lan == "sc"){
    lan = "tc"
    nlan = "c"
  }

  #Find the latest available time
  LTime = ISOdatetime(year  =   year(ETime - days(1)),
                      month =  month(ETime - days(1)),
                      day   =    day(ETime - days(1)),
                      hour  = 12,
                      min   = 00,
                      sec   = 00,
                      tz = "HongKong")

  #Find the latest leap year
  for(i in year(LTime):0){
    if(leap_year(i)){
      leap = i
      break
    }
  }

  #Starting to download
  URL              = data.frame(Num = seq(1, 366, 1))
  URL$Time         = LTime - days((URL$Num - 1))
  URL$Time[366]    = ISOdatetime(year  = leap,
                                 month =  2,
                                 day   = 29,
                                 hour  = 12,
                                 min   = 00,
                                 sec   = 00,
                                 tz = "HongKong")
  URL$Date_p       = paste0(sprintf("%04d",   year(URL$Time)),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Date_m       = paste0(sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$URL          = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap24hrs",
                            URL$Date_m, "0000", nlan, ".png")
  URL$DIR          = paste0(getwd(),
                            "/", "Data",
                            "/", "RAIN",
                            "/", "RAIN(png)-dy-", lan,
                            "/", substr(URL$Date_p, 1, 4),
                            "/", substr(URL$Date_p, 1, 6),
                            "/", "RAIN-dy-", toupper(lan), "-", URL$Date_p, ".png")

  #"https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap24hrs05010000c.png"#
  #"https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap24hrs05010000e.png"#
  #Demo for the website

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
               listfail = listfail)
}
