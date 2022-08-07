#' Downloads all the satellite images and lighting images from the Hong Kong Observatory. If there is a Chinese and English version, only downloads the English version.
#'
#' @return
#' @export
#'
#' @examples load_all()
load_all = function(listfail = F){
  message("...Downloading all available HKO data...")


  message("")
  message("Lighting --- 064km, Cloud-to-Cloud")
  load_ltng(range = 64, type = "cc", listfail = listfail)
  message("")
  message("Lighting --- 064km, Cloud-to-Ground")
  load_ltng(range = 64, type = "cg", listfail = listfail)
  message("")
  message("Lighting --- 256km, Cloud-to-Cloud")
  load_ltng(range = 256, type = "cc", listfail = listfail)
  message("")
  message("Lighting --- 256km, Cloud-to-Ground")
  load_ltng(range = 256, type = "cg", listfail = listfail)


  message("")
  message("Satellite --- 8 times, True-Color")
  load_satl(magn = 8, type = "tc", listfail = listfail)
  message("")
  message("Satellite --- 4 times, True-Color")
  load_satl(magn = 4, type = "tc", listfail = listfail)
  message("")
  message("Satellite --- 2 times, True-Color")
  load_satl(magn = 2, type = "tc", listfail = listfail)
  message("")
  message("Satellite --- 8 times, Infra-Red")
  load_satl(magn = 8, type = "ir", listfail = listfail)
  message("")
  message("Satellite --- 4 times, Infra-Red")
  load_satl(magn = 4, type = "ir", listfail = listfail)
  message("")
  message("Satellite --- 2 times, Infra-Red")
  HKOsatellite(magn = 2, type = "ir", listfail = listfail)
  message("")
  message("Satellite --- 8 times, Deep-Convection")
  load_satl(magn = 8, type = "dc", listfail = listfail)
  message("")
  message("Satellite --- 4 times, Deep-Convection")
  load_satl(magn = 4, type = "dc", listfail = listfail)
  message("")
  message("Satellite --- 2 times, Deep-Convection")
  load_satl(magn = 2, type = "dc", listfail = listfail)


  message("")
  message("Wind and Gust --- CSV")
  load_wind_csv(listfail = listfail)
  message("")
  message("Hourly Rainfall --- English")
  load_rain_hr(listfail = listfail)
  message("")
  message("Daily Rainfall --- English")
  load_rain_dy(listfail = listfail)
  message("")
  message("Rainfall --- nowcast data (English)")
  load_rain_csv(DDays = 3, listfail = listfail)


  message("")
  message("5 minute tidal height (HKO) --- English")
  load_tide(type = "hko", listfail = listfail)
  message("")
  message("10 minute tidal height (MD) --- English")
  load_tide(type = "md", listfail = listfail)


  message("")
  message("-----------Download completed-----------")
}
