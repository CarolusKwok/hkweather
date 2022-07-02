#' Downloads all the satellite images and lighting images from the Hong Kong Observatory. No parameters are utilized
#'
#' @return
#' @export
#'
#' @examples HKOall()
HKOall = function(){
  HKOlighting(range = 64, type = "CC", DDays = 4.5, DTime = Sys.time(), listfail = F)
  HKOlighting(range = 64, type = "CG", DDays = 4.5, DTime = Sys.time(), listfail = F)
  HKOlighting(range = 256, type = "CC", DDays = 4.5, DTime = Sys.time(), listfail = F)
  HKOlighting(range = 256, type = "CG", DDays = 4.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 8, type = "tc", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 4, type = "tc", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 2, type = "tc", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 8, type = "ir", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 4, type = "ir", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 2, type = "ir", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 8, type = "dc", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 4, type = "dc", DDays = 3.5, DTime = Sys.time(), listfail = F)
  HKOsatellite(magn = 2, type = "dc", DDays = 3.5, DTime = Sys.time(), listfail = F)
}
