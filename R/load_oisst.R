#' Download NOAA OISST data
#'
#' OISST is a daily, global sea surface temperature data, with a resolution of ~0.25deg since 1981-09-01
#'
#' @param SDay Starting Day, which is the older time
#' @param EDay Ending Day, which is the later time
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_oisst("1981-09-01", "1981-09-01")
load_oisst = function(EDay = "1981-09-01", SDay = "1981-09-01", listfail = F){
  hkw_lib()

  OISST_base_url = "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
  URL = data.frame(t = seq(as.Date(SDay), as.Date(EDay), by = "day")) %>%
    mutate(t_day = gsub("-", "", t),
           t_month = substr(t_day, 1, 6),
           t_year = year(t),
           URL = paste0(OISST_base_url, t_month, "/", "oisst-avhrr-v02r01.", t_day ,".nc"),
           DIR = paste0(getwd(),"/Data/OISST/", t_year, "/", t_month, "/", t_day, ".nc"))

  hkw_dir.cre3(wDIR = URL$DIR, filename = T)
  hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$t,listfail = listfail)
}
