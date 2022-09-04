#' Download weather images from HKO
#'
#' @param ETime The time of the lastest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param DDays The duration of images to be downloaded in days. Only accepts numerical values.
#' @param STime The time of the earliest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param type The abbr of the station, which includes lfs, wlp, elc, kfb, tpk, tm2, tlc, sk2, skg, cwb, cwa, ks2, klt, hko, ic2, cp1, vpb, vpa, gsi, swh, slw, dnl, pe2, cch, cce,lam, wl2, wgl. You may include multiple stations in your download, using c(station1, station2, ...).
#' @param listfail List the date/time of failed download attempts. Only accepts T/F/1/0.
#'
#' @return
#' @export
#'
#' @examples load_wxph()
load_wxph = function(ETime = Sys.time(), DDays = 2.5, STime = NA, type = "all", listfail = F){
  hkweather::hkw_lib()
  #Addtional variables
  dit = 5
  type_list = c("lfs", "wlp", "elc", "kfb", "tpk",
                "tm2", "tlc", "sk2", "skg", "cwb",
                "cwa", "ks2", "klt", "hko", "ic2",
                "cp1", "vpb", "vpa", "gsi", "swh",
                "slw", "dnl", "pe2", "cch", "cce",
                "lam", "wl2", "wgl"               )

  #Test input
  if("all" %in% type){type = type_list}
  flag_ETime = !is.POSIXct(ETime)
  flag_DDays = !is.numeric(DDays)
  flag_STime = ifelse(!is.na(STime), !is.POSIXct(STime), F)
  flag_type = F
  for(i in 1:length(type)){
    if(!(type[i] %in% type_list)){
      flag_type = T
      break
    }
  }
  flag_listfail = !(listfail == 1 | listfail == 0 | is.logical(listfail))
  flag_all   = flag_ETime + flag_DDays + flag_STime + flag_type + flag_listfail
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_ETime){message("Variable ETime is wrong! (POSIXct date/time only)")}
    if(flag_DDays){message("Variable DDays is wrong! (numeric values only)")}
    if(flag_STime){message("Variable STime is wrong! (POSIXct date/time only)")}
    if(flag_type){message("Variable type is wrong! (choose from list!)")}
    if(flag_listfail){message("Variable listfail is wrong! (T/F/1/0 only)")}
    return(message("---Download Failed---"))
  }

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
  URL              = data.frame(Num = seq(1, 288*DDays-1, 1))
  URL$Time         = LTime - minutes((URL$Num - 1) * dit)
  URL$Date_p       = paste0(sprintf("%04d",   year(URL$Time)),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Date_p_n     = paste0(sprintf("%02d",   year(URL$Time) %% 100),
                            sprintf("%02d",  month(URL$Time)),
                            sprintf("%02d",    day(URL$Time)))
  URL$Time_p       = paste0(sprintf("%02d",   hour(URL$Time)),
                            sprintf("%02d", minute(URL$Time)))

  for(i in 1:length(type)){
    select = type[i]
    URL$URL = paste0("http://www.weather.gov.hk/wxinfo/aws/hko_mica/",
                     tolower(select),
                     "/img", toupper(select), "_", URL$Date_p_n, "_", URL$Time_p, ".jpg")
    URL$DIR = paste0(getwd(),
                     "/", "Data",
                     "/", "WXPH",
                     "/", "WXPH(", select, ")",
                     "/", substr(URL$Date_p, 1, 4),
                     "/", substr(URL$Date_p, 1, 6),
                     "/", URL$Date_p,
                     "/", "WXPH(", select, ")_", URL$Date_p, "_", URL$Time_p, ".jpg")

    #01: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/lfs/imgLFS_220817_1430.jpg"
    #02: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/wlp/imgWLP_220817_1445.jpg"
    #03: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/elc/imgELC_220817_1440.jpg"
    #04: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/kfb/imgKFB_220817_1405.jpg"
    #05: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/tpk/imgTPK_220817_1515.jpg"
    #06: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/tm2/imgTM2_220817_1420.jpg"
    #07: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/tlc/imgTLC_220817_1625.jpg"
    #08: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/sk2/imgSK2_220817_1415.jpg"
    #09: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/skg/imgSKG_220817_1420.jpg"
    #10: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/cwb/imgCWB_220817_1425.jpg"
    #11: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/cwa/imgCWA_220817_1420.jpg"
    #12: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/ks2/imgKS2_220817_1420.jpg"
    #13: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/klt/imgKLT_220817_1430.jpg"
    #14: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/hko/imgHKO_220817_1445.jpg"
    #15: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/ic2/imgIC2_220817_1610.jpg"
    #16: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/cp1/imgCP1_220817_1520.jpg"
    #17: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/vpb/imgVPB_220817_1430.jpg"
    #18: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/vpa/imgVPA_220817_1425.jpg"
    #19: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/gsi/imgGSI_220817_1445.jpg"
    #20: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/swh/imgSWH_220817_1430.jpg"
    #21: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/slw/imgSLW_220817_1435.jpg"
    #22: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/dnl/imgDNL_220817_1430.jpg"
    #23: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/pe2/imgPE2_220817_1435.jpg"
    #24: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/cch/imgCCH_220817_1420.jpg"
    #25: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/cce/imgCCE_220817_1430.jpg"
    #26: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/lam/imgLAM_220817_1430.jpg"
    #27: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/wl2/imgWL2_220817_1425.jpg"
    #28: "https://www.weather.gov.hk/wxinfo/aws/hko_mica/wgl/imgWGL_220817_1435.jpg"
    #Demo for the website
    hkw_dir.cre3(wDIR = URL$DIR, filename = T)
    hkw_fil.cre2(URL = URL$URL, DIR = URL$DIR, Time = URL$Time,
                 listfail = listfail)
    }
}
