#' Present HKO predicted tide information
#'
#' @param Station Code of the Tidal Station. Only accepts CCH/ CLK/ CMW/ KCT/ KLW/ LOP/ MWC/ QUB/ SPW/ TAO/ TBT/ TMW/ TPK/ WAG.
#' @param ETime The latest time to be presented. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param STime The earliest time to be presented. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#'
#' @return
#' @export
#'
#' @examples load_tide_pre()
load_tide_pre = function(Station = "all", ETime = Sys.time(), STime = Sys.time(), Fullname = T){
  hkweather::hkw_lib()

  #Test input
  tide_pre_station = tide_pre_station

  if("all" %in% Station){
    Station = tide_pre_station$Station
  }

  flag_Station = F
  for(i in 1:length(Station)){
    if(!(Station[i] %in% tide_pre_station$Station)){
      flag_Station = T
      break
    }
  }
  flag_ETime = !is.POSIXct(ETime)
  flag_STime = !is.POSIXct(STime)
  flag_all   = flag_Station + flag_ETime + flag_STime
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_Station){message("Variable Station is wrong! (CCH/ CLK/ CMW/ KCT/ KLW/ LOP/ MWC/ QUB/ SPW/ TAO/ TBT/ TMW/ TPK/ WAG only)")}
    if(flag_ETime){message("Variable ETime is wrong! (POSIXct date/time only)")}
    if(flag_STime){message("Variable STime is wrong! (POSIXct date/time only)")}
    return(message("---Download Failed---"))
  }

  #Additional Variable
  Choice = unique(Station)

  #Filter Station
  df = tide_pre %>%
    filter(Station %in% Choice)

  #Filter Time
  ETime = with_tz(ETime, "HongKong")
  STime = with_tz(STime, "HongKong")
  ETime = ISOdatetime(year =  year(ETime - hours(hour(ETime) %% 1  -1)),
                      month = month(ETime - hours(hour(ETime) %% 1 -1)),
                      day =   day(ETime - hours(hour(ETime) %% 1   -1)),
                      hour =  hour(ETime - hours(hour(ETime) %% 1  -1)),
                      min =   0,
                      sec =   0,
                      tz = "HongKong")
  STime = ISOdatetime(year =  year(STime - hours(hour(ETime) %% 1  +1)),
                      month = month(STime - hours(hour(ETime) %% 1 +1)),
                      day =   day(STime - hours(hour(ETime) %% 1   +1)),
                      hour =  hour(STime - hours(hour(ETime) %% 1  +1)),
                      min =   0,
                      sec =   0,
                      tz = "HongKong")
  df = df %>%
    filter(DateTime >= STime & DateTime <= ETime) %>%
    select(-DateTime)

  #Use full name not code if required
  if(Fullname == T){
    df = inner_join(df, tide_pre_station, by = "Station") %>%
      mutate(Station = Fullname) %>%
      select(-Fullname) %>%
      arrange(Station)
  }
  df
}
