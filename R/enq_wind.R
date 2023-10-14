#' Find wind speed at different common scales and tropical cyclone scales
#'
#' @param kmh Wind speed in kilometer per hour
#' @param mps Wind speed in meter per second
#' @param mph Wind speed in mile per hour
#' @param fps Wind speed in feet per second
#' @param kt Wind speed in knots
#'
#' @return 1 dataframe
#' @export
#'
#' @examples enq_wind(10, 20, 30, 40, 60)
enq_wind = function(kmh, mps, mph, fps, kt){
  hkweather::hkw_lib()

  #Check the input
  flag_kmh = ifelse(!hasArg(kmh), F, ifelse(is.numeric(kmh), F, T))
  flag_mps = ifelse(!hasArg(mps), F, ifelse(is.numeric(mps), F, T))
  flag_mph = ifelse(!hasArg(mph), F, ifelse(is.numeric(mph), F, T))
  flag_fps = ifelse(!hasArg(fps), F, ifelse(is.numeric(fps), F, T))
  flag_kt  = ifelse(!hasArg(kt), F, ifelse(is.numeric(kt), F, T))

  flag_all = flag_kmh | flag_mps | flag_mph | flag_fps | flag_kt

  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_kmh){message("ERROR: variable kmh is incorrect. (Only accepts numbers)")}
    if(flag_mps){message("ERROR: variable mps is incorrect. (Only accepts numbers")}
    if(flag_mph){message("ERROR: variable mph is incorrect. (Only accepts numbers)")}
    if(flag_fps){message("ERROR: variable fps is incorrect. (Only accepts numbers)")}
    if(flag_kt ){message("ERROR: variable kt is incorrect. (Only accepts numbers)")}
    return(message("---ENQUIRE ERROR---"))
  }

  #START#
  if(hasArg(kmh)){
    df_kmh = data.frame(kmh = kmh,
                        mps = NA,
                        mph = NA,
                        fps = NA,
                        kt  = NA) %>%
      mutate(mps = kmh/3.6,
             mph = kmh * 0.621371992,
             fps = kmh * 0.911344415,
             kt  = kmh * 0.539956803)

  }
  else {
    df_kmh = data.frame(kmh = NA,
                        mps = NA,
                        mph = NA,
                        fps = NA,
                        kt  = NA)
  }
  if(hasArg(mps)){
    df_mps = data.frame(kmh = NA,
                        mps = mps,
                        mph = NA,
                        fps = NA,
                        kt  = NA) %>%
      mutate(kmh = mps * 3.6,
             mph = kmh * 0.621371992,
             fps = kmh * 0.911344415,
             kt  = kmh * 0.539956803)
  }
  else {df_mps = data.frame(kmh = NA,
                            mps = NA,
                            mph = NA,
                            fps = NA,
                            kt  = NA)}
  if(hasArg(mph)){
    df_mph = data.frame(kmh = NA,
                        mps = NA,
                        mph = mph,
                        fps = NA,
                        kt  = NA) %>%
      mutate(kmh = mph * 1.609344,
             mps = kmh/3.6,
             fps = kmh * 0.911344415,
             kt  = kmh * 0.539956803)
  }
  else {df_mph = data.frame(kmh = NA,
                            mps = NA,
                            mph = NA,
                            fps = NA,
                            kt  = NA)}
  if(hasArg(fps)){
    df_fps = data.frame(kmh = NA,
                        mps = NA,
                        mph = NA,
                        fps = fps,
                        kt  = NA) %>%
      mutate(kmh = fps * 1.09728,
             mps = kmh/3.6,
             mph = kmh * 0.621371992,
             kt  = kmh * 0.539956803)

  }
  else {
    df_fps = data.frame(kmh = NA,
                        mps = NA,
                        mph = NA,
                        fps = NA,
                        kt  = NA)
  }
  if(hasArg(kt)){
    df_kt  = data.frame(kmh = NA,
                        mps = NA,
                        mph = NA,
                        fps = NA,
                        kt  = kt) %>%
      mutate(kmh = kt * 1.852,
             mps = kmh/3.6,
             mph = kmh * 0.621371992,
             fps = kmh * 0.911344415)

  }
  else {
    df_kt  = data.frame(kmh = NA,
                        mps = NA,
                        mph = NA,
                        fps = NA,
                        kt  = NA)
  }

  df = bind_rows(df_kmh, df_mps, df_mph, df_fps, df_kt) %>%
    drop_na() %>%
    mutate(beaufort = ifelse(kmh <= 2,   "00",
                      ifelse(kmh <= 6,   "01",
                      ifelse(kmh <= 12,  "02",
                      ifelse(kmh <= 19,  "03",
                      ifelse(kmh <= 30,  "04",
                      ifelse(kmh <= 40,  "05",
                      ifelse(kmh <= 51,  "06",
                      ifelse(kmh <= 62,  "07",
                      ifelse(kmh <= 75,  "08",
                      ifelse(kmh <= 87,  "09",
                      ifelse(kmh <= 103, "10",
                      ifelse(kmh <= 117, "11",
                      ifelse(kmh <= 132, "12",
                      ifelse(kmh <= 149, "13",
                      ifelse(kmh <= 166, "14",
                      ifelse(kmh <= 184, "15",
                      ifelse(kmh <= 201, "16",
                      ifelse(kmh <= 219, "17",
                      ifelse(kmh > 219, "17+", NA)))))))))))))))))))) %>%
    mutate(sshws = ifelse(kmh <= 62, "TD",
                   ifelse(kmh <= 118, "TS",
                   ifelse(kmh <= 153, "C1",
                   ifelse(kmh <= 177, "C2",
                   ifelse(kmh <= 208, "C3",
                   ifelse(kmh <= 251, "C4",
                   ifelse(kmh > 251, "C5", NA)))))))) %>%
    mutate(jtwc = ifelse(kmh <= 62, "TD",
                  ifelse(kmh <= 117, "TS",
                  ifelse(kmh <= 239, "T",
                  ifelse(kmh > 239, "SuperT", NA))))) %>%
    mutate(jma = ifelse(kmh <=  61, "TD",
                 ifelse(kmh <=  88, "TS",
                 ifelse(kmh <= 117, "STS",
                 ifelse(kmh <= 156, "TY",
                 ifelse(kmh <= 193, "STY",
                 ifelse(kmh >  194, "VTY", NA))))))) %>%
    mutate(hko = ifelse(kmh <= 62, "TD",
                 ifelse(kmh <= 87, "TS",
                 ifelse(kmh <= 117, "STS",
                 ifelse(kmh <= 149, "T",
                 ifelse(kmh <= 184, "ST",
                 ifelse(kmh > 184, "SuperT", NA)))))))
  return(df)
}
