#' Download atmospheric sounding data metrics
#'
#' Understanding the sounding is important for understanding the atmospheric condition, especially for extreme weather.
#' This catches the metrics provided by the University of Wyoming.
#'
#' @param ETime The time of the lastest data. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#' @param DDays The duration of data to be downloaded in days. Only accepts numerical values.
#' @param STime The time of the oldest data. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#'
#' @return
#' @export
#'
#' @examples load_asnd_data()
load_asnd_data = function(ETime = Sys.time(), DDays = 7, STime = NA, Station = 45004){
  hkweather::hkw_lib()
  #Addtional variables
  dit = 12

  #For ETime and STime to be HKT and have UTC version
  ETime = with_tz(ETime, tzone = "HongKong")
  ETime_UTC = with_tz(ETime, tzone = "UTC")

  if(!is.na(STime)){
    STime = with_tz(STime, tzone = "HongKong")
    STime_UTC = with_tz(STime, tzone = "UTC")
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
  hour = hour(ETime_UTC)
  LTime_UTC = ISOdatetime(year  =   year(ETime_UTC - hours(hour %% dit)),
                          month =  month(ETime_UTC - hours(hour %% dit)),
                          day   =    day(ETime_UTC - hours(hour %% dit)),
                          hour  =   hour(ETime_UTC - hours(hour %% dit)),
                          min   = 0,
                          sec   = 0,
                          tz = "UTC")

  #Create Dataframe
  URL = data.frame(Num = seq(1, 2 * DDays, 1))
  URL$Time = LTime_UTC - hours((URL$Num - 1) * dit)
  URL$Year = sprintf("%04d", as.numeric(year(URL$Time)))
  URL$Month = sprintf("%02d", as.numeric(month(URL$Time)))
  URL$DayHour = paste0(sprintf("%02d", as.numeric(day(URL$Time))), sprintf("%02d", as.numeric(hour(URL$Time))))
  URL$URL = paste0("https://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST",
                   "&YEAR=", URL$Year,
                   "&MONTH=", URL$Month,
                   "&FROM=", URL$DayHour,
                   "&TO=", URL$DayHour,
                   "&STNM=", Station)

  #Start reading Wyoming
  df_final = data.frame(PRES = NA,
                        HGHT = NA,
                        TEMP = NA,
                        DWPT = NA,
                        RHUM = NA,
                        MIXR = NA,
                        DRCT = NA,
                        SKNT = NA,
                        THTA = NA,
                        THTE = NA,
                        THTV = NA,
                        Time = NA)

  message(      "..........Initiate download process..........")
  message(      "Download process info")
  message(paste("     Starting Time:", with_tz(Sys.time(), tz = "HongKong"), paste = ""))
  message(      "Download info")
  message(paste("      Duration:", DDays, "days", paste = ""))
  message(paste("      Attempts:", nrow(URL)))
  attp_sum = 0

  for(i in 1:nrow(URL)){
    readlines = T
    read_line = NA
    read_attp = 0
    read_data = F
    read_URL = URL$URL[i]
    read_Time = with_tz(URL$Time[i], tzone = "HongKong")
    defaultW = getOption("warn")
    options(warn = -1)
    while(readlines == T){
      read_attp = read_attp + 1
      read_line = tryCatch(readLines(read_URL),
                           error = function(e){NA})
      if("   PRES   HGHT   TEMP   DWPT   RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV" %in% read_line){
        readlines = F
        read_data = T
      }
      if(read_attp >= 50){
        message("Download fail")
        readlines = F
      }
    }
    options(warn = defaultW)
    readlines = T
    message(paste0("Attempts for ", read_Time, ": ", read_attp))
    attp_sum = attp_sum + read_attp
    read_attp = 0

    #Data Processing
    if(read_data == T){
      df = data.frame(Lines = read_line)
      index_s = match("-----------------------------------------------------------------------------", df$Lines)+4
      index_e = match("</PRE><H3>Station information and sounding indices</H3><PRE>", df$Lines)-1
      df = df[(index_s:index_e),]
      df = data.frame(value = df)


      df = df %>%
        mutate(PRES = as.numeric(substr(value, 1, 7))) %>%
        mutate(HGHT = as.numeric(substr(value, 8, 8+7))) %>%
        mutate(TEMP = as.numeric(substr(value, 15, 15+7))) %>%
        mutate(DWPT = as.numeric(substr(value, 22, 22+7))) %>%
        mutate(RHUM = as.numeric(substr(value, 29, 29+7))) %>%
        mutate(MIXR = as.numeric(substr(value, 36, 36+7))) %>%
        mutate(DRCT = as.numeric(substr(value, 43, 43+7))) %>%
        mutate(SKNT = as.numeric(substr(value, 50, 50+7))) %>%
        mutate(THTA = as.numeric(substr(value, 57, 57+7))) %>%
        mutate(THTE = as.numeric(substr(value, 64, 64+7))) %>%
        mutate(THTV = as.numeric(substr(value, 71, 71+7))) %>%
        select(-value) %>%
        mutate(Time = read_Time) %>%
        mutate(RAD = DRCT * pi/180) %>%
        mutate(UWND = -SKNT * sin(RAD),
               VWND = -SKNT * cos(RAD))
      df_final = bind_rows(df_final, df)
      read_data = F
    }
  }
  message(       "--------------Download complete--------------")
  message(       "Download process info")
  message(paste0("    Ending Time: ", with_tz(Sys.time(), tz = "HongKong")))
  message(       "Download info")
  message(paste0("  Attempts: ", attp_sum))

  df_final = df_final %>%
    drop_na() %>%
    mutate(Station = Station) %>%
    mutate(Hour = paste0(year(Time), "-", month(Time), "-", day(Time), " ", hour(Time))) %>%
    mutate(Min  = "Min_00") %>%
    select(-Time) %>%
    relocate(Station, Hour, Min, .before = PRES)

  return(df_final)
}
