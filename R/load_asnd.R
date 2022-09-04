#' xx
#'
#' @param ETime The time of the lastest image. Only accepts POSIXct (tip: create POSIXct via ISOdatetime).
#'
#' @return
#' @export
#'
#' @examples load_asnd()
load_asnd = function(ETime = Sys.time(), DDays = 7, STime = NA){
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
                   "&STNM=45004")

  #Start reading Wyoming
  df_final = data.frame(Station = NA,
                        Hour = NA,
                        Min = NA,
                        Type = NA,
                        Value = NA)

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
    read_URL = URL$URL[i]
    read_Time = with_tz(URL$Time[i], tzone = "HongKong")


    defaultW = getOption("warn")
    options(warn = -1)
    while(readlines == T){
      read_attp = read_attp + 1
      read_line = tryCatch(readLines(read_URL),
                           error = function(e){NA})
      if(!is.na(read_line[1]) & !("Can't get" %in% read_line)){
        readlines = F
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

    #Summary Processing
    df = data.frame(Lines = read_line)
    index_s = match(x = "</PRE><H3>Station information and sounding indices</H3><PRE>", df$Lines) +1
    index_e = match(x = "<P>Description of the ", df$Lines) -2
    df = data.frame(Lines = df[c(index_s:index_e),1],
                    Position = NA,
                    Value = NA) %>%
      mutate(Position = as.numeric(gregexpr(':', Lines)),
             Value = substr(Lines,(Position + 2),999),
             Type = trimws(substr(Lines, 1, (Position - 1)))) %>%
      select(-c(Position, Lines)) %>%
      filter(Type != "" & !(Type %in% c("Station latitude", "Station longitude", "Station elevation")))
    index_s = match(x = "Station number", df$Type)
    df = df %>%
      mutate(Station = df$Value[index_s]) %>%
      filter(!(row_number() %in% index_s))
    index_s = match(x = "Observation time", df$Type)
    df = df %>%
      mutate(Hour = paste0(sprintf("%04d", year(read_Time)), "-",
                           sprintf("%02d", month(read_Time)), "-",
                           sprintf("%02d", day(read_Time)), " ",
                           sprintf("%02d", hour(read_Time))),
             Min = "Min_00") %>%
      filter(!(row_number() %in% index_s)) %>%
      relocate(Station, Hour, Min, Type, Value)

    df_final = bind_rows(df_final, df)
  }
  message(       "--------------Download complete--------------")
  message(       "Download process info")
  message(paste0("    Ending Time: ", with_tz(Sys.time(), tz = "HongKong")))
  message(       "Download info")
  message(paste0("  Attempts: ", attp_sum))
  df_final = df_final %>%
    drop_na() %>%
    arrange(Hour) %>%
    arrange(Station) %>%
    arrange(Type) %>%
    mutate(Value = as.numeric(Value))
  df_final
}
