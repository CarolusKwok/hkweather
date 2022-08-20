#' Creates an hkweather obj from source files
#'
#' @param rDIR Reading Directory. Which folder should I read to get all the information?
#' @param type Type of data. Which type of data it is? (Only accepts gtmp/ mslp/ rain/ rhum/ sart/ temp/ tide/ wind)
#'
#' @return
#' @export
#'
#' @examples obj_create(getwd(), type = "tide")
obj_create = function(rDIR, type){
  hkweather::hkw_lib()
  #Create a list of files to read in the dataframe
  File_list = list()
  for (i in 1:length(rDIR)){
    File_list = append(File_list, list.files(rDIR[i], pattern = "*.csv", full.names = T))
  }

  #Check variables!
  if(length(File_list) < 1){
    return(message("ERROR: No csv files present!"))
  }
  if(length(type) != 1 | !(type %in% c("gtmp", "mslp", "rain", "rhum", "sart", "temp", "tide", "wind"))){
    return(message("ERROR: Data type incorrect!"))
  }

  #Start! Sort the files
  File_list = stringr::str_sort(File_list)
  File_s = tools::file_path_sans_ext(basename(File_list[1]))
  File_e = tools::file_path_sans_ext(basename(File_list[length(File_list)]))

  #Create a dataframe, and force all data to have the same class as the first item
  df = File_list %>% lapply(fread) %>% rbindlist()

  #Rename and Reorder df
  if(type == "gtmp"){
    colnames(df) = c("DateTime", "Station", "GTMP")
    df = df %>%
      mutate(Hour = paste0(substr(DateTime, 1, 4),
                           "-", substr(DateTime, 5, 6),
                           "-", substr(DateTime, 7, 8),
                           " ", substr(DateTime, 9, 10))) %>%
      mutate(Min = paste0("Min_", substr(DateTime, 11, 12))) %>%
      mutate(GTMP = as.numeric(GTMP)) %>%
      select(-DateTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #"Station", Hour, Min, "GTMP"
    colnames(df) = c("Station", "Hour", "Min", "Grass Temperature (°C)")
  }

  if(type == "mslp"){
    #Date time,Automatic Weather Station,Mean Sea Level Pressure(hPa)
    colnames(df) = c("DateTime", "Station", "MSLP")
    df = df %>%
      mutate(Hour = paste0(substr(DateTime, 1, 4),
                           "-", substr(DateTime, 5, 6),
                           "-", substr(DateTime, 7, 8),
                           " ", substr(DateTime, 9, 10))) %>%
      mutate(Min = paste0("Min_", substr(DateTime, 11, 12))) %>%
      mutate(MSLP = as.numeric(MSLP)) %>%
      select(-DateTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #Station, Hour, Min, MSLP
    colnames(df) = c("Station", "Hour", "Min", "Mean Sea Level Pressure (hPa)")
  }

  if(type == "rain"){
    return(message("ERROR: rain is not support yet!"))
  }

  if(type == "rhum"){
    #Date time,Automatic Weather Station,Relative Humidity(percent)
    colnames(df) = c("DateTime", "Station", "RHUM")
    df = df %>%
      mutate(Hour = paste0(substr(DateTime, 1, 4),
                           "-", substr(DateTime, 5, 6),
                           "-", substr(DateTime, 7, 8),
                           " ", substr(DateTime, 9, 10))) %>%
      mutate(Min = paste0("Min_", substr(DateTime, 11, 12))) %>%
      mutate(RHUM = as.numeric(RHUM)) %>%
      select(-DateTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #Station, Hour, Min, RHUM
    colnames(df) = c("Station", "Hour", "Min", "Relative Humidity (%)")
  }

  if(type == "sart"){
    #Date time,Automatic Weather Station,Global Solar Radiation(watt/square meter),Direct Solar Radiation(watt/square meter),Diffuse Radiation(watt/square meter)
    colnames(df) = c("DateTime", "Station", "SART_Glo", "SART_Dir", "SART_Dif")
    df = df %>%
      mutate(Hour = paste0(substr(DateTime, 1, 4),
                           "-", substr(DateTime, 5, 6),
                           "-", substr(DateTime, 7, 8),
                           " ", substr(DateTime, 9, 10))) %>%
      mutate(Min = paste0("Min_", substr(DateTime, 11, 12))) %>%
      mutate(SART_Glo = as.numeric(SART_Glo)) %>%
      mutate(SART_Dir = as.numeric(SART_Dir)) %>%
      mutate(SART_Dif = as.numeric(SART_Dif)) %>%
      select(-DateTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #Station, Hour, Min, SART_Glo, SART_Dir, SART_Dif
    colnames(df) = c("Station", "Hour", "Min", "Global Solar Radiation (W/m2)", "Direct Radiation (W/m2)", "Diffuse Radiation (W/m2)")
  }

  if(type == "temp"){
    #Date time,Automatic Weather Station,Air Temperature(degree Celsius)
    colnames(df) = c("DateTime", "Station", "TMEP")
    df = df %>%
      mutate(Hour = paste0(substr(DateTime, 1, 4),
                           "-", substr(DateTime, 5, 6),
                           "-", substr(DateTime, 7, 8),
                           " ", substr(DateTime, 9, 10))) %>%
      mutate(Min = paste0("Min_", substr(DateTime, 11, 12))) %>%
      mutate(TEMP = as.numeric(TEMP)) %>%
      select(-DateTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #Station, Hour, Min, TEMP
    colnames(df) = c("Station", "Hour", "Min", "Air Temperature (°C)")
  }


  if(type == "tide"){
    colnames(df) = c("Station", "Date", "Time", "TIDE")
    df = df %>%
      mutate(TIDE = as.numeric(TIDE)) %>%
      mutate(newDate = paste(Date, substr(Time, 1, 2))) %>%
      mutate(newTime = paste0("Min_", substr(Time, 4,5))) %>%
      select(-Date, -Time) %>%
      rename(Hour = newDate, Min = newTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #Station, Hour, Min, TIDE
    colnames(df) = c("Station", "Hour", "Min", "Tidal Height (m)")
  }

  if(type == "wind"){
    colnames(df) = c("DateTime", "Station", "Direction", "Wind", "Gust")
    df = df %>%
      mutate(Hour = paste0(substr(DateTime, 1, 4),
                           "-", substr(DateTime, 5, 6),
                           "-", substr(DateTime, 7, 8),
                           " ", substr(DateTime, 9, 10))) %>%
      mutate(Min = paste0("Min_", substr(DateTime, 11, 12))) %>%
      mutate(Direction = ifelse(Direction == "North",     360,
                         ifelse(Direction == "Northwest", 315,
                         ifelse(Direction == "West",      270,
                         ifelse(Direction == "Southwest", 225,
                         ifelse(Direction == "South",     180,
                         ifelse(Direction == "Southeast", 135,
                         ifelse(Direction == "East",       90,
                         ifelse(Direction == "Northeast",  45,
                         ifelse(Direction == "Variable",  -10,
                         ifelse(Direction == "Calm",      -20, NA))))))))))) %>%
      mutate(Direction = as.numeric(Direction)) %>%
      mutate(Wind = as.numeric(Wind)) %>%
      mutate(Gust = as.numeric(Gust)) %>%
      select(-DateTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    #Station, Hour, Min, Direction, Wind, Gust
    colnames(df) = c("Station", "Hour", "Min", "Mean Direction", "10-min mean wind speed (km/h)", "10-min max gust speed (km/h)")
  }

  df = df %>%
    pivot_longer(-c("Station", "Hour", "Min"), names_to = "Type", values_to = "Value") %>%
    distinct() %>%
    arrange(Min) %>%
    arrange(Hour) %>%
    arrange(Station) %>%
    arrange(Type)

  #Write list of df and sheet name
  df = as.data.frame(df)
  obj = list(type = type,
             legend = list("all"),
             data   = list(df),
             pivot  = list("XXXX"))
  class(obj) = "hkweather"
  obj
}
