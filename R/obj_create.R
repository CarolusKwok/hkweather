#' Creates an hkweather obj from source files
#'
#' @param rDIR Reading Directory. Which folder should I read to get all the information?
#' @param type Type of data. Which type of data it is? "tide" or "wind"
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
  File_list = stringr::str_sort(File_list)
  File_s = tools::file_path_sans_ext(basename(File_list[1]))
  File_e = tools::file_path_sans_ext(basename(File_list[length(File_list)]))

  #Create a dataframe, and force all data to have the same class as the first item
  df = File_list %>% lapply(fread) %>% rbindlist()

  #Rename and Reorder df
  if(type == "tide"){
    colnames(df) = c("Station", "Date", "Time", "Height")
    df = df %>%
      mutate(Height = as.numeric(Height)) %>%
      mutate(newDate = paste(Date, substr(Time, 1, 2))) %>%
      mutate(newTime = paste0("Min_", substr(Time, 4,5))) %>%
      select(-Date, -Time) %>%
      rename(Hour = newDate, Min = newTime) %>%
      relocate(Station, Hour, Min, .before = everything())
    colnames(df) = c("Station", "Hour", "Min", "Tidal Height (m)")
  }

  #North       = 360
  #Northwest   = 315
  #West        = 270
  #Southwest   = 225
  #South       = 180
  #Southeast   = 135
  #East        = 90
  #Northeast   = 45
  #Variable    = -10
  #Calm        = -20
  #N/A

  if(type == "wind"){
    colnames(df) = c("DateTime", "Station", "Direction", "Wind", "Gust")
    df = df %>%
      mutate(newDate = paste0(substr(DateTime, 1, 4),
                              "-", substr(DateTime, 5, 6),
                              "-", substr(DateTime, 7, 8),
                              " ", substr(DateTime, 9, 10))) %>%
      mutate(newTime = paste0("Min_", substr(DateTime, 11, 12))) %>%
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
      rename(Hour = newDate, Min = newTime) %>%
      relocate(Station, Hour, Min, .before = everything())
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
             data   = list(df))

  obj
}
