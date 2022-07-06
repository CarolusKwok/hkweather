#' Orders HKOtide results
#'
#' Tidies up HKOtide csv files.
#' Can create csv in working directory with default name.
#'
#' @param DIR Path of the directory. Accepts list of directory.
#' @param load Writes CSV file to Working Directory.
#'
#' @return
#' @export
#'
#' @examples HKOtide(getwd())
cre_csv_tide = function(DIR, load = F){
  hkw_lib()
  file = list()
  for (i in 1:length(DIR)){
    file = append(file, as.list(list.files(DIR[[i]], full.names = T)))
    message(paste(DIR[[i]], "selected"))
  }
  file = stringr::str_sort(file, numeric = T)
  df_org = data.frame()
  for (i in 1:length(file)){
    temp_df = read.csv(file[i])
    df_org = rbind(df_org, temp_df)
    remove(temp_df)
  }
  colnames(df_org) <- c('Station','Date','Time', 'Height')
  df_name = df_org %>%
    select("Station") %>%
    distinct()
  file_1 = tools::file_path_sans_ext(basename(file[1]))
  file_n = tools::file_path_sans_ext(basename(file[length(file)]))
  df_list = list()
  for (i in 1:nrow(df_name)){
    df_order = df_org %>%
      filter(Station == df_name$Station[i]) %>%
      mutate(Time = as.POSIXct(paste(Date, Time, "00"), format="%Y-%m-%d %H:%M %S", tz = "HongKong")) %>%
      arrange(Time) %>%
      distinct() %>%
      mutate(Date = paste(Date, sprintf("%02d", lubridate::hour(Time)))) %>%
      mutate(Min = paste0("Min_", sprintf("%02d", lubridate::minute(Time)))) %>%
      select(-c(Time)) %>%
      pivot_wider(names_from = Min, values_from = Height) %>%
      relocate(Min_00, .after = Date) %>%
      relocate(Min_05, .after = Min_00) %>%
      relocate(Min_10, .after = Min_05) %>%
      relocate(Min_15, .after = Min_10) %>%
      relocate(Min_20, .after = Min_15) %>%
      relocate(Min_25, .after = Min_20) %>%
      relocate(Min_30, .after = Min_25) %>%
      relocate(Min_35, .after = Min_30) %>%
      relocate(Min_40, .after = Min_35) %>%
      relocate(Min_45, .after = Min_40) %>%
      relocate(Min_50, .after = Min_45) %>%
      relocate(Min_55, .after = Min_50)
    #df_order[!is.numeric(df_order)] = NA
    df_order[df_order == "----"] = NA
    if(load == T){
      write.csv(df_order,paste0(getwd(),"/",file_1,"-",file_n,"-",df_name$Station[i],".csv"))
    }
    df_list = append(df_list, list(df_order))
  }
  df_list
}
