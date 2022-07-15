#' Orders HKOtide results (v2)
#'
#' Tidies up HKOtide csv files. Creates a list of files and names
#' Can create csv in working directory with default name.
#'
#' @param write Writes CSV file to Working Directory.
#' @param DIR Path of the directory. Accepts list of directory.
#'
#' @return
#' @export
#'
#' @examples tide_tbl(getwd())
tide_tbl = function(DIR, write = T){
  hkweather::hkw_lib()
  defaultWarn = getOption("warn")
  options(warn = -1)
  File_list = list()
  for (i in 1:length(DIR)){
    File_list = append(File_list, list.files(DIR[i], pattern = "*.csv", full.names = T))
  }
  File_list = stringr::str_sort(File_list)
  File_s = tools::file_path_sans_ext(basename(File_list[1]))
  File_e = tools::file_path_sans_ext(basename(File_list[length(File_list)]))

  df = data.frame()
  df = File_list %>% lapply(fread) %>% rbindlist()
  colnames(df) = c("Station", "Date", "Time", "Height")

  df_name_list = df %>%
    select(Station)%>%
    distinct()

  df_list = list()
  for(i in 1:nrow(df_name_list)){
    df_org = df %>%
      filter(Station == df_name_list$Station[i]) %>%
      mutate(NewDate = paste(Date, substr(Time, 1, 2))) %>%
      mutate(NewMin = paste0("Min_", substr(Time, 4,5))) %>%
      select(-c(Date, Time)) %>%
      filter(Height != "----") %>%
      mutate(Height = as.numeric(Height)) %>%
      distinct()

    df_dup = df_org %>%
      group_by(Station, NewDate, NewMin) %>%
      summarise(n = dplyr::n(), .groups = "drop") %>%
      filter(n > 1L)

    if(nrow(df_dup)>0){
      for(n in 1:nrow(df_dup)){
        df_dup_select = df_org %>%
          filter(NewDate == df_dup$NewDate[n]) %>%
          filter(NewMin == df_dup$NewMin[n])

        df_dup$Height[n] = mean(df_dup_select$Height)
        df_org = df_org %>%
          filter(!(NewDate == df_dup$NewDate[n] & NewMin  == df_dup$NewMin[n]))
      }
      df_dup = df_dup %>%
        select(-n) %>%
        relocate(Height, .after = Station)

      df_org = bind_rows(df_org, df_dup)
    }

    df_org = df_org %>%
      pivot_wider(names_from = NewMin, values_from = Height)

    colnames(df_org)[2] = "Date"
    df_org = df_org %>%
      relocate(Min_00, Min_05,
               Min_10, Min_15,
               Min_25, Min_20,
               Min_30, Min_35,
               Min_40, Min_45,
               Min_50, Min_55, .after = Date) %>%
      arrange(Date)
    if(write == T){
      write.csv(df_org, paste0(File_s,"-",File_e,"-",df_name_list$Station[i],".csv"))
    }
    df_list = append(df_list, list(df_org))
  }
  options(warn = defaultWarn)

  df_list
}
