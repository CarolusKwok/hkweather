#' Creates graph of HKOtide results (from tidy csv)
#'
#' Creates a list with ggplot graph and station name.
#'
#' @param df Dataframe of input data. Must contain the following columns, "Station", "Date", "Min_00...Min_55"
#' @param DIR Path of the directory. Accepts list of directory.
#'
#' @return
#' @export
#'
#' @examples cre_svg_tide(df = df)
cre_svg_tide = function(df = NA, DIR = NA){
  hkw_lib()
  #Check check check if df input ok!
  if(!is.na(DIR)){
    if(length(DIR) > 1){
      message("Something is wrong!")
      message("Variable DIR should be the pathway of a single file.")
      return("---Convertion failed---")
    }
    message("DIR exist!")
    message("Override df variable!")
    df = read.csv(DIR)
  }
  df[df == "----"] = NA
  if(!is.data.frame(df)){
    message("The input is not a dataframe!")
    return("---Convertion failed---")
  }
  #Pivot longer
  df_order = df %>%
    mutate(X = "---") %>%
    pivot_longer(!c(X, Station, Date), names_to = "Min", values_to = "Height") %>%
    mutate(Min = (substr(Min,5,6))) %>%
    mutate(Date = paste0(Date, Min)) %>%
    select(-c(Min, X)) %>%
    mutate(Date = as.POSIXct(Date, tryFormats = "%Y-%m-%d %H%M", tz = "HongKong")) %>%
    mutate(Height = as.numeric(Height))

  #Ploting and outputting plot
  plot_tide = df_order %>%
    ggplot(aes(x = Date, y = Height))+
    geom_point(color = "blue", alpha = 0.5)+
    geom_path()+
    scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M", date_breaks = "1 days")+
    scale_y_continuous(breaks = seq(-999,999,1), minor = seq(-999,999,0.1))+
    labs(y = paste0("Tidal Height (mCD) (", df_order$Station[1], ")")) +
    theme_bw()+
    theme(axis.title = element_text(size = 12),
          panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.75),
          panel.grid.minor.y = element_line(linetype = "dashed"))
  print(plot_tide)
  list(plot_tide, df_order$Station[1])
}
