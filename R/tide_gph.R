#' Graphs HKOtide and MDtide results
#'
#' @param df dataframe of a "tidy" MD and HKO tide results
#'
#' @return
#' @export
#'
#' @examples tide_gph(x)
tide_gph = function(df = NA){
  hkw_lib()
  if(is.na(df)){
    message("Dataframe not inserted")
    message("Please choose the csv file now")
    df = read_csv(choose.files())

    df = df %>%
      select(-...1)
  }
  df = df %>%
    pivot_longer(!c(Station, Date), names_to = "Min", values_to = "Height") %>%
    drop_na() %>%
    mutate(Date = Date + minutes(substr(Min, 5, 6))) %>%
    select(-Min)

  plot_pt = df %>%
    ggplot(aes(x = Date, y = Height)) +
    geom_point(alpha = 0.1, color = "blue")+
    scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")+
    labs(y = paste0("Tidal Height (mCD) (",df$Station[1],")"), x = "Date")+
    theme_bw()

  plot_vio = df %>%
    ggplot(aes(x = Station, y = Height))+
    geom_violin(alpha = 0.5, fill = "blue", draw_quantiles = c(.95,.75,.50,.25,.05))+
    geom_boxplot(width = 0.025, outlier.colour = "red", outlier.alpha = 0.5, outlier.shape = 23)+
    theme_bw()+
    labs(y = "Tidal Height (mCD)", x = "Station")

  list(plot_pt, plot_vio)
}
