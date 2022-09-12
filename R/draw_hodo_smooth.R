#' Draws a smoothed hodograph line
#'
#' @param data Data from 1 station. Only accepts a dataframe that includes column HGHT, UWND, and VWND.
#' @param df Degree of freedom. Default is set at 0.7, i.e. 70% of all sounding data available.
#' @param hght Height of the atmosphere to be predicted. Only accepts a list of numbers.
#'
#' @return
#' @export
#'
#' @examples draw_hodo_smooth(data)
draw_hodo_smooth = function(data, df = 0.7, hght = seq(0, 16000, 1)){
  hkweather::hkw_lib()
  data_date = data %>%
    select(Hour) %>%
    distinct()

  data_smooth = data.frame(Station = NA,
                           Hour = NA,
                           HGHT = NA,
                           UWND_smooth = NA,
                           VWND_smooth = NA)

  for(i in 1:nrow(data_date)){
    data_sel = data %>%
      filter(Hour == data_date$Hour[i])
    station = data_sel %>%
      select(Station) %>%
      distinct()
    station = unique(station$Station)

    len = length(unique(data_sel$HGHT))
    pre = smooth.spline(x = data_sel$HGHT, y = data_sel$UWND, df = len*df)
    pre_uwnd = predict(pre, hght)
    pre = smooth.spline(x = data_sel$HGHT, y = data_sel$VWND, df = len*df)
    pre_vwnd = predict(pre, hght)
    pre_data = data.frame(Station = station,
                          Hour = data_date$Hour[i],
                          Min = "Min_00",
                          HGHT = pre_uwnd$x,
                          UWND_smooth = pre_uwnd$y,
                          VWND_smooth = pre_vwnd$y) %>%
      filter(HGHT >= min(data_sel$HGHT)) %>%
      filter(HGHT <= max(data_sel$HGHT))
    data_smooth = bind_rows(data_smooth, pre_data)
  }
  data_smooth = data_smooth %>%
    drop_na()

  data = data %>%
    mutate(UWND_smooth = NA,
           VWND_smooth = NA) %>%
    bind_rows(data_smooth) %>%
    arrange(Hour)
  return(data)
}
