#' Draw hodograph
#'
#' @param data Data from 1 station. Only accepts a dataframe that includes column PRES, TEMP, and DWPT.
#' @param limit The layer of atmosphere it draws. The default is to practically draw all available data. Only accepts a list of 2 numbers.
#' @param type The type of hodograph drawn. Only accepts "smooth"/ "raw".
#' @param space Drawn when type is equal to "smooth". The spacing between each dot. The default is to draw a dot per 500 unit of altitude. Only accepts 1 number.
#' @param df Drawn when type is equal to "smooth". Degree of freedom. Default is set at 0.7, i.e. 70% of all sounding data available.
#' @param hght Drawn when type is equal to "smooth". Height of the atmosphere to be predicted. Only accepts a list of numbers.
#'
#' @return
#' @export
#'
#' @examples draw_hodo_plot(data)
draw_hodo_plot = function(data, limit = c(0, 99999), equal = "both", type = "smooth", space = 500, df = 0.7, hght = seq(0, 99999, 1)){
  hkweather::hkw_lib()
  #Check the data
  flag_data = ifelse((is.data.frame(data) & "HGHT" %in% colnames(data) & "VWND" %in% colnames(data) & "UWND" %in% colnames(data)), F, T)
  flag_limit= ifelse(length(limit) == 2 & is.numeric(limit), F, T)
  flag_type = ifelse(type == "smooth" | type == "raw", F, T)
  flag_df = F
  flag_hght = F
  flag_space = F

  flag_all = flag_data | flag_limit | flag_type

  if(type == "smooth"){
    flag_space = ifelse(is.numeric(space), F, T)
    flag_df  = ifelse(is.numeric(df) & 0 < df & df <= 1, F, T)
    flag_hght = ifelse(is.numeric(hght), F, T)
    flag_all = flag_all | flag_df | flag_hght
  }

  if(flag_all == T){
    message("ERROR: Something is wrong with your input")
    if(flag_data){message("ERROR: variable data is incorrect. (Data frame only, and must have column HGHT, VWND, UWND)")}
    if(flag_limit){message("ERROR: Variable limit is incorrect. (A list of 2 numbers only)")}
    if(flag_space){message("ERROR: variable space is incorrect. (A number only)")}
    if(flag_type){message("ERROR: Variable type is incorrect. ('smooth'/ 'raw' only)")}
    if(flag_df){message("ERROR: variable df is incorrect. (A number between 0 and 1 only)")}
    if(flag_hght){message("ERROR: variable hght is incorrect. (A list of numbers only)")}
    return(message("---PLOT ERROR---"))
  }

  #Get the hours of data
  data_date = data %>%
    select(Hour) %>%
    distinct()

  #Smooth the data if needed
  if(type == "smooth"){
    if(("UWND_smooth" %in% colnames(data) | "VWND_smooth" %in% colnames(data))){
      message("The data was smoothened! New smoothening will be applied!")
      if("UWND_smooth" %in% colnames(data)){
        data = data %>%
          filter(is.na(UWND_smooth)) %>%
          select(-UWND_smooth)
      }
      if("VWND_smooth" %in% colnames(data)){
        data = data %>%
          filter(is.na(VWND_smooth)) %>%
          select(-VWND_smooth)
      }
    }
    data = draw_hodo_smooth(data, df = df, hght = hght)
  }

  #Start plotting
  list_legend = list()
  list_plot   = list()

  for(i in 1:nrow(data_date)){
    if(type == "smooth"){
      data_sel = data %>%
        filter(Hour == data_date$Hour[i]) %>%
        filter(!is.na(UWND_smooth)) %>%
        filter(!is.na(VWND_smooth)) %>%
        filter(limit[1] <= HGHT & HGHT <= limit[2])

      data_lbl = data_sel %>%
        mutate(HGHT_500 = HGHT %% space) %>%
        filter(HGHT == min(HGHT) | HGHT == max(HGHT) | HGHT_500 == 0)
      max = max(data_sel$HGHT)
      min = min(data_sel$HGHT)

      plot = data_sel %>%
        ggplot(aes(x = UWND_smooth, y = VWND_smooth, color = HGHT, group = Station))+
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_path(show.legend = F, size = 1)+
        geom_point(data = data_lbl, size = 2, shape = 21)+
        scale_x_continuous(breaks = seq(-1000, 1000, 10),
                           minor_breaks = seq(-1000, 1000, 1))+
        scale_y_continuous(breaks = seq(-1000, 1000, 10),
                           minor_breaks = seq(-1000, 1000, 1))+
        scale_color_gradient2(low = "#0000FF", mid = "#008000", high = "#FF0000", midpoint = mean(data_sel$HGHT))+
        labs(title = paste0("Hodograph at ", data_date$Hour[i], " (Altitude: ", min," - ", max, ")"),
             y = "V-Wind, smoothed", x = "U-Wind, smoothed", color = "Height")+
        theme_bw()

      if(equal == "both"){
        scale = max(abs(min(data_sel$VWND_smooth)),
                    abs(max(data_sel$VWND_smooth)),
                    abs(min(data_sel$UWND_smooth)),
                    abs(max(data_sel$UWND_smooth)))
        plot = plot +
          coord_cartesian(xlim = c(-scale, +scale), ylim = c(-scale, +scale))
      }
      if(equal == "u"){
        scale = max(abs(min(data_sel$UWND_smooth)),
                    abs(max(data_sel$UWND_smooth)))
        plot = plot +
          coord_cartesian(xlim = c(-scale, +scale))
      }
      if(equal == "v"){
        scale = max(abs(min(data_sel$VWND_smooth)),
                    abs(max(data_sel$VWND_smooth)))
        plot = plot +
          coord_cartesian(ylim = c(-scale, +scale))
      }
      if(equal == "uv" | equal == "vu"){
        scale_u = max(abs(min(data_sel$UWND_smooth)),
                      abs(max(data_sel$UWND_smooth)))
        scale_v = max(abs(min(data_sel$VWND_smooth)),
                      abs(max(data_sel$VWND_smooth)))
        plot = plot +
          coord_cartesian(xlim = c(-scale_u, +scale_u), ylim = c(-scale_v, +scale_v))

      }
      list_legend = append(list_legend, data_date$Hour[i])
      list_plot   = append(list_plot, list(plot))
    }
    if(type == "raw"){
      data_sel = data %>%
        filter(Hour == data_date$Hour[i]) %>%
        filter(!is.na(UWND)) %>%
        filter(!is.na(VWND)) %>%
        filter(limit[1] <= HGHT & HGHT <= limit[2])
      max = max(data_sel$HGHT)
      min = min(data_sel$HGHT)

      plot = data_sel %>%
        ggplot(aes(x = UWND, y = VWND, color = HGHT, group = Station))+
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_path(show.legend = F, size = 1)+
        geom_point(size = 2, shape = 21)+
        scale_x_continuous(breaks = seq(-1000, 1000, 10),
                           minor_breaks = seq(-1000, 1000, 1))+
        scale_y_continuous(breaks = seq(-1000, 1000, 10),
                           minor_breaks = seq(-1000, 1000, 1))+
        scale_color_gradient2(low = "#0000FF", mid = "#008000", high = "#FF0000", midpoint = mean(data_sel$HGHT))+
        labs(title = paste0("Hodograph at ", data_date$Hour[i], "(Altitude: ", min, " - ", max, ")"),
             y = "V-Wind, raw", x = "U-Wind, raw", color = "Height")+
        theme_bw()

      if(equal == "both"){
        scale = max(abs(min(data_sel$VWND)),
                    abs(max(data_sel$VWND)),
                    abs(min(data_sel$UWND)),
                    abs(max(data_sel$UWND)))
        plot = plot +
          coord_cartesian(xlim = c(-scale, +scale), ylim = c(-scale, +scale))
      }
      if(equal == "u"){
        scale = max(abs(min(data_sel$UWND)),
                    abs(max(data_sel$UWND)))
        plot = plot +
          coord_cartesian(xlim = c(-scale, +scale))
      }
      if(equal == "v"){
        scale = max(abs(min(data_sel$VWND)),
                    abs(max(data_sel$VWND)))
        plot = plot +
          coord_cartesian(ylim = c(-scale, +scale))
      }
      if(equal == "uv" | equal == "vu"){
        scale_u = max(abs(min(data_sel$UWND)),
                      abs(max(data_sel$UWND)))
        scale_v = max(abs(min(data_sel$VWND)),
                      abs(max(data_sel$VWND)))
        plot = plot +
          coord_cartesian(xlim = c(-scale_u, +scale_u), ylim = c(-scale_v, +scale_v))

      }
      list_legend = append(list_legend, data_date$Hour[i])
      list_plot   = append(list_plot, list(plot))
    }
  }
  list = list(legend = list_legend,
              plot = list_plot)
  return(list)
}
