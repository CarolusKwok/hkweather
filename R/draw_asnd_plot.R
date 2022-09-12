#' Draw atmospheric sounding plot
#'
#' @param data Data from 1 station. Only accepts a dataframe that includes column PRES, TEMP, and DWPT.
#' @param DABT Draws the dry adiabatic line. Default as T
#' @param MABT Draws the moist adiabatic line. Default as T.
#' @param IHUM Draws the isohume line. Default as T.
#' @param PRES Pressure range of the atmospheric sounding plot.
#' @param TEMP Temperature range of the atmospheric sounding plot.
#'
#' @return
#' @export
#'
#' @examples draw_asnd_plot()
draw_asnd_plot = function(data = NA, DABT = T, MABT = T, IHUM = T, PRES = c(1000, 100), TEMP = c(-100, 50)){
  hkweather::hkw_lib()
  #Test if stuff makes sense
  flag_DABT = ifelse(is.logical(DABT) | is.numeric(DABT), F, T)
  flag_MABT = ifelse(is.logical(DABT) | is.numeric(DABT), F, T)
  flag_IHUM = ifelse(is.logical(IHUM) | is.numeric(IHUM), F, T)
  flag_PRES = ifelse(is.numeric(PRES) & length(PRES) == 2, F, T)
  flag_TEMP = ifelse(is.numeric(TEMP) & length(TEMP) == 2, F, T)
  flag_data = ifelse(is.na(data[1]) | (is.data.frame(data) & "PRES" %in% colnames(data) & "TEMP" %in% colnames(data) & "DWPT" %in% colnames(data)),
                     F, T)

  flag_all = flag_DABT | flag_MABT | flag_IHUM | flag_PRES | flag_TEMP | flag_data
  flag_all = sum(flag_all)
  if(flag_all > 0){
    message("ERROR: Something is wrong with your input")
    if(flag_DABT){message("ERROR: variable DABT is incorrect. (T/ F/ list of numbers only)")}
    if(flag_MABT){message("ERROR: variable MABT is incorrect. (T/ F/ list of numbers only)")}
    if(flag_IHUM){message("ERROR: variable IHUM is incorrect. (T/ F/ list of numbers only)")}
    if(flag_PRES){message("ERROR: variable PRES is incorrect. (T/ F/ list of numbers only)")}
    if(flag_TEMP){message("ERROR: variable TEMP is incorrect. (2 numbers only)")}
    if(flag_data){message("ERROR: variable data is incorrect. (Data frame only, and must have column PRES, TEMP, and DWPT)")}
    return(message("---PLOT ERROR---"))
  }

  #Start loading the plot
  a = PRES
  b = TEMP

  df_dabt = NA
  df_mabt = NA
  df_ihum = NA

  if(DABT != F){
    if(is.logical(DABT)){
      df_dabt = draw_line_dabt()
    }
    if(is.numeric(DABT)){
      df_dabt = draw_line_dabt(DABT)
    }
  } else {
    df_dabt = data.frame(type  = NA,
                         group = NA,
                         PRES  = NA,
                         TEMP  = NA)
  }
  if(MABT != F){
    if(is.logical(MABT)){
      df_mabt = draw_line_mabt()
    }
    if(is.numeric(MABT)){
      df_mabt = draw_line_mabt(MABT)
    }
  } else {
    df_mabt = data.frame(type  = NA,
                         group = NA,
                         PRES  = NA,
                         TEMP  = NA)
  }
  if(IHUM != F){
    if(is.logical(IHUM)){
      df_ihum = draw_line_ihum()
    }
    if(is.numeric(IHUM)){
      df_ihum = draw_line_ihum(IHUM)
    }
  } else {
    df_ihum = data.frame(type  = NA,
                         group = NA,
                         PRES  = NA,
                         TEMP  = NA)
  }

  df = bind_rows(df_dabt, df_mabt, df_ihum) %>%
    filter(!is.na(type)) %>%
    mutate(group = paste0(group, type))

  plot_raw = ggplot(df, aes(x = TEMP, y = PRES))+
    geom_path(data = filter(df, type == "DABT"), aes(group = group), color = "#E56B64", size = 0.5)+
    geom_path(data = filter(df, type == "IHUM"), aes(group = group), color = "#00BA38", size = 0.5)+
    geom_path(data = filter(df, type == "MABT"), aes(group = group), color = "#619CFF", size = 0.5)+
    theme_bw()+
    scale_y_continuous(breaks = seq(0, 1500, 100), trans = "log10")+
    scale_x_continuous(breaks = seq(-280, 100, 10))+
    labs(y = "Pressure (hPa)", x = "Temperature (°C)", color = "Type")+
    coord_cartesian(ylim = a, xlim = b, expand = F)

  #Add the data if there is
  if(is.data.frame(data)){
    data_hour = data %>%
      select(Hour) %>%
      distinct()

    list_legend = list()
    list_plot = list()
    for(i in 1:nrow(data_hour)){
      data_fil = data %>%
        filter(Hour == data_hour$Hour[i])
      plot_asnd = plot_raw +
        labs(title = paste0("Atmospheric Sounding at ", unique(data_fil$Hour)))+
        geom_path(data = data_fil, aes(y = PRES, x = TEMP), size = 1, color = "RED", inherit.aes = F)+
        geom_path(data = data_fil, aes(y = PRES, x = DWPT), size = 1, color = "BLUE", inherit.aes = F)

      list_legend = append(list_legend, data_hour$Hour[i])
      list_plot   = append(list_plot, list(plot_asnd))
    }

    plot_raw = list(legend = list_legend,
                    plot_asnd = list_plot)
  }
  #return the plot
  return(plot_raw)
}
