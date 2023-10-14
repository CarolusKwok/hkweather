#' Draw rose plot
#'
#' @param drct Direction of the wind
#' @param wind Wind speed or any other type of data.
#' @param unit Wind speed unit, provided in enq_wind. Default as "kmh"
#' @param time_unit Time per observation, in hours. Default as 0.5.
#' @param scale Scale of the wind speed for classification. Accepts "beaufort" or "hko". Default as "beaufort".
#'
#' @return a list including ggplot and data
#' @export
#'
#' @examples draw_rose(100, seq(0, 220, 5))
draw_rose = function(drct, wind, unit = "kmh", time_unit = 0.5, scale = "beaufort"){
  library(hkweather)
  hkweather::hkw_lib()
  #Start!
  #Get wind data
  if(unit == "kmh"){
    data = enq_wind(kmh = wind)
  }
  if(unit == "mps"){
    data = enq_wind(mps = wind)
  }
  if(unit == "mph"){
    data = enq_wind(mph = wind)
  }
  if(unit == "fps"){
    data = enq_wind(fps = wind)
  }
  if(unit == "kt"){
    data = enq_wind(kt = wind)
  }
  data = data %>%
    bind_cols(data.frame(drct = drct))

  if(scale == "beaufort"){
    color = data.frame(values = c("Black",      #17+
                                  "Red",        #17
                                  "darkmagenta",     #16
                                  "Maroon", #15
                                  "deeppink3",     #14
                                  "deeppink",#13
                                  "Purple",     #12
                                  "Violet",     #11
                                  "Blue",       #10
                                  "cornflowerblue", #09
                                  "aquamarine1",       #08
                                  "darkorange", #07
                                  "darkgoldenrod1", #06
                                  "yellowgreen",  #05
                                  "green",#04
                                  "chartreuse", #03
                                  "azure4", #02
                                  "grey", #01
                                  "white"),
                       breaks = c("17+", sprintf("%02d", seq(17, 0, -1))))
    data = data %>%
      mutate(drct = ifelse(drct == 360, 0, drct)) %>%
      group_by(drct, beaufort) %>%
      summarise(n = n()*time_unit)
    plot = data %>%
      ggplot(aes(x = drct, y = n, fill = beaufort, group = fct_rev(beaufort)))+
      geom_col(width = 2, color = "black")+
      scale_x_continuous(limits = c(0, 360),
                         breaks = seq(0, 360, 22.5))+
      scale_fill_manual(values = color$values, breaks = color$breaks)+
      labs(x = "Direction", y = "Hours", fill = "Beaufort\nScale\nForce")+
      coord_polar()+
      theme_bw()+
      theme(axis.title = element_blank(),
            legend.key = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(lineheight = 0.8, face = "bold",
                                      hjust = 0.5))
  }
  if(scale == "hko"){
    color = data.frame(breaks = c("SuperT", "ST", "T", "STS", "TS", "TD"),
                       values = c("#C464D9", "#FF8F20", "#FFD821", "#5EBAFF", "#00FAF4", "#BEFDA8"))
    data = data %>%
      mutate(drct = ifelse(drct == 360, 0, drct)) %>%
      group_by(drct, hko) %>%
      summarise(n = n()*time_unit) %>%
      mutate(group = ifelse(hko == "TD", "01",
                     ifelse(hko == "TS", "02",
                     ifelse(hko == "STS", "03",
                     ifelse(hko == "T", "04",
                     ifelse(hko == "ST", "05",
                     ifelse(hko == "SuperT", "06", NA))))))) %>%
      arrange(group)
    plot = data %>%
      ggplot(aes(x = drct, y = n, fill = hko, group = fct_rev(group)))+
      geom_col(width = 2, color = "black")+
      scale_x_continuous(limits = c(0, 360),
                         breaks = seq(0, 360, 22.5))+
      scale_fill_manual(breaks = color$breaks, values = color$values)+
      labs(x = "Direction", y = "Hours", fill = "HKO\nscale")+
      coord_polar()+
      theme_bw()+
      theme(axis.title = element_blank(),
            legend.key = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(lineheight = 0.8, face = "bold",
                                      hjust = 0.5))
  }
  message("Graphing complete!")
  message("Remember to futher customize by the following commands")
  message("...+geom_hline(yintercept = ...)")
  message("...+geom_text(aes(x = 0, y = ..., label = ...)")
  message("...+theme(panel.grid = element_none(), axis.text = element_none())")
  return(list(rose_plot = list(plot), data = data))

}
