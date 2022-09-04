#' Graph all the data within an hkweather object
#'
#' @param obj a hkweather object
#' @param graph Type of graph you want to plot. Is it a "point", "violin", or "bar"
#'
#' @return
#' @export
#'
#' @examples obj_graph(obj)
obj_graph = function(obj, graph = "all"){
  hkweather::hkw_lib()
  #additional variable
  graph_list = c("point", "line", "violin")
  #Check input
  if("all" %in% graph){
    graph = graph_list
  }
  flag_obj = F
  flag_graph = F
  if(class(obj)[1] != "hkweather"){
    flag_obj = T
  }
  for(i in 1:length(graph)){
    if(!(graph[i] %in% graph_list)){
      flag_graph = T
      break
    }
  }
  flag_all = flag_obj + flag_graph
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_obj){message("Variable obj is wrong! (hkweather object only)")}
    if(flag_graph){message("Variable graph is wrong! (point/ line/ violin only)")}
    return(message("---Graph Failed---"))
  }

  #pivot longer so everything looks good and create DateTime
  #Remember to reorder everything as "#Station #Hour #Min #Type #Value #DateTime"
  if("wide" %in% unique(obj$pivot)){
    obj = obj_pivot(obj)
  }
  for(i in 1:length(obj$data)){
    obj$data[[i]] = obj$data[[i]] %>%
      as.data.frame() %>%
      mutate(DateTime = paste0(Hour," ", substr(Min, 5, 6))) %>%
      mutate(DateTime = as.POSIXct(DateTime,
                                   format="%Y-%m-%d %H %M")) %>%
      relocate(Station, Hour, Min, Type, Value, DateTime)
  }

  #start graphing
  plot_point = list()
  plot_line = list()
  plot_violin = list()
  for(i in 1:length(obj$data)){
    data = obj$data[[i]]
    #understand the data! multiple stations/ types?
    station = data %>%
      select(Station) %>%
      distinct()
    type = data %>%
      select(Type) %>%
      distinct()
    scenario = ifelse(nrow(station) == 1 & nrow(type) == 1, 1,
               ifelse(nrow(station)  > 1 & nrow(type) == 1, 2,
               ifelse(nrow(station) == 1 & nrow(type)  > 1, 3,
               ifelse(nrow(station)  > 1 & nrow(type)  > 1, 4, NA))))

    #GRAPH POINT
    if("point" %in% graph){
      plot = data %>%
        ggplot(aes(x = DateTime, y = Value))
      #color = Station, shape = Type
      if(scenario == 1){
        plot = plot +
          geom_point(color = "Blue", shape = 16, alpha = 0.5) +
          labs(y = paste0(type$Type[1], " (", station$Station[1], ")"),
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      if(scenario == 2){
        plot = plot +
          geom_point(aes(color = Station), shape = 16, alpha = 0.5)+
          labs(y = paste0(type$Type[1]),
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      if(scenario == 3){
        plot = plot +
          geom_point(aes(shape = Type), color = "Blue", alpha = 0.5)+
          labs(y = paste0("Value (", station$Station[1], ")"),
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      if(scenario == 4){
        plot = plot +
          geom_point(aes(color = Station, shape = Type), alpha = 0.5)+
          labs(y = "Value",
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      plot_point = append(plot_point, list(plot))
    }
    #GRAPH LINE
    if("line" %in% graph){
      plot = data %>%
        ggplot(aes(x = DateTime, y = Value))
      #color = Station, shape = Type
      if(scenario == 1){
        plot = plot +
          geom_line(color = "Black")+
          geom_point(color = "Blue", shape = 16, alpha = 0.5) +
          labs(y = paste0(type$Type[1], " (", station$Station[1], ")"),
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      if(scenario == 2){
        plot = plot +
          geom_line(aes(color = Station))+
          geom_point(aes(color = Station), shape = 16, alpha = 0.5)+
          labs(y = paste0(type$Type[1]),
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      if(scenario == 3){
        plot = plot +
          geom_line(color = "Black")+
          geom_point(aes(shape = Type), color = "Blue", alpha = 0.5)+
          labs(y = paste0("Value (", station$Station[1], ")"),
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      if(scenario == 4){
        plot = plot +
          geom_line(aes(color = Station))+
          geom_point(aes(color = Station, shape = Type), alpha = 0.5)+
          labs(y = "Value",
               x = "Date & Time")+
          scale_x_datetime(date_labels = "%m/%d\n%H:%M")+
          theme_bw()
      }
      plot_line = append(plot_line, list(plot))
    }
    #GRAPH VIOLIN
    if("violin" %in% graph){
      plot = data %>%
        ggplot(aes(x = Station, y = Value))
      #color = Type, fill = Type
      if(scenario == 1 | scenario == 2){
        plot = plot +
          geom_violin(color = "Blue", fill = "Blue", draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5)+
          labs(y = paste0(type$Type[1]),
               x = "Station")+
          theme_bw()
      }
      if(scenario == 3 | scenario == 4){
        plot = plot +
          geom_violin(aes(color = Type, fill = Type), draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.5)+
          labs(y = "Values",
               x = "Station")+
          theme_bw()
      }
      plot_violin = append(plot_violin, list(plot))
    }
  }
  #Return object
  if("point" %in% graph){obj$plot_point = plot_point}
  if("line" %in% graph){obj$plot_line = plot_line}
  if("violin" %in% graph){obj$plot_violin = plot_violin}
  class(obj) = "hkweather"
  obj
}
