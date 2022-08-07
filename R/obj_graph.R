#' Graph
#'
#' @param obj a hkweather object
#' @param graph Type of graph you want to plot. Is it a "point", "violin", or "bar"
#'
#' @return
#' @export
#'
#' @examples obj_graph(obj)
obj_graph = function(obj, graph = c("point", "violin")){
  hkweather::hkw_lib()

  #pivot longer so everything looks good and create DateTime
  #Remember to reorder everything as "#Station #Hour #Min #Type #Value #DateTime"

  obj = obj_pivot(obj, "longer")
  for(i in 1:length(obj$data)){
    obj$data[[i]] = obj$data[[i]] %>%
      as.data.frame() %>%
      mutate(DateTime = paste0(Hour," ", substr(Min, 5, 6))) %>%
      mutate(DateTime = as.POSIXct(DateTime,
                                   format="%Y-%m-%d %H %M")) %>%
      relocate(Station, Hour, Min, Type, Value, DateTime)
  }

  #graph!
  if("point" %in% graph){
    message("Graphing point plots")
    obj$plot_point = list()
    for(i in 1:length(obj$data)){
      plot = obj$data[[i]] %>%
        ggplot(aes(x = DateTime, y = Value, color = Station, shape = Type)) +
        geom_point(alpha = 0.5) +
        theme_bw()
      obj$plot_point = append(obj$plot_point, list(plot))
    }
  }
  if("violin" %in% graph){
    message("Graphing violin plots")
    obj$plot_violin = list()
    for(i in 1:length(obj$data)){
      plot = obj$data[[i]] %>%
        ggplot(aes(x = Station, y = Value, color = Type, fill = Type)) +
        geom_violin(alpha = 0.5) +
        theme_bw()
      obj$plot_violin = append(obj$plot_violin, list(plot))
    }
  }

  #Return object
  obj
}
