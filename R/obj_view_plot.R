#' View the plots of an hkweather obj
#'
#' @param obj an hkweather obj
#' @param type type of plot to show, only accepts
#' @param index index
#'
#' @return
#' @export
#'
#' @examples obj_view_plot(obj, index = 2)
obj_view_plot = function(obj, type = "all", index = 1){
  #additional variable
  length = length(obj$data)
  type_list = names(obj)
  #format type
  for(i in 1:length(type)){
    type[i] = paste0("plot_", type[i])
  }
  if("plot_all" %in% type){
    type = type_list
    select = grepl("plot", type)
    new_type = list()
    for(i in 1:length(select)){
      if(select[i] == T){
        new_type = append(new_type, type[i])
      }
    }
    type = new_type
  }

  #Check input
  flag_obj = F
  flag_type = F
  flag_index = F
  if(class(obj)[1] != "hkweather"){
    flag_obj = T
  }
  for(i in 1:length(type)){
    if(!(type[i] %in% type_list)){
      flag_type = T
      break
    }
  }
  for(i in 1:length(index)){
    if(index[i] > length | index[i] < 1 | !is.numeric(index[i])){
      flag_index = T
      break
    }
  }

  flag_all = flag_obj + flag_type + flag_index
  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    return(message("---View Failed---"))
  }

  #Start selecting
  plot_list = list()
  for(i in 1:length(index)){
    if("plot_point" %in% type){
      plot_list = append(plot_list, list(obj$plot_point[[index[i]]]))
    }
    if("plot_line" %in% type){
      plot_list = append(plot_list, list(obj$plot_line[[index[i]]]))
    }
    if("plot_violin" %in% type){
      plot_list = append(plot_list, list(obj$plot_violin[[index[i]]]))
    }
  }
  plot_list
}
