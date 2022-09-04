#' View the legend of object
#'
#' @param obj a hkweather obj
#'
#' @return
#' @export
#'
#' @examples obj_view_legend(obj)
obj_view_legend = function(obj){
  hkweather::hkw_lib()
  #check if obj is hkweather obj
  flag_obj = F
  if(class(obj)[1] != "hkweather"){
    flag_obj = T
  }
  if(flag_obj == T){
    message("Warning! Something is wrong in the input")
    message("Variable obj is wrong! (hkweather object only)")
    return(message("---View Failed---"))
  }
  #start!
  legend = obj$legend
  data = data.frame(Num = 1:length(legend),
                    Legend = NA)
  for(i in 1:length(legend)){
    data$Legend[i] = obj$legend[[i]]
  }
  data = data %>%
    select(Legend)
  data
}
