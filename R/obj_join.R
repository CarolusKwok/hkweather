#' Joins 2 hkweather object together
#'
#' @param obj1 a hkweather obj
#' @param obj2 a hkweather obj
#'
#' @return
#' @export
#'
#' @examples obj_join(obj1, obj2)
obj_join = function(obj1, obj2){
  hkweather::hkw_lib()

  #Check if class of obj1 and obj2 = hkweather
  flag_obj1 = F
  flag_obj2 = F
  if(class(obj1) != "hkweather"){
    flag_obj1 = T
  }
  if(class(obj2) != "hkweather"){
    flag_obj2 = T
  }
  flag_all = flag_obj1 + flag_obj2

  if(flag_all > 0){
    message("Warning! Something is wrong in the input")
    if(flag_obj1){message("Variable obj1 is wrong! (hkweather object only)")}
    if(flag_obj2){message("Variable obj2 is wrong! (hkweather object only)")}
    return(message("---Join Failed---"))
  }

  #combind the 2 "all" datafiles, and remove!
  obj1_all_index = match("all", obj1$legend)
  obj2_all_index = match("all", obj2$legend)

  obj3_all = bind_rows(obj1[["data"]][[obj1_all_index]],
                       obj2[["data"]][[obj2_all_index]])

  #join obj
  obj3 = list(type   = append(obj1$type,   obj2$type),
              legend = list("all"),
              data   = list(obj3_all),
              pivot  = list("XXXX"))
  class(obj3) = "hkweather"
  obj3
}
