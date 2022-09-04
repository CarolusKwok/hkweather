#' Orders everything a table
#'
#'
#' @param obj a hkweather obj
#' @param split how should the data be split? By "type", "station", or "both"?
#'
#' @return
#' @export
#'
#' @examples table("1/1/1", "tide", write = "xlsx")
obj_table = function(obj, split = c("type", "both")){
  hkweather::hkw_lib()

  #check if obj is a hkweather obj
  flag_obj = F
  if(class(obj) != "hkweather"){
    flag_obj = T
  }
  if(flag_obj == T){
    message("Warning! Something is wrong in the input")
    message("Variable obj is wrong! (hkweather object only)")
    return(message("---Split Failed---"))
  }

  #find where the data is
  loc = match("all", obj$legend)
  df = as.data.frame(obj$data[[loc]])

  #create a new obj
  obj_n = list(type = obj$type,
               legend = list("all"),
               data = list(df),
               pivot = list("XXXX"))

  #split data by type
  if("type" %in% split){
    df_dis = df %>%
      select(Type) %>%
      distinct()

    if(nrow(df_dis) > 1){
      for(i in 1:nrow(df_dis)){
        df_sel = df %>%
          filter(Type == df_dis$Type[i]) %>%
          distinct()

        #append to list
        obj_n$legend = append(obj_n$legend, paste0("all-",df_dis$Type[i]))
        obj_n$data   = append(obj_n$data,   list(df_sel))
      }
    }
  }

  #split data by station
  if("station" %in% split){
    df_dis = df %>%
      select(Station) %>%
      distinct()

    if(nrow(df_dis) > 1){
      for(i in 1:nrow(df_dis)){
        df_sel = df %>%
          filter(Station == df_dis$Station[i]) %>%
          distinct()

        #append to list
        obj_n$legend = append(obj_n$legend, paste0(df_dis$Station[i],"-all"))
        obj_n$data   = append(obj_n$data,   list(df_sel))
      }
    }
  }

  #split data by both
  if("both" %in% split){
    df_dis = df %>%
      select(Station, Type) %>%
      distinct()

    df_dis_station = df %>%
      select(Station) %>%
      distinct()

    df_dis_type = df %>%
      select(Type) %>%
      distinct()

    if(nrow(df_dis) != nrow(df_dis_station) & nrow(df_dis) != nrow(df_dis_type)){
      for(i in 1:nrow(df_dis)){
        df_sel = df %>%
          filter(Type == df_dis$Type[i] & Station == df_dis$Station[i]) %>%
          distinct()

        #append to list
        obj_n$legend = append(obj_n$legend, paste0(df_dis$Station[i],"-",df_dis$Type[i]))
        obj_n$data   = append(obj_n$data, list(df_sel))
      }
    }
  }

  #return
  obj_n$pivot = append(list("XXXX"), rep("long", length(obj_n$data)-1))
  class(obj_n) = "hkweather"
  obj_n
}
