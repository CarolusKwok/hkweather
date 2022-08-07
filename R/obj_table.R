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

  loc = match("all", obj$legend)
  df = as.data.frame(obj$data[[loc]])
  #split data by type
  if("type" %in% split){
    df_dis = df %>%
      select(Type) %>%
      distinct()

    for(i in 1:nrow(df_dis)){
      df_sel = df %>%
        filter(Type == df_dis$Type[i]) %>%
        distinct()

      #append to list
      obj$legend = append(obj$legend, paste0("all-",df_dis$Type[i]))
      obj$data   = append(obj$data,   list(df_sel))
    }
  }

  #split data by station
  if("station" %in% split){
    df_dis = df %>%
      select(Station) %>%
      distinct()

    for(i in 1:nrow(df_dis)){
      df_sel = df %>%
        filter(Station == df_dis$Station[i]) %>%
        distinct()

      #append to list
      obj$legend = append(obj$legend, paste0(df_dis$Station[i],"-all"))
      obj$data   = append(obj$data,   list(df_sel))
    }
  }

  #split data by both
  if("both" %in% split){
    df_dis = df %>%
      select(Station, Type) %>%
      distinct()

    for(i in 1:nrow(df_dis)){
      df_sel = df %>%
        filter(Type == df_dis$Type[i] & Station == df_dis$Station[i]) %>%
        distinct()

      #append to list
      obj$legend = append(obj$legend, paste0(df_dis$Station[i],"-",df_dis$Type[i]))
      obj$data   = append(obj$data, list(df_sel))
    }
  }

  #return
  obj
}
