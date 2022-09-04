#' Pivot data of an object
#'
#' Only pivots data with a single station and a single data type
#'
#' @param obj a hkweather object
#'
#' @return
#' @export
#'
#' @examples obj_pivot(obj)
obj_pivot = function(obj){
  hkweather::hkw_lib()
  #Check if class of obj = hkweather
  flag_obj = F
  if(class(obj)[1] != "hkweather"){
    flag_obj = T
  }
  if(flag_obj == T){
    message("Warning! Something is wrong in the input")
    message("Variable obj is wrong! (hkweather object only)")
    return(message("---Pivot Failed---"))
  }

  #Start to pivot
  for(i in 1:length(obj$pivot)){
    pivot = obj$pivot[[i]]
    data = obj$data[[i]]
    block = F

    if(pivot == "XXXX" & block == F){
      next
      block = T
    }
    if(pivot == "long" & block == F){
      #need to turn it to wide!
      data = data %>%
        distinct()

      #check if there is duplicate
      data_dup = data %>%
        group_by(Station, Type, Hour, Min) %>%
        summarise(n = dplyr::n(), .groups = "drop") %>%
        filter(n > 1L)
      data_dup$avg = NA

      #if there is duplicate, get avg, remove from data
      if(nrow(data_dup) > 0){
        for(j in 1:nrow(data_dup)){
          data_avg = data %>%
            filter(Hour    == data_dup$Hour[j]   ) %>%
            filter(Min     == data_dup$Min[j]    ) %>%
            filter(Station == data_dup$Station[j]) %>%
            filter(Type    == data_dup$Type[j]   )

          data_dup$avg[j] = mean(data_avg$Value)
          data = data %>%
            filter(Hour    != df_dup$Hour[j] &
                   Min     != df_dup$Min[j]  &
                   Station != df_dup$Station[j] &
                   Type    != df_dup$Type[j])
        }
        data_dup = data_dup %>%
          select(-n) %>%
          rename(Value = avg)
      }

      #merge data with dup filtered data
      if(nrow(data_dup)>0){
        data = bind_rows(data, data_dup) %>%
          arrange(Min) %>%
          arrange(Hour) %>%
          arrange(Station) %>%
          arrange(Type)
      }

      #pivot wider
      data = data %>%
        distinct() %>%
        arrange(Min) %>%
        pivot_wider(names_from = Min, values_from = Value) %>%
        arrange(Hour) %>%
        as.data.frame()
      pivot = "wide"
      block = T
    }
    if(pivot == "wide" & block == F){
      #need to turn it to long!
      colnames = colnames(data)
      remove = c("Station", "Hour", "Min", "Type", "Value")
      for(j in 1:length(remove)){
        if(remove[j] %in% colnames){
          colnames = colnames[colnames != remove[j]]
        }
      }
      if(length(colnames) > 0){
        data = data %>%
          pivot_longer(cols = -c("Station", "Hour", "Type"), names_to = "Min", values_to = "Value") %>%
          arrange(Min) %>%
          arrange(Hour) %>%
          arrange(Type) %>%
          arrange(Station)
        pivot = "long"
      }
      block = T
    }

    #RETURN EVERYTHING BACK TO OBJ
    obj$data[[i]] = data
    obj$pivot[[i]] = pivot
    block = F
  }
  obj
}
