#' Pivot data of an object
#'
#' Only pivots data with a single station and a single data type
#'
#'
#' @param obj a hkweather object
#' @param pivot how should I pivot this? "longer" or "wider"
#'
#' @return
#' @export
#'
#' @examples obj_pivot(obj, "wider")
obj_pivot = function(obj, pivot){
  hkweather::hkw_lib()

  for(i in 1:length(obj$data)){
    #Check if it's of single station and single type
    df = obj$data[[i]]
    df_dis = df %>%
      as_tibble() %>%
      select(Station, Type) %>%
      distinct()
    if(nrow(df_dis) > 1){next}

    #Pivot longer
    if(pivot == "longer"){
      colnames = colnames(df)
      remove = c("Station", "Hour", "Min", "Type", "Value")
      for(j in 1:length(remove)){
        if(remove[j] %in% colnames){
          colnames = colnames[colnames != remove[j]]
        }
      }
      if(length(colnames) > 0){
        df = df %>%
          pivot_longer(cols = -c("Station", "Hour", "Type"), names_to = "Min", values_to = "Value") %>%
          arrange(Min) %>%
          arrange(Hour) %>%
          arrange(Type) %>%
          arrange(Station)
      }
    }

    #Pivot wider
    if(pivot == "wider"){
      #select all necessary columns
      require = c("Station", "Hour", "Min", "Type", "Value")
      colnames = colnames(df)
      require = require %in% colnames
      flag = T
      for(j in 1:length(require)){
        if(require[j] == F){
          flag = F
        }
      }
      if(flag == F){
        message(paste0("Can not pivot ", i))
        next
      }

      df = df %>%
        select(Station, Hour, Min, Type, Value) %>%
        distinct()

      #check if there is duplicates
      df_dup = df %>%
        group_by(Station, Type, Hour, Min) %>%
        summarise(n = dplyr::n(), .groups = "drop") %>%
        filter(n > 1L)
      df_dup$avg = NA

      #if there is duplicate, get their average, remove from selected data
      if(nrow(df_dup) > 0 ){
        for(j in 1:nrow(df_dup)){
          df_avg = df %>%
            filter(Hour    == df_dup$Hour[j]   ) %>%
            filter(Min     == df_dup$Min[j]    ) %>%
            filter(Station == df_dup$Station[j]) %>%
            filter(Type    == df_dup$Type[j]   )

          df_dup$avg[j] = mean(df_avg$Value)
          df = df %>%
            filter(Hour    != df_dup$Hour[j] &
                   Min     != df_dup$Min[j]  &
                   Station != df_dup$Station[j] &
                   Type    != df_dup$Type[j])
        }

        df_dup = df_dup %>%
          select(-n) %>%
          rename(Value = avg)
      }

      #merge selected data with dup filtered data
      if(nrow(df_dup) > 0){
        df = bind_rows(df, df_dup) %>%
          arrange(Min) %>%
          arrange(Hour) %>%
          arrange(Station) %>%
          arrange(Type)
      }
      #pivot wider
      df = df %>%
        distinct() %>%
        arrange(Min) %>%
        pivot_wider(names_from = Min, values_from = Value) %>%
        arrange(Hour) %>%
        as.data.frame()
    }

    #Return it back to obj
    #return it back to obj
    #return it back to obj
    obj$data[[i]] = df
  }

  obj
}
