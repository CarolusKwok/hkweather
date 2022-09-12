#' Find the saturation vapor pressure or reverse
#'
#' Using Buck equation. Check more in https://www.omnicalculator.com/chemistry/vapour-pressure-of-water.
#'
#' @param temp Air temperature, in degree Celsius
#' @param es Saturation vapor pressure, in hPa
#' @param find what to find
#'
#' @return
#' @export
#'
#' @examples enq_es(temp = 20, find = "es)
enq_es = function(temp, es, find = "es"){
  hkweather::hkw_lib()
  #check input
  flag_temp = F
  flag_es   = F
  if(hasArg(temp)){
    if(!is.numeric(temp)){
      flag_temp = T
    }
  }
  if(hasArg(es)){
    if(!is.numeric(es)){
      flag_es = T
    }
  }
  flag_find = ifelse(find == "es" | find == "temp", F, T)

  if(flag_find == F){
    if(find == "temp" & !hasArg(es)){
      flag_find = T
    }
    if(find == "es" & !hasArg(temp)){
      flag_find = T
    }
  }
  flag_all = flag_temp | flag_es | flag_find
  if(flag_all == T){
    message("ERROR: Something is wrong with your input.")
    if(flag_temp){message("ERROR: variable temp is incorrect. (Only accepts numbers)")}
    if(flag_es){message("ERROR: variable es is incorrect. (Only accepts numbers")}
    if(flag_find){message("ERROR: variable find is incorrect. (Only accepts es/ temp)")}
    return(message("---ENQUIRE ERROR---"))
  }

  #Additional variables
  e0 = 6.1121 #hPa

  #Calculate the saturation pressure
  if(find == "es"){
    df = data.frame(temp = temp,
                    es = NA) %>%
      mutate(es = e0 * exp((18.678 - (temp/234.5)) * (temp / (257.14 + temp))))
    return(df)
  }
  if(find == "temp"){
    df = data.frame(es = es,
                    tp = NA,
                    temp1 = NA,
                    temp2 = NA) %>%
      mutate(tp = log(es/e0)) %>%
      mutate(temp1 = 2189.9955 - 117.25*tp - 117.25 * sqrt(tp^2 - 41.74218336 * tp + 348.867684)) %>%
      mutate(temp2 = 2189.9955 - 117.25*tp + 117.25 * sqrt(tp^2 - 41.74218336 * tp + 348.867684)) %>%
      select(-tp)
    return(df)
  }
}
