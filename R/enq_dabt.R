#' Find condition, dry-adibetically
#'
#' @param temp1 Air temperature at condition 1, in degree Celsius
#' @param pres1 Air pressure at condition 1, in hPa
#' @param pres2 Air pressure at condition 2, in hPa
#' @param temp2 Air temperature at condition 2, in degree Celsius
#' @param find what to find? temp or pres?
#'
#' @return
#' @export
#'
#' @examples enq_dabt(20, 1000, 800, find = "temp")
enq_dabt = function(temp1, pres1, pres2, temp2, find = "temp"){
  hkweather::hkw_lib()
  #Check input
  flag_temp1 = F
  flag_pres1 = F
  flag_temp2 = F
  flag_pres2 = F
  if(hasArg(temp1)){
    if(!is.numeric(temp1)){
      flag_temp1 = T
    }
  }
  if(hasArg(pres1)){
    if(!is.numeric(pres1)){
      flag_pres1 = T
    }
  }
  if(hasArg(temp2)){
    if(!is.numeric(temp2)){
      flag_temp2 = T
    }
  }
  if(hasArg(pres2)){
    if(!is.numeric(pres2)){
      flag_pres2 = T
    }
  }

  flag_find = ifelse(find == "temp" | find == "pres", F, T)

  if(flag_find == F){
    if(find == "temp"){
      if(!hasArg(temp1) | !hasArg(pres1) | !hasArg(pres2)){
        flag_find = T
      }
    }
    if(find == "pres"){
      if(!hasArg(temp1) | !hasArg(pres1) | !hasArg(temp2)){
        flag_find = T
      }
    }
  }

  flag_all = flag_temp1 | flag_pres1 | flag_temp2 | flag_pres2 | flag_find
  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_temp1){message("ERROR: variable temp1 is incorrect. (Only accepts numbers)")}
    if(flag_pres1){message("ERROR: variable pres1 is incorrect. (Only accepts numbers")}
    if(flag_temp2){message("ERROR: variable temp2 is incorrect. (Only accepts numbers)")}
    if(flag_pres2){message("ERROR: variable pres2 is incorrect. (Only accepts numbers")}
    if(flag_find){message("ERROR: variable find is incorrect. (Only accepts temp/ pres)")}
    return(message("---ENQUIRE ERROR---"))
  }

  #Additional variable
  RdCp = 0.28571
  #Calculate Pres2
  if(find == "pres"){
    df = data.frame(temp1 = temp1,
                    pres1 = pres1,
                    temp2 = temp2,
                    pres2 = NA) %>%
      mutate(temp1K = temp1 + 273.15) %>%
      mutate(temp2K = temp2 + 273.15) %>%
      mutate(pres2 = 10 ^ (log10(temp2K/ temp1K) / RdCp + 3)) %>%
      select(-temp1K, -temp2K)
    return(df)
  }
  #Calculate Temp2
  if(find == "temp"){
    df = data.frame(temp1 = temp1,
                    pres1 = pres1,
                    temp2 = NA,
                    pres2 = pres2) %>%
      mutate(temp1K = temp1 + 273.15) %>%
      mutate(temp2K = ((pres2 / pres1)^RdCp) * temp1K) %>%
      mutate(temp2 = temp2K - 273.15) %>%
      select(-temp1K, -temp2K)
    return(df)
  }
}
