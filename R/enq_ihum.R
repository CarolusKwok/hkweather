#' Find temperature and pressure at isohume or reverse, assuming saturated
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
#' @examples enq_ihum(0, 1013, 500, find = "temp")
enq_ihum = function(temp1, pres1, pres2, temp2, find = "temp"){
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

  #Find the mixing ratio
  es_rs = enq_rs(temp = temp1, pres = pres1, find = "rs")
  es = es_rs$es
  rs = es_rs$rs

  #Calculate temp2
  if(find == "temp"){
    temp2 = enq_rs(pres = pres2, rs = rs, find = "temp")$temp
    df = data.frame(temp1 = temp1,
                    pres1 = pres1,
                    temp2 = temp2,
                    pres2 = pres2,
                    rs    = rs,
                    es    = es)
    return(df)
  }
  #Calculate pres2
  if(find == "pres"){
    pres2 = enq_rs(temp = temp2, rs = rs, find = "pres")$pres
    df = data.frame(temp1 = temp1,
                    pres1 = pres1,
                    temp2 = temp2,
                    pres2 = pres2,
                    rs    = rs,
                    es    = es)
    return(df)
  }
}
