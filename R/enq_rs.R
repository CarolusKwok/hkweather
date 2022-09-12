#' Find saturation mixing ratio or reverse
#'
#' @param temp Air temperature, in degree Celsius
#' @param pres Saturation vapor pressure, in hPa
#' @param rs Saturation mixing ratio, in g_H2O/kg_air
#' @param find what to find
#'
#' @return
#' @export
#'
#' @examples enq_rs(1, 1013, find = "rs")
enq_rs = function(temp, pres, rs, find = "rs"){
  hkweather::hkw_lib()
  #Check input
  flag_temp = F
  flag_pres = F
  flag_rs = F
  flag_find = ifelse(find == "temp" | find == "pres" | find == "rs", F, T)

  if(hasArg(temp)){
    if(!is.numeric(temp)){
      flag_temp = T
    }
  }
  if(hasArg(pres)){
    if(!is.numeric(pres)){
      flag_pres = T
    }
  }
  if(hasArg(rs)){
    if(!is.numeric(rs)){
      flag_rs = T
    }
  }

  if(flag_find == F){
    if(find == "temp" & (!hasArg(pres) | !hasArg(rs))){
      flag_find = T
    }
    if(find == "pres" & (!hasArg(temp) | !hasArg(rs))){
      flag_find = T
    }
    if(find == "rs" & (!hasArg(temp) | !hasArg(pres))){
      flag_find = T
    }
  }

  flag_all = flag_temp | flag_pres | flag_rs | flag_find
  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_temp){message("ERROR: variable temp is incorrect. (Only accepts numbers)")}
    if(flag_pres){message("ERROR: variable pres is incorrect. (Only accepts numbers")}
    if(flag_rs){message("ERROR: variable rs is incorrect. (Only accepts numbers")}
    if(flag_find){message("ERROR: variable find is incorrect. (Only accepts temp/ pres/ rs)")}
    return(message("---ENQUIRE ERROR---"))
  }

  #additional variable
  ep = 621.97 #g/kg

  #find rs
  if(find == "rs"){
    #calcuate es
    es = enq_es(temp = temp, find = "es")$es

    #calcuate rs
    df = data.frame(temp = temp,
                    pres = pres,
                    rs   = NA,
                    es   = es) %>%
      mutate(rs = (ep * es) / (pres - es))
    return(df) #in g/kg
  }
  #find pres
  if(find == "pres"){
    #calculate es
    es = enq_es(temp, find = "es")$es

    #calculate pres
    df = data.frame(temp = temp,
                    pres = NA,
                    rs   = rs,
                    es   = es) %>%
      mutate(pres = ((ep + rs) * es) / rs)
    return(df)
  }
  #find temp
  if(find == "temp"){
    #calculate es
    #rs = (ep * es) / (pres - es)
    df = data.frame(temp = NA,
                    pres = pres,
                    rs   = rs,
                    es   = NA) %>%
      mutate(es = (rs * pres) / (ep + rs))

    #calculate temp
    temp = enq_es(es = df$es, find = "temp") %>%
      mutate(temp_sel = ifelse(abs(temp1) < abs(temp2), temp1, temp2))
    temp = temp$temp_sel
    df$temp = temp
    return(df)
  }
}
