#' Find condition, moist-adibetically
#'
#' @param temp1 Temperature at Pressure level 1, in degC. Only accepts numbers or a list of numbers.
#' @param pres1 Pressure at Pressure level 1, in hPa. Only accepts numbers or a list of numbers.
#' @param pres2 Pressure at Pressure level 2, in hPa. Only accepts numbers or a list of numbers.
#' @param acc Accuracy of the calculation. Smaller number, Higher accuracy. Only accepts number.
#'
#' @return
#' @export
#'
#' @examples enq_mabt(c(10, 20, 30), 1000, 500) should return -125.4218, -119.2539, -112.3171, as a list
enq_mabt = function(temp1, pres1, pres2, acc = 0.1){
  #Test input
  flag_temp1 = ifelse(is.numeric(temp1), F, T)
  flag_pres1 = ifelse(is.numeric(pres1), F, T)
  flag_pres2 = ifelse(is.numeric(pres2), F, T)
  flag_acc   = ifelse(is.numeric(acc), F, T)

  flag_t1_p1 = F
  flag_t1_p2 = F
  flag_p1_p2 = F

  if(length(temp1) != 1){
    if(length(pres1) != 1){
      if(length(temp1) != length(pres1)){
        flag_t1_p1 = T
      }
    }
  }
  if(length(temp1) != 1){
    if(length(pres2) != 1){
      if(length(temp1) != length(pres2)){
        flag_t1_p2 = T
      }
    }
  }
  if(length(pres1) != 1){
    if(length(pres2) != 1){
      if(length(pres1) != length(pres2)){
        flag_t1_p1 = T
      }
    }
  }

  flag_all = flag_temp1 | flag_pres1 | flag_pres2 | flag_acc | flag_t1_p1 | flag_t1_p2 | flag_p1_p2

  if(flag_all == T){
    message("ERROR: Something is wrong with your input.")
    if(flag_temp1){message("ERROR: variable temp1 is incorrect. (Only accept numbers)")}
    if(flag_pres1){message("ERROR: variable pres1 is incorrect. (Only accept numbers)")}
    if(flag_pres2){message("ERROR: variable pres2 is incorrect. (Only accept numbers)")}
    if(flag_acc){message("ERROR: variable acc is incorrect. (Only accept numbers)")}
    if(flag_t1_p1){message("ERROR: variable temp1 and pres1 are incorrect. (Only accept list of numbers with the same length)")}
    if(flag_t1_p2){message("ERROR: variable temp1 and pres2 are incorrect. (Only accept list of numbers with the same length)")}
    if(flag_p1_p2){message("ERROR: variable pres1 and pres2 are incorrect. (Only accept list of numbers with the same length)")}
    return(message("---ENQUIRE ERROR---"))
  }

  #Additional Variables
  Rv = 461
  T0 = 273.15
  e0 = 0.6113
  L  = 2.5*10^6
  ep = 0.622
  a  = 0.28571
  b  = 13500000
  c  = 2488.4
  LRv= L/Rv

  #Start calculations
  max_len = max(c(length(temp1), length(pres1), length(pres2)))
  temp2_sel = c()
  for(i in 1:max_len){
    temp1_sel = temp1[i]
    pres1_sel = pres1[i]
    pres2_sel = pres2[i]

    if(is.na(temp1_sel)){temp1_sel = temp1}
    if(is.na(pres1_sel)){pres1_sel = pres1}
    if(is.na(pres2_sel)){pres2_sel = pres2}

    df = data.frame(PRES = seq(pres1_sel, pres2_sel, ifelse(pres1_sel < pres2_sel, acc, -acc)),
                    TEMP = NA,
                    TEMPK= NA,
                    es   = NA,
                    rs   = NA,
                    TP   = NA)
    df$TEMP[1] = temp1_sel
    for(i in 1:nrow(df)){
      df$TEMPK[i] = df$TEMP[i] + 273.15
      df$es   [i] = e0 * exp(LRv *(1/T0 - 1/df$TEMPK[i]))
      df$rs   [i] = (ep * df$es[i]) / (df$PRES[i] - df$es[i])
      df$TP   [i] = (a * df$TEMPK[i] + c* df$rs[i])/
        (df$PRES[i] * (1 + b * df$rs[i]/(df$TEMPK[i]^2)))
      if(i != nrow(df)){
        df$TEMP[i+1] = df$TEMP[i] + df$TP[i] * (df$PRES[i+1] - df$PRES[i])
      }
    }
    temp2_sel = c(temp2_sel, df$TEMP[nrow(df)])
  }
  remove(df)
  return(temp2_sel)
}
