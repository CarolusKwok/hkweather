#' Find lifted condensation level (LCL)
#'
#' Only outputs lifted condensation level, and not in reverse.
#' Only accepts dewpoint and temperature input at the same pressure.
#' Outputs lclp (LCL, pressure) and lclt (LCL, temperature), in the unit of hPa and deg Celsius.
#'
#' @param dwpt Dewpoint temperature, in degree Celsius
#' @param temp Air temperature, in degree Celsius
#' @param pres Air pressure, in hPa
#'
#' @return
#' @export
#'
#' @examples enq_lcl(c(10,21), 20, 1000)
enq_lcl = function(dwpt, temp, pres){
  hkweather::hkw_lib()
  #test input
  flag_dwpt = !is.numeric(dwpt)
  flag_temp = !is.numeric(temp)
  flag_pres = !is.numeric(pres)

  flag_all = flag_dwpt | flag_temp | flag_pres

  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_dwpt){message("ERROR: variable dwpt is incorrect. (Only accepts numbers)")}
    if(flag_temp){message("ERROR: variable temp is incorrect. (Only accepts numbers")}
    if(flag_pres){message("ERROR: variable pres is incorrect. (Only accepts numbers")}
    return(message("---ENQUIRE ERROR---"))
  }
  #start calculation

  data = data.frame(dwpt = dwpt,
                    temp = temp,
                    pres = pres,
                    lclp = NA,
                    lclt = NA,
                    rs   = NA,
                    es   = NA) %>%
    mutate(rs = enq_rs(temp = dwpt, pres = pres, find = "rs")$rs)%>%
    mutate(es = enq_es(temp = dwpt, find = "es")$es)

  data_est = enq_lcl_est(dwpt = data$dwpt, temp = data$temp, pres = data$pres)

  data = data %>%
    mutate(lclp_est = data_est$lclp)

  for(i in 1:nrow(data)){
    message(paste0("Computing observation ", i))
    org_t = data$temp[i]
    org_d = data$dwpt[i]
    org_p = data$pres[i]
    org_rs = data$rs[i]

    ana_p = data$pres[i]
    ana_t = data$temp[i]
    ana_d = data$dwpt[i]

    if(data$dwpt[i] > data$temp[i]){
      ana_p = ana_p - 10

      while(ana_t < ana_d){
        ana_p = ana_p + 5
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p - 6
      while(ana_t < ana_d){
        ana_p = ana_p + 2
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p - 3
      while(ana_t < ana_d){
        ana_p = ana_p + 0.02
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p - 0.03
      while(ana_t < ana_d){
        ana_p = ana_p + 0.002
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p - 0.003
      while(ana_t < ana_d){
        ana_p = ana_p + 0.0002
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p - 0.0003
      while(ana_t < ana_d){
        ana_p = ana_p + 0.00001
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
    }
    if(data$dwpt[i] < data$temp[i]){
      ana_p = ana_p + 10

      while(ana_t > ana_d){
        ana_p = ana_p - 5
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p + 6
      while(ana_t > ana_d){
        ana_p = ana_p - 2
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p + 3
      while(ana_t > ana_d){
        ana_p = ana_p - 0.02
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p + 0.03
      while(ana_t > ana_d){
        ana_p = ana_p - 0.002
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p + 0.003
      while(ana_t > ana_d){
        ana_p = ana_p - 0.0002
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
      ana_p = ana_p + 0.0003
      while(ana_t > ana_d){
        ana_p = ana_p - 0.00001
        ana_t = enq_dabt(temp1 = org_t, pres1 = org_p, pres2 = ana_p, find = "temp")$temp2
        ana_d = enq_rs(rs = org_rs, pres = ana_p, find = "temp")$temp
      }
    }
    ana_a = (ana_t + ana_d) / 2
    ana_p = enq_dabt(temp1 = org_t, pres1 = org_p, temp2 = ana_a, find = "pres")$pres2

    data$lclp[i] = ana_p
    data$lclt[i] = ana_a
  }
  return(data)
}
