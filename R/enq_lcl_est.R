#' Find lifted condensation level (LCL) through estimation
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
enq_lcl_est = function(dwpt, temp, pres){
  #test input
  flag_dwpt = !is.numeric(dwpt)
  flag_temp = !is.numeric(temp)
  flag_pres = !is.numeric(pres)
  flag_length = length(dwpt) != length(temp) |
                length(dwpt) != length(pres) |
                length(temp) != length(pres)

  flag_all = flag_dwpt | flag_temp | flag_pres | flag_length

  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_dwpt){message("ERROR: variable dwpt is incorrect. (Only accepts numbers)")}
    if(flag_temp){message("ERROR: variable temp is incorrect. (Only accepts numbers)")}
    if(flag_pres){message("ERROR: variable pres is incorrect. (Only accepts numbers)")}
    if(flag_length){message("ERROR: variable length is incorrect. (must be of the same length)")}
    return(message("---ENQUIRE ERROR---"))
  }
  #additional variable
  b = 1.225
  CpR = 3.5

  #start calculation
  data = data.frame(dwpt = dwpt,
                    temp = temp,
                    pres = pres,
                    lclp = NA,
                    lclt = NA,
                    rs_dwp = NA,
                    es_dwp = NA,
                    rs_lcl = NA,
                    es_lcl = NA) %>%
    mutate(dwptK= dwpt + 273.15,
           tempK= temp + 273.15) %>%
    mutate(lclp = pres * (1 - b*(tempK - dwptK)/tempK)^CpR) %>%
    mutate(lclt = enq_dabt(temp1 = temp, pres1 = pres, pres2 = lclp, find = "temp")$temp2) %>%
    mutate(rs_dwp = enq_rs(temp = dwpt, pres = pres, find = "rs")$rs) %>%
    mutate(es_dwp = enq_es(temp = dwpt, find = "es")$es) %>%
    mutate(rs_lcl = enq_rs(temp = lclt, pres = lclp, find = "rs")$rs) %>%
    mutate(es_lcl = enq_es(temp = lclt, find = "es")$es) %>%
    select(-dwptK, -tempK)
  return(data)
}
