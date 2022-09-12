#' Draw isohume lines
#'
#' Isohume lines refers to lines of the same mixing ratio, at different temperature and pressure (assuming the air is saturated).
#' This function creates these lines by creating a data frame.
#'
#' @param rs Mixing ratios to be drawn, in g/kg.
#' @param pres Pressure range to draw. Default to be between 1500 to 5 hPa. Must be a list of 2 number that can be divided by 5.
#'
#' @return
#' @export
#'
#' @examples draw_line_ihum()
draw_line_ihum = function(rs = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 30, 50),
                          pres = c(1500, 5)){
  hkweather::hkw_lib()
  #Check moist
  flag_rs = !is.numeric(rs)
  flag_pres = (!is.numeric(pres) | (length(pres) != 2))
  if(flag_pres == F){
    if(pres[1]%%5 != 0){
      flag_pres = T
    }
    if(pres[2]%%5 != 0){
      flag_pres = T
    }
  }
  flag_all = flag_rs | flag_pres
  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_rs){message("ERROR: variable rs is incorrect. (Only accepts numbers)")}
    if(flag_pres){message("ERROR: variable pres is incorrect. (Only accepts 2 numbers, divisible by 5)")}
    return(message("---DRAW ERROR---"))
  }

  #Solution
  data1 = data.frame(rs = rs)
  data2 = data.frame(pres = seq(max(pres), min(pres), -5))
  data3 = expand_grid(data1, data2) %>%
    mutate(temp = enq_rs(rs = rs, pres = pres, find = "temp")$temp) %>%
    rename(group = rs, PRES = pres, TEMP = temp) %>%
    mutate(type = "IHUM") %>%
    relocate(type, group, PRES, TEMP)
  return(data3)
}
