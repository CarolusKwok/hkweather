#' Loads dry adiabetic line
#'
#' Dry diabetic line refers to the temperature of dry, moisture free gas raising into the atmosphere.
#' This function creates these lines by creating a data frame.
#'
#' @param temp Temperature of said gas at a pressure of 1000hPa.
#' @param pres Pressure range to draw. Default to be between 1500 to 5 hPa. Must be a list of 2 number that can be divided by 5.
#'
#' @return
#' @export
#'
#' @examples draw_line_dabt()
draw_line_dabt = function(temp = seq(-50, 50, 10), ini_pres = 1000, pres = c(5, 1500)){
  hkweather::hkw_lib()
  #Check moist
  flag_temp = !is.numeric(temp)
  flag_pres = (!is.numeric(pres) | (length(pres) != 2))
  if(flag_pres == F){
    if(pres[1]%%5 != 0){
      flag_pres = T
    }
    if(pres[2]%%5 != 0){
      flag_pres = T
    }
  }
  flag_all = flag_temp | flag_pres
  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_temp){message("ERROR: variable temp is incorrect. (Only accepts numbers)")}
    if(flag_pres){message("ERROR: variable pres is incorrect. (Only accepts 2 numbers, divisible by 5)")}
    return(message("---DRAW ERROR---"))
  }

  #Solution
  data1 = data.frame(temp1 = temp,
                     pres1 = ini_pres)
  data2 = data.frame(pres2 = seq(max(pres), min(pres), -5))
  data3 = expand_grid(data1, data2) %>%
    mutate(temp2 = enq_dabt(temp1, pres1, pres2, find = "temp")$temp2,
           type  = "DABT") %>%
    rename(group = temp1) %>%
    relocate(type, group) %>%
    select(-pres1) %>%
    rename(PRES = pres2) %>%
    rename(TEMP = temp2)
  return(data3)
}
