#' Find temperature at different common scales
#'
#' @param degc Temperature in degree Celsius
#' @param degf Temperature in degree Fahrenheit
#' @param degk Temperature in degree Kelvin
#'
#' @return
#' @export
#'
#' @examples enq_temp(10, -40, 273.15)
enq_temp = function(degc, degf, degk){
  hkweather::hkw_lib()
  #check input
  flag_degc = !is.numeric(degc)
  flag_degf = !is.numeric(degf)
  flag_degk = !is.numeric(degk)

  flag_all = flag_degc | flag_degf | flag_degk
  if(flag_all){
    message("ERROR: Something is wrong with your input.")
    if(flag_degc){message("ERROR: variable degc is incorrect. (Only accepts numbers)")}
    if(flag_degf){message("ERROR: variable degf is incorrect. (Only accepts numbers")}
    if(flag_degk){message("ERROR: variable degk is incorrect. (Only accepts numbers)")}
    return(message("---ENQUIRE ERROR---"))
  }

  #START
  if(hasArg(degc)){
    df_degc = data.frame(degc = degc,
                         degf = NA,
                         degk = NA) %>%
      mutate(degk = degc + 273.15,
             degf = degc*9/5 + 32)
  } else {
    df_degc = data.frame(degc = NA,
                         degf = NA,
                         degk = NA)
  }
  if(hasArg(degf)){
    df_degf = data.frame(degc = NA,
                         degf = degf,
                         degk = NA) %>%
      mutate(degc = (degf-32) *5/9,
             degk = degc + 273.15)
  } else {
    df_degf = data.frame(degc = NA,
                         degf = NA,
                         degk = NA)
  }
  if(hasArg(degk)){
    df_degk = data.frame(degc = NA,
                         degf = NA,
                         degk = degk) %>%
      mutate(degc = degk - 273.15,
             degf = degc*9/5 + 32)
  } else {
    df_degk = data.frame(degc = NA,
                         degf = NA,
                         degk = NA)
  }
  df = bind_rows(df_degc, df_degf, df_degk) %>%
    drop_na()
  return(df)
}
