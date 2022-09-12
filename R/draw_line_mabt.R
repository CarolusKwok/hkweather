#' Loads moist adiabetic line
#'
#' Moist diabetic line refers to the temperature of saturated gas raising into the atmosphere.
#' This function creates these lines by creating a data frame.
#'
#' @param temp Temperature of said gas at a pressure of 1000hPa.
#'
#' @return
#' @export
#'
#' @examples load_line_mabt()
draw_line_mabt = function(temp = seq(-50, 50, 10)){
  hkweather::hkw_lib()

  df = data.frame(type = NA, group = NA, PRES = NA, TEMP = NA)
  Rv = 461
  T0 = 273.15
  e0 = 0.6113
  L  = 2.5*10^6
  ep = 0.622
  a  = 0.28571
  b  = 13500000
  c  = 2488.4
  LRv= L/Rv

  for(i in 1:length(temp)){
    line = temp[i]

    df_1 = data.frame(PRES = seq(100, 0, -0.05),
                      TEMP = NA,
                      TEMPK= NA,
                      es   = NA,
                      rs   = NA,
                      TP   = NA)
    df_1$TEMP[1] = line
    for(i in 1:nrow(df_1)){
      df_1$TEMPK[i] = df_1$TEMP[i] + 273.15
      df_1$es   [i] = e0 * exp(LRv *(1/T0 - 1/df_1$TEMPK[i]))
      df_1$rs   [i] = (ep * df_1$es[i]) / (df_1$PRES[i] - df_1$es[i])
      df_1$TP   [i] = (a * df_1$TEMPK[i] + c* df_1$rs[i])/
        (df_1$PRES[i] * (1 + b * df_1$rs[i]/(df_1$TEMPK[i]^2)))
      if(i != nrow(df_1)){
        df_1$TEMP[i+1] = df_1$TEMP[i] + df_1$TP[i] * (df_1$PRES[i+1] - df_1$PRES[i])
      }
    }

    df_2 = data.frame(PRES = seq(100, 150, +0.05),
                      TEMP = NA,
                      TEMPK= NA,
                      es   = NA,
                      rs   = NA,
                      TP   = NA)
    df_2$TEMP[1] = line
    for(i in 1:nrow(df_2)){
      df_2$TEMPK[i] = df_2$TEMP[i] + 273.15
      df_2$es   [i] = e0 * exp(LRv *(1/T0 - 1/df_2$TEMPK[i]))
      df_2$rs   [i] = (ep * df_2$es[i]) / (df_2$PRES[i] - df_2$es[i])
      df_2$TP   [i] = (a * df_2$TEMPK[i] + c* df_2$rs[i])/
        (df_2$PRES[i] * (1 + b * df_2$rs[i]/(df_2$TEMPK[i]^2)))
      if(i != nrow(df_2)){
        df_2$TEMP[i+1] = df_2$TEMP[i] + df_2$TP[i] * (df_2$PRES[i+1] - df_2$PRES[i])
      }
    }

    df_3 = bind_rows(df_1, df_2) %>%
      distinct() %>%
      arrange(TEMP) %>%
      select(PRES, TEMP) %>%
      mutate(PRES = PRES * 10) %>%
      mutate(group = line) %>%
      mutate(type = "MABT") %>%
      relocate(type, group, PRES, TEMP)
    df = bind_rows(df, df_3)
  }
  df = df %>%
    drop_na()
  return(df)
}
