#' Warning! This is an internal command!
#' Users should not use this!
#' Creates directory for HKWeather. This should not be used alone.
#'
#' @param DIR xxx
#' @param expand xxx
#' @param ... xxx
#'
#' @return
#' @export
#'
#' @examples hkw_dir.cre2(a = c("1"))
hkw_dir.cre2 = function(DIR = getwd(), expand = "all T", ...){
  hkweather::hkw_lib()
  list = list(...)

  if(length(expand) != length(list) & expand[1] != "all T"){
    return(message("Error: Length of Expand is not same as ..."))
  }
  if(expand[1] == "all T"){
    expand = data.frame(a = (1:length(list)), b = T)$b
  }

  DIR = getwd()
  DIR_df = data.frame(V3 = list[[1]]) %>%
    mutate(V3 = paste0("/", V3))
  CRE_df = DIR_df %>%
    mutate(DIR = paste0(DIR, V3),
           Present = dir.exists(DIR)) %>%
    filter(Present == F)
  if(nrow(CRE_df) > 0){
    lapply(CRE_df$DIR, dir.create)
  }

  oldw = getOption("warn")
  options(warn = -1)
  ###
  for(i in 2:length(list)){
    if(expand[i] == T | expand[i] == "F"){
      DIR_df = expand.grid(DIR_df$V3, list[[i]]) %>%
        mutate(V3 = paste0(Var1,"/",Var2)) %>%
        select(V3)
      CRE_df = DIR_df %>%
        mutate(DIR = paste0(DIR, V3),
               Present = dir.exists(DIR)) %>%
        filter(Present == F)
      if(nrow(CRE_df) > 0){
        lapply(CRE_df$DIR, dir.create)
      }
    }
    if(expand[i] == F | expand[i] == "F"){
      DIR_df = DIR_df %>%
        mutate(V4 = list[[i]]) %>%
        mutate(V3 = paste0(V3,"/",V4)) %>%
        select(V3)
      CRE_df = DIR_df %>%
        mutate(DIR = paste0(DIR, V3),
               Present = dir.exists(DIR)) %>%
        filter(Present == F)
      if(nrow(CRE_df) > 0){
        lapply(CRE_df$DIR, dir.create)
      }
    }
    if(expand[i] == "inbase"){
    }
    if(expand[i] == "inname"){
    }

  }
  ###
  options(warn = oldw)
}
