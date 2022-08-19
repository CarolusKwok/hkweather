#' Warning! This is an internal command!
#' Users should not use this!
#' Creates directory for HKWeather. This should not be used alone.
#'
#' @param wDIR xxxx
#' @param filename xxxx
#'
#' @return
#' @export
#'
#' @examples hkw_dir.cre3(wDIR = paste0(getwd(), "/a", "/b", "/c.csv"), filename = T)
hkw_dir.cre3 = function(wDIR, filename){
  #Load all library
  hkweather::hkw_lib()

  #Create DIR dataframe
  DIR = data.frame(DIR = wDIR)
  DIR_temp = DIR
  DIR_cre = data.frame(DIR = NA)
  if(filename == F){
    DIR_cre = DIR
  }

  while(nrow(DIR_temp) != 0){
    DIR_temp = DIR_temp %>%
      mutate(DIR = dirname(DIR)) %>%
      filter(DIR != "." & substr(DIR, nchar(DIR)-1, nchar(DIR)) != ":/") %>%
      distinct()

    if(nrow(DIR_temp) > 0){
      DIR_cre = bind_rows(DIR_temp, DIR_cre)
    }
  }
  DIR_cre = DIR_cre %>%
    filter(!is.na(DIR)) %>%
    distinct() %>%
    mutate(exist = dir.exists(DIR)) %>%
    filter(exist == F)

  #Start making the directory!
  if(nrow(DIR_cre) > 0){
    lapply(DIR_cre$DIR, dir.create)
    message("Directory successfully created")
  }
}
