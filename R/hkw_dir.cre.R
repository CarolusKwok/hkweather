#' Warning! This is an internal command!
#' Users should not use this!
#' Creates directory for HKWeather. This should not be used alone.
#'
#' @param dir xxx
#' @param pri xxx
#' @param sec xxx
#'
#' @return
#' @export
#'
#' @examples hkw_dir.cre(pri = c("Rfhr", "C"), sec = data.frame$Date)
hkw_dir.cre = function(dir = getwd(), pri, sec){
  dir = paste(dir, "/", sep = "")
  for(i in 1:length(pri)){
    dir = paste(dir, pri[i], sep = "")
  }
  dir.create(dir, showWarnings = F)
  for(i in 1:length(sec)){
    dir.create(paste(dir, "/", sec[i], sep = ""), showWarnings = F)
  }
  message("Directory successfully created")
}
