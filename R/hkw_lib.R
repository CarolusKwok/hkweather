#' Loads all necessary library for HKW
#'
#' Warning! This is an internal system command. Users should not use this!
#'
#' @return
#' @export
#'
#' @examples hkw_lib()
hkw_lib = function(){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(gifski))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(magick))
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(tidyr))
}
