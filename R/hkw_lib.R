#' Loads all necessary library for HKW
#'
#' So I don't need to type dplyr:: 200 times
#' Warning! This is an internal system command. Users should not use this!
#'
#' @return
#' @export
#'
#' @examples hkw_lib()
hkw_lib = function(){
  suppressPackageStartupMessages(library(xlsx))
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(gifski))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(magick))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(svglite))
}
