#' Create an hkweather object from processed file and dataframe
#'
#' @param rDIR Reading Directory. Which folder should I read to get all the information?
#' @param type Type of data. Which type of data it is? (Only accepts gtmp/ mslp/ rain/ rhum/ sart/ temp/ tide/ wind)
#' @param read Data format of processed data. (Only accepts df/ xlsx/ csv)
#'
#' @return
#' @export
#'
#' @examples obj_read(load_tide_pre(), type = "tide", read = "df")
obj_read = function(rDIR, type, read = "df"){
  if(read == "df"){
    obj = list(type = type,
               legend = list("all"),
               data   = list(rDIR),
               pivot  = list("XXXX"))
  }
  class(obj) = "hkweather"
  obj
}



#read the data
#if(type == "csv"){
#  message("Reading CSV")
#  obj = list(legend = NA,
#             data = read_csv(rDIR))
#}

#if(type == "xlsx"){
#  message("Reading XLSX")
#  sheetname = (openxlsx::getSheetNames(rDIR))

#  obj = list(legend = as.list(sheetname),
#             data = sheetname)
#  obj$data = lapply(obj$data, openxlsx::read.xlsx, xlsxFile = rDIR)
#}
