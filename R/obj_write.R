#' Write data of an object as file
#'
#' @param obj a hkweather object
#' @param type
#'
#' @return
#' @export
#'
#' @examples obj_write(obj)

obj_write = function(obj, wDIR, type = "xlsx"){
  hkweather::hkw_lib()
  #prepare the location and file prefix to export
  if(!hasArg(wDIR)){
    wDIR = getwd()
  }

  default = options(warning)
  options(warn = -1)
  dir.create(paste0(wDIR,"/Analysed Data"))
  dir.create(paste0(wDIR,"/Analysed Data", "/Tables"))
  options(warn = 0)

  wDIR =     paste0(wDIR, "/Analysed Data", "/Tables")

  for(i in 1:length(obj$legend)){
    if("all" %in% obj$legend[[i]]){
      break
    }
  }

  df = obj$data[[i]]
  max_hour = max(df$Hour)
  min_hour = min(df$Hour)

  obj_px = paste0(obj$type,"-",
                  min_hour,"-",
                  max_hour)

  #write the file
  if(type == "xlsx"){
    wb = createWorkbook()
    for(i in 1:length(obj$legend)){
      sheet = createSheet(wb, paste0(obj$legend[[i]]))
      addDataFrame(as.data.frame(obj$data[[i]]), sheet = sheet, startColumn = 1, row.names = F)
    }
    saveWorkbook(wb, paste0(wDIR, "/", obj_px, ".xlsx"))
  }
  if(type == "csv"){
    dir.create(paste0(wDIR, "/", obj_px))
    wDIR =     paste0(wDIR, "/", obj_px)

    for(i in 1:length(obj$legend)){
        write_csv(obj$data[[i]], paste0(wDIR, "/", obj_px, "-", obj$legend[[i]], ".csv"))
    }
  }
}
