



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
