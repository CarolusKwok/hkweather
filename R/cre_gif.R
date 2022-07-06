#' Creates a .gif file based on images within working directory.
#'
#' @param DIR Path of the directory. Accepts list of directory.
#' @param name Name of the gif file created. Default as the combination of start and end frame file names.
#' @param fps Frame per second of the gif file created. Default as 25 fps.
#'
#' @return
#' @export
#'
#' @examples cre_gif()
cre_gif = function(DIR = NA, name = NA, fps = 25){
  hkw_lib()
  #Check if DIR exist
  if(sum(is.na(DIR))){
    message("Warning! Something is wrong in the input")
    message("Variable DIR is missing! (Directory paths only)")
    return("---GIF Creation Failed---")
  }
  #Check if any one of the DIR is incorrect
  flag_DIR = F
  for (i in 1:length(DIR)){
    if(!dir.exists(DIR[[i]])){
      message("Warning! Something is wrong in the input")
      message(paste(DIR[[i]], "is incorrect!"))
      flag_DIR = T
    }
  }
  if(flag_DIR){
    return("---GIF Creation Failed---")
  }
  #Obtain frame list and file name (if not specified)
  frame = list()
  for (i in 1:length(DIR)){
    frame = append(frame, as.list(list.files(DIR[[i]], full.names = T)))
    message(paste(DIR[[i]], "selected"))
  }
  frame = str_sort(frame, numeric = T)
  if(is.na(name)){
    frame_1 = frame[1]
    frame_n = frame[length(frame)]
    frame_1 = tools::file_path_sans_ext(basename(frame_1))
    frame_n = tools::file_path_sans_ext(basename(frame_n))
    name = paste0(frame_1,"-",frame_n)
  }
  #Start animation
  message("Reading frames")
  frame_list = lapply(frame, image_read)
  message("Joining frames")
  frame_join = image_join(frame_list)
  message("Writing file")
  mimage_write_gif(image = frame_join, path = paste0(getwd(), "/", name, ".gif"), delay = 1/fps)
  rm(list = c(frame_list, frame_join, frame_1, frame_n))
  gc()
}
