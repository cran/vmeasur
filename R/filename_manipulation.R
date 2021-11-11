


#' Make a file name that identifies an animal
#'
#' @param animal animal id
#' @param treatment treatment type
#' @param video source video which was measured
#' @param roi region of interest imaged
#' @param list a list containing all the data, for convenience when being called from other functions
#' @param extension the filename extension required
#'
#' @return the name of a file to be used
#'
#' @noRd
#'
#' @examples
#' # filename = make_filename("1", "Test", extension = ".csv")
#'
make_filename = function(animal = "NS", treatment = "NS", video = "NS", roi = "NS", list = NULL, extension = "")
{
  if(!is.null(list))
  {
    animal = list["animal"]
    treatment = list["treatment"]
    video = list["video"]
    roi = list["roi"]
  }

  paste("[animal_", animal, "]", "[treatment_", treatment, "]", "[video_", video, "]", "[roi_", roi, "]", extension, sep = "")
}




#' Divide a filename into multiple variables that have been assigned to the vessel
#'
#' @param filename the filename to be broken up into sections
#'
#' @return a table containing the content of the variable that has been broken up
#'
#' @importFrom stringr str_replace
#' @importFrom tidyr separate
#'
#' @noRd
#'
break_filename = function(filename)
{
  if(is.data.frame(filename))
  {
    working_filename = "[animal_NS][treatment_NS][video_NS][roi_NS]_widths"
  } else{
    working_filename = file_path_sans_ext(basename(filename))
  }



  breakout = str_split(working_filename, "\\]\\[")[[1]]
  breakout = str_replace(breakout, "\\]", "") %>% str_remove("\\[")
  breakout = data.frame(breakout)
  breakout = breakout %>% separate(breakout, c("variable", "value"), sep = "_", extra = "drop")

  br_list = as.list(breakout$value)
  names(br_list) = breakout$variable

  return(br_list)
}


#' Collect the variables needed to make a file name from the user using a popup
#'
#' @param animal the animal that the name should be generated for
#' @param treatment type of treatment the animal received
#' @param video id of the video which was captured
#' @param roi the region of interest selected
#'
#' @importFrom utils edit
#'
#' @noRd
#'
#' @return a list containing the critical filename values collected from the user
#'
#' @examples
#' # Run in interactive mode only
#' # make_filename(list = collect_filename())
collect_filename = function(animal = "NS", treatment = "NS", video = "NS", roi = "NS")
{
  blank_data = data.frame(Variable = c("Animal", "Treatment", "Video", "ROI"), Value = c(animal, treatment, video, roi))
  blank_data = edit(blank_data)
  br_list = as.list(blank_data$Value)
  names(br_list) = tolower(blank_data$Variable)
  return(br_list)
}








