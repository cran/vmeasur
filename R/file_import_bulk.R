#' Import a saved file
#'
#' @param filename the csv file to import from
#'
#' @return a cleaned up data frame
#'
#' @importFrom dplyr bind_rows mutate group_by ungroup
#' @importFrom tidyr extract_numeric
#' @importFrom utils read.csv
#' @importFrom stringr str_split str_remove
#' @importFrom readr parse_number
#'
#'
#' @noRd
#'
#' @examples
#' i = 1
#'
#'
import_file = function(filename)
{
  if(is.data.frame(filename))
  {
    csvfile = filename
  }else{
    csvfile = read.csv(filename)
  }


  csvfile$X = NULL
  csvfile$X.1 = NULL

  file_source = basename(file_path_sans_ext(filename)) %>% str_split("_")

  video = file_source[[1]][[1]]
  treatment = (file_source[[1]][[2]] %>% str_split("S"))[[1]][[1]] %>% str_replace_all("[:digit:]","")
  animal = (file_source[[1]][[2]] %>% str_split("S"))[[1]][[1]] %>% str_replace_all("[A-Za-z]","")
  field = (file_source[[1]][[2]] %>% str_to_upper() %>% str_split("S"))[[1]][[2]]
  vessel = file_source[[1]][[3]]


  # csvfile %>% select(y, excluded) %>% distinct()
  # toexclude = csvfile %>% group_by(y) %>% summarise(excluded = sum(excluded==1))

  csvfile = csvfile %>% group_by(`y_position`) %>%
    mutate(y_position_excluded= sum(y_position_excluded)>1) %>%
    ungroup() %>%
    mutate(p_width = ifelse(!`y_position_excluded`, `p_width`, NA)) %>%
    mutate(video = video, treatment = treatment, animal = animal, field = field, vessel = vessel, roi = roi_name, y = y_position)





  return(csvfile)

}



#' Parallel mass csv import code
#'
#' @param current_dir list of csv files to import
#' @param y_bin number of pixels to put in each ybin
#'
#'
#' @importFrom dplyr row_number
#'
#' @return A bulk list of imported csv files
#'
#' @noRd
#'
#' @examples
#' # Not applicable to CRAN
#'
import_folder_bin = function(current_dir, y_bin = 30)
{

  # list out the CSV files to import
  folder_files = list.files(current_dir, recursive = TRUE, pattern = "width.csv$", full.names = TRUE)

  if(length(folder_files)==1)
  {
    fulldata = import_file(folder_files)
  } else{

  # Import them all with lapply and combine with dplyr

    applied = foreach(currentfile = folder_files) %do%
        {
       import_file(currentfile)
       }

  fulldata = dplyr::bind_rows(applied, .id = "file_id")
  }


  fulldata = fulldata %>% group_by(y, roi, animal, treatment, video) %>%
    mutate(frame_id = row_number())

  pixel_bin = y_bin

  fulldata_grouped = fulldata %>% mutate(ygroup = ((y-1) %/% pixel_bin) + 1)  %>%
    group_by(treatment, animal, roi, ygroup) %>%
    mutate(max = max(y), min = min(y), npix = max-min +1, trace = cur_group_id()) %>%
    filter(npix == pixel_bin) %>%
    ungroup()

  fulldata_mean = fulldata_grouped %>% filter(!y_position_excluded) %>% ungroup() %>%
    group_by(`frame_id`, `video`, `animal`, `treatment`, `roi`, `ygroup`) %>%
    summarise(p_mean = mean(p_width, na.rm = TRUE), p_median = median(p_width, na.rm = TRUE)) %>%
    group_by(`animal`, `treatment`, `roi`, `ygroup`) %>%
    mutate(trace_id = cur_group_id())

  return(fulldata_mean)

}



#' Bin and import a csv file
#'
#' @param file_location location of the file
#' @param y_bin bin size
#' @param raw is the data raw, or should averages be calculated for each bin
#'
#' @return
#' @noRd
#'
#' @examples
import_file_bin = function(file_location, y_bin = 30, raw = FALSE)
{

  if(is.data.frame(file_location)) {
    fulldata = file_location
  } else
  {
  fulldata = import_file(file_location)
  }


  fulldata = fulldata %>% group_by(y, roi, animal, treatment, video) %>%
    mutate(frame_id = row_number())

  pixel_bin = y_bin

  fulldata_grouped = fulldata %>% mutate(ygroup = ((y-1) %/% pixel_bin) + 1)  %>%
    group_by(treatment, animal, roi, ygroup) %>%
    mutate(max = max(y), min = min(y), npix = max-min +1, trace = cur_group_id()) %>%
    filter(npix == pixel_bin) %>%
    ungroup()

  if(isTRUE(raw))
  {
    return(fulldata_grouped)
  }

  fulldata_mean = summarise_import_file_bin(fulldata_grouped, file_location)


  return(fulldata_mean)

}



#' Title
#'
#' @param fulldata_grouped
#'
#' @return
#'
#' @noRd
#'
#' @examples
summarise_import_file_bin = function(fulldata_grouped, file_location)
{
  toreturn = fulldata_grouped %>% filter(!y_position_excluded) %>%
    group_by(`frame_id`, `video`, `animal`, `treatment`, `roi`, `ygroup`) %>%
    summarise(p_mean = mean(p_width, na.rm = TRUE), p_median = median(p_width, na.rm = TRUE)) %>%
    group_by(`animal`, `treatment`, `roi`, `ygroup`) %>%
    mutate(trace_id = cur_group_id())

  toreturn$filename = basename(file_location)

  return(toreturn)
}


