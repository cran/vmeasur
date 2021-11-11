# Data documentation

globalVariables(c("end_value" ,"event_change" ,"event_duration" ,"event_end" ,"event_start", "excluded",
"i", "p_width", "start_value", "value" ,'y', "animal", "baseline_change", "event_maxima", "frame", "frame_id", "lagd", "lagz
luminance", "max_range", "max_value", "maxcont", "npix", "opts", "p_mean", "previous",
"reading", "site", "source_video", "trace_id", "treatment", "vessel", "x", "ygroup", "lagz", "luminance",
"filename", "mean_diameter", "roi", "video", "CA" ,"CD", "EDD" ,"EDD2", "EF", "ESD", "FD", "bin", "cont_id", "event_id",
"source_file" ,"variable" ,"xintercept",    "cont_group_id" ,"current_csv" ,"current_tree", "currentfile", "locallist",
"mean_width" ,"nvideo" ,"remaining" ,"roi_name" ,"segment" ,"tree" ,"y_group" ,"y_group_pos",
"y_position" ,"y_position_excluded"))


#' Example lymphatic width dataset
#'
#' A data set containing the widths of a test vessel in each frame of a video. Identical
#' in format to that produced by select_roi and threshold_vessel
#'
#' @format A data frame with 245,230 rows and 5 variables:
#' \describe{
#'   \item{X.1}{identification number of each row}
#'   \item{y}{y position in the image}
#'   \item{p_width}{width of the vessel at that position, in pixels}
#'   \item{excluded}{was that row excluded due to an air bubble}
#'   \item{filename}{which frame was the pixel row acquired from}
#'   ...
#' }
#'
#'
#' @source Collected for this package by Peter Russell (2021)
"example_vessel"
