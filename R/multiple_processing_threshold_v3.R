# library(doFuture)
# registerDoFuture()
#
# plan("multisession")
#
# library(imager)
# library(tidyverse)
# library(tools)
#
# starttime = Sys.time()
# threshold_apply(threshold = '0.579052239452482',roi_name = '[animal_10][treatment_yes][video_image112][roi_30]', video_path = 'P:/Width testing/image112.avi',
#                 radians = 1.37211044508047, xlength = 40, ylength = 154, xstart = 450, ystart = 722, fps = 1)
# Sys.time() - starttime

#' Threshold a video with pre-determined parameters
#'
#' Using pre-determined values this function generates ROI from a video. If parameters are not known, use select_roi()
#' This function is optimized to run in parallel, so should be relatively rapid. If running slowly, check the scratch disk is set correctly.
#'
#' @param threshold The threshold for the red channel. Range 0-1.
#' @param roi_name Name assigned to the region of interest
#' @param video_path Location of the video file to process
#' @param radians Degrees to rotate the image, in radians
#' @param xlength Number of x pixels in the ROI
#' @param ylength Number of y pixels in the ROI
#' @param xstart ROI starting x co-ordinate
#' @param ystart ROI starting y co-ordinate
#' @param image_list If pre-computed, a list of images to use rather than a video
#' @param fps Number of fps to process, this can be set lower for validation
#'
#'
#' @return Saves the quantified CSV and overlaid video in the same directory as the video
#'
#' @importFrom utils setTxtProgressBar  write.csv read.csv
#' @importFrom foreach `%dopar%` foreach
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr `%>%`
#' @importFrom imager as.cimg
#' @importFrom doFuture registerDoFuture
#' @importFrom tcltk tk_choose.dir
#' @importFrom dplyr mutate
#' @importFrom future availableCores
#' @importFrom stringr str_replace
#'
#'
#'
#' @export
#'
threshold_apply = function(threshold = 0.5, roi_name = "test", video_path = 'image826.avi',radians = 0.217604550320612,xlength = 60,ylength = 242,xstart = 696,ystart = 323, image_list = NULL, fps = NULL)
{

  starttime = Sys.time()

  #Make a full processing run in RAM

 output_folder = output_dir(dirname(video_path), use_default = TRUE)

# Crop video and save output

  scratch_dir(wipe_scratch = TRUE)

  scratch = scratch_dir(file_name = video_path)

  unlink(gsub("/$", "", scratch), recursive = TRUE, force = TRUE)
  dir.create(scratch)

  filter_string = paste("rotate = '",radians,":out_w=rotw(",radians,"):out_h=roth(",radians,"):c = red',",
                        "crop=",xlength,":",ylength,":",xstart,":",ystart,"",
                        sep = "")

  if(!is.null(fps))
  {
    filter_string = paste("fps = ",fps,",",filter_string, sep = "")
  }

  av::av_encode_video(video_path, paste(scratch, "/%03d_raw.png", sep = ""), vfilter = filter_string, codec = "png")

  cropped_file_list = list.files(scratch, full.names = TRUE, pattern = "\\_raw.png$")



  # Run analytics on each of the unpacked frames


  starttime = Sys.time()


  options(future.rng.onMisuse = "ignore")

  load.image(cropped_file_list[[1]]) %>% plot()

  message("Making segmentation")

  bundlesize = length(cropped_file_list)/as.numeric(availableCores())

  split_file_list = split(cropped_file_list, ceiling(seq_along(cropped_file_list)/bundlesize))

  # if(length(cropped_file_list>20))
  # {
  # plan("multisession", .skip = TRUE)
  # }else
  # {
  #   plan("sequential")
  # }

  foreach(locallist = split_file_list) %dopar%
  {

  for(current_frame  in locallist)
    {

       current_frame_threshold = threshold_image(current_frame, threshold)
       imager::save.image(current_frame_threshold[[1]], current_frame %>% str_replace("_raw.png", "_threshold.png"))
       current_frame_threshold[[2]]$filename = current_frame_threshold[[2]]$filename %>% str_replace("_raw", "")
       utils::write.csv(current_frame_threshold[[2]], current_frame %>% str_replace("_raw.png", "_width.csv"))


      current_frame_spread =  image_intensity_spread(current_frame)
      current_frame_spread$frame = current_frame_spread$frame %>% str_replace("_raw", "")
      utils::write.csv(current_frame_spread, current_frame %>% str_replace("_raw.png", "_profile.csv"))
    }
  }

  message("Copying results")


  output_file_base = paste(output_folder, "/", basename(file_path_sans_ext(video_path)), "_", roi_name, "_", sep = "")

  file_list = list.files(scratch, full.names = TRUE, pattern = "_raw.png$")
  av::av_encode_video(file_list, output = paste(output_file_base, "raw.avi", sep = ""),codec = "libx264", verbose = 24)

  file_list = list.files(scratch, full.names = TRUE, pattern = "_threshold.png$")
  load.image(file_list[[1]]) %>% plot()
  av::av_encode_video(file_list, output = paste(output_file_base, "threshold.avi", sep = ""),codec = "libx264", verbose = 24)


  file_list = list.files(scratch, full.names = TRUE, pattern = "_width.csv$")
  all_csv = lapply(file_list, read.csv) %>% bind_rows()
  all_csv = all_csv %>% mutate(X = NULL, y_position = y, y = NULL,
                               y_position_excluded = excluded, excluded = NULL,
                               frame = filename, filename = NULL,
                               roi_name = roi_name)
  write.csv(all_csv, paste(output_file_base, "width.csv"))

  file_list = list.files(scratch, full.names = TRUE, pattern = "_profile.csv$")
  all_csv = lapply(file_list, read.csv) %>% bind_rows()
  all_csv = all_csv %>% mutate(X = NULL,
                               x_position = x, x = NULL,
                               roi_name = roi_name)
  write.csv(all_csv, paste(output_file_base, "profile.csv", sep = ""))


  unlink(gsub("/$", "", scratch), recursive = TRUE, force = TRUE)

  print(Sys.time() - starttime)

}





