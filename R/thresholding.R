#' Threshold an image
#'
#' @param file_path Path to the image for quantification
#' @param threshold Threshold to set for the inverse channel. Range 0-1.
#' @param min_area The minimum number of pixels in an area to not ignore as noise
#'
#' @return A list containing
#'              1) the processed threshold image
#'              2) calculated widths
#'
#' @importFrom imager load.image imsplit clean parany
#' @importFrom dplyr summarise group_by
#' @importFrom magrittr `%>%`
#' @importFrom purrr discard
#'
#' @noRd
#'
#' @examples
#' # No examples
#'
threshold_image <- function(file_path, threshold, min_area = 100)
{

  # Load the image and split it into it's channels
  im <- imager::load.image(file_path)
  im.c = imager::imsplit(im,"c")[[1]]

  # plot(im.c)

  # Histogram plot if needed for debugging
  #hist(as.data.frame(im.c)$value)

  # Threshold the image

  px = im.c<threshold
  px = imager::clean(px,2)
  # plot(px)

  # If pixels have been located, split them into contiguous areas of greater than 100px in size

  if(sum(px)>0)
  {
    pxconn = imager::split_connected(px) %>% purrr::discard(~ sum(.) < min_area)
  } else
  {
    pxconn = list()
  }

  # If there is more than one area that passes, stick them together into a single mask

  if(length(pxconn)>1)
  {
    region =  pxconn %>% parany %>% plot
  } else
  {
    region = px
  }

  # Plot out the greyscale for debugging
  #hist(as.data.frame(grayscale(im))$value)


  # Find and expand the overlapped pixels
  maximal <- (im.c>0.99)
  maximal = imager::grow(maximal, 10)

  # Superimpose the two searches to find any overlap
  overlaid_regions = region+maximal

  # Combine the data into a visualization
  boundary_lines = imager::imappend(imager::imlist(imager::boundary(region)==2,imager::boundary(region), maximal), "c")
  imagelist = imager::imlist(imager::as.cimg(boundary_lines), im)
  overlap_image = imager::parmax(imagelist)
  # plot(overlap_image)

  # Calculate overlaid pixels
  overlapping = as.data.frame(overlaid_regions)
  yoverlap = overlapping %>% dplyr::group_by(`y`) %>% dplyr::summarise(bubble = max(`value`)==2)

  # Calculate vessel widths
  output = as.data.frame(as.cimg(region))
  widths = output %>% dplyr::group_by(`y`) %>% dplyr::summarise(p_width = sum(`value`))

  # Unify data and add metadata
  widths$excluded = yoverlap$bubble
  widths$filename = basename(tools::file_path_sans_ext(file_path))

  return(list(overlap_image, widths))

}


#' Apply a threshold to a single frame
#'
#' @param file_path path to the file to be used. If left blank, the user will be
#' prompted to make a selection
#' @param threshold The threshold to use
#' @param min_area Minimum area to recognize as a vessel. Any smaller items will
#' be ignored
#'
#' @return a data frame containing the widths of the vessel in each row of the image,
#' and if any rows were excluded due to overexposure
#'
#' @export
threshold_vessel <- function(file_path = tk_file.choose(), threshold, min_area = 100)
{
  threshold_image(file_path, threshold, min_area)
}


#' Title
#'
#' @param image_test The image to test
#'
#' @return
#'
#' @importFrom imager R
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr group_by mutate summarise ungroup
#'
#' @noRd
#'
#' @examples
image_intensity_spread = function(image_test)
{
  frameid = file_path_sans_ext(image_test) %>% basename()

  red_data = load.image(image_test) %>% R() %>% as.data.frame() %>% group_by(x) %>% mutate(value = 1-value)  %>%
    summarise(mean = mean(value)) %>% ungroup() %>% mutate(shiftedx = x - x[which.max(mean)]) %>% mutate(frame = frameid)

  return(red_data)
}



#' #' Threshold a video with pre-determined parameters
#' #'
#' #' Using pre-determined values this function generates ROI from a video. If parameters are not known, use select_roi()
#' #' This function is optimized to run in parallel, so should be relatively rapid. If running slowly, check the scratch disk is set correctly.
#' #'
#' #' @param threshold The threshold for the red channel. Range 0-1.
#' #' @param roi_name Name assigned to the region of interest
#' #' @param video_path Location of the video file to process
#' #' @param radians Degrees to rotate the image, in radians
#' #' @param xlength Number of x pixels in the ROI
#' #' @param ylength Number of y pixels in the ROI
#' #' @param xstart ROI starting x co-ordinate
#' #' @param ystart ROI starting y co-ordinate
#' #' @param image_list If pre-computed, a list of images to use rather than a video
#' #' @param output_folder The folder to save the results in, if required
#' #'
#' #'
#' #' @return Saves the quantified CSV and overlaid video in the same directory as the video
#' #'
#' #' @importFrom utils setTxtProgressBar  write.csv read.csv
#' #' @importFrom foreach `%dopar%` foreach
#' #' @importFrom tools file_path_sans_ext
#' #' @importFrom magrittr `%>%`
#' #' @importFrom imager as.cimg
#' #' @importFrom doFuture registerDoFuture
#' #' @importFrom tcltk tk_choose.dir
#' #'
#' #'
#' #' @export
#' #'
#' #'
#' threshold_apply = function(threshold = 0.5, roi_name = "test", video_path = 'image826.avi',radians = 0.217604550320612,xlength = 60,ylength = 242,xstart = 696,ystart = 323, image_list = NULL)
#' {
#'
#' output_folder = dirname(video_path)
#'
#'
#' video_folder = dirname(video_path)
#'
#' video_name = roi_name
#'
#'
#'
#' if(!is.null(image_list))
#' {
#'   file_list = image_list
#'   temp_path = dirname(file_list[1])
#' }
#' else
#' {
#'   temp_path = paste(scratch_dir(), "/", video_name, sep = "")
#'   dir.create(temp_path)
#'
#'   filter_string = paste("rotate = '",radians,":out_w=rotw(",radians,"):out_h=roth(",radians,"):c = red',",
#'                         "crop=",xlength,":",ylength,":",xstart,":",ystart,"",
#'                         sep = "")
#'
#'   av::av_encode_video(video_path, paste(temp_path, "/%03d.png", sep = ""), vfilter = filter_string, codec = "png")
#'
#'   file_list = list.files(temp_path, full.names = TRUE, pattern = "\\.png$")
#' }
#'
#' crop_file_list = file_list
#' av::av_encode_video(crop_file_list, output = paste(temp_path, "/crop.avi", sep = ""),codec = "libx264", verbose = 24)
#' file.copy(paste(temp_path, "/crop.avi", sep = ""), paste(output_folder,"/", basename(file_path_sans_ext(video_path)),"_",video_name,"_cropped.avi", sep = ""))
#'
#'
#'   registerDoFuture()
#'   plan(multisession)
#'
#'   options(future.rng.onMisuse = "ignore")
#'
#'
#' result <- foreach(i = file_list, .combine = rbind) %dopar%
#'   {
#'
#'     processed_image = threshold_image(i, threshold)
#'
#'     file_path = i
#'
#'     save_image_path = paste(tools::file_path_sans_ext(i), "_overlaid.png", sep = "")
#'     save_csv_path = paste(tools::file_path_sans_ext(i), "_overlaid.csv", sep = "")
#'
#'     imager::save.image(processed_image[[1]], save_image_path)
#'     utils::write.csv(processed_image[[2]], save_csv_path)
#'   }
#'
#'
#'
#' file_list = list.files(temp_path, full.names = TRUE, pattern = "\\overlaid.png$")
#' av::av_encode_video(file_list, output = paste(temp_path, "/overlaid.avi", sep = ""),codec = "libx264", verbose = 24)
#'
#'
#' file_list = list.files(temp_path, full.names = TRUE, pattern = "\\overlaid.csv$")
#' ldf <- lapply(file_list , read.csv)
#' df.final <- do.call("rbind", ldf)
#'
#' write.csv(df.final, paste(temp_path, "/widths.csv", sep = ""))
#'
#' # dir.create(paste(output_folder,"/", basename(file_path_sans_ext(video_path)), "_processing/", sep = ""))
#'
#' file.copy(paste(temp_path, "/widths.csv", sep = ""), paste(output_folder,"/", basename(file_path_sans_ext(video_path)),"_", video_name,"_widths.csv", sep = ""))
#'
#' file.copy(paste(temp_path, "/overlaid.avi", sep = ""), paste(output_folder,"/", basename(file_path_sans_ext(video_path)),"_",video_name,"_overlaid.avi", sep = ""))
#'
#' unlink(temp_path, recursive = TRUE)
#'
#' }
