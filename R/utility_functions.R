#' Calculate an automatically generated threshold as a number
#'
#' @param file_path path of the file to calculate the threshold from
#'
#' @importFrom magrittr %>%
#' @importFrom imager load.image imsplit
#'
#' @return The numerical threshold automatically calculated by imager
#'
#' @noRd
#'
#' @examples
#' i = 1
#'
# # calculate_auto_threshold(imager::boats)
#'
calculate_auto_threshold = function(file_path)
{
  im = imager::load.image(file_path)
  im.c = imager::imsplit(im,"c")[[1]]

  threshold_mask = as.data.frame(imager::as.cimg(imager::threshold(im.c)))
  im.c.df = as.data.frame(im.c)

  im.c.df$masked = threshold_mask$value

  auto_threshold = min(subset(im.c.df, im.c.df$masked>0)$value)

  return(auto_threshold)

}


#' Crop an image, starting at (x,y) co-ordinates
#'
#' Crop an image with the same data used by FFMPEG, allowing for the same
#' co-ordinates to be used with av. Note av is much faster in aggregate so
#' should be used for processing whole videos rather than generating and
#' cropping a series of images.
#'
#' @param img an imager image to crop
#' @param xstart starting x co-ordinate
#' @param ystart starting y co-ordinate
#' @param xlength x axes length
#' @param ylength y axes length
#'
#' @importFrom dplyr filter
#' @importFrom imager autocrop as.cimg
#'
#' @importFrom dplyr filter
#' @importFrom imager autocrop as.cimg
#'
#' @return a cropped image
#'
#' @noRd
#'
#' @examples
#' # TEST HERE
#'
crop_dims = function(img, xstart, ystart, xlength, ylength)
{
  img.df = as.data.frame(img)
  img.df.c = img.df %>% filter(img.df$x>xstart,img.df$y>ystart,img.df$x<xstart+xlength,img.df$y<ystart+ylength)
  toreturn = autocrop(as.cimg(img.df.c))
  return(toreturn)
}


#' Tile multiple images into a single image
#'
#' @param output_list list of images to turn into a matrix
#' @param width matrix width in images, default 2
#'
#' @importFrom imager imlist pad ci imappend
#'
#' @return an image, with each input arranged in a matrix
#'
#' @noRd
#'
#' @examples
#' imagelist = imager::imlist(imager::boats, imager::boats, imager::boats)
#' matrix = make_matrix(imagelist)
#' plot(matrix)
#'
make_matrix = function(output_list, width = 2)
{
  total_images = length(output_list)
  current_col = imlist()
  overall_matrix = imlist()

  current_width = 0

  for(i in 1:total_images)
  {
    currentimg = output_list[i][[1]]
    currentimg = pad(currentimg, 5, "xy")

    current_col = ci(current_col, currentimg)
    current_width = current_width + 1

    if(width == current_width || i == total_images)
    {
      current_col_image = imappend(current_col, "x")
      overall_matrix = ci(overall_matrix, current_col_image)
      current_col = imlist()
      current_width = 0
    }
  }


  returnimage = imappend(overall_matrix, "y")

  return(returnimage)
}



#' Set the output directory
#'
#' @param set The directory to set to
#' @param use_default Should the default value be used, or the system value
#' @param set_default Should the system value be updated
#'
#' @return The file path to export to
#'
#' @export
output_dir = function(set = NULL, use_default = FALSE, set_default = FALSE)
{

  if(isTRUE(set_default))
  {
    options("quantifyvessel-output_dir" = use_default)
  }

  if(isTRUE(use_default) && isTRUE(unlist(options("quantifyvessel-output_dir"))))
  {
    return(set)
  }

  if(!is.null(set))
  {
    options("quantifyvessel-scratch_dir"= set)
  }

  if(is.null(unlist(options("quantifyvessel-scratch_dir"))))
  {
    scratch = tempdir()
  }
  else
  {
    scratch = options("quantifyvessel-scratch_dir")[[1]]
  }

  if(scratch == tempdir())
  {
    print("Outputting to temporary directory")
  }

  return(scratch)

}


#' Set the scratch directory for vmeasur
#'
#' vmeasur uses av to unpack temporary image files, which are then stored for
#' further usage. This runs better if done to a high speed storage location such
#' as a ram drive. This function sets that directory, and provides other options
#' for specifying the structure of this temporary data.
#'
#' If not specified, the default R tempdir is used
#'
#' @param set new directory to set. If left blank, no directory change will occur
#' @param random_subfolder Should a random sub folder be created
#' @param file_name Specify the name of the directory
#' @param wipe_scratch Should the folder be cleared before use
#'
#' @return the current location of the scratch directory
#'
#' @importFrom stringr str_replace_all
#'
#' @export
#'
#' @examples
#' scratch_dir()
#' scratch_dir("R:")
#'
scratch_dir = function(set = NULL, random_subfolder = FALSE, file_name = FALSE, wipe_scratch = FALSE)
{

  if(!is.null(set))
  {
    options("quantifyvessel-scratch_dir"= set)
  }

  if(is.null(unlist(options("quantifyvessel-scratch_dir"))))
  {
    scratch = tempdir()
  }
  else
  {
    scratch = options("quantifyvessel-scratch_dir")[[1]]
  }

  if(!isFALSE(file_name))
  {
    file_changetime = file.mtime(file_name) %>% str_replace_all(":", " ")
    file_changetime = paste(basename(file_path_sans_ext(file_name))," ", file_changetime, sep = "")
    scratch = paste(scratch_dir(), "/", file_changetime, "/", sep = "")
    if(file.exists(scratch))
    {
    unlink(scratch, recursive = TRUE, force = TRUE)
    }
    dir.create(scratch)
  }

  if(isTRUE(random_subfolder))
  {
    runif(1, min = 0, max = 1000)
    randomstring = hash(runif(1, min = 0, max = 1000))
    scratch = paste(scratch, randomstring, "/", sep = "")
    dir.create(scratch)
  }

  if(isTRUE(wipe_scratch))
  {
    directory_list = list.dirs(options("quantifyvessel-scratch_dir")[[1]])
    unlink(directory_list, recursive = TRUE, force = TRUE)
  }

  return(scratch)
}

