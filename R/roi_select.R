#' Select a ROI from a video file
#'
#' This function provides a graphical tool to walk the user through selecting a ROI from an AVI video.
#'
#' @return Saves an annotated AVI and CSV file in the same directory as the video.
#' Will also output and copy the parameters used to create the video.
#'
#' @export
#'
#' @importFrom av av_video_images av_encode_video
#' @importFrom imager load.image grabLine draw_rect grabLine imrotate grabRect as.imlist as.cimg draw_rect
#' @importFrom rlang hash
#' @importFrom utils menu
#' @importFrom tools file_path_sans_ext
#' @importFrom rlang hash
#' @importFrom magrittr %>%
#' @importFrom crayon green
#' @importFrom svDialogs dlg_input
#'
#'
#' @examples
#'
#' \dontrun{
#'    select_roi()
#' }
#'
select_roi = function()
{


video_path = file.choose()
video_folder = dirname(video_path)
video_name = basename(file_path_sans_ext(video_path))

workingdir = paste(scratch_dir(),"/",hash(video_name), sep = "")

av_video_images(video_path, format = "png", destdir = workingdir, fps = 1)

raw = load.image(paste(workingdir,"/image_000001.png", sep = ""))

latchcrop = TRUE
latchthresh = TRUE

while(latchcrop || latchthresh){


while(latchcrop)
{

angleselect = grabLine(raw, output = "coord")

degrees = atan((angleselect["y0"]-angleselect["y1"])/(angleselect["x0"]-angleselect["x1"]))/(2*pi)*360
rotation = (90-abs(degrees))%%90 * (degrees/abs(degrees))
radians = rotation/360*(2*pi)
rotated = imrotate(raw, rotation)

areaselect = grabRect(rotated, output = "coord")

xstart = min(areaselect["x1"],areaselect["x0"])
ystart = min(areaselect["y1"],areaselect["y0"])

ylength = max(areaselect["y1"],areaselect["y0"])-ystart
xlength = max(areaselect["x1"],areaselect["x0"])-xstart

ylength = ylength + ylength %% 2
xlength = xlength + xlength %% 2

cropped = crop_dims(rotated, xstart, ystart, xlength, ylength)

plot(cropped)

status = menu(c("Change", "Accept", "Exit"))

if(status == 3)
{
  stop("Function exited")
} else if(status == 2)
{
  latchcrop = FALSE
  latchthresh = TRUE
}


}



filter_string = paste("rotate = '",radians,":out_w=rotw(",radians,"):out_h=roth(",radians,"):c = red',",
                      "crop=",xlength,":",ylength,":",xstart,":",ystart,"",
                      sep = "")

av_encode_video(video_path, paste(workingdir, "/test1w%03d_cropped.png", sep = ""), vfilter = filter_string, codec = "png")

file_list = list.files(workingdir, full.names = TRUE, pattern = "\\_cropped.png$")

prettylist = pretty(1:length(file_list),n = 6)
prettylist = subset(prettylist, prettylist<length(file_list))

file_list_s = file_list[prettylist]

overallthreshold = mean(unlist(lapply(file_list, calculate_auto_threshold)))
autothreshold = overallthreshold

latchthresh = TRUE

while(latchthresh){

plotmatrix = as.imlist(lapply(lapply(file_list_s, threshold_image, threshold = overallthreshold), function(x) { as.cimg(x[[1]])}))

make_matrix(plotmatrix, width = 10) %>% plot

print(paste("Default thresold:", autothreshold))
print(paste("Current threshold is:", overallthreshold))

print("Input any number less than 1 to change the threshold")

status = menu(c("Change", "Accept", "Re-crop", "Exit"))

if(status == 4)
{
  stop("Function exited")
} else if(status == 2)
{
  latchthresh = FALSE
}
else if(status == 3)
{
  latchthresh = FALSE
  latchcrop = TRUE
} else
{
  cat("\n New threshold")
  overallthreshold <- as.numeric(scan(what=character(),nmax=1,quiet=TRUE))
}


}

}
draw_rect(rotated,xstart,ystart,xstart+xlength, ystart+ylength,color="green",opacity=0.3) %>% plot()

print(paste("File name is:", video_name))

roi_name = make_filename(list = collect_filename(video = video_name))


video_path = gsub("\\\\", "/", video_path)

variables = paste("threshold = '",overallthreshold,"',",
      "roi_name = '",roi_name,"',",
      "video_path = '", video_path, "',",
      " radians = ", radians,",",
      " xlength = ", xlength,",",
      " ylength = ", ylength,",",
      " xstart = ", xstart,",",
      " ystart = ", ystart, sep ="")


function_string = paste("threshold_apply(",variables,")")

function_string = paste("\n", function_string, "\n ")

cat(crayon::green(function_string))


threshold_apply(threshold = overallthreshold, roi_name = roi_name, video_path = video_path, radians = radians, xlength = xlength, ylength = ylength, xstart = xstart, ystart = ystart, image_list = file_list)

unlink(workingdir, recursive = TRUE)

}


