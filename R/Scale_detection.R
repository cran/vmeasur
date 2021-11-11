#' Calibrate the pixel size using a test image
#'
#' In order to calculate absolute densities from pixel sizes, the size of the
#' field captured by an operating microscope must be determined. This function
#' allows the user to select an image of a ruler captured under a microscope,
#' before automatically determining the scale.
#'
#' @param file_path The path to the image of a ruler to use for calibration. If
#' left blank, the user will be prompted to select the file.
#'
#' @return A graphical representation of the ruler and calibration process. The
#' number of pixels per mm will also be displayed.
#'
#' @importFrom  imager load.image imrotate grabLine grabRect
#' @importFrom  ggpubr ggarrange
#' @importFrom  ggplot2 ggplot geom_line geom_raster geom_vline theme labs aes scale_y_continuous scale_fill_gradient
#' @importFrom  dplyr mutate filter summarise lag
#' @importFrom  stats acf median
#' @importFrom  imager draw_text grayscale
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' file = paste(system.file(package = "vmeasur"), "extdata/mm_scale.jpg", sep = "/")
#' calibrate_pixel_size(file)
#'
#' }
#'
calibrate_pixel_size = function(file_path = tk_file.choose())
{

if(is.null(file_path))
{
  standard = file.choose()
}
else
{
  standard = file_path
}

image = load.image(standard)


image_display = pad(image, 100, "y", pos = -1) %>%
  draw_text(10, 10, "Draw a line along the edge of the calibration scale", "white", opacity = 1, fsize = 50)

result = grabLine(image_display)


rotation = tan(abs(result['y0'] - result['y1'])/ abs(result['x0'] - result['x1'])) /(2*pi)*360
rotated = imrotate(image, rotation) %>% pad( 100, "y", pos = -1) %>%
  draw_text(10, 10, "Select the section containg a graded scale", "white", opacity = 1, fsize = 50)

rectangle = grabRect(rotated)

rotated.df = rotated %>% grayscale %>% as.data.frame()

rotated_clean = rotated.df %>% filter(x<rectangle["x1"], y<rectangle["y1"]) %>%
  filter(x>rectangle["x0"], y>rectangle["y0"]) %>%
  mutate(x = x-min(x)+1, y = y-min(y)+1)

# rotated_clean %>% as.cimg() %>% plot()




grouped = rotated_clean %>% group_by(x) %>% summarise(luminance = mean(value))
ggplot(grouped) + geom_line(aes(x = x, y = luminance))


      output = as.vector(acf(grouped$luminance, lag.max = length(grouped$luminance))$acf)

      data.df = data.frame(lag = c(1:length(output)), reading = output)

      res = data.df %>% ungroup() %>% mutate(`previous` = lag(reading)) %>%
        mutate(switch = (`reading`<0 & `previous`>0)) %>%
        filter(`switch`) %>%
        mutate(`lagz` = `lag`-min(`lag`)) %>%
        mutate(`lagd` = `lag` - lag(`lag`)) %>%
        mutate(`residual` = `lagz` %% median(`lagd`, na.rm = TRUE))

      detected_pixels = median(res$lagd, na.rm = TRUE)

      rotated_to_plot = filter(rotated.df, y>100)

      p0 = ggplot(rotated_to_plot) + geom_raster(aes(x = `x`, y = `y`, fill = `value`)) +
        scale_y_continuous(trans=scales::reverse_trans()) +
        scale_fill_gradient(low="black",high="white") +
        theme(legend.position = "none") +
        labs(x = "X position (pixels)", y = "Y position (pixels)", title = "Calibration Image")

      p1 = ggplot(rotated_clean) + geom_raster(aes(x = `x`, y = `y`, fill = `value`)) +
        scale_y_continuous(trans=scales::reverse_trans()) +
        scale_fill_gradient(low="black",high="white") +
        geom_vline(xintercept = res$lag, color = "blue") +
        theme(legend.position = "none") +
        labs(x = "X position (pixels)", y = "Y position (pixels)", title = "Calibration area selected")

      p2 = ggplot(grouped) + geom_line(aes(x = `x`, y = `luminance`)) +
        geom_vline(xintercept = res$lag, color = "blue") +
        labs(x = "X position (pixels)", y = "Average Luminance", title = paste("Fitted scale: ", detected_pixels, "px"))

      plotreturn = ggarrange(p0,p1,p2, ncol = 1)

      cat(paste("Pixels between scale gradations:", detected_pixels))
      cat("\n")
      cat(paste("Gradation units/pixel:", 1/detected_pixels))
      cat("\n")

      return(plotreturn)


}



#' Choose a single file with tcltk
#'
#' @param path a path, if not specified
#'
#' @return a single filename
#'
#' @importFrom tcltk tk_choose.files
#'
#' @noRd
#'
#' @examples
#' ##
tk_file.choose = function(path = tk_choose.files())
{
  return(path[[1]])
}


