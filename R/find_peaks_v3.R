#' Find peaks in a vascular time series
#'
#' @param input_vector vector of values to analyze
#' @param kband K smoothing window to apply to the data
#' @param nups number of increases before and after the dataset to threshold on
#' @param min_change minimum size of change to be termed significant
#' @param min_dist Minimum distance between the minima
#' @param plot should a plot be returned along with the detected peaks
#'
#' @importFrom stats ksmooth time
#' @importFrom pracma findpeaks
#' @importFrom graphics grid points
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate rowwise cur_group_id
#' @importFrom ggplot2 aes geom_point scale_color_manual scale_colour_viridis_c guides guide_legend theme labs
#'
#'
#'
#' @return A vector of peaks detected
#'
#' @noRd
#'
#'
#' @examples
#' i = 1
#' # Test to come
#'
find_contraction_events = function(input_vector, kband = 20, nups = 10, min_change = 0.25, min_dist = 10, plot = FALSE, pixel_scale = 73, time_scale = 22.5)
{

  smooth_vector = ksmooth(time(1:length(input_vector)), input_vector, 'normal', bandwidth = kband)$y
  inverted_vector = 0-smooth_vector

  x = findpeaks(inverted_vector, nups = nups, ndowns = nups, zero = "+", minpeakdistance = min_dist)

  # Not run:
  # plot(smooth_vector, type="l", col="navy")
  # grid()
  # points(x[, 2], x[, 1], pch=20, col="maroon")
  #
  # return(x)

  colour_fill = c("grey30", "#C77CFF", "#7CAE00", "#00BFC4", "#F8766D")

  blankdata = data.frame(x = c(1:length(input_vector)), y = input_vector)
  blankgraphdata = ggplot() + geom_line(data = blankdata, aes(x = x/time_scale, y = y/pixel_scale, color = "No contractions found")) +
    labs(y = "Mean Diameter (mm)", x = "Time (s)", colour = "Tracking property") + theme(legend.title.align=0.5)

  blankgraph = list()
  blankgraph[[1]] = blankgraphdata
  blankgraph[2] = NA

  if(!isTRUE(nrow(x)>1))
  {
    return(blankgraph)
  }



  if(isTRUE(nrow(x)>=1))
  {
    events = data.frame(event_maxima = x[,2], event_start = x[,3],event_end = x[,4],type = "contract")
  }else
  {
    return(blankgraph)
  }

  events$start_value = smooth_vector[events$event_start]
  events$end_value = smooth_vector[events$event_end]
  events$max_value = smooth_vector[events$event_maxima]

  events = events %>% rowwise() %>%
                      mutate(
                             `baseline_change` = (`start_value`-`max_value`),
                             `event_duration` = `event_end` - `event_start`,
                             `cont_duration` = `event_maxima` - `event_start`,
                             `fill_duration` = `event_end` - `event_maxima`,
                             `event_gradient` = `baseline_change`/`event_duration`)

  raw_events = events

  #events = subset(events, !events$event_end == length(input_vector) & !events$event_start == 1)

  events = events %>% filter(abs(`baseline_change`)>min_change) %>% filter(`event_duration`>min_dist)

  if(nrow(events) ==0)
  {
    return(blankgraph)
  }


#
#     gg_color_hue <- function(n) {
#       hues = seq(15, 375, length = n + 1)
#       hcl(h = hues, l = 65, c = 100)[1:n]
#     }
#
    # "#00B0F6",


  function_plot = ggplot() + geom_line(aes(x = (c(1:length(smooth_vector)))/time_scale, y = input_vector/pixel_scale, color = "Raw data")) +
    geom_line(aes(x = c(1:length(smooth_vector))/time_scale, y = smooth_vector/pixel_scale, color = "Smoothed"), alpha = 0.8) +
    # geom_point(data = raw_events, aes(x = event_start, y = start_value, color = "Event Start")) +
    # geom_point(data = raw_events, aes(x = event_end, y = end_value, color = "Event end")) +
    # geom_point(data = raw_events, aes(x = `event_maxima`/time_scale, y = `max_value`/pixel_scale, color = "Events found and excluded"))+
    geom_point(data = events, aes(x = `event_maxima`/time_scale, y = `max_value`/pixel_scale, color = "Event minima"), size = 2) +
    geom_point(data = events, aes(x = `event_start`/time_scale, y = `start_value`/pixel_scale, color = "Event start"), size = 4) +
    geom_point(data = events, aes(x = `event_end`/time_scale, y = `end_value`/pixel_scale, color = "Event end"), size = 2) +
    scale_color_manual(values = c(colour_fill), breaks = c("Raw data", "Smoothed", "Event minima", "Event start", "Event end"))

  function_plot = function_plot + guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 0, 0, 0), shape = c(NA, NA, 19, 19, 19) ) ) )

  function_plot = function_plot + labs(y = "Mean Diameter (mm)", x = "Time (s)", colour = "Tracking property") + theme(legend.title.align=0.5)

  return(list(function_plot, events))

}


#' Quantify the vessel width over an entire ROI
#'
#' This function calculates the overall widths and contraction parameters for the vessel as a whole.
#'
#' @param widths_file A CSV file created by select_roi or threshold_vessel
#' @param pixel_scale The number of pixels per mm, can be calculated with
#' calibrate_pixel_size if unknown
#'
#' @return A list containing:
#' A graph showing the detected contraction events,
#' Details of each contraction event,
#' The mean and standard deviation of the calculated contraction physiological parameters,
#' The raw data used in the quantification process
#'
#'
#' @export
#'
#' @examples
#'
#' quantify_mean_width(vmeasur::example_vessel)
#'
quantify_mean_width = function(widths_file, pixel_scale = 73)
{

  if(is.data.frame(widths_file))
  {
    width_data = widths_file
  }
  else
  {
    width_data = import_file(widths_file)
  }

  width_data_summary = width_data %>% group_by(filename) %>% summarise(mean_diameter = mean(p_width, na.rm = TRUE), sd_diameter = sd(p_width, na.rm = TRUE))

  width_data_summary = width_data_summary %>% arrange(as.numeric(filename))

  events_returned =  find_contraction_events(width_data_summary$mean_diameter, kband = 20)

  events_returned[[3]] = calculate_physiological(events_returned[[2]])

  events_returned[[4]] = width_data

  return(events_returned)

}

