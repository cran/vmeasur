#' Quantify multiple animals at the same time
#'
#' @param working_folder The folder containing the completed analysis
#'
#' @importFrom future plan multisession
#' @importFrom doFuture registerDoFuture
#' @importFrom progressr handlers handler_progress with_progress progressor
#' @importFrom foreach foreach %dopar%
#' @importFrom progressr progressor handlers
#' @importFrom tcltk tk_choose.dir
#'
#' @return saves a pdf of a series of quantified folders in the root directory
#'
#' @noRd
#'
#' @examples
#' # Used interactivley only
#'
quantify_folders = function(working_folder = tk_choose.dir())
{

folders_to_process = list.dirs(working_folder, recursive = FALSE)

# graph_folder(folders_to_process[1])


registerDoFuture()
plan(multisession)

handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
  )))

with_progress({
  p <- progressor(steps = length(folders_to_process))
  y <- foreach(x = folders_to_process) %dopar% {
    quantify_folder(x)
    p(message = sprintf("Completed %s", x), class = "sticky")
    gc()
  }
})

}




#' Quantify all ROI in a single folder
#'
#' @param folder The folder to quantify
#' @param kband The k value to use in smoothing the data
#'
#' @return Saves a PDF file of the analyasis generated
#'
#' @noRd
#'
#' @importFrom pdftools pdf_combine
#' @importFrom ggplot2 aes facet_wrap ggsave
#' @importFrom readr write_csv
#' @importFrom dplyr select distinct
#' @importFrom tcltk tk_choose.dir
#'
#'
#' @examples
#' # Select a folder and it will be quantified
#'
#' folder = choose.dir()
#'
quantify_folder = function(folder, kband = 40)
{


  var1 = import_folder_bin(folder)
  var1.1 = var1 %>% group_by(frame_id, video, animal, treatment, roi, ygroup, trace_id) %>% summarise(n = n())

  var2 = var1.1 %>% group_by(trace_id) %>% mutate(max_range = max(p_mean)-min(p_mean)) %>%
    group_by(roi) %>% mutate(maxcont = max_range == max(max_range))

  # Plot out that data
  ggplot(var2) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(ygroup), group = ygroup)) + facet_wrap(~ roi)

  # roi1 = var2 %>% filter(roi == "AP19S1.1_1.1") %>% filter(trace_id == 1)
  # roi1 = roi1 %>% group_by(trace_id, frame_id) %>% summarise(p_mean = mean(p_mean))
  # ggplot(roi1) + geom_line(aes(x = frame_id, y = p_mean, color = trace_id))

  var2.1 = var2

  animal_trt = paste("Animal", unique(var2.1$animal), "Treatment", paste(unique(var2.1$treatment)), sep = "_")[[1]]

  quant_folder = paste(scratch_dir(), "\\", hash(paste(folder, Sys.time())), sep = "")
  dir.create(quant_folder)

  folder_files = list.files(folder, recursive = TRUE, pattern = "\\_ width.csv$", full.names = TRUE)

  # for(one_region in folder_files)
  # {
  #   heat_plot = plot_heatmap(one_region)
  #
  #   overall_plot_heat_file = paste(quant_folder, "\\",animal_trt, file_path_sans_ext(basename(one_region)), "_heatmap.pdf", sep = "")
  #
  #   ggsave(overall_plot_heat_file, heat_plot, width = 297, height = 210, units = "mm")
  #
  # }



# Import a whole folder of data, binned into ygroups



overall_plot = ggplot(var2.1) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(ygroup), group = ygroup)) +
  labs(title = animal_trt) + facet_wrap(~ roi)

overall_plot_file = paste(quant_folder, "\\",animal_trt, "_overall.pdf", sep = "")
overall_data_file = paste(folder, "\\",animal_trt, "_overall.csv", sep = "")

ggsave(overall_plot_file, overall_plot, width = 297, height = 210, units = "mm")
write_csv(var2, overall_data_file)

for (trace in unique(var2.1$trace_id))
{
local_data = subset(var2.1, var2.1$trace_id == trace)

title_data = local_data %>% select(animal, treatment, roi, ygroup) %>% distinct()
title = paste(paste(colnames(title_data), title_data, sep = "_"), collapse = ",")

peak_data = find_contraction_events(input_vector = local_data$p_mean, min_dist = 100, kband = kband, min_change = 0.5, nups = 10)

write.csv(peak_data[2], paste(quant_folder,"\\", title, "_peaks.csv", sep = ""))

breaks = local_data %>% group_by(video) %>% summarise(max_frame = max(frame_id), min_frame = min(frame_id))
breaks$minima = min(local_data$p_mean)

if(!is.null(peak_data[[1]]))
{
 graph_output = peak_data[[1]] + labs(title = title) + geom_vline(aes(xintercept = breaks$max_frame))
}
else
{
  graph_output = ggplot(local_data) + geom_line(aes(x = frame_id, y = p_mean)) + labs(title = title) + geom_vline(xintercept = breaks$max_frame)
}

ggsave(paste(quant_folder,"\\", title, "_peaks.pdf", sep = ""), graph_output, width = 297, height = 210, units = "mm")

}

heatmap_list = list.files(quant_folder, pattern = "_heatmap.pdf", full.names = TRUE)
pdf_list = list.files(quant_folder, pattern = "_peaks.pdf", full.names = TRUE)


pdf_combine(c(heatmap_list,overall_plot_file, pdf_list), paste(folder, "\\", animal_trt,"_combined.pdf", sep = ""))


folder_files = list.files(quant_folder, recursive = TRUE, pattern = "\\_peaks.csv$", full.names = TRUE)

import_csv_with_source = function(csv_file)
{
  try({
  dataframe = read.csv(csv_file)
  dataframe$source_file = file_path_sans_ext(basename(csv_file))
  dataframe$X1 = NULL
  return(dataframe)
  })
  return(NULL)
}


# Import them all with lapply and combine with dplyr
applied = lapply(folder_files, import_csv_with_source)
contraction_data = dplyr::bind_rows(applied, .id = "file_id")

overall_contraction_file = paste(folder, "\\",animal_trt, "_contractions.csv", sep = "")

write.csv(contraction_data, overall_contraction_file)

unlink(quant_folder)


}



#' Plot a heatmap showing a vessel's contraction over time
#'
#' @param heatmap_file a vmeasur file of vessel diameters over time
#'
#' @return a ggplot2 heatmap
#'
#' @noRd
#'
#' @importFrom ggplot2 ggplot geom_raster aes scale_y_reverse scale_x_continuous labs scale_fill_gradient
#'
#' @examples
#'
#' #heatmap_file = file.choose()
#' #plot_heatmap(heatmap_file)
plot_heatmap = function(heatmap_file)
{

  if(is.data.frame(heatmap_file))
  {
    heatmap_data = heatmap_file
  } else {
  heatmap_data = read.csv(heatmap_file)
  }

  heatmap_data$frame_id = as.numeric(heatmap_data$filename)

  heatmap = (ggplot(heatmap_data) + geom_raster(aes(x = as.numeric(frame_id)/22.8, y = y/73, fill = p_width/73)) +
    scale_y_reverse(expand = c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    labs(y = "Vessel Position (mm from top of image)", x = "Time (s)", fill = "Vessel \n Diameter (mm)") +
    scale_fill_viridis_c()) + theme(legend.title.align=0.5)

  return(heatmap)
}

#' Plot a line graph of a vessel's change in diameter over time
#'
#' @param widths_file a vmeasur created file showing the widths of the vessel over time
#'
#' @return a ggplot2 of the vessel
#'
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 ggplot geom_line aes
#' @importFrom stats sd
#'
#' @noRd
#'
#' @examples
#'
#' #widths_file = file.choose()
#' #plot_heatmap(heatmap_file)
plot_line = function(widths_file)
{
  width_data = import_file(widths_file)

  width_data_summary = width_data %>% group_by(filename) %>% summarise(mean_diameter = mean(p_width, na.rm = TRUE), sd_diameter = sd(p_width, na.rm = TRUE))

  heatmap = ggplot(width_data_summary) + geom_line(aes(x = as.numeric(filename)/22.8, y = mean_diameter))

  return(heatmap)
}




#' Quantify the width of a vessel continuously along it's length
#'
#' Generate heat maps and line plots showing the changes in vessel diameter along
#' it's length
#'
#' @param widths_file A csv file created by select_roi or threshold_vessel. The
#' user will be prompted to select a file if this is not specified.
#'
#' @importFrom ggplot2 coord_flip scale_x_reverse geom_line aes theme labs scale_x_continuous geom_vline
#' @importFrom scales reverse_trans
#' @importFrom dplyr group_by summarize
#' @importFrom stats sd
#'
#' @return Two plots: A heat map of the vessel diameter at each position over
#' time and a plot showing the maximum change in diameter over time
#'
#' @export
#'
#' @examples
#'
#' quantify_width_position(vmeasur::example_vessel)
#'
quantify_width_position = function(widths_file = tk_file.choose())
{

  if(is.data.frame(widths_file))
  {
    width_data = widths_file
  }else{
    width_data = read.csv(widths_file)
  }



  # Fig A
  pa = plot_heatmap(width_data)

  # Fig B

  width_summ = width_data %>% group_by(y) %>% summarise(range = max(p_width) - min(p_width), sd = sd(p_width))

  vlines = data.frame(xintercept = c(1:11)*30/73, colour = "blue3")

  pb = ggplot(width_summ) + geom_line(aes(x = y/73, y = range/73, color = "Vessel width"))  +
    geom_vline(aes(xintercept = xintercept, color = "Regions selected"), data= vlines, show.legend = FALSE) + coord_flip() +
    labs(y = "Maximum change in diameter (mm)", x = "Vessel Position (mm from top of image)", color = "") + theme(legend.title.align=0.5) +
    scale_x_continuous(expand = c(0,0), trans = reverse_trans())

  return(list(pa, pb))

}


#' Quantify the contractility of a vessel in sections along it's length
#'
#' Quantify the physiological parameters in each section of the vessel along it's
#' length.
#'
#' @param widths_file A csv file created by select_roi or threshold_vessel. If
#' not specified, the user will be prompted to make a selection.
#'
#' @importFrom foreach `%do%`
#' @importFrom ggplot2 guides guide_colorbar scale_shape_manual geom_errorbar element_rect `.pt`
#' scale_color_continuous scale_fill_viridis_c scale_size_area scale_color_gradient scale_x_continuous scale_y_reverse theme
#' @importFrom dplyr arrange cur_data filter bind_rows group_by mutate
#' @importFrom grid grobTree linesGrob gpar
#' @importFrom rlang `%||%`
#' @importFrom scales alpha
#'
#' @return Graphs showing the contractility over time, contraction position and
#' amplitude detected, length of contraction and a heatmap overlay for verification
#' of the overall data.
#'
#' @export
#'
#' @examples
#'
#' # quantify_mean_width_sections(widths_file = vmeasur::example_vessel)
#'
quantify_mean_width_sections = function(widths_file = tk_file.choose())
{

  widths_binned = import_file_bin(widths_file, raw = TRUE)


  bins = unique(widths_binned$ygroup)


  result = foreach(bin = bins) %do%
    {
      local_widths = widths_binned %>% filter(ygroup == bin)
      mean_width = quantify_mean_width(local_widths)
      return(mean_width)
    }

  contraction_detection = sapply(result, "[", 2) %>% bind_rows(.id = "ygroup")
  contraction_phys = sapply(result, "[", 3) %>% bind_rows(.id = "ygroup")
  raw_data = sapply(result, "[", 4) %>% bind_rows(.id = "ygroup")


  widths_binned_summary = summarise_import_file_bin(widths_binned, "TEST")

  var2 = widths_binned_summary %>% group_by(trace_id) %>% mutate(max_range = max(p_mean)-min(p_mean)) %>%
    group_by(roi) %>% mutate(maxcont = max_range == max(max_range))

  # Plot out that data
  pc = ggplot(var2) + geom_line(aes(x = frame_id/22.8, y = p_mean/73, color = as.factor(paste(round(ygroup*30/73,2)-0.41, "to", round(ygroup*30/73,2), "mm")))) +
    labs(x = "Time (s)", y = "Mean Diameter (mm)", color = "Vessel Section") + theme(legend.title.align=0.5)

  # Generate contraction background data

  contraction = contraction_detection

  contraction$source_file = as.numeric(contraction$ygroup)


  pe = ggplot(contraction) + geom_point(aes(baseline_change/73, (source_file*30-15)/73, color = event_maxima/22.5), size = 2, alpha = 0.7) + scale_y_reverse() +
    labs(x = "Contraction Amplitude (mm)", y = "Distance from top of image (mm)", color = "Time at \n maximal \n contraction (s)") +
    scale_color_gradient(low = "purple", high = "darkorange") + guides(colour = guide_colorbar(reverse=T, min = 0, max = 30)) + theme(legend.title.align=0.5)


  cont_lab = contraction %>% group_by(source_file) %>% arrange(event_start) %>%
    mutate(cont_id = match(event_start, cur_data()[["event_start"]]))


  pf = ggplot(cont_lab) + geom_point(aes(baseline_change/73, event_duration/22.8, color = as.factor(paste(round(as.numeric(ygroup)*30/73,2)-0.41, "to", round(as.numeric(ygroup)*30/73,2), "mm")), shape = as.factor(cont_id)), size = 3) +
    labs(shape = "Contraction\n number", x = "Contraction Amplitude (mm)", y = "Contraction Duration (s)", color = "Vessel Section") +
    scale_shape_manual(values = c(1,0,3,10)) + theme(legend.title.align=0.5)


  draw_key_bracket <- function(data, params, size) {
    grobTree(
      linesGrob(c(0.15, 0.8),
                c(0.5, 0.5)),
      linesGrob(c(0.15, 0.15),
                c(0.15, 0.85)),
      linesGrob(c(0.8, 0.8),
                c(0.85, 0.15)),
      gp = gpar(
        col = data$colour %||% "grey20",
        fill = alpha(data$fill %||% "white", data$alpha),
        lwd = (data$size %||% 1) * .pt,
        lty = data$linetype %||% 1
      )
    )
  }




  heatmap = quantify_width_position(widths_file)[[1]]

  suppressMessages({
  pd = heatmap + geom_point(aes(x = event_maxima/22.8, y =  (source_file*30-15)/73, size = baseline_change/73), data = cont_lab, color = "white") +
    geom_errorbar(aes(xmin = event_start/22.8, xmax = event_end/22.8, y = (source_file*30-15)/73, linetype = "Contraction \n Duration"), color = "white", data = contraction, key_glyph = draw_key_bracket) +
    scale_y_reverse(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0))+
    labs(size = "Contraction \n amplitude (mm)", linetype = "", shape = "Contracton Number") +
    theme(legend.key = element_rect(fill = "black"))
  })

  pd = pd + scale_size_area(limits = c(0.01, 0.22), breaks = c(0.01, 0.08, 0.15, 0.22)) + theme(legend.title.align=0.5)

  return(list(pc, pe, pf, pd, cont_lab, contraction_phys))

}

