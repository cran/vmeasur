
#' Quantify the content of an entire directory of sub-directories at once
#'
#' @param target_folder The folder to quantify the readings in
#'
#' @return A PDF file for each directory quantified, showing the quantification
#'
#' @importFrom stringr str_remove
#' @importFrom grDevices dev.off pdf
#' @importFrom stats runif
#' @importFrom ggplot2 geom_hline
#'
#'
#'
#' @export
quantify_directory = function(target_folder)
{

width_csv = list.files(target_folder, full.names = TRUE, recursive = FALSE, pattern = "_ width.csv")

width_csv_list = foreach(current_csv = width_csv, .combine = bind_rows) %do%
{
  import_data = read.csv(current_csv)
  import_data$roi_name = basename(current_csv) %>% str_remove("_ width.csv")

  return(import_data)
}

target_folder = output_dir(set = target_folder, use_default = TRUE)

gc()

pixel_bin = 100

width_csv_list$roi_name = str_to_upper(width_csv_list$roi_name)
width_csv_list$y_group = (((width_csv_list$y_position-1) %/% pixel_bin) + 1)

width_csv_list = width_csv_list %>% group_by(roi_name, y_group) %>% mutate(n = n()) %>%
  ungroup() %>%
  group_by(roi_name) %>%
  filter(n == max(n)) %>% mutate(n = NULL) %>% ungroup()

width_csv_list = width_csv_list %>% group_by(roi_name, y_position_excluded) %>%
  mutate(y_position_excluded = max(y_position_excluded)==1) %>%
  filter(y_position_excluded == 0) %>%
  ungroup()

all_ygroup = width_csv_list %>% select(roi_name) %>% distinct() %>%
                separate(roi_name, c("video", "roi"), sep = "_", extra = "merge", remove = FALSE) %>%
                left_join(width_csv_list) %>% mutate(roi_name = NULL) %>%
                group_by(roi, y_group, y_position) %>% mutate(frame = row_number()) %>%
                ungroup()


summary_ygroup = width_csv_list %>% group_by(y_group, frame, roi_name) %>%
                  summarise(mean_width = mean(p_width)) %>%
                  separate(roi_name, c("video", "roi"), sep = "_", extra = "merge", remove = FALSE) %>%
                  ungroup() %>%
                  group_by(roi, y_group) %>% arrange(video, frame) %>%
                  mutate(frame = row_number()) %>%
                  ungroup()

video_breaks = summary_ygroup %>% group_by(video) %>% summarise(maxframe = max(frame))

locations_plot = summary_ygroup %>% ggplot() + geom_line(aes(x = frame, y = mean_width, color = as.factor(y_group), linetype = video)) + facet_wrap(~roi) +
  geom_vline(xintercept = video_breaks$maxframe)


localdata = summary_ygroup %>% group_by(roi, y_group, video) %>% group_split()


pdf(paste(target_folder, "\\", basename(target_folder), "_contraction_detection.pdf", sep = ""), onefile = TRUE, height = 8.27, width = 11.69)



all_contractions = foreach(i = c(1:(length(localdata)))) %do%
{
  print(paste(i, "in", length(localdata)))
  result = find_contraction_events(input_vector = localdata[[i]]$"mean_width", kband = 30)

  print(result[[1]] + labs(title = unique(localdata[[i]]$roi_name)))

  if(is.na(result[[2]]))
  {return(NULL)} else
  {
  table = result[[2]]
  }

  start_time = min(localdata[[i]]$frame)
  table$event_maxima = table$event_maxima + start_time
  table$event_start = table$event_start + start_time
  table$event_end = table$event_end + start_time

  table$roi = unique(localdata[[i]]$roi)
  table$roi_name = unique(localdata[[i]]$roi_name)
  table$video = unique(localdata[[i]]$video)
  table$y_group = unique(localdata[[i]]$y_group)

  rm(result)
  gc()

  return(table)
}

dev.off()

contractions = bind_rows(all_contractions)
# rm(all_contractions)
# gc()


contractions = contractions %>% mutate(roi = str_to_upper(roi)) %>%
  separate(roi, c("remaining", "vessel"), sep = "_", remove = FALSE) %>%
  mutate(treatment = substr(remaining, 1,3), treatment = str_remove(treatment, "[0-9]")) %>%
  mutate(animal = substr(remaining, 3,5), animal = str_remove(animal, "[A-z]")) %>%
  mutate(site = str_remove(remaining, paste(treatment, animal, "S", sep = ""))) %>%
  mutate(remaining = NULL) %>% mutate(tree = substr(vessel,1,1), segment = substr(vessel, 3,length(vessel)))


ygroup_details = all_ygroup %>% select(video, roi) %>% distinct() %>%  mutate(roi = str_to_upper(roi)) %>%
  separate(roi, c("remaining", "vessel"), sep = "_", remove = FALSE) %>%
  mutate(treatment = substr(remaining, 1,3), treatment = str_remove(treatment, "[0-9]")) %>%
  mutate(animal = substr(remaining, 3,5), animal = str_remove(animal, "[A-z]")) %>%
  mutate(site = str_remove(remaining, paste(treatment, animal, "S", sep = ""))) %>%
  mutate(remaining = NULL) %>% mutate(tree = substr(vessel,1,1), segment = substr(vessel, 3,length(vessel)))


full_details = contractions %>%
  select(video, vessel, treatment, animal, site, tree, roi, segment) %>%
  distinct() %>% mutate(roi = str_to_upper(roi))

all_ygroup = all_ygroup %>% mutate(X = NULL, y_position_excluded = NULL)

all_ygroup_2 = all_ygroup %>% inner_join(ygroup_details)

all_ygroup_2 = all_ygroup_2 %>% group_by(animal, treatment, site, tree) %>%
  mutate(nvideo = length(unique(video)))


contractions = add_contraction_group(contractions)
contractions = calculate_physiological(contractions) #  %>% filter(CA>0.015)

contractions = all_ygroup_2 %>% ungroup() %>% select(tree, nvideo) %>% distinct() %>%
  inner_join(contractions, by = c('tree'))

# # print(ggplot() + geom_raster(aes(x = frame, y = y_position, fill = p_width), data = all_ygroup_labeled)+  scale_fill_viridis_c(option = "mako") +
#   geom_point(aes(x = event_maxima, y = ((y_group*pixel_bin)-(pixel_bin/2)), size = baseline_change/2, color = as.factor(as.numeric(as.factor(contraction_group)))), data = cont3) +
#   geom_errorbar(aes(xmin = event_start, xmax = event_end, y = y_group*pixel_bin-pixel_bin/2), data = cont3, color = "white") +
#   geom_vline(xintercept = video_breaks$maxframe, color = "grey") +
#   facet_grid(vars(segment), vars(tree)), space = "free_y", scales = "free_y", shrink = TRUE)

pdf(paste(target_folder, "\\", basename(target_folder), "_contraction_summary.pdf", sep = ""), onefile = TRUE, height = 8.27, width = 11.69)



print(locations_plot)

foreach(current_tree =  unique(all_ygroup_2$tree)) %do%
{
  print(current_tree)

  sub_all = all_ygroup_2 %>% select(p_width, y_position, frame, tree, segment, y_group) %>%
    subset(tree == current_tree) %>%
    mutate(y_position = y_position - (y_position %% 2) , frame = frame - (frame %%2)) %>%
    group_by(y_position, frame, segment, y_group) %>%
    summarise(p_width = mean(p_width, na.rm = TRUE)) %>%
    arrange(segment, y_position) %>%
    group_by(segment, y_position) %>%
    mutate(y_position = cur_group_id())

  ygroup_pos = sub_all %>% select(y_group, segment, y_position) %>% distinct() %>%
    group_by(y_group, segment) %>%
    summarise(y_group_pos = mean(y_position))

  sub_cont = contractions %>% subset(tree == current_tree)

  sub_cont = left_join(sub_cont, ygroup_pos)

  sub_cont$cont_group_id = as.factor(as.numeric(as.factor(sub_cont$contraction_group)))

  graphic = ggplot() + geom_raster(aes(y = frame, x = y_position, fill = p_width), data = sub_all)+  scale_fill_viridis_c(option = "mako") +
          geom_point(aes(y = event_maxima, x = y_group_pos, size = CA, color = cont_group_id), data = sub_cont) +
          geom_errorbar(aes(ymin = event_start*22.8, ymax = event_end*22.8, x = y_group_pos), data = sub_cont, color = "white") +
          geom_hline(yintercept = video_breaks$maxframe, color = "grey") +
          geom_line(aes(x = y_group_pos, y = event_maxima, color = cont_group_id), data = sub_cont) +
          scale_x_reverse() + coord_flip()

  print(graphic)

  rm(sub_all)
  rm(ygroup_pos)
  rm(graphic)

  gc()
  return(NULL)
}


dev.off()

write.csv(contractions, paste(target_folder, "\\", basename(target_folder), "_contraction.csv", sep = ""))
# write.csv(summary_ygroup, paste(target_folder, "\\", basename(target_folder), "_y_group_summary.csv", sep = ""))
# write.csv(all_ygroup_labeled, paste(target_folder, "\\", basename(target_folder), "_all_ygroup.csv", sep = ""))

rm(localdata, locations_plot, all_contractions)
rm(contractions, summary_ygroup, all_ygroup)
gc()

return(NULL)

}



















