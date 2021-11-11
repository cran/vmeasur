# function()
# {
#
#
#   summary.df %>% select(site, animal, treatment, source_video) %>%
#     distinct() %>%
#     group_by(site, animal, treatment) %>%
#     summarise(n = n())
#
#   unique(summary.df$site)
#
#
# # Y banded ROI
#
# roi_list = summary.df %>% select(site, animal, treatment, vessel) %>% distinct()
#
#
# for(j in c(1:nrow(roi_list)))
# {
#   try({
#
#   roi = roi_list[j,]
#   print(paste(j, "of",nrow(roi_list), "-",ceiling(j/nrow(roi_list)*100),"%"))
#
# # Find peaks locally
#
# fulldata_mean_mini = summary.df %>% filter(site == roi$site, animal == roi$animal, vessel == roi$vessel, treatment == roi$treatment)
#
#
# # video_shift = (fulldata_mean_mini %>% group_by(source_video) %>% summarise(video_shift = max(frame_id)))$video_shift
# #
# # ggplot(fulldata_mean_mini) +
# #   geom_line(aes(x = frame_id, y = p_mean, color = paste(vessel, "/", ygroup))) +
# #   geom_vline(xintercept = video_shift) +
# #   labs(title = (paste(i, ")" , roi$treatment, roi$animal, " S",roi$site, sep = "")))
#
# fulldata_mean_mini = fulldata_mean_mini %>% group_by(vessel, source_video) %>% mutate(p_mean = ksmooth(time(c(1:length(p_mean))),p_mean, "normal", 30)$y) %>% ungroup()
#
# trace_table = fulldata_mean_mini %>% select(site, animal, vessel, treatment, source_video, ygroup) %>% distinct()
# trace_table$id = c(1:nrow(trace_table))
#
# res = list()
# stat = list()
#
# for(i in c(1:nrow(trace_table)))
# {
#   #print(paste(j,i))
#   filters = trace_table[i,]
#   local_data = fulldata_mean_mini %>% filter(site == filters$site, animal == filters$animal, vessel == filters$vessel, treatment == filters$treatment, source_video == filters$source_video, ygroup == filters$ygroup)
#   output = find_contraction_events(input_vector = local_data$p_mean, min_dist = 30, kband = 30, min_change = 1, nups = 10)
#   if(!is.null(output))
#   {
#     output$id = i
#     res[[i]] = output
#   }
#
#
#   nullo = function(vector)
#   {
#     if(is.null(vector))
#     {
#       return (0)
#     }
#     else
#     {
#     return(vector)
#     }
#   }
#
#   output$EF = ((output$start_value)^2 - (output$max_value)*2)/((output$start_value)^2)
#
#   dat = list()
#
#
#
#   dat["max_EF"] = max(nullo(output$baseline_change))
#   dat["mean_EF"] = mean(nullo(output$baseline_change))
#   dat["median_EF"] = median(nullo(output$baseline_change))
#   dat["sum_EF"] = sum(nullo(output$baseline_change))
#   dat["sd_EF"] = sd(nullo(output$baseline_change))
#
#   dat["n_contraction"] = length(nullo(output$event_maxima))
#
#   dat["FPF"] = dat[["n_contraction"]] * dat[["mean_EF"]]
#
#   dat["source_video"] = unique(local_data$source_video)
#   dat["site"] = unique(local_data$site)
#   dat["animal"] = unique(local_data$animal)
#   dat["treatment"] = unique(local_data$treatment)
#   dat["vessel"] = unique(local_data$vessel)
#   dat["ygroup"] = unique(local_data$ygroup)
#
#
#
#   dat["max_magnitude"] = max(nullo(output$baseline_change))
#   dat["mean_magnitude"] = mean(nullo(output$baseline_change))
#   dat["median_magnitude"] = median(nullo(output$baseline_change))
#   dat["sum_magnitude"] = sum(nullo(output$baseline_change))
#   dat["sd_magnitude"] = sd(nullo(output$baseline_change))
#
#   dat["max_ef"] = sd(nullo(output$baseline_change/output$start_value))
#   dat["mean_ef"] = sd(nullo(output$baseline_change/output$start_value))
#   dat["median_ef"] = sd(nullo(output$baseline_change/output$start_value))
#
#   dat["max_duration"] = max(nullo(output$event_duration))
#   dat["mean_duration"] = mean(nullo(output$event_duration))
#   dat["median_duration"] = median(nullo(output$event_duration))
#   dat["sum_duration"] = sum(nullo(output$event_duration))
#   dat["sd_duration"] = sd(nullo(output$event_duration))
#
#   dat["max_cont_duration"] = max(nullo(output$cont_duration))
#   dat["mean_cont_duration"] = mean(nullo(output$cont_duration))
#   dat["median_cont_duration"] = median(nullo(output$cont_duration))
#   dat["sum_cont_duration"] = sum(nullo(output$cont_duration))
#   dat["sd_cont_duration"] = sd(nullo(output$cont_duration))
#
#   dat["max_fill_duration"] = max(nullo(output$fill_duration))
#   dat["mean_fill_duration"] = mean(nullo(output$fill_duration))
#   dat["median_fill_duration"] = median(nullo(output$fill_duration))
#   dat["sum_fill_duration"] = sum(nullo(output$fill_duration))
#   dat["sd_fill_duration"] = sd(nullo(output$fill_duration))
#
#   dat["max_gradient"] = max(nullo(output$event_gradient))
#   dat["mean_gradient"] = mean(nullo(output$event_gradient))
#   dat["median_gradient"] = median(nullo(output$event_gradient))
#   dat["sum_gradient"] = sum(nullo(output$event_gradient))
#   dat["sd_gradient"] = sd(nullo(output$event_gradient))
#
#   dat["mean_width"] = max(local_data$p_mean)
#   dat["median_width"] = median(local_data$p_mean)
#   dat["max_width"] = max(local_data$p_mean)
#
#   local_stat = as.data.frame.list(dat)
#   stat[[i]] = local_stat
#
# }
#
# res.df = bind_rows(res)
# stat.df = bind_rows(stat)
#
# vessid = (paste(roi$treatment, roi$animal, "_S",roi$site, "_V", roi$vessel, sep = ""))
# file_root = paste("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/quant2/",vessid, sep = "")
#
# write.csv(stat.df, paste(file_root, "_stat.csv", sep = ""))
#
#
# if(nrow(res.df)==0)
# {
#   stop("Done, no_cont")
# }
#
# stop("Done")
#
# full_trace_table = res.df %>% left_join(trace_table, by = c("id"))
#
# full_trace_contractions = full_trace_table %>% group_by(site, animal, vessel, treatment, source_video, ygroup) %>%
#       mutate(contractions = n())
#
# combined_full_table = fulldata_mean_mini %>% left_join(full_trace_contractions) %>% mutate(event_maxima_id = event_maxima - frame + frame_id)
#
#
# file_root = paste("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/quant2/",vessid, sep = "")
#
# ggplot(combined_full_table) +
#   geom_line(aes(x = frame_id, y = p_mean, color = as.character(contractions), group = source_video))+
#   geom_vline(xintercept = video_shift) +
#   geom_point(aes(x = event_maxima_id, y = max_value), color = "blue") +
#   labs(title = vessid) +
#   facet_wrap(~paste(vessel, "/", ygroup))
#
# ggsave(file=paste(file_root, ".pdf", sep = ""), width = 297, height = 210, units = "mm")
#
#
# full_table_summary = combined_full_table %>% group_by(source_video, site, animal, treatment, vessel) %>%
#   summarise(max_cont = max(contractions,0, na.rm = TRUE),
#             median_cont = median(contractions),
#             mean_cont = mean(contractions),
#             sd_cont = sd(contractions),
#             mean_magnitude = mean(baseline_change),
#             median_magnitude = median(baseline_change),
#             sd_magnitude = sd(baseline_change),
#             max_magnitude = max(baseline_change,0),
#             sum_magnitude = sum(baseline_change, na.rm = TRUE))
#
# write.csv(full_table_summary, paste(file_root, "_summary.csv", sep = ""))
# })
# }
#
#
# csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/quant2/",
#                        recursive = TRUE, full.names = TRUE)
#
# summary_csv = subset(csv_files,(str_count(csv_files, "stat.csv")>0))
#
# summary_data  = lapply(summary_csv, read.csv, as.is = TRUE)
#
# summary_cont.df = summary_data[[1]]
#
# for(i in c(2:length(summary_data)))
# {
#   print(i)
#   summary_cont.df = rbind(summary_cont.df, summary_data[[i]])
# }
#
#
#
# }
