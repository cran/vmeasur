

#' Title
#'
#' @param contraction_data
#'
#' @return
#'
#' @importFrom foreach foreach %do%
#' @importFrom dplyr mutate group_split inner_join left_join n
#' @importFrom stringr str_to_upper
#' @importFrom stats runif
#'
#' @noRd
#'
#' @examples
add_contraction_group = function(contraction_data)
{

  cd2 = contraction_data %>% group_by(tree) %>% group_split()

  processed = foreach(w4 = cd2, .combine = bind_rows) %do%
    {

      w4$plainid = c(1:nrow(w4))

      close = foreach(row = c(1:nrow(w4))) %do%
        {
          currentrow = w4[row,]
          distances = abs(w4$event_maxima - currentrow$event_maxima)<20
          closerows = subset(w4, distances)
          in_spec = paste(closerows$plainid, collapse = ",")

          return(in_spec)
        }

      w4$close = unlist(close)


      w4$close = overlap_ranges(w4$close)

      w4 = w4 %>% mutate(contraction_group = close, close = NULL)

      return(w4)
    }

  return(processed)

}


# localdata = contractions %>% group_by(treatment, animal, tree) %>% group_split()
#
# grouped = add_contraction_group(localdata[[3]])
#
# heat_data = all_ygroup_labeled %>% filter(tree == "3")
#
#
# ggplot(grouped) + geom_line(aes(y = event_maxima,
#                            x = paste(segment,y_group, sep = "-"),
#                            group = contraction_group,
#                            color = as.factor(contraction_group))) +
#   geom_point(aes(y = event_maxima,
#                 x = paste(segment,y_group, sep = "-"),
#                 group = contraction_group,
#                 color = as.factor(contraction_group)))+
#   coord_flip()
#
#
# foreach(currentdata = localdata) %do%
#   {
#     current_contractions = contractions %>% filter(roi == unique(currentdata$roi))
#
#     heatmap = ggplot() + geom_raster(aes(x = frame, y = y_position, fill = p_width), data = currentdata)+  scale_fill_viridis_c(option = "mako") +
#       geom_point(aes(x = event_maxima, y = ((y_group*pixel_bin)-(pixel_bin/2)), size = baseline_change), data = current_contractions, color = "white") +
#       geom_errorbar(aes(xmin = event_start, xmax = event_end, y = y_group*pixel_bin-pixel_bin/2), data = current_contractions, color = "white") +
#       geom_vline(xintercept = video_breaks$maxframe, color = "grey")
#
#     print(heatmap)
#   }
#
#
#
# w10 = add_contraction_group(current_contractions)
#
# ggplot(w10) + geom_point(aes(x = event_maxima,
#                             y = paste(vessel, y_group, sep = "-"),
#                             color = as.factor(`close`)))
#
# ggplot(w4) + geom_line(aes(y = event_maxima,
#                             x = paste(vessel, y_group, sep = "-"),
#                             group = close,
#                             color = as.factor(`close`)))
#
#
# v1 = "1,2,3,4,5"
# v2 = "4,5,6"
#
# v2 = "60,61,62,63,64,65,66,67,68,69,70,71,72"
# v1 = "61,62,63,64,65,66,67,68,69,70,71,72"
#
# combine_lists(v1, v2)

#' Title
#'
#' @param v1
#' @param v2
#'
#' @return
#'
#' @noRd
#'
#' @examples
combine_lists = function(v1, v2)
{
  v1v = unlist(strsplit(v1, ","))
  v2v = unlist(strsplit(v2, ","))

  if(sum(v1v %in% v2v, v2v %in% v1v)>1)
  {
    return(paste(sort(as.numeric(unique(c(v1v, v2v)))), collapse = ","))
  } else
    return(v1)
}



#' Title
#'
#' @param v1
#' @param bigvector
#'
#' @return
#'
#' @noRd
#'
#' @examples
combine_all_lists = function(v1, bigvector)
{
  for(i in bigvector)
  {
    v1 = combine_lists(v1,i)
    bigvector = c(v1, bigvector)
  }

  return(v1)
}

overlap_ranges = function(string_vector)
{

  unique_string_vector = unique(string_vector)

  result = foreach(row = string_vector) %do%
    {
      parse1 = combine_all_lists(row, unique_string_vector)
      return(parse1)
    }

  unique_result = unique(result)

  result = foreach(row = result) %do%
    {
      parse1 = combine_all_lists(row, unique_result)
      return(parse1)
    }

  return(unlist(result))
}


# w5 = w4 %>% rowwise() %>%
#   mutate(close = combine_all_lists(close, w4$close))
#
# unique(w5$close)
#
#
#
# w6 = w5 %>% separate_rows(close) %>%
#   mutate(close = as.numeric(close), plainid = as.numeric(plainid)) %>%
#   rowwise() %>%
#   mutate(low = min(close, plainid), high = max(close,plainid)) %>%
#   mutate(plainid = NULL, close = NULL) %>% distinct()
#
# w6 = w6 %>% group_by(low) %>% summarise(high = max(high)) %>%
#   group_by(high) %>% summarise(low = min(low))


