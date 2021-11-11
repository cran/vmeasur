# contraction_file = file.choose()
# contraction_data = read_csv(contraction_file)
#
# contraction_data = events_returned[[2]]
#
#
# contraction = contraction_data
#
# calculate_physiological(contraction)
#
#
# pixel_mm = 73
# FPS = 22.8

#' Title
#'
#' @param cont Contraction data
#' @param pixel_mm Size of each mm in pixels
#' @param FPS frames per second of video
#' @param mean should a mean be returned, or the actual results
#'
#' @importFrom dplyr arrange mutate group_by summarize
#' @importFrom tidyr pivot_longer
#'
#' @importFrom magrittr `%>%`
#'
#' @return
#'
#' @noRd
#'
#' @examples
#' # Not exported
#'
calculate_physiological = function(cont, pixel_mm = 73, FPS = 22.8, mean = TRUE)
{

cont = cont %>% arrange(event_start) %>% mutate(type = NULL)

cont$event_id = c(1:nrow(cont))

cont = cont %>% mutate(EDD = start_value/73)
cont = cont %>% mutate(EDD2 = end_value/73)
cont = cont %>% mutate(ESD = max_value/73)

cont = cont %>% mutate(Nadir = min(ESD))
cont = cont %>% mutate(Peak = max(EDD, EDD2))


cont = cont %>% mutate(ED = event_duration/FPS, event_duration = NULL)
cont = cont %>% mutate(CD = (event_maxima-event_start)/FPS, event_start = event_start/FPS, fill_duration = NULL)
cont = cont %>% mutate(FD = (event_end - event_maxima)/FPS, event_end = event_end/FPS)

mean_event_duration = mean(cont$ED)
contraction_frequency = 1/(mean_event_duration/60)

cont = cont %>% mutate(CA = EDD-ESD, baseline_change = NULL)
cont = cont %>% mutate(EF = ((EDD^2-ESD^2)/EDD^2)*100)
cont = cont %>% mutate(FPF = (EF*contraction_frequency))


cont = cont %>% mutate(CS = CA/CD, event_gradient = NULL)
cont = cont %>% mutate(FS = (EDD2-ESD)/FD)
cont = cont %>% mutate(PRF = (EDD2-ESD)/CA*100)

# cont_mean = cont %>% pivot_longer(-event_id, names_to = "variable") %>%
#   group_by(variable) %>% summarise(mean = signif(mean(value),4), sd = signif(sd(value),4)) %>%
#   mutate(overall = paste(mean, " (", sd, ")", sep = ""))


# if(isTRUE(mean))
# {
#   return(cont_mean)
# }
# else
# {
#   return(cont)
# }

return(cont)

}



