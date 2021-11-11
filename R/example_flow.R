# blank = function()
# {
#
#   libraries = c("imager", "av", "tools", "ggplot2", "dplyr", "rlang", "foreach", "doSNOW", "pbmcapply", "devtools")
#   new.packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
#   if(length(new.packages)) install.packages(new.packages)
#   lapply(libraries, library, character.only = TRUE)
#
#   # SH29S2
#
#
#   setwd("//files.auckland.ac.nz/research/ressci202000061-PROM-study/vmeasur")
#   devtools::load_all()
#
#   scratch_dir("Q://")
#   setwd("//files.auckland.ac.nz/research/ressci202000061-PROM-study")
#
#   select_roi()
#
#
#
#   #  All ROIs listed below
#   # AP19S1.1_1.1
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image112.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image113.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image114.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image115.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image116.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image117.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image118.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#   threshold_apply( threshold = '0.711484593837535',roi_name = 'AP19S1.1_1.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image119.avi', radians = 0.24756263670402, xlength = 60, ylength = 126, xstart = 288, ystart = 429 )
#   #AP19S1.1_2.1
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image112.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image113.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image114.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image115.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image116.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image117.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image118.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   threshold_apply( threshold = '0.576470588235294',roi_name = 'AP19S1.1_2.1',video_path = '//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/AP19S1/image119.avi', radians = 0.678463009402465, xlength = 60, ylength = 154, xstart = 423, ystart = 634 )
#   #Ap19S1.1_2.2
#
#
#
#
#
#
#
#
# }
#
#
#
#
#
#
#
#
#
#
#
#
#
