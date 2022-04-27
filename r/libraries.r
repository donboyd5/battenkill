
# base libraries
# library(magrittr)
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
library(glue)
library(slider) # part of tidyverse
library(fs)
library(vctrs)

# input
# library(vroom) # do not load vroom, it causes too many problems, but make sure it is installed
library(readxl)
library(openxlsx)
library(data.table)
library(archive)

# output
library(knitr)
library(kableExtra)

library(scales)
library(RColorBrewer)

# Boyd libraries
# library(bdata)
library(bggtools)
library(bmaps)
library(btools)