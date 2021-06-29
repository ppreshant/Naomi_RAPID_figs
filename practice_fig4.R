# Teaching R plots to Naomi

# 29-6-21

# Loading libraries ----
library(tidyverse) 

# Preliminary stuff ----

# Set plot theme universally (for ggplot2) : format as classic, colours = Set1
ggplot2::theme_set(ggplot2::theme_classic()) # theme
scale_colour_discrete <- function(...) { # palette
  scale_colour_brewer(..., palette="Set1")
}


# Load data ----
raw_dat <- readxl::read_excel('data_to learn_figs.xlsx')

# data wrangling ----
long_dat <- raw_dat %>% 
  rename(Time = `Time (min)`) %>% # renaming the Time column
  pivot_longer(-Time, names_to = 'Sample_type', values_to = 'log_virus_removal')

group_segregated_dat <- long_dat %>% 
  mutate(numcol = str_extract(Sample_type, '[:digit:]*'), 
         lettrcol = str_extract(Sample_type, '[:alpha:]'))
# String help PDF - http://edrub.in/CheatSheets/cheatSheetStringr.pdf


# plotting ----
time_series_plot <- ggplot(data = long_dat, 
                           mapping = aes(x = Time, y = log_virus_removal, 
                                         colour = Sample_type)) + 
  geom_point() +
  geom_line()

time_series_plot


timepoints_avg_barplt <- ggplot(data = group_segregated_dat,
                                mapping = aes(x = Sample_type, y = log_virus_removal)) + 
  geom_bar(data = ~ filter(.x, Time == 90),
           alpha = .4,
           stat = 'identity') + # make barplot for the last timepoint == 90 minutes
  
  geom_point(mapping = aes(colour = Time), position = 'jitter') + # plot points for all the timepoints
  scale_color_viridis_b() + # make the colour legend discrete -- work in progress (make each break as the data points)
  # https://ggplot2.tidyverse.org/reference/scale_colour_continuous.html#details

  facet_grid(~ lettrcol, space = 'free', scales = 'free')  

  # scale_fill_discrete(palette = "Set1") # format colours properly. doesn't work

timepoints_avg_barplt

