# Templates created to supplement the journal club discussion on alternatives to bar plots
# by Francisko de Moraes Rezende, 2020-11-18, francisko.rezende@gmail.com

# loading and installing packages that you might be missing ---------------

list_of_packages <- c("tidyverse", "janitor", "ggbeeswarm")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


library(tidyverse)
library(janitor) # for "cleaning" variable names
library(ggbeeswarm) # for plotting the nicer-looking (in my opinion) jitter

# independent data --------------------------------------------------------

ind_data <- tibble::tribble(
              ~Group.1, ~Group.2, ~Group.3, ~Group.4, ~Group.5,
                    5L,       7L,       9L,      42L,       2L,
                    3L,       3L,       7L,       2L,       0L,
                    6L,       9L,      10L,       5L,       3L,
                    8L,      10L,      12L,      55L,       5L,
                   10L,      33L,      14L,       9L,       7L,
                   13L,      15L,      17L,      12L,      10L,
                    1L,      18L,      20L,      15L,      13L,
                    4L,       6L,      40L,       3L,       1L,
                   18L,      20L,      22L,       NA,      15L,
                    4L,      30L,      35L,       NA,       1L,
                    7L,       NA,      42L,       NA,       4L,
                    9L,       NA,      13L,       NA,       6L,
                   14L,       NA,       NA,       NA,      11L,
                   15L,       NA,       NA,       NA,      12L,
                   17L,       NA,       NA,       NA,      14L
              ) %>%  # creates a data-frame with the data provided in the paper's supplementary material
  janitor::clean_names() %>%  # cleans the variable names (eg makes them lowercase and removes punctuation)
  tidyr::pivot_longer(cols = everything(), names_to = "groups") %>%  # converts the data-frame to long form
  dplyr::mutate(groups = factor(groups))  # converts groups into a factor

ggplot2::ggplot(ind_data, aes(x = groups, y = value)) +  # initiates the plot and links "groups" to the x axis and "value" to the y axis
  ggbeeswarm::geom_quasirandom(groupOnX = T) +  # plots the points adding some random noise to them to avoid overlap, specially useful in larger data sets
  ggplot2::stat_summary(geom = "crossbar",
               fun = "median")  # creates a bar at the median of each group accepts other statistics (eg mean) as well

# non-independent data ----------------------------------------------------

non_ind_data <- tibble::tribble(
                  ~Subject.IDs, ~Condition.1.Name, ~Condition.2.Name,
                            1L,                5L,                8L,
                            2L,                1L,                5L,
                            3L,                7L,               12L,
                            4L,                9L,               11L,
                            5L,                2L,                9L,
                            6L,                6L,                7L,
                            7L,                4L,                5L,
                            8L,               11L,               14L,
                            9L,               14L,               16L,
                           10L,               13L,               18L,
                           11L,               17L,               15L,
                           12L,               15L,               12L,
                           13L,                NA,                NA,
                           14L,                NA,                NA,
                           15L,                NA,                NA
                  ) %>%  # creates a data-frame with the data provided in the paper's supplementary material
   janitor::clean_names() %>%  # cleans the variable names (eg makes them lowercase and removes punctuation)
   dplyr::rename(id = subject_i_ds) %>%   # changes the name of "subject_i_ds" to "id"
   tidyr::pivot_longer(cols = -1, names_to = "condition") %>%   # converts the data-frame to long form
   dplyr::mutate(condition = factor(condition))  # converts groups into a factor

ggplot2::ggplot(non_ind_data, aes(x = condition, y = value, group = id)) +  # initiates the plot and links "condition" to the x axis, "value" to the y axis, and groups the data based on the information from "id"
  ggplot2::geom_point() +  # plots the points
  ggplot2::geom_line()  # plots the lines linking the points

non_ind_data_wide <- tibble::tribble(
  ~Subject.IDs, ~Condition.1.Name, ~Condition.2.Name,
  1L,                5L,                8L,
  2L,                1L,                5L,
  3L,                7L,               12L,
  4L,                9L,               11L,
  5L,                2L,                9L,
  6L,                6L,                7L,
  7L,                4L,                5L,
  8L,               11L,               14L,
  9L,               14L,               16L,
  10L,               13L,               18L,
  11L,               17L,               15L,
  12L,               15L,               12L,
  13L,                NA,                NA,
  14L,                NA,                NA,
  15L,                NA,                NA
) %>%  # creates a data-frame with the data provided in the paper's supplementary material
  janitor::clean_names() %>%  # cleans the variable names (eg makes them lowercase and removes punctuation)
  dplyr::rename(id = subject_i_ds,  # renames "subject_i_ds" to "id"
         condition_1 = condition_1_name,  # renames "condition_1_name" to "condition_1"
         condition_2 = condition_2_name) %>%  # renames "condition_2_name" to "condition_2"
  dplyr::mutate(difference = condition_2 - condition_1,  # creates a new variable called "difference" that has the difference between condition_2 and condition_1
         group = factor("a"))  # creates a variable called "group" that is a factor (a data type used to code discrete data) that has a single value (a) for all observations

ggplot2::ggplot(non_ind_data_wide, aes(x = group, y = difference)) + # initiates the plot and links "group" to the x axis and "difference" to the y axis
  ggplot2::geom_point() +  # plot the points
  ggplot2::stat_summary(geom = "crossbar",  # plots a bar
               fun = "median",  # at the median
               width = .1) +  # reduces the bar's width (not thickness), I chose this value by trial and error
  ggplot2::geom_abline(slope = 0, intercept = 0, color = "steelblue")  # plots an horizontal blue line at y = 0