# Templates created to supplement the journal club discussion on alternatives to bar plots
# by Francisko de Moraes Rezende, 2020-11-18, francisko.rezende@gmail.com

library(tidyverse)
library(janitor)
library(ggbeeswarm)

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
              ) %>% 
  clean_names() %>% 
  pivot_longer(cols = everything(), names_to = "groups") %>% 
  mutate(groups = factor(groups))

ggplot(ind_data, aes(x = groups, y = value)) +
  geom_quasirandom(groupOnX = T) +
  stat_summary(geom = "crossbar",
               fun = "median")

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
                  ) %>% 
   clean_names() %>% 
   rename(id = subject_i_ds) %>% 
   pivot_longer(cols = -1, names_to = "condition") %>% 
   mutate(condition = factor(condition))

ggplot(non_ind_data, aes(x = condition, y = value, group = id)) +
  geom_point() +
  geom_line()


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
) %>%
  clean_names() %>%
  rename(id = subject_i_ds,
         condition_1 = condition_1_name,
         condition_2 = condition_2_name) %>% 
  mutate(difference = condition_2 - condition_1,
         group = factor("a"))

ggplot(non_ind_data_wide, aes(x = group, y = difference)) +
  geom_point() +
  stat_summary(geom = "crossbar",
               fun = "median",
               fun.max = "median",
               fun.min = "median",
               width = .1)
