## ---------------------------
##
## Script name: generate_test_data.R
##
## Purpose of script: Generate and save all files necessary to run pilot study
##
## Author: Susan Vanderplas
##
## Date Created: 2022-12-13
##
## ---------------------------
##
## Notes:
##   Intended to be run from project directory. Stored in ./code/
##   Read CM values
##   Choose one set
##   Generate n_sets replicates
##   For each replicate, create a Type 1 and Type 3 plot
##   Save files: csv with data, ggplot, openSCAD script, STL file
##
## ---------------------------

n_sets <- 1
types <- c("Type1", "Type3")

library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


## Assemble the data based on random process
create_data <- function(value1, value2, random_process = "beta") {
  # Returns vector of values + 8 random items 
  values = c(value1, value2)
  #Random bars
  if(random_process == 'beta')(rand_bars = 100 * rbeta(n = 8, shape1 = 2, shape2 = 2))
  if(random_process == 'unif')(rand_bars = runif(n = 8, min = 0, max = 100))
  
  return(c(values, rand_bars))
}

## Arrange data according to type 1, 3
fix_data_type <- function(data, type, shuffle_first = F) {
  df = tibble(Height = data, 
              Identifier = rep(c("v1", "v2", "random"), c(1, 1, 8)))
  
  # One bar is always in position 2
  value_bars <- c(2, ifelse(type == 1, 3, 7))
  
  if (shuffle_first) {
    value_bars <- sample(value_bars, 2)
  }
  
  rdm_bars <- setdiff(1:10, value_bars) %>%
    sample(., 8)
  
  df %>%
    mutate(Order = c(value_bars, rdm_bars)) %>%
    arrange(Order) %>%
    mutate(Group = rep(LETTERS[1:2], each = 5),
           GroupOrder = rep(1:5, times = 2)) %>%
    mutate(IDchr = str_replace_all(Identifier, c("v1" = "\u25B2", "v2" = "\u25CF", "random" = ""))) %>%
    select(Order, Group, GroupOrder, Height, Identifier, IDchr)
}

source("code/Bar2D.R")

scad <- readLines("other/template.scad")

## Write the SCAD file
write_scad <- function(dat, filename) {
  
  # Generate SCAD file
  scad_data <- dat %>%
    # Add blank row for middle between two groups
    bind_rows(., data.frame(Order = 5.5, Group = "", GroupOrder = 1, Height = 0, Identifier = "", IDchr = "")) %>%
    arrange(Order)
  
  letters_str <- sprintf("letters = [%s];", paste0('"', scad_data$IDchr, '"', collapse = ", "))
  barsize_str <- sprintf("bar_size_z = [%s];", paste0(scad_data$Height, collapse = ", "))
  groups_str <- sprintf("groups = [%s];", paste0('"', scad_data$Group, '"', collapse = ", "))
  code_str <- sprintf("code = \"%s\";", filename)
  
  scad[2:5] <- c(code_str, barsize_str, letters_str, groups_str)
  
  writeLines(scad, con = paste0(filename, '.scad'))
}

write_data <- function(dat, filename) {
  
  # Make directory if it doesn't exist
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = T)
  }
  
  # Write csv file
  write.csv(dat, paste0(filename, ".csv"), row.names = F)
  
  # Write scad file
  write_scad(dat, filename)
  
  # Create chart
  plot_2d = Bar2D(dat)
  ggsave(plot = plot_2d, filename = paste0(filename, ".png"), 
         width = 4.125, height = 5, units = "in", dpi = 300)
         # width = 130*5, height = 100*5, units = "px")
  
  # Render STL (if openscad is available)
  if (system("which openscad") == 0)
    system(sprintf("openscad -o %s %s", paste0(filename, ".stl"), paste0(filename, ".scad")))
  
  return(plot_2d)
}
# 
# #Reading data and selecting random possible set
# 
# set.seed(21)
# data <- read.csv('CM_possible_values.csv') %>%
#   nest(data = -Set) %>%
#   arrange(Set) %>%
#   slice_sample(n = 1) %>%
#   crossing(rep = 1:n_sets, .) %>%
#   unnest(data) %>%
#   rename(ratio = X) %>%
#   mutate(bars = purrr::map2(Value1, Value2, create_data)) %>%
#   crossing(type = c(1, 3)) %>%
#   mutate(filename = sprintf("data/Set%02d/Ratio%04d/Type%d-Rep%02d", Set, ratio, type, rep)) %>%
#   mutate(data = purrr::map2(bars, type, fix_data_type)) %>%
#   mutate(plot_2d = purrr::map2(data, filename, write_data))
# 
# save(data, file = "data/Overall_Data_Frame.Rdata")
