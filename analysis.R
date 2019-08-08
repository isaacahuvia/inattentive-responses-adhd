rm(list = ls())
library(dplyr)
library(plyr)
library(yaml)
lookup <- yaml::read_yaml("P:\\ECHO Deliverables\\inattentive-responses-adhd\\file lookup.yaml")

load(file = lookup$analysis_ready)

df %>%
  dplyr::group_by(class) %>%
  dplyr::summarise_all(mean)