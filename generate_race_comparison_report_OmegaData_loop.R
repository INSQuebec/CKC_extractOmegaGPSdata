rm(list = ls())
library(tidyverse)
library(highcharter)
library(plotly)
library(readxl)
library(ggformula)

########### Define Params for the report ##########

race_ID <- c("0013", "0014", "0015", "0016") # Input the race_number to filter the databases, string
boat_class <- "WK2" # Enter the boat class of the race, for example "MK4", "WC2", "MC1", etc.
phase <- "Heats-Combined" # Name of the phase, for example "SF-1", "FA"

############# #####################################

# Load database
directory <- "C:/Users/alex/Dropbox/MTL Admos and Spin Files/Paris2024_GPSfiles/"
race_df_suffixe <- "Paris2024_Olympics"

summary_data <- read_csv(paste0(directory, "summaryDataframe_", race_df_suffixe, ".csv"))

splits_data <- read_csv(paste0(directory, "splitsDataframe_", race_df_suffixe, ".csv"))

for (race in 1:length(race_ID)) {
  
phase2 <- paste0("Heat-", race)

  race_summary_data <- filter(summary_data, race_number %in% race_ID[race]) %>%
  mutate(Date = as.Date(Date),
         race_rank = rank(total_time_sec), # recalculate rank in case more than one race chosen (ex. SF1 and SF2)
         race_winning_time = min(total_time_sec), # recalculate rank in case more than one race chosen (ex. SF1 and SF2)
         diff_to_winner_sec = total_time_sec - race_winning_time,
         diff_to_winner_percent = (total_time_sec - race_winning_time)/race_winning_time * 100,
         Country = make.unique(Country))

race_splits_data <-  filter(splits_data, race_number %in% race_ID[race]) %>% 
  group_by(split_distance) %>%  # Split
  mutate(position_to_leader_sec = round((min(split_cumtime)-split_cumtime),2),
         position_to_leader_percent = round((min(split_cumtime)-split_cumtime) / min(split_cumtime) *100,2),
         position_to_leader_meters = round(position_to_leader_sec*split_velocity,2),
         Country = make.unique(Country)) %>%
  ungroup()

parameters <- list(race_ID = race_ID[race],
                   boat_class = boat_class,
                   phase = phase2,
                   race_summary_data = race_summary_data,
                   race_splits_data = race_splits_data
)


# Render Rmd template
RendermyREPORT <- function(parameters = list()){
  rmarkdown::render(input = "race_comparison_report_OmegaData.Rmd",
                    output_file = paste0(directory, "Reports/", race_df_suffixe, "_",  boat_class, "_", phase2, ".html"),
                    output_format = "all",
                    params = parameters,
                    encoding = "UTF-8", clean = T)
}

RendermyREPORT(parameters = parameters) 
}

race_summary_data <- filter(summary_data, race_number %in% race_ID) %>%
  mutate(Date = as.Date(Date),
         race_rank = rank(total_time_sec), # recalculate rank in case more than one race chosen (ex. SF1 and SF2)
         race_winning_time = min(total_time_sec), # recalculate rank in case more than one race chosen (ex. SF1 and SF2)
         diff_to_winner_sec = total_time_sec - race_winning_time,
         diff_to_winner_percent = (total_time_sec - race_winning_time)/race_winning_time * 100,
         Country = make.unique(Country))

race_splits_data <-  filter(splits_data, race_number %in% race_ID) %>% 
  group_by(split_distance) %>%  # Split
  mutate(position_to_leader_sec = round((min(split_cumtime)-split_cumtime),2),
         position_to_leader_percent = round((min(split_cumtime)-split_cumtime) / min(split_cumtime) *100,2),
         position_to_leader_meters = round(position_to_leader_sec*split_velocity,2),
         Country = make.unique(Country)) %>%
  ungroup()

parameters <- list(race_ID = race_ID,
                   boat_class = boat_class,
                   phase = phase,
                   race_summary_data = race_summary_data,
                   race_splits_data = race_splits_data
)


# Render Rmd template
RendermyREPORT <- function(parameters = list()){
  rmarkdown::render(input = "race_comparison_report_OmegaData.Rmd",
                    output_file = paste0(directory, "Reports/", race_df_suffixe, "_",  boat_class, "_", phase, ".html"),
                    output_format = "all",
                    params = parameters,
                    encoding = "UTF-8", clean = T)
}

RendermyREPORT(parameters = parameters) 

