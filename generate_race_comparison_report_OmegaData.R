rm(list = ls())
library(tidyverse)
library(highcharter)
library(plotly)
library(readxl)
library(ggformula)


# Load database
directory <- "C:/Users/sgaudet/Downloads/gps_files_tokyo_2020_csp_210807/"
race_df_suffixe <- "Tokyo Olympics_2021-08-07"

summary_data <- read_csv(paste0(directory, "summaryDataframe_", race_df_suffixe, ".csv")) %>%
  mutate(Date = as.Date(Date),
         diff_to_winner_sec = total_time_sec - race_winning_time,
         diff_to_winner_percent = (total_time_sec - race_winning_time)/race_winning_time * 100) #%>% 
  # # Create a unique ID
  # unite("ID", Date, `Boat Class`, Distance, Country, Nickname, Event, Phase, `Piece ID`, sep = "_", na.rm = T, remove = F)

splits_data <- read_csv(paste0(directory, "splitsDataframe_", race_df_suffixe, ".csv")) %>% 
  # # Create a unique ID
  # unite("ID", Date, `Boat Class`, Distance, Country, Nickname, Event, Phase, `Piece ID`, sep = "_", na.rm = T, remove = F) %>% 
  group_by(race_number, split_distance) %>%  # Split
  mutate(position_to_leader_sec = round((min(split_cumtime)-split_cumtime),2),
         position_to_leader_percent = round((min(split_cumtime)-split_cumtime) / min(split_cumtime) *100,2),
         position_to_leader_meters = round(position_to_leader_sec*split_velocity,2)) %>%
  ungroup()

# Define Params for the report

race_ID <- "0118" # Input the race_number to filter the databases, string
boat_class <- "MK4" # Enter the boat class of the race, for example "MK4", "WC2", "MC1", etc.
phase <- "SF-2" # Name of the phase, for example "SF-1", "FA"

race_summary_data <- filter(summary_data, race_number == race_ID)
race_splits_data <-  filter(splits_data, race_number == race_ID)

parameters <- list(race_ID = race_ID,
                   boat_class = boat_class,
                   phase = phase,
                   race_summary_data = race_summary_data,
                   race_splits_data = race_splits_data
)


# Render Rmd template
RendermyREPORT <- function(parameters = list()){
  rmarkdown::render(input = "race_comparison_report_OmegaData.Rmd",
                    output_file = paste0(directory, race_df_suffixe, "_", race_ID, "_", boat_class, "_", phase, ".html"),
                    output_format = "all",
                    params = parameters,
                    encoding = "UTF-8", clean = T)
}

RendermyREPORT(parameters = parameters)
