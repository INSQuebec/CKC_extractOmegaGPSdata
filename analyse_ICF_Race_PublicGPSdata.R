# Load libraries
library(tidyverse)
library(lubridate)
library(hms)
library(DescTools)

# Helper functions --------------------------------------------------------

#' Function to extract Data from Omega GPS files
#' 
#' @param filename filepath of the csv file
#' 
#' 
extractOmegaGPSData <- function(filename){
  print(paste("processing file:", filename))
  race_number <- str_remove(filename, directory) %>% 
    str_remove(., ".csv")
  
  race_data <- read_delim(filename,
                          delim = ";", col_types = "nccnnccnnccnnccnnccnnccnnccnnccnnccnnccnn") %>% 
    drop_na(Distance) %>% 
    mutate_at(vars(contains("Stroke")), .funs = function(x) as.numeric(x)) %>% 
    mutate_at(vars(contains("Stroke")), .funs = function(x){
      case_when(x == 0 ~ NA, TRUE ~ x)
    }
    )
  
  
  if(!str_detect(last(names(race_data)), "Stroke")){
    race_data <- select(race_data, 1:last_col(1)) 
  }
  
  processed_race_data <- race_data %>% 
    pivot_longer(cols = -Distance, names_to = c(".value", "Lane"), names_pattern = "^(.*)(\\d+)") %>% 
    rename("Country" = "ShortName", "SR"= "Stroke") %>% 
    arrange(Lane, Distance) %>% 
    mutate(DPS = Speed / (SR/60),
           split10 = RoundTo(Distance, 10, "ceiling"),
           split50 = RoundTo(Distance, 50, "ceiling"),
           split100 = RoundTo(Distance, 100, "ceiling"),
           split250 = RoundTo(Distance, 250, "ceiling"),
           Time = paste0("00:", Time),
           Time = hms::parse_hms(Time),
           Speed = as.numeric(Speed),
           SR = as.numeric(SR),
           race_number = race_number) %>% 
    select(race_number, everything())
  
}

#' Function to extract Data from Omega GPS files
#' 
#' @param race_GPS_date dataframe with the data of a single race, taken from processed_race_data
#' 
#' 
getSplitsTable <- function(race_GPS_data){
  race_distance <- max(race_GPS_data$Distance, na.rm = T)
  
  if(race_distance == 200){
    splits <- group_by(processed_race_data, race_number, Lane, Country, split10) %>% 
      summarise(split_cumtime = max(Time),
                split_SR = round(mean(SR, na.rm = T),0),
                split_DPS = round(mean(DPS, na.rm = T),2)) %>% 
      mutate(split_time = round(as.numeric(split_cumtime)-lag(as.numeric(split_cumtime), default = 0),3),
             split_velocity = 10 / split_time,
             split_cumtime = round(as.numeric(split_cumtime),3)) %>% 
      rename(split_distance = split10) %>% 
      mutate_all(.funs = function(x){
        case_when(x %in% c(Inf, -Inf, "NaN") ~ NA, TRUE ~ x)}) %>% 
      select(race_number, Lane, Country, split_distance, split_time, split_cumtime, split_velocity, split_SR, split_DPS)
  } else {
    splits <- group_by(processed_race_data, race_number, Lane, Country, split50) %>% 
      summarise(split_cumtime = max(Time),
                split_SR = round(mean(SR, na.rm = T),0),
                split_DPS = round(mean(DPS, na.rm = T),2)) %>% 
      mutate(split_time = round(as.numeric(split_cumtime)-lag(as.numeric(split_cumtime), default = 0),3),
             split_velocity = 50 / split_time,
             split_cumtime = round(as.numeric(split_cumtime),3)) %>% 
      rename(split_distance = split50) %>% 
      mutate_all(.funs = function(x){
        case_when(x %in% c(Inf, -Inf, "NaN") ~ NA, TRUE ~ x)}) %>% 
      select(race_number, Lane, Country, split_distance, split_time, split_cumtime, split_velocity, split_SR, split_DPS)
  }
}


# Steps to extract data ------------------------------------------------------

# Load data 

# Input Competition Name and date (change manually before running script)
competition_name <- "Tokyo Olympics"
competition_date <- "2021-08-07"
directory <- "C:/Users/sgaudet/Downloads/gps_files_tokyo_2020_csp_210807/" 

files <- dir(directory, full.names = T)

# Process GPS files
processed_race_data <- lapply(files, extractOmegaGPSData) %>% 
  bind_rows()


# Summarise race data by 10m or 50m splits

splits_dataframe <- lapply(split(processed_race_data, f = processed_race_data$race_number), getSplitsTable) %>% 
  bind_rows()


# Summary table
summary_dataframe <- group_by(processed_race_data, race_number, Lane, Country) %>% 
  summarise(Distance = last(Distance),
            max_vel = max(Speed, na.rm = T),
            max_SR = max(SR, na.rm = T),
            avg_vel = round(last(Distance)/max(as.numeric(Time)),2),
            avg_SR = round(mean(SR, na.rm = T),0),
            avg_DPS = round(mean(DPS, na.rm = T),3),
            total_time = last(Time)) %>% 
  mutate_all(.funs = function(x){
    case_when(x %in% c(Inf, -Inf, "NaN") ~ NA, TRUE ~ x)}) %>% 
  mutate(total_time_sec = round(as.numeric(total_time),3))



write_csv(splits_dataframe, paste0(directory, "splitsDataframe_", competition_name, "_", competition_date, ".csv"))
write_csv(summary_dataframe, paste0(directory,"summaryDataframe_", competition_name, "_", competition_date, ".csv"))

