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
  race_number <- str_remove(filename, paste0(directory, ".*/")) %>% 
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




#' Function to perform time corrections
#' 
#' @param race_GPS_data dataframe with the data of a single race, taken from processed_race_data
#' 
#' 
#' 
correctOmegaGPSData <- function(race_GPS_data){
  
  corrected_race_data <- race_GPS_data %>% 
  group_by(Lane, Distance) %>% 
    
  lag(1:5, default =0)
  mutate(volume = c(0,diff(cumVol)))
  mutate(volume = cumVol - lag(cumVol, default = cumVol[1], order_by=period))
  
  race_data$Wind_direction <- NA_real_
  race_data$Wind_speed <- NA_real_
  race_data$GMT_sec <- NA_real_
  race_data$AFT_sec <- NA_real_
  race_data$Total_cor <- NA_real_
  race_data$Corrected_time_sec <- NA_real_
  
  
  race_data$Split_time <- NA_real_
  race_data$Split_time_sec <- NA_real_
  race_data$Split_wind_direction <- NA_real_
  race_data$Split_wind_speed <- NA_real_
  race_data$Total_split_cor <- NA_real_
  race_data$Corrected_split_time_sec <- NA_real_
  
  
  Lanes <- unique(race_data$Lane)
  Splits <- unique(race_data$Split_distance)
  
  
  for (Lane in 1:length(Lanes)) {
    if (!is.na(unique(race_data$Time[race_data$Lane == Lanes[Lane]]))) {
      time <- unique(race_data$Time[race_data$Lane == Lanes[Lane]])
      time_sec <- unique(race_data$Time_sec[race_data$Lane == Lanes[Lane]])
      distance <- unique(race_data$Distance[race_data$Lane == Lanes[Lane]])
      discipline <- unique(race_data$Discipline[race_data$Lane == Lanes[Lane]])
      race_start_time <- unique(race_data$Start_time[race_data$Lane == Lanes[Lane]])
      race_finish_time <- as_hms(race_start_time + time)
      avg_velocity <- unique(race_data$Speed[race_data$Lane == Lanes[Lane]])
      water_temperature <- unique(race_data$Water_temp[race_data$Lane == Lanes[Lane]])
      
      if (race_start_time >= wind_data$Time_adj[1] & race_finish_time <= wind_data$Time_adj[nrow(wind_data)]){
        
        race_wind <- wind_data %>% filter(Time_adj >= race_start_time & Time_adj <= race_finish_time)
        
        EW_Average <- mean(race_wind$EW_Vector) #Average in Radians
        
        NS_Average <- mean(race_wind$NS_Vector) #Average in Radians
        
        wind_speed <- sqrt(EW_Average^2 + NS_Average^2) 
        
        Atan2Dir <- atan2(EW_Average, NS_Average) 
        
        wind_direction <- DescTools::RadToDeg(Atan2Dir)
        
        if(wind_direction < 0) {wind_direction <- wind_direction + 360}
        
        
        
        if (grepl("(K1|KL)",discipline, ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor_K1[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor_K1[1,] - wind_direction))
          wind_cor_factor <- cor_factor_K1[row_index,col_index]/(1000/distance)}
        
        if (grepl("K2",discipline, ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor_K2[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor_K2[1,] - wind_direction))
          wind_cor_factor <- cor_factor_K2[row_index,col_index]/(1000/distance)}
        
        if (grepl("K4",discipline, ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor_K4[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor_K4[1,] - wind_direction))
          wind_cor_factor <- cor_factor_K4[row_index,col_index]/(1000/distance)}
        
        
        if (grepl("(C1|VL)",discipline, ignore.case=T)) {
          row_index <-  which.min(abs(as.data.frame(t(cor_factor_C1[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor_C1[1,] - wind_direction)) 
          wind_cor_factor <- cor_factor_C1[row_index,col_index]/(1000/distance)}
        
        if (grepl("C2",discipline, ignore.case=T)) {
          row_index <-  which.min(abs(as.data.frame(t(cor_factor_C2[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor_C2[1,] - wind_direction)) 
          wind_cor_factor <- cor_factor_C2[row_index,col_index]/(1000/distance)}
        
        water_cor_factor <- distance/avg_velocity*(water_temperature - 22.5)*0.0018
        
        Total_cor <- wind_cor_factor[1,1] + water_cor_factor
        
        Corr_time_sec <- Total_cor + time_sec
        
        race_data$Wind_direction[race_data$Lane == Lanes[Lane]] <- wind_direction
        race_data$Wind_speed[race_data$Lane == Lanes[Lane]] <- wind_speed
        race_data$Total_cor[race_data$Lane == Lanes[Lane]] <- Total_cor
        race_data$Corrected_time_sec[race_data$Lane == Lanes[Lane]] <- Corr_time_sec}
      
      if (!grepl("(KL|VL)", discipline) & paste(discipline, distance) %in% paste(GMT_AFT$Discipline, GMT_AFT$Distance)) {
        race_data$GMT_sec[race_data$Lane == Lanes[Lane]] <- as.numeric(GMT_AFT$GMT[GMT_AFT$Discipline == discipline & GMT_AFT$Distance == distance & GMT_AFT$Classification == "SR"])
        race_data$AFT_sec[race_data$Lane == Lanes[Lane]] <- as.numeric(GMT_AFT$GMT[GMT_AFT$Discipline == discipline & GMT_AFT$Distance == distance & GMT_AFT$Classification == "SR"])}
    }
    for (Split in 1:length(Splits)) {
      if (!is.na(race_data$Cum_time[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]])){
        cum_time <- race_data$Cum_time[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]]
        if (Split == 1){
          time_sec <- race_data$Cum_time_sec[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]]
          time <- cum_time
          distance <- Splits[Split]
        }
        else {
          time_sec <- race_data$Cum_time_sec[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] - race_data$Cum_time_sec[race_data$Split_distance == Splits[Split-1] & race_data$Lane == Lanes[Lane]]
          time <- cum_time - race_data$Cum_time[race_data$Split_distance == Splits[Split-1] & race_data$Lane == Lanes[Lane]]
          distance <- Splits[Split] - Splits[Split-1]
        }
        discipline <- race_data$Discipline[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]]
        race_finish_time <- as_hms(race_start_time + cum_time)
        avg_velocity <- race_data$Split_speed[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]]
        
        if (race_start_time >= wind_data$Time_adj[1] & race_finish_time <= wind_data$Time_adj[nrow(wind_data)]){
          
          race_wind <- wind_data %>% filter(Time_adj >= race_start_time & Time_adj <= race_finish_time)
          
          EW_Average <- mean(race_wind$EW_Vector) #Average in Radians
          
          NS_Average <- mean(race_wind$NS_Vector) #Average in Radians
          
          wind_speed <- sqrt(EW_Average^2 + NS_Average^2) 
          
          Atan2Dir <- atan2(EW_Average, NS_Average) 
          
          wind_direction <- DescTools::RadToDeg(Atan2Dir)
          
          if (wind_direction < 0) {wind_direction <- wind_direction + 360}
          
          
          
          if (grepl("(K1|KL)",discipline, ignore.case=T)) {
            row_index <- which.min(abs(as.data.frame(t(cor_factor_K1[,1])) - wind_speed))
            col_index <- which.min(abs(cor_factor_K1[1,] - wind_direction))
            wind_cor_factor <- cor_factor_K1[row_index,col_index]/(1000/distance)}
          
          if (grepl("K2",discipline, ignore.case=T)) {
            row_index <- which.min(abs(as.data.frame(t(cor_factor_K2[,1])) - wind_speed))
            col_index <- which.min(abs(cor_factor_K2[1,] - wind_direction))
            wind_cor_factor <- cor_factor_K2[row_index,col_index]/(1000/distance)}
          
          if (grepl("K4",discipline, ignore.case=T)) {
            row_index <- which.min(abs(as.data.frame(t(cor_factor_K4[,1])) - wind_speed))
            col_index <- which.min(abs(cor_factor_K4[1,] - wind_direction))
            wind_cor_factor <- cor_factor_K4[row_index,col_index]/(1000/distance)}
          
          
          if (grepl("(C1|VL)",discipline, ignore.case=T)) {
            row_index <-  which.min(abs(as.data.frame(t(cor_factor_C1[,1])) - wind_speed))
            col_index <- which.min(abs(cor_factor_C1[1,] - wind_direction)) 
            wind_cor_factor <- cor_factor_C1[row_index,col_index]/(1000/distance)}
          
          if (grepl("C2",discipline, ignore.case=T)) {
            row_index <-  which.min(abs(as.data.frame(t(cor_factor_C2[,1])) - wind_speed))
            col_index <- which.min(abs(cor_factor_C2[1,] - wind_direction)) 
            wind_cor_factor <- cor_factor_C2[row_index,col_index]/(1000/distance)}
          
          water_cor_factor <- distance/avg_velocity*(water_temperature - 22.5)*0.0018
          
          Total_cor <- wind_cor_factor[1,1] + water_cor_factor
          
          Corr_time_sec <- Total_cor + time_sec
          
          race_data$Split_wind_direction[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] <- wind_direction
          race_data$Split_wind_speed[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] <- wind_speed
          
          race_data$Split_time[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] <- time
          
          race_data$Split_time_sec[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] <- time_sec
          
          race_data$Total_split_cor[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] <- Total_cor
          race_data$Corrected_split_time_sec[race_data$Split_distance == Splits[Split] & race_data$Lane == Lanes[Lane]] <- Corr_time_sec}
        
      }
    }
  }
}

#' Function to extract Data from Omega GPS files
#' 
#' @param race_GPS_date dataframe with the data of a single race, taken from processed_race_data
#' 
#' 
getSplitsTable <- function(race_GPS_data){
  race_distance <- max(race_GPS_data$Distance, na.rm = T)
  
  if(race_distance == 200){
    splits <- group_by(race_GPS_data, race_number, Lane, Country, split10) %>% 
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
    splits <- group_by(race_GPS_data, race_number, Lane, Country, split50) %>% 
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
competition_name <- "Paris2024_Olympics"
competition_date <- "2024-08-06"
directory <- "C:/Users/alex/Dropbox/MTL Admos and Spin Files/Paris2024_GPSfiles/"

files <- Sys.glob(paste0(directory, "*/*.csv"))

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
  mutate(total_time_sec = round(as.numeric(total_time),3)) %>% 
  group_by(race_number) %>% 
  mutate(race_rank = rank(total_time_sec),
         race_winning_time = min(total_time_sec, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Date = competition_date,
         Event = competition_name)



write_csv(splits_dataframe, paste0(directory, "splitsDataframe_", competition_name, ".csv"), append = F)
write_csv(summary_dataframe, paste0(directory,"summaryDataframe_", competition_name, ".csv"), append = F)
