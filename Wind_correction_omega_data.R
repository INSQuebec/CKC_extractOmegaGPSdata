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

#' Function to extract data from wind meter file
#' 
#' @param wind_file filepath of the txt file
#' @param start_time time wind device turned on
#'
#' 
extractWindData <- function(wind_file, start_time){

#Read wind data
wind_data <- read.table(wind_file, sep = ",", skip = 9) %>% 
  select(!V1) %>% #Remove first column
  rename(Dir = V2, Speed = V3, CDir = V4, CSpeed = V5, GPSLocation = V6, DateTime = V7) %>% #Rename columns
  separate("DateTime", c("Date", "Time"), sep = "T", remove=F) %>% # Separate date and time into two columns and split at location of "T", keep original column
  mutate(Date = as_datetime(Date, format = "%Y-%m-%d")) %>%  #Change date format
  mutate(DateTime = as_datetime(str_replace(DateTime, "T"," "),format = "%Y-%m-%d %H:%M:%OS")) %>%  #Change datetime format
  mutate(Time = as_hms(DateTime)) %>% #Change time data to hms format
  drop_na(Time) %>% 
  mutate(Time_adj = as_hms(Time + as_hms(as_hms(start_time) - Time[1])),
         EW_Vector = sin(DegToRad(Dir))*Speed,
         NS_Vector = cos(DegToRad(Dir))*Speed)

}


#' Function to add parameters to dataframe
#' 
#' @param race_data
#' @param params
#' 
#' 
addParams <- function(race_data, params){
race_data_with_params <- race_data %>% 
  group_by(race_number) %>%
  mutate(Discipline = params$Discipline[params$race_number == unique(race_number)],
         Date = params$Date[params$race_number == unique(race_number)],
         Start_time = params$Start_time[params$race_number == unique(race_number)],
         Water_temp = params$water_temperature[params$race_number == unique(race_number)]) %>% 
  ungroup()
}

#' Function to perform split time corrections
#' 
#' @param splits_dataframe_with_params
#' @param wind_data
#' @param cor_factor
#' 
#' 
#' 
correctSplitTime <- function(splits_dataframe_with_params,wind_data, cor_factor){
  
  corrected_split_times <- splits_dataframe_with_params %>% 
    group_by(race_number, Lane) %>% 
    mutate(distance = split_distance - lag(split_distance, default = 0),
           split_start_time = as_hms(Start_time + lag(as_hms(split_cumtime), default = as_hms(0))),
           split_finish_time = as_hms(Start_time + as_hms(split_cumtime)),
           Finish_time = as_hms(Start_time + max(as_hms(split_cumtime))),
           wind_speed = NA_real_,
           wind_direction = NA_real_,
           Total_cor_split = NA_real_,
           Corrected_split_time = NA_real_) %>% 
    ungroup()
  
  for (i in 1:nrow(corrected_split_times)) {
    if (!is.na(corrected_split_times$split_time[i])) {
      
      if (corrected_split_times$split_start_time[i] >= wind_data$Time_adj[1] & corrected_split_times$split_finish_time[i] <= wind_data$Time_adj[nrow(wind_data)]){
        
        race_wind <- wind_data %>% filter(Time_adj >= corrected_split_times$split_start_time[i] & Time_adj <= corrected_split_times$split_finish_time[i])
        
        EW_Average <- mean(race_wind$EW_Vector) #Average in Radians
        
        NS_Average <- mean(race_wind$NS_Vector) #Average in Radians
        
        wind_speed <- sqrt(EW_Average^2 + NS_Average^2) 
        
        corrected_split_times$wind_speed[i] <- wind_speed
        
        Atan2Dir <- atan2(EW_Average, NS_Average) 
        
        wind_direction <- RadToDeg(Atan2Dir)
        
        if (wind_direction < 0) { wind_direction <- wind_direction + 360 }
        
        corrected_split_times$wind_direction[i] <- wind_direction
        
        
        if (grepl("(K1|KL)",corrected_split_times$Discipline[i], ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor$K1[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$K1[1,] - wind_direction))
          wind_cor_factor <- cor_factor$K1[row_index,col_index]/(1000/corrected_split_times$distance[i])}
        
        else if (grepl("K2",corrected_split_times$Discipline[i], ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor$K2[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$K2[1,] - wind_direction))
          wind_cor_factor <- cor_factor$K2[row_index,col_index]/(1000/ corrected_split_times$distance[i])}
        
        else if (grepl("K4",corrected_split_times$Discipline[i], ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor$K4[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$K4[1,] - wind_direction))
          wind_cor_factor <- cor_factor$K4[row_index,col_index]/(1000/corrected_split_times$distance[i])}
        
        else if (grepl("(C1|VL)",corrected_split_times$Discipline[i], ignore.case=T)) {
          row_index <-  which.min(abs(as.data.frame(t(cor_factor$C1[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$C1[1,] - wind_direction)) 
          wind_cor_factor <- cor_factor$C1[row_index,col_index]/(1000/corrected_split_times$distance[i])}
        
        else if (grepl("C2",corrected_split_times$Discipline[i], ignore.case=T)) {
          row_index <-  which.min(abs(as.data.frame(t(cor_factor$C2[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$C2[1,] - wind_direction)) 
          wind_cor_factor <- cor_factor$C2[row_index,col_index]/(1000/corrected_split_times$distance[i])}
        
        water_cor_factor <- corrected_split_times$distance[i]/corrected_split_times$split_velocity[i]*(corrected_split_times$Water_temp[i] - 22.5)*0.0018
        
        Total_cor <- wind_cor_factor[1,1] + water_cor_factor
        
        Corr_time <- Total_cor + corrected_split_times$split_time[i]
        
        corrected_split_times$Total_cor_split[i] <- Total_cor
        corrected_split_times$Corrected_split_time[i] <- Corr_time
      }
    }
  }
  return(corrected_split_times)
}

#' Function to perform race time corrections
#' 
#' @param processed_race_data_with_params
#' @param wind_data
#' @param cor_factor
#' @param GMT_AFT
#' 
#' 
#' 
correctRaceTime <- function(processed_race_data_with_params,wind_data, cor_factor,GMT_AFT){
  
  corrected_race_times <- processed_race_data_with_params %>% 
    group_by(race_number, Lane) %>% 
    filter(Distance == max(Distance)) %>% 
    ungroup() %>% 
    mutate(Race_time = as.numeric(Time),
           Avg_velocity = Distance/Race_time,
           Finish_time = as_hms(Start_time + Time),
           wind_speed = NA_real_,
           wind_direction = NA_real_,
           Total_cor = NA_real_,
           Corrected_time = NA_real_,
           GMT = NA_real_,
           AFT = NA_real_)
  
  for (i in 1:nrow(corrected_race_times)) {
    if (!is.na(corrected_race_times$Race_time[i])) {
      
      if (corrected_race_times$Start_time[i] >= wind_data$Time_adj[1] & corrected_race_times$Finish_time[i] <= wind_data$Time_adj[nrow(wind_data)]){
        
        race_wind <- wind_data %>% filter(Time_adj >= corrected_race_times$Start_time[i] & Time_adj <= corrected_race_times$Finish_time[i])
        
        EW_Average <- mean(race_wind$EW_Vector) #Average in Radians
        
        NS_Average <- mean(race_wind$NS_Vector) #Average in Radians
        
        wind_speed <- sqrt(EW_Average^2 + NS_Average^2) 
        
        corrected_race_times$wind_speed[i] <- wind_speed
        
        Atan2Dir <- atan2(EW_Average, NS_Average) 
        
        wind_direction <- RadToDeg(Atan2Dir)
        
        if (wind_direction < 0) { wind_direction <- wind_direction + 360 }
        
        corrected_race_times$wind_direction[i] <- wind_direction
        
        
        if (grepl("(K1|KL)",corrected_race_times$Discipline[i], ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor$K1[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$K1[1,] - wind_direction))
          wind_cor_factor <- cor_factor$K1[row_index,col_index]/(1000/corrected_race_times$Distance[i])}
        
        else if (grepl("K2",corrected_race_times$Discipline[i], ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor$K2[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$K2[1,] - wind_direction))
          wind_cor_factor <- cor_factor$K2[row_index,col_index]/(1000/ corrected_race_times$Distance[i])}
        
        else if (grepl("K4",corrected_race_times$Discipline[i], ignore.case=T)) {
          row_index <- which.min(abs(as.data.frame(t(cor_factor$K4[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$K4[1,] - wind_direction))
          wind_cor_factor <- cor_factor$K4[row_index,col_index]/(1000/corrected_race_times$Distance[i])}
        
        else if (grepl("(C1|VL)",corrected_race_times$Discipline[i], ignore.case=T)) {
          row_index <-  which.min(abs(as.data.frame(t(cor_factor$C1[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$C1[1,] - wind_direction)) 
          wind_cor_factor <- cor_factor$C1[row_index,col_index]/(1000/corrected_race_times$Distance[i])}
        
        else if (grepl("C2",corrected_race_times$Discipline[i], ignore.case=T)) {
          row_index <-  which.min(abs(as.data.frame(t(cor_factor$C2[,1])) - wind_speed))
          col_index <- which.min(abs(cor_factor$C2[1,] - wind_direction)) 
          wind_cor_factor <- cor_factor$C2[row_index,col_index]/(1000/corrected_race_times$Distance[i])}
        
        water_cor_factor <- corrected_race_times$Distance[i]/corrected_race_times$Avg_velocity[i]*(corrected_race_times$Water_temp[i] - 22.5)*0.0018
        
        Total_cor <- wind_cor_factor[1,1] + water_cor_factor
        
        Corr_time <- Total_cor + corrected_race_times$Race_time[i]
        
        corrected_race_times$Total_cor[i] <- Total_cor
        corrected_race_times$Corrected_time[i] <- Corr_time
        
        if (paste(corrected_race_times$Discipline[i], corrected_race_times$Distance[i]) %in% paste(GMT_AFT$Discipline, GMT_AFT$Distance)) {
          corrected_race_times$GMT[i] <- GMT_AFT$GMT[GMT_AFT$Discipline == corrected_race_times$Discipline[i] & GMT_AFT$Distance == corrected_race_times$Distance[i] & GMT_AFT$Classification == "SR"]
          corrected_race_times$AFT[i] <- GMT_AFT$GMT[GMT_AFT$Discipline == corrected_race_times$Discipline[i] & GMT_AFT$Distance == corrected_race_times$Distance[i] & GMT_AFT$Classification == "SR"]}
      }
    }
  }
  return(corrected_race_times)
}


#' Function to extract Data from Omega GPS files
#' 
#' @param race_GPS_data dataframe with the data of a single race, taken from processed_race_data
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

# Load CKC correction factors for each discipline
cor_factor_file <- "CKC Correction Factors.xlsx"
cor_factor <- cor_factor_file %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_xlsx,path = cor_factor_file,col_names=F) %>% 
  map(~ .x %>% select(...1 | where(~!any(is.na(.)))))

GMT_AFT <- read_xlsx("GMT_AFT_CKC_2024.xlsx") %>% 
  mutate(across(where(is.POSIXct), as_hms))


params <- read_xlsx("Paris2024_metadata_and_conditions_copy.xlsx") %>% 
  rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>% 
  mutate(across(where(is.POSIXct) & !Date, as_hms))

# Input Competition Name and date (change manually before running script)
competition_name <- "Paris2024_Olympics"
competition_date <- "2024-08-06"
directory <- "C:/Users/alex/Dropbox/MTL Admos and Spin Files/Paris2024_GPSfiles/"

files <- Sys.glob(paste0(directory, "*/*.csv"))

# Process GPS files
processed_race_data <- lapply(files, extractOmegaGPSData) %>% 
  bind_rows()

# Input wind device start time and wind file path
start_time <- params$wind_station_turned_on_time[1]
wind_file <- "C:/Users/alex/Dropbox/MTL Admos and Spin Files/Wind data/Wind WCup May 12 929 start 1458 end.txt"

#Add fractional seconds to start time if user does not
if (!grepl("\\.",start_time)) {start_time = paste0(start_time,".0")}

#read and process wind data
wind_data <- extractWindData(wind_file, start_time)

# Summarise race data by 10m or 50m splits

splits_dataframe <- lapply(split(processed_race_data, f = processed_race_data$race_number), getSplitsTable) %>% 
  bind_rows()


splits_dataframe_with_params <- addParams(splits_dataframe, params) 

#correct split times
corrected_split_times <- correctSplitTime(splits_dataframe_with_params, wind_data, cor_factor)

processed_race_data_with_params <- addParams(processed_race_data, params)

#correct race times
corrected_race_times <- correctRaceTime(processed_race_data_with_params, wind_data, cor_factor, GMT_AFT)


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
