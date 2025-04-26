library(dplyr)
library(tidyr)
library(stringr)

add.FirstGen <- function(data){
  output_data_FG <- data %>%
    mutate(
      FirstGen = case_when(
        Parent_Education == "Associate degree" ~ "First Generation",
        Parent_Education == "Bachelor's degree" ~ "Continuing Generation",
        Parent_Education == "Doctorate degree (PhD)" ~ "Continuing Generation",
        Parent_Education == "Elementary or middle school" ~ "First Generation",
        Parent_Education == "Master's degree" ~ "Continuing Generation",
        Parent_Education == "Professional degree (for example: MD, JD, DJ, DVM)" ~ "Continuing Generation",
        Parent_Education == "Some college" ~ "First Generation",
        Parent_Education == "Some high school" ~ "First Generation",
        Parent_Education == "Trade or Technical School degree" ~ "First Generation",
        TRUE ~ NA
      )
    )
}

add.Race <- function(data){
  #generate ID for students
  data$ID <- 1:nrow(data)
  
  #list all race gropus
  race_groups <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic, Latino, or Spanish","Middle Eastern or North African", "Native Hawaiian or other Pacific Islander", "White" )
  
  #split the comma-separated chars in race groups
  #This step slices the data, so we save to a subset       ###look at self-report as well###
  race_data <- data %>%
    dplyr::select(STU_ID, Race) %>%
    separate_rows(Race, sep = ",") %>%
    mutate(Race = str_trim(Race)) %>%
    distinct()
  
  #Create binary variables for each race group by detecting strings
  race_data <- race_data %>%
    mutate(
      is_White = str_detect(Race, regex("White", ignore_case = TRUE)),
      is_Asian = str_detect(Race, regex("Asian", ignore_case = TRUE)),
      is_Black = str_detect(Race, regex("Black or African American", ignore_case = TRUE)),
      is_Hisp = str_detect(Race, regex("Hispanic|Latino|Spanish", ignore_case = TRUE)),
      is_Other = str_detect(Race, regex("Indian|Middle|Native", ignore_case = TRUE))
    )
  
  #collapse data by original ID
  race_binary <- race_data %>%
    group_by(STU_ID) %>%
    summarise(
      is_Asian = max(is_Asian),
      is_White = max(is_White),
      is_Black = max(is_Black),
      is_Hisp = max(is_Hisp),
      is_Other = max(is_Other)
    )
  
  #create one categorical variable
  race_category <- race_binary %>%
    group_by(STU_ID) %>%
    summarise(
      just_White = as.integer(sum(is_White) == 1 & sum(c(is_Black, is_Asian, is_Hisp, is_Other)) == 0),
      just_Black = as.integer(sum(is_Black) == 1 & sum(c(is_White, is_Asian, is_Hisp, is_Other)) == 0),
      white_asian = as.integer(sum(c(is_White, is_Asian)) > 1 & sum(c(is_Black, is_Hisp, is_Other)) == 0),
      just_Asian = as.integer(sum(is_Asian) == 1 & sum(c(is_White, is_Black, is_Hisp, is_Other)) == 0),
      just_Hisp = as.integer(sum(is_Hisp) == 1 & sum(c(is_White, is_Black, is_Asian, is_Other)) == 0),
    )
  
  #create URM status, we have non-urm, black, hisp, and other urm
  race_category <- race_category %>%
    mutate(
      race_cat = case_when(
        just_White == 1 ~ "Non URM",
        just_Black == 1 ~ "Black",
        just_Asian == 1 ~ "Non URM",
        just_Hisp == 1 ~ "Hispanic",
        white_asian == 1 ~ "Non URM",
        TRUE ~ "Other URM"
      )
    )
  
  output_data_Race <- left_join(data, race_category, by = "STU_ID")
}

add.Gender <- function(add){
  gender_data <- data %>%
    dplyr::select(STU_ID, Gender) %>%
    separate_rows(Gender, sep = ",") %>%
    mutate(Gender = str_trim(Gender)) %>%
    distinct()
  
  gender_data <- gender_data %>%
    mutate(
      is_Male = str_detect(Gender, regex("Masculine",ignore_case = TRUE)),
      is_Female = str_detect(Gender, regex("Female",ignore_case = TRUE)),
      is_Nonbinary = str_detect(Gender, regex("Agender|Genderfluid|Intersex|cisgender|Questioning|Transgender|Traditional", ignore_case = TRUE))
    )
  
  gender_binary <- gender_data %>%
    group_by(STU_ID) %>%
    summarise(
      is_Male = max(is_Male),
      is_Female = max(is_Female),
      is_Nonbinary = max(is_Nonbinary)
    )
  
  gender_category <- gender_binary %>%
    group_by(STU_ID) %>%
    summarise(
      just_Male = as.integer(sum(is_Male) == 1 & sum(c(is_Female, is_Nonbinary)) == 0),
      just_Female = as.integer(sum(is_Female) == 1 & sum(c(is_Male, is_Nonbinary)) == 0),
      just_Nonbinary = as.integer(sum(is_Nonbinary) == 1 & sum(c(is_Male, is_Female)) == 0),
    )
  
  gender_category <- gender_category %>%
    mutate(
      gender_cat = case_when(
        just_Male == 1 ~ "Male",
        just_Female == 1 ~ "Female",
        just_Nonbinary == 1 ~ "Nonbinary",
        TRUE ~ "Nonbinary"
      )
    )
  
  output_data_Gender <- left_join(data, gender_category, by = "STU_ID")
}

add.All <- function(data){
  output_data <- data %>%
    mutate(
      FirstGen = case_when(
        Parent_Education == "Associate degree" ~ "First Generation",
        Parent_Education == "Bachelor's degree" ~ "Continuing Generation",
        Parent_Education == "Doctorate degree (PhD)" ~ "Continuing Generation",
        Parent_Education == "Elementary or middle school" ~ "First Generation",
        Parent_Education == "Master's degree" ~ "Continuing Generation",
        Parent_Education == "Professional degree (for example: MD, JD, DJ, DVM)" ~ "Continuing Generation",
        Parent_Education == "Some college" ~ "First Generation",
        Parent_Education == "Some high school" ~ "First Generation",
        Parent_Education == "Trade or Technical School degree" ~ "First Generation",
        TRUE ~ NA
      )
    )
  
  #generate ID for students
  output_data$ID <- 1:nrow(output_data)
  
  #list all race gropus
  race_groups <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic, Latino, or Spanish","Middle Eastern or North African", "Native Hawaiian or other Pacific Islander", "White" )
  
  #split the comma-separated chars in race groups
  #This step slices the data, so we save to a subset       ###look at self-report as well###
  race_data <- output_data %>%
    dplyr::select(STU_ID, Race) %>%
    separate_rows(Race, sep = ",") %>%
    mutate(Race = str_trim(Race)) %>%
    distinct()
  
  #Create binary variables for each race group by detecting strings
  race_data <- race_data %>%
    mutate(
      is_White = str_detect(Race, regex("White", ignore_case = TRUE)),
      is_Asian = str_detect(Race, regex("Asian", ignore_case = TRUE)),
      is_Black = str_detect(Race, regex("Black or African American", ignore_case = TRUE)),
      is_Hisp = str_detect(Race, regex("Hispanic|Latino|Spanish", ignore_case = TRUE)),
      is_Other = str_detect(Race, regex("Indian|Middle|Native", ignore_case = TRUE))
    )
  
  #collapse data by original ID
  race_binary <- race_data %>%
    group_by(STU_ID) %>%
    summarise(
      is_Asian = max(is_Asian),
      is_White = max(is_White),
      is_Black = max(is_Black),
      is_Hisp = max(is_Hisp),
      is_Other = max(is_Other)
    )
  
  #create one categorical variable
  race_category <- race_binary %>%
    group_by(STU_ID) %>%
    summarise(
      just_White = as.integer(sum(is_White) == 1 & sum(c(is_Black, is_Asian, is_Hisp, is_Other)) == 0),
      just_Black = as.integer(sum(is_Black) == 1 & sum(c(is_White, is_Asian, is_Hisp, is_Other)) == 0),
      white_asian = as.integer(sum(c(is_White, is_Asian)) > 1 & sum(c(is_Black, is_Hisp, is_Other)) == 0),
      just_Asian = as.integer(sum(is_Asian) == 1 & sum(c(is_White, is_Black, is_Hisp, is_Other)) == 0),
      just_Hisp = as.integer(sum(is_Hisp) == 1 & sum(c(is_White, is_Black, is_Asian, is_Other)) == 0),
    )
  
  #create URM status, we have non-urm, black, hisp, and other urm
  race_category <- race_category %>%
    mutate(
      race_cat = case_when(
        just_White == 1 ~ "Non URM",
        just_Black == 1 ~ "Black",
        just_Asian == 1 ~ "Non URM",
        just_Hisp == 1 ~ "Hispanic",
        white_asian == 1 ~ "Non URM",
        TRUE ~ "Other URM"
      )
    )
  
  output_data <- left_join(output_data, race_category, by = "STU_ID")
  
  gender_data <- output_data %>%
    dplyr::select(STU_ID, Gender) %>%
    separate_rows(Gender, sep = ",") %>%
    mutate(Gender = str_trim(Gender)) %>%
    distinct()
  
  gender_data <- gender_data %>%
    mutate(
      is_Male = str_detect(Gender, regex("Masculine",ignore_case = TRUE)),
      is_Female = str_detect(Gender, regex("Female",ignore_case = TRUE)),
      is_Nonbinary = str_detect(Gender, regex("Agender|Genderfluid|Intersex|cisgender|Questioning|Transgender|Traditional", ignore_case = TRUE))
    )
  
  gender_binary <- gender_data %>%
    group_by(STU_ID) %>%
    summarise(
      is_Male = max(is_Male),
      is_Female = max(is_Female),
      is_Nonbinary = max(is_Nonbinary)
    )
  
  gender_category <- gender_binary %>%
    group_by(STU_ID) %>%
    summarise(
      just_Male = as.integer(sum(is_Male) == 1 & sum(c(is_Female, is_Nonbinary)) == 0),
      just_Female = as.integer(sum(is_Female) == 1 & sum(c(is_Male, is_Nonbinary)) == 0),
      just_Nonbinary = as.integer(sum(is_Nonbinary) == 1 & sum(c(is_Male, is_Female)) == 0),
    )
  
  gender_category <- gender_category %>%
    mutate(
      gender_cat = case_when(
        just_Male == 1 ~ "Male",
        just_Female == 1 ~ "Female",
        just_Nonbinary == 1 ~ "Nonbinary",
        TRUE ~ "Nonbinary"
      )
    )
  
  output_data <- left_join(output_data, gender_category, by = "STU_ID")
}