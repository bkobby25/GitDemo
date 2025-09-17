################################################################
# PROJECT: In Class Assignment for Class 1 Survey
# PURPOSE: Data Analysis for Class 1 Survey 
# DATA:    Survey data collected from ADA students
# AUTHOR:  KWABENA BOATENG
# CREATED: 09/14/2025 
# NOTES:   Assignment 1 Code 
################################################################


C1survey <- read_csv ("https://github.com/kijohnson/Advanced-Data-Analysis/raw/refs/heads/main/Class%201%20Survey%20Fall%202025%202.csv")


dim(C1survey)



colnames(C1survey)



names(C1survey)[1:27] <- c("id", "like_cats", "like_dogs", "have_desert", "slogan", 
                           "fav_day", "larkORowl", "fav_food", "fav_drink", "fav_season", 
                           "fav_month", "hobby", "program", "specialization", "stat_software", 
                           "R_exp", "coding_comfort", "coding_length", "top_three", 
                           "public_health_interest", "fav_num", "bday", "bmonth", 
                           "country", "state", "city", "highest_educ_level")



types <- c("factor", "integer", "numeric", "character")

# Count variables of each type
type_counts <- sapply(types, function(t) sum(sapply(C1survey, class) == t))

# Display the result
type_counts

list (bday_counts = table(C1survey$bday, useNA = "always"), 
      
      bmonth_counts = table(C1survey$bmonth, useNA = "always"))

birthdays <- c("10", "13", "18", "5", "13", "June 2nd", "1", "6", "17", "20", "30", 
               "1", "28", "8", "15-Nov", "24-Jun", "August 2nd 1997", NA)
birthdays_clean <- gsub(".*?(\\d+).*", "\\1", birthdays)

birthdays_clean <- as.numeric(birthdays_clean)

birthdays_clean

show(birthdays_clean)

C1survey$birthdays_clean <- birthdays_clean

C1survey <- C1survey %>%
  mutate(birthdays= gusb("[0-9]","",bday))


month_lookup <- c(
  "january" = 1, "february" = 2, "march" = 3, "april" = 4, "may" = 5,
  "june" = 6, "july" = 7, "august" = 8, "september" = 9, "october" = 10,
  "november" = 11, "december" = 12
  
  library(dplyr)
  library(stringr)
  
# 1. Standardize the original column
  C1survey <- C1survey %>%
    mutate(
      bmonth_clean = tolower(trimws(bmonth))
    )
  
  C1survey <- C1survey %>%
    mutate(
      bmonth_num = as.numeric(str_extract(bmonth_clean, "\\d+"))
    )
  
  
  C1survey <- C1survey %>%
    mutate(
      bmonth_2 = case_when(
        bmonth_clean %in% c("january") ~ 1,
        bmonth_clean %in% c("february") ~ 2,
        bmonth_clean %in% c("march") ~ 3,
        bmonth_clean %in% c("april") ~ 4,
        bmonth_clean %in% c("may") ~ 5,
        bmonth_clean %in% c("june") ~ 6,
        bmonth_clean %in% c("july") ~ 7,
        bmonth_clean %in% c("august") ~ 8,
        bmonth_clean %in% c("september", "septemeber") ~ 9,  # typo fix
        bmonth_clean %in% c("october") ~ 10,
        bmonth_clean %in% c("november") ~ 11,
        bmonth_clean %in% c("december") ~ 12,
        bmonth_num %in% 1:12 ~ bmonth_num,   # numeric months 1–12
        TRUE ~ NA_real_                       # everything else -> NA
      )
    )
  
  show(C1survey$bmonth_2)
  
  
  C1survey <- C1survey %>%
    mutate(birthdays_clean = str_extract(bday, "\\d+"))
  
  C1survey <- C1survey %>%
    mutate(birthdays_clean = as.numeric(str_extract(bday, "\\d+")))
  
  #showing if it exists
  table(C1survey$birthdays_clean, useNA = "always")
  
  class(C1survey$bday)
  class(C1survey$bmonth)
  str(C1survey$bday)
  
  
  C1survey$bday_2 <- C1survey$bday %>% as.numeric(C1survey$bday, "%d")
  
  table(C1survey$bday_2)
  
  C1survey$bmonth_2 <- C1survey$bmonth %>% as.numeric(C1survey$bmonth, "%m")
  
  table(C1survey$bmonth_2)
  
  bmonth_2 <- c(`1`=3, `2`=5, `3`=2, `4`=5, `5`=8, 
                `6`=5, `7`=4, `8`=4, `9`=3, `10`=5, `11`=4)
  
  C1survey <- C1survey %>% 
    select(-any_of(c("bday_new", "bday_2", "bday_vector", "bday_2freq", "median_bday")))
  
  #Finding the median birthday 
  median_birthday <- median(C1survey$birthdays_clean, na.rm = TRUE)
  median_birthday
  
  #Creating an object called bmonth_2
  bmonth_2 <- C1survey$bmonth_2
  
  #Finding the median birth month
  median_bmonth <- median(bmonth_2, na.rm = TRUE)
  median_bmonth
  
  C1survey$bseason <- with(C1survey, 
                    ifelse(bmonth_2 %in% c(12, 1, 2), "Winter",
                    ifelse(bmonth_2 %in% 3:5, "Spring",
                    ifelse(bmonth_2 %in% 6:8, "Summer",
                    ifelse(bmonth_2 %in% 9:11, "Fall", NA)))))
  
  
  season_table <- table(
    Month = C1survey$bmonth_2,
    Season = C1survey$bseason,
    useNA = "ifany"  # include any NAs if present
  )
  
  season_table
  
  season_table <- C1survey %>%
    count(bmonth_2, bseason) %>%           # count occurrences of each month-season pair
    pivot_wider(names_from = bseason,          # make seasons the columns
                values_from = n,              # fill with counts
                values_fill = 0)              # fill missing combinations with 0
  
  season_table
  
  
  season_totals <- addmargins(season_table, margin = 2)  # sum over columns
  
  season_totals
  
  # Create a frequency table of the variable
  tab <- table(C1survey$`What is your level of R experience (1=None, 5=Advanced)?`)
  
  # Get the most frequent value (statistical mode)
  mode_value <- names(tab)[which.max(tab)]
  mode_value
  
  
  
  
  
  