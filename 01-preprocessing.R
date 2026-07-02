##packages=========
library(haven)
library(tidyverse)
library(summarytools)
library (dplyr)
library(ggplot2)
library(rio)
library(multcomp)


##read in data ========================
raw_demo <- read.csv("raw_data/Rep01_demographics_anonymized.csv")
raw_data1 <- read.csv("raw_data/Rep01_anonymized.csv")

###assignment==========================
##create dataset removing all of the  inclusion/exclusion rules of our preregistration. See sections:
  #M4 -> people age< 18 excluded
  #“M5 How will participant drop-out be handled?” -> exclusion
    #We will use listwise deletion based on all variables that are necessary for each statistical analysis.
  #“M7 Data cleaning and screening”
    #For H1 and H2, we will treat outliers in the behavioral humility measure as it was done by the original authors (see p. 263):
    #They excluded participants who “listed a number of strengths and weaknesses that were more than 3 SDs from the mean”. Inspecting the original data file revealed that the procedure was actually “num_strengths_z > 3 OR num_weaknesses_z > 3”
    #They excluded participants who “did not follow the directions in the writing section”. Inspecting the original data file revealed that this means “exclude all participants who listed 0 weaknesses”. We consider this as questionable (as 0 weaknesses are a valid point on the scale), but we repeat the procedure for the direct replication.
    #We assume non-normality of the balance score (as found in the original study) and consequently will apply the same log transformation as the original authors.
  
    #For E4 (alternative ways of computing the hypothesis test), we will test different ways of doing the statistical hypothesis test as robustness checks.
  
  #“AP1 Criteria for post-data collection exclusion of participants, if any”
    #exclude participants who answered “no” to the question: “I have completed the study conscientiously. I consent to my data being used for the analysis.”
    #exclude participants who did not write anything in the humility writing exercise
  #“AP2 

###viewdata========
table(raw_demo$SelectOut)
View(raw_demo)
view(dfSummary(raw_demo))

view(raw_data1)
View(dfSummary(raw_data1))
raw_data1 %>%
  select(writing_assignment) %>%
  View()
nrow(raw_data1)
###look at the responses of the conscientious question: no one answered no, so I excluded no data, but many missing values, PreReg says that we only exculde participants who anwered no
names(raw_data1)
raw_data1 %>% count(conscientious)
raw_data1 %>% count(video_problems)
raw_data1 %>% count(video_played)
raw_data1 %>% count(sound_on)##counting the responses


### Preprocessing =============================================================

# Pre-calculate the mean and SD for the outlier detection (Category 1)
# Doing this upfront keeps the case_when function clean and readable
mean_str <- mean(raw_data1$Number.Strengths, na.rm = TRUE)
sd_str   = sd(raw_data1$Number.Strengths, na.rm = TRUE)
mean_wk  <- mean(raw_data1$Number.Weaknesses, na.rm = TRUE)
sd_wk    <- sd(raw_data1$Number.Weaknesses, na.rm = TRUE)

dat <- raw_data1 %>%
  mutate(
    # Create the classification column
    SelectOut = case_when(
      
      # CATEGORY 3: Technical difficulties & lack of conscientiousness ---
      # Only Participants that answered "Yes" (code = 1) to all screening questions are included
      
      video_played != 1   ~ 3,
      sound_on != 1       ~ 3,
      conscientious != 1  ~ 3,
      
      # CATEGORY 2: Missing text assignment or 0 strengths ---
      is.na(writing_assignment) | trimws(writing_assignment) == "" ~ 2,
      #Number.Strengths == 0                                        ~ 2,
      
      # CATEGORY 1: Outliers (> 3 SD) in strengths OR weaknesses ---
      #abs(Number.Strengths - mean_str) > (3 * sd_str) | 
        #abs(Number.Weaknesses - mean_wk) > (3 * sd_wk)               ~ 1,
      
      # --- CATEGORY 0: Clean / Unproblematic data (Default) ---
      TRUE ~ 0
    )
  ) %>%
  # Filter: Keep only the valid data
  filter(SelectOut == 0)

# Preview the cleaned data
View(dat)

  ## clean demographics dataset
      ###M4 exclude participants under 18 
      names(raw_demo)
      demo <- raw_demo %>%
        filter(age >= 18)
      view(dfSummary(demo))
      
###viewdata again ========
table(raw_demo$SelectOut)
View(raw_demo)
view(dfSummary(raw_demo))

View(dat)
view(dfSummary(dat)) 

View(demo)
view(dfSummary(demo)) 


view(raw_data1)
View(dfSummary(raw_data1))
raw_data1 %>%
  select(writing_assignment) %>%
  View()

### export  
# Export the processed data
export(dat, file="processed_data/data.csv") #save data in file
export(demo, file="processed_data/demo.csv") #save data in file
