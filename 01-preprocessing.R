##packages=========
library(haven)
library(tidyverse)
library(summarytools)
library (dplyr)
library(ggplot2)
library(rio)


##read in data ========================
raw_demo <- read.csv("raw_data/Rep01_demographics_anonymized.csv")
raw_data1 <- read.csv("raw_data/Rep01_anonymized.csv")


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

###preprocessing=======================

dat <- raw_data1 %>%
  mutate(
    # classification column
    SelectOut = case_when(
      # Condition for Category 2: Number.Strengths is exactly 0
      Number.Strengths == 0 ~ 2,
      
      # Condition for Category 1: Outlier in either Strengths OR Weaknesses (> 3 SD)
      abs(Number.Strengths - mean(Number.Strengths, na.rm = TRUE)) > (3 * sd(Number.Strengths, na.rm = TRUE)) | 
        abs(Number.Weaknesses - mean(Number.Weaknesses, na.rm = TRUE)) > (3 * sd(Number.Weaknesses, na.rm = TRUE)) ~ 1,
      
      # Category 0: Everything else (Unproblematic)
      TRUE ~ 0
    )
  ) %>%
  
  # Filter
  filter(SelectOut == 0)

# Export the processed data
export(dat, file="processed_data/data.csv") #save data in file



###viewdata==
table(raw_demo$SelectOut)
View(raw_demo)
view(dfSummary(raw_demo))

View(dat)
view(dfSummary(dat)) ##was genau ist das? ~Chiara

view(raw_data1)
View(dfSummary(raw_data1))
raw_data1 %>%
  select(writing_assignment) %>%
  View()

###M4 exclude participants under 18 
names(raw_demo)
demo <- raw_demo %>%
  filter(age >= 18)
view(dfSummary(demo))

###look at the responses of the conscientious question: no one answered no, so I excluded no data, but many missing values, PreReg says that we only exculde participants who anwered no
names(raw_data1)
raw_data1 %>% count(conscientious)
raw_data1 %>% count(video_problems)
raw_data1 %>% count(video_played)
raw_data1 %>% count(sound_on)

###AP1 exclude participants who answered no to the question if the video played
raw_data1 %>%
  filter(video_played == 1)

###AP1 exclude participants who answered no to the question if tone worked 
raw_data1 %>%
  filter(sound_on == 1)

###AP1 exclude participants who didnt write anything to the writing assignment
nrow(raw_data1)
dat_clean <- raw_data1 %>%
  filter(!is.na(writing_assignment),
         trimws(writing_assignment) != "")
nrow(dat_clean)
view(dfSummary(dat_clean))
