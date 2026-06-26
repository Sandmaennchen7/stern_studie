##packages=========
library(haven)
library(tidyverse)
library(summarytools)
library (dplyr)
library(ggplot2)
library(rio)


##read in data ========================
raw_data_d <-read.csv('Rep01_demographics_anonymized.csv')
raw_data <- read.csv('Rep01_anonymized.csv')


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
dat <-raw_data %>%
  #outlier <- df(..(mean(Number.Strengths)- Number.Strengths)^2 > 3)
  #excl <- df(..Number.Strengths==0)
  #mutate ()-> new column where 0 -> unproblematic data, 1 -> outlier, 2-> where numberofstrengths are =0
  filter(SelectOut==0)
export(dat, file="processed_data/data.csv") #save data in file



###viewdata==
table(raw_data$SelectOut)
View(raw_data)
view(dfSummary(raw_data))

View(data)
view(dfSummary(data))