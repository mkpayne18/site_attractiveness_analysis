#-------------------------------------------------------------------------------
# Project title: Going beyond the distance: streamflow alters site attractiveness
# to stray hatchery-origin chum salmon in Southeast Alaska

# Molly K Payne

# Purpose of this script: Calculate updated response variable -> average effective
# number of hatchery strays

# Last updated: December 30, 2023
#-------------------------------------------------------------------------------

## Read in data
surv_1319 <- read.csv("data/stream_survey_data.csv") #2013-2019 stream survey data,
#downloaded from hatcherywild.org using login credentials. This data is also
#being made public with paper publication. Technician names have been removed from
#the .csv for privacy.

#Calculate proportion sampled from raw (unclean) data for use in supplementary info:
surv_1319$Proportion_sampled <- surv_1319$NumberofSpecimens/surv_1319$DeadCount
prop_1 <- surv_1319 %>% filter(Proportion_sampled <= 1)
saveRDS(prop_1, "output/prop_1.rds")
surv_1319 <- surv_1319 %>% select(-Proportion_sampled)



#1. Determine true dead counts for 2017-2019 streams =========================
#Unlike 2013-2015 surveys, 2017-2019 surveys are conducted either every day or
#every other day, so you need to account for the excess dead in each DeadCount

#Do this by subtracting out the previous day's dead count from each dead count.
#The previous day's dead count should include all dead that are remaining from
#the previous set of days, as well as the new specimens being sampled that day.
#Only subtract each previous day's dead count if the previous day's dead count
#is less than the current day dead count. Current day dead counts that are smaller
#are all likely to be new dead, as the old, already-sampled carcasses would have
#been washed out or otherwise disappeared (that's why there would be fewer total
#dead compared to previous day)
fitness_1719 <- surv_1319 %>% filter(Year %in% c("2017", "2018", "2019"))

#Create "helper" column (the amount to subtract from the dead count)
fitness_1719$subtract <- rep(NA, length(fitness_1719$AdfgStreamCode))
#Create your estimated dead counts column to input your subtracted counts
fitness_1719$Estimated_dead <- rep(NA, length(fitness_1719$AdfgStreamCode))

subtract_col_fxn <- function(vec){
  vec2 <- c(0, vec) #offsets each vector of dead counts by 1 position
  vec3 <- vec2[-length(vec2)] #removes final val from each vector
  return(vec3)
} #fxn offsets the dead counts by 1 position, allowing you to easily subtract the 
#previous day's dead count from the current dead count by putting the previous
#day's dead count in another column

final_deads_1719 <- fitness_1719 %>% group_by(StreamName, Year) %>%
  mutate(subtract = subtract_col_fxn(DeadCount)) %>%
  mutate(Estimated_dead = ifelse(DeadCount > subtract,
                                 DeadCount - subtract, DeadCount)) #as noted ab-
#ove, only subtract the previous day's dead count from the current day's dead
#count if previous day < current day dead count. If this condition is not met, 
#then set DeadCount = DeadCount (bc we assume these are new dead)




#1.1. Tailor final_deads_1719 and combine with 2013-2015 survey data ===========
final_deads_1719 <- final_deads_1719[,-29] #remove the "subtract" column
final_deads_1719$DeadCount <- final_deads_1719$Estimated_dead
final_deads_1719 <- final_deads_1719[,-29] #remove estimated dead column, as these
#numbers are now in the dead count col

surv2 <- surv_1319 %>% filter(!Year %in% c("2017", "2018", "2019"))
surv2 <- rbind.data.frame(surv2, final_deads_1719)




#1.2. 2014 fitness streams adjust dead count ===================================
## Note also that in 2014, the 'fitness' streams were sampled heavily compared to
#other sites. These dead counts should similarly be adjusted:
surv14 <- surv2 %>% group_by(Year, StreamName) %>%
  summarise(num_surv = n()) %>% filter(Year == "2014")
print(surv14[surv14$num_surv > 10,])
print(surv14[surv14$num_surv < 10,])
rm(surv14)


fitness_14 <- surv2 %>%
  filter(Year %in% c("2014"),
         StreamName %in% c("Fish Creek", "Admiralty Creek", "Sawmill Creek",
                           "Prospect Creek"))


#Create "helper" column (the amount to subtract from the dead count)
fitness_14$subtract <- rep(NA, length(fitness_14$AdfgStreamCode))
#Create your estimated dead counts column to input your subtracted counts
fitness_14$Estimated_dead <- rep(NA, length(fitness_14$AdfgStreamCode))

subtract_col_fxn #use same function from section 5.2 above

fitness_deads_14 <- fitness_14 %>% group_by(StreamName, Year) %>%
  mutate(subtract = subtract_col_fxn(DeadCount)) %>%
  mutate(Estimated_dead = ifelse(DeadCount > subtract,
                                 DeadCount - subtract, DeadCount))


### Replace 2014 fitness stream rows with these^^ in full df:
remove_fitness_14 <- surv2 %>%
  filter(!Year %in% c("2014") |
           !StreamName %in% c("Fish Creek", "Admiralty Creek", "Sawmill Creek",
                              "Prospect Creek"))

fitness_deads_14 <- fitness_deads_14[,-29] #remove the "subtract" column
fitness_deads_14$DeadCount <- fitness_deads_14$Estimated_dead
fitness_deads_14 <- fitness_deads_14[,-29] #remove estimated dead column
surv3 <- rbind.data.frame(remove_fitness_14, fitness_deads_14)








#2. Final part! Estimate dead counts for 2008-2011 data ########################
HW_data <- read.csv("data/2008_2019_HW_Data.csv")
HW_data$Year <- as.factor(HW_data$Year)
#Filter the 2008-2011 years only
HW_data_811 <- HW_data %>% filter(Year %in% c("2008", "2009", "2010", "2011"))
#these data^^ are for individual fish. Summarize by survey:
HW_data_811$SurveyDate <- as.Date(HW_data_811$SurveyDate, format = "%m/%d/%y")
HW_by_survey <- HW_data_811 %>% group_by(StreamName, SurveyDate) %>%
  summarise(NumberofSpecimens = length(From_H), NumberStrays = sum(From_H))
HW_by_survey <- as.data.frame(HW_by_survey)


#2.1. Fit linear model =========================================================
#Use linear model to predict missing 2008-2011 dead counts based on the number
#of specimens

#Fit linear model using 2013-2015 data only that does not include the fitness
#streams (Fish, Sawmill, Prospect, Admiralty) since the fitness streams were
#sampled more often than 2013-2015 non-fitness streams and all 2008-2011 streams.
#Non-fitness 2013-2015 streams will likely have different dead counts within an
#individual survey compared to the fitness streams and will therefore be more
#representative of a 2008-2011 stream
non_fitness_1315 <- surv3 %>% filter(!StreamName %in% c("Fish Creek",
                                                               "Admiralty Creek",
                                                               "Sawmill Creek",
                                                               "Prospect Creek"))
plot(DeadCount ~ NumberofSpecimens, non_fitness_1315)
lm_dead <- lm(DeadCount ~ NumberofSpecimens, non_fitness_1315, na.action = na.omit)
abline(lm_dead)
summary(lm_dead) #slope is 1.4, which means that larger specimen counts increas-
#ingly correspond to dead counts that are so large that they can't all be sampled.
#This matches the observed data (see on plot), whereby the smaller number of
#specimens means that there generally weren't that many fish and the technicians
#were able to get them all


#2.2. Predict dead counts for 2008-2011 streams ================================
pred_deads_811 <- as.data.frame(predict(lm_dead, HW_by_survey))
deads_811 <- cbind.data.frame(HW_by_survey, pred_deads_811)
colnames(deads_811)[5] <- "DeadCount"
#Note that there are a handful of negative dead counts that need to be corrected
deads_811$DeadCount[deads_811$DeadCount < 0] <- 0





#2.3. Add 2008-2011 dead counts to full dataframe ==============================
mtrx <- as.data.frame(matrix(nrow = length(deads_811$StreamName),
                             ncol = 22))
mtrx_deads_811 <- cbind.data.frame(rep(NA, length(deads_811$StreamName)),
                                   deads_811, mtrx)
mtrx_deads2 <- mtrx_deads_811[,c(1,28,2,3,7:20,6,27,26,4,5,21:25)]
names(mtrx_deads2) <- names(surv3)
#Fill in year column
mtrx_deads2$SurveyDate <- as.Date(mtrx_deads2$SurveyDate, format = "%m/%d/%y")
mtrx_deads2$Year <- as.factor(format(mtrx_deads2$SurveyDate, format="%Y"))

#Combine into final dataframe
surv4 <- rbind.data.frame(surv3, mtrx_deads2)
surv4$DeadCount <- round(surv4$DeadCount)






#3. Predict 2013-2015 missing dead counts ######################################
sum(is.na(surv4$DeadCount)) #18 remaining dead count NAs, all in 2013-2015

#Predict these missing dead counts with the model you created
surv5 <- surv4 %>%
  mutate(DeadCount = ifelse(is.na(DeadCount),
                            round((NumberofSpecimens*coef(lm_dead)[2])+coef(lm_dead)[1]),
                            DeadCount))
sum(is.na(surv5$DeadCount))
sum(is.na(surv5$NumberofSpecimens))
surv5[surv5$DeadCount < 0,] #the model predicted 1 dead count to
#be < 0. Set this = 0
surv5$DeadCount[surv5$DeadCount < 0] <- 0


### Your final dataset that contains all of the dead counts to use for rescaling
#your model response variable is surv5
surv5







#4. Rescale your model response variable #######################################
NumberStrays_NA <- surv5 %>%
  filter(is.na(NumberStrays)) #These should all be zeros (bc NumberofSpecimens =
#0), so change them to zeros
surv5$NumberStrays[is.na(surv5$NumberStrays)] <- 0


#4.1. Survey data (surv5) final tailoring ======================================
### Calculate proportion sampled as the NumberofSpecimens/DeadCount
surv5$Proportion_sampled <-
  surv5$NumberofSpecimens/surv5$DeadCount
sum(is.na(surv5$Proportion_sampled))
prop_sampled_NA <- surv5 %>% filter(is.na(Proportion_sampled)) #these NAs
#should be 1s because NumberofSpecimens = 0 and DeadCount = 0, therefore the prop.
#sampled would be 1
surv5$Proportion_sampled[is.na(surv5$Proportion_sampled)] <- 1
sum(is.na(surv5$Proportion_sampled)) #0
surv5$Proportion_sampled[surv5$Proportion_sampled < 0] #none



#Looking at the data, there are some rows where the NumberofSpecimens is actually
#greater than the TotalCount (Alive + Dead), which doesn't really make any sense.
#But, this was true in the raw data (i.e., it wasn't anything I did):
length(surv_1319[surv_1319$NumberofSpecimens > surv_1319$TotalCount,])
#raw data^^ total = 28
length(surv5[surv5$NumberofSpecimens > surv5$TotalCount,])
#my final data^^, total = 29
#So, I'm not sure what to make of this. I will assume my specimen counts are 
#accurate and set all proportions sampled > 1 equal to 1, as per C. Cunningham's
#suggestion

surv5$Proportion_sampled[surv5$Proportion_sampled > 1] <- 1
#Final check:
na_or_below_0 <- function(column){
  sum(is.na(column))
  length(column[column < 0])
}
na_or_below_0(surv5$DeadCount)
na_or_below_0(surv5$NumberofSpecimens)
na_or_below_0(surv5$NumberStrays)



#4.2. Calculate effective number of hatchery fish! =============================
#Effective number of hatchery fish = # hatchery strays / proportion sampled
surv5$Effective_number_strays <-
  surv5$NumberStrays/surv5$Proportion_sampled
#Are there any NAs?
sum(is.na(surv5$Effective_number_strays)) #yes, 7 of them
effective_straysNA <- surv5 %>% filter(is.na(Effective_number_strays))
#these^^ streams all have 0 hatchery strays, and a proportion sampled = 0 as well.
#The dead counts for these streams ranges from 1-11 individuals. Maybe these are
#some previously sampled dead, and that's why 0 fish were sampled? Hopefully 1-11 
#fish is the max error associated with any of my dead counts
#Since the number of strays for these 7 NAs = 0, then the effective number of 
#strays would also = 0
surv5$Effective_number_strays[is.na(surv5$Effective_number_strays)] <- 0
sum(is.na(surv5$Effective_number_strays)) #none
surv5$Effective_number_strays[surv5$Effective_number_strays < 0] #none



#4.3. Calculate strays by season ===============================================
new_response_var <- surv5 %>% group_by(StreamName, Year) %>%
  summarise(Total_effective_strays = sum(Effective_number_strays),
            Number_of_surveys = length(SurveyDate))

#NoteL
#The total effective number of strays column for a given stream-year in new_
#response_var will not always equal the number of hatchery strays / proportion
#sampled for a stream-year because you changed the set the proportions sampled
#that were greater than 1 equal to 1 instead (section 4.1 above)

sum(is.na(new_response_var$Total_effective_strays)) #0
sum(is.na(new_response_var$Number_of_surveys)) #0

#Calculate average (effective) number of strays (dividing by # of surveys):
new_response_var$Avg_number_strays <-
  new_response_var$Total_effective_strays/new_response_var$Number_of_surveys

### Compare to dataset used previously to determine the response variable
Master_dataset <- read.csv("data/Master_dataset.csv")
head(Master_dataset)
Master_dataset$Year <- as.factor(Master_dataset$Year)
Master_subset <- Master_dataset[,c(2:5,8,9)]
colnames(Master_subset)[5] <- "old_Number_surveys"
colnames(Master_subset)[6] <- "old_Avg_strays"
compare_response <- left_join(new_response_var, Master_subset,
                              by = c("StreamName", "Year"))

#How does the previous average # of strays differ from the effective average # of
#strays?
plot(compare_response$old_Avg_strays ~ compare_response$Avg_number_strays)
abline(0,1, col = "red")
#does not appear to differ drastically at smaller sample sizes. At larger sample
#sizes, the new (effective) number of strays is larger than the previous Avg_strays
#bc we've inflated many of those observations where proportionally less of the
#total dead was sampled



#Use the Avg_number_strays column from
new_response_var #to update your model response variable
saveRDS(new_response_var, file = "output/new_response_var.rds")

#Also save surv5. You will need it for supplementary information figures
saveRDS(surv5, "output/new_response_survey_dat.rds")





