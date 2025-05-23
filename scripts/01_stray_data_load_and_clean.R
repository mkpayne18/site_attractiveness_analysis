#-------------------------------------------------------------------------------
# Project title: Streamflow shapes site attractiveness to stray hatchery-origin
# chum salmon in Southeast Alaska

# Molly K Payne

# Purpose of this script: Load and clean straying data

# Last updated: July 6, 2024
#-------------------------------------------------------------------------------
require(tidyverse)

#Read in data containing response variable (number of strays in a stream and
#year) and covariate data
Master_dataset <- read.csv("data/Master_dataset.csv")
head(Master_dataset)
#Year as factor
Master_dataset$Year <- factor(Master_dataset$Year)
str(Master_dataset)


#Remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled slightly
#outside of what would be considered summer-run chum salmon in SEAK
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
stray_dat <- Master_dataset[!(Master_dataset$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
stray_dat <- stray_dat[!(stray_dat$Year == "2010" &
                           stray_dat$StreamName == "Saook Bay West Head"), ]


#Change NA values in WMA_Releases_by_Yr to 0 (these may be considered 0s because
#you've effectively "sampled" within 40km of a stream and found no releases, so
#it is not missing data)
stray_dat$WMA_Releases_by_Yr[is.na(stray_dat$WMA_Releases_by_Yr)] <- 0


#remove rows containing NAs
stray_dat <- stray_dat[complete.cases(stray_dat), ]
rownames(stray_dat) <- 1:nrow(stray_dat)
colnames(stray_dat)[8] <- "Number_surveys"



#Update model response var to be the average EFFECTIVE number of hatchery strays,
#which takes into account bias in the response variable due to the proportion of
#carcasses sampled (see 'Quantifying straying' in Methods section of Payne et al.
#2024).

#In a nutshell, streams with larger dead counts have proportionally fewer fish
#sampled, so the # of strays detected would be biased low in these streams. I've
#dealt with this by calculating the average effective number of hatchery strays,
#which weights the number of strays detected by the proportion of carcasses
#sampled. Where a smaller proportion of the carcasses were sampled, the number
#of strays gets inflated by that proportion to more accurately approximate the
#true number of hatchery strays. Refer to response_var_update.R for the calculation
#Also note in section 4.3 of the response_var_update script that the two versions
#of the response variable don't differ greatly

#Replace Avg_number_strays with effective average number of strays. Avg effective
#strays can be found in the Avg_number_strays column of new_response_var object
#created in response_var_update.R:

#First read in the new response variable data object containing the effective
#number of strays (created in response_var_update.R):
new_response_var <- readRDS("output/new_response_var.rds")
colnames(stray_dat)[8] <- "old_Number_surveys"
colnames(stray_dat)[9] <- "old_Avg_strays"
stray_dat2 <- left_join(stray_dat, new_response_var, by = c("StreamName", "Year"))
stray_dat3 <- stray_dat2[,c(1:3,20:22,10:19)] #remove some really old cols that
#I'm no longer using for anything
colnames(stray_dat3)[5] <- "Number_surveys"
stray_dat <- stray_dat3 #overwrite your former dataset
#Total_effective_strays is the sum of the effective number of strays from each
#individual survey in a stream in a year (and effective strays = raw # of
#strays / proportion of total dead count sampled; it accounts for bias in the
#response variable). The Avg_number_strays column is now the
#Total_effective_strays / Number_surveys

#Remove un-needed items
rm(stray_dat2, stray_dat3, new_response_var, delete, Master_dataset)


### Final output from this script:
stray_dat #dataset to use for modeling. Read into EDA script next

#Also save as RDS to be pulled into data vis scripts later on:
saveRDS(stray_dat, "output/stray_dat.rds")


