#Chapter 2 - Cognitive Task Data Processing

#the following code processes the output of the cognitive task
#developed by Stephen Fleming and Max Rollwage
#the following functions were developed by Audrey Mazancieux 
#and Oliver Warrington based on Stephen Fleming's stan functions:

#Function_trials2counts.R
#Function_metad_group.R

library(magrittr)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(corrplot)

#--------------------------------------------------------------------------------------------------------------

#DATA PRE-PROCESSING

#create df with all of the participants' raw experimental data together, switchers and stickers

stickersv1 = read.csv('../Data/Stickers v1/data_exp_70615-v1_task-6ybr.csv')

stickersv2 = read.csv('../Data/Stickers v2/data_exp_70615-v2_task-6ybr.csv')

stickersv3 = read.csv('../Data/Stickers v3/data_exp_70615-v3_task-6ybr.csv')

stickers = full_join(stickersv1, stickersv2)
stickers = full_join(stickers, stickersv3)

#create column 'switch' with value '0' to indicate that participant did not switch

stickers = stickers %>%
  mutate(switch = 0) 

#exclude rows of participant 5249280 who made an incomplete submission
stickers = stickers[!(stickers$Participant.Private.ID=='5249280'),]

switchersv1 = read.csv('../Data/Switchers v1/data_exp_49341-v1_task-6ybr.csv')

switchersv2 = read.csv('../Data/Switchers v2/data_exp_49341-v2_task-6ybr.csv')

switchersv3 = read.csv('../Data/Switchers v3/data_exp_78465-v3_task-6ybr.csv')

switchers = full_join(switchersv1, switchersv2)
switchers = full_join(switchers,switchersv3)

#create column 'switch' with value '1' to indicate that participants switched

switchers = switchers %>%
  mutate(switch = 1) 

rawData = full_join(switchers, stickers)

#keep only participants who completed the experiment, 
#i.e. remove those who are still 'live'
rawData = subset(rawData, rawData$Participant.Status=='complete')

#only keep participants who passed the exclusion criteria

#invalid submissions to remove from the main dataframe
allInvalidSubmissions = read.csv('../Data/all_invalid_submissions.csv')
allInvalidSubmissions = as.character(allInvalidSubmissions$x)

#exclude all the rows that correspond to the Participant.Public.ID values
#included in the list that follows %in%

rawData = rawData[! rawData$Participant.Public.ID %in% allInvalidSubmissions,]

length(levels(rawData$Participant.Public.ID))

#delete empty factor levels, i.e. the participant IDs whose rows were
#excluded in the lines above
rawData$Participant.Public.ID = factor(rawData$Participant.Public.ID, exclude = NULL)

#check that the empty factor levels have been removed
length(levels(rawData$Participant.Public.ID))

#----------------------------------------------------------------------------------------------------------------------------

#load demographic data

demoStickers = read.csv('../Data/Demographic data/Stickers_demo.csv')
demoSwitchers =read.csv('../Data/Demographic data/Switchers.csv')

#keep only approved submissions 

demoStickers = subset(demoStickers, demoStickers$status == 'APPROVED')

demoSwitchers = subset(demoSwitchers, demoSwitchers$status =='APPROVED')

#create column 'switch' indicating if participant switched or not

demoStickers = demoStickers %>%
  mutate(switch = 0)

demoSwitchers = demoSwitchers %>%
  mutate(switch = 1)

#merge stickers and switchers dfs

demoAll = full_join(demoStickers,demoSwitchers)

#keep only columns of interest

demoAll = dplyr::select(demoAll,
                        participant_id,
                        switch,
                        age, 
                        Current.U.S.state.of.residence, 
                        Employment.Status, 
                        Ethnicity, 
                        Highest.education.level.completed, 
                        Sex,
                        time_taken)

#set time taken in minutes rather than in seconds
demoAll$time_taken = (demoAll$time_taken/60)

#remove invalid submissions   
demoAll = demoAll[! demoAll$participant_id %in% allInvalidSubmissions,]

#remove empty factors corresponding to those invalid submissions
demoAll$participant_id = factor(demoAll$participant_id)

#change the column name of participant_id to Participant.Public.ID so that you
#can merge them together afterwards
names(demoAll)[names(demoAll)=='participant_id'] = 'Participant.Public.ID'

#check that you have the same IDs in the Prolific and Gorilla dfs

prolificIDs = demoAll$Participant.Public.ID

gorillaIDs = levels(rawData$Participant.Public.ID)

intersect(prolificIDs,gorillaIDs)

setdiff(prolificIDs,gorillaIDs)

#check if participants pass the exclusion criteria in "Exclusion criteria script"


#--------------------------------------------------------------------------------------------------------------------

#select relevant columns

myData = dplyr::select(rawData,
                       Participant.Private.ID,
                       Participant.Public.ID,
                       time_elapsed,
                       Reactiontime,
                       responses,
                       confidence_rating,
                       key_press,
                       correct,
                       trial_type,
                       Trial_real_experiment,
                       Task_type,
                       current_trial_difficuly,
                       label,
                       switch)

#recode the $correct column so that the incorrect trials are '0' rather than 'NA'
myData$correct = tidyr::replace_na(myData$correct, 0)

#convert Participant.Public.ID to factor to loop through factor levels, 
#i.e. through each participant 
myData$Participant.Public.ID = as.factor(myData$Participant.Public.ID)

df2 = subset(myData,
             myData$Task_type == 'simpleperceptual' |
             myData$Task_type == 'Change_of_mind')

df2$responses = factor(df2$responses)

#remove rows corresponding to the participants practicing how to slide 
#the confidence rating scale
df2 = df2[!df2$label=='confidprac',]

#create new variable "Direction", to say if the stim with more 
#flickering dots was on the left or on the right

df2 = df2 %>%
  mutate(Direction = case_when(df2$key_press == '87' & df2$correct == 1 ~ 'Left',
                               df2$key_press == '87' & df2$correct == 0 ~ 'Right',
                               df2$key_press == '69' & df2$correct == 1 ~ 'Right',
                               df2$key_press == '69' & df2$correct == 0 ~ "Left"))

#create StimID variable with Left as 0 and Right as 1
df2 = df2 %>%
  mutate(StimID = case_when(df2$Direction == "Left" ~ 0,
                            df2$Direction == "Right" ~ 1))

#create 'Response' column indicating whether each participant 
#responded "Left" or "Right" in the key_press variable 
#87 = Left = 0 
#69 = Right = 1

df2 = df2 %>%
  mutate(Response = case_when(df2$key_press == '87' ~ 0,
                              df2$key_press == '69' ~ 1))

df2 = df2 %>%
  mutate(confidenceRating = case_when(df2$responses == '{"Q0":1}' ~ 1,
                                      df2$responses == '{"Q0":2}' ~ 2,
                                      df2$responses == '{"Q0":3}' ~ 3,
                                      df2$responses == '{"Q0":4}' ~ 4,
                                      df2$responses == '{"Q0":5}' ~ 5,
                                      df2$responses == '{"Q0":6}' ~ 6,
                                      df2$responses == '{"Q0":7}' ~ 7,
                                      df2$responses == '{"Q0":8}' ~ 8,
                                      df2$responses == '{"Q0":9}' ~ 9))

#find column numbers to create smaller df
which(colnames(df2)=='StimID')
which(colnames(df2)=='Response')
which(colnames(df2)=='confidenceRating')
which(colnames(df2)=='Participant.Public.ID')
which(colnames(df2)=='switch')

#create dataframe where the StimID, Response and confidence_ratings rows are aligned  

#keep only the columns that you'll need to create the inputs to trials2count()
#i.e. $StimID, $Response and $confidenceRating
df3 = df2[,c(16,17,18,2,14) ]

#align the confidence ratings with the other columns   

for (row in 1: length(df3$confidenceRating)) { #loop from 1 through the rest of the column
  if (row %% 2 == 1) { df3$confidenceRating[row] = df3$confidenceRating[row+1]} # if the remainder of the division is 1, it indicates that the row number was odd 
  if (row %% 2 == 0) { df3$confidenceRating[row] = NA} #above, we assign to each odd-numbered row the value of the following row
  
}

df4 = na.omit(df3) 

write.csv(df4, '../Data/df4.csv', row.names = FALSE)

#INPUTS to trials2counts()

stimID =    df4$StimID
response =  df4$Response
rating =    df4$confidenceRating
nRatings = as.numeric(9)

source('Function_trials2counts.R')

newlist = trials2counts(stimID,response,rating,nRatings)
print(newlist)

#-----------------------------------------------------------------------------

#measuring confirmation bias, i.e. post-decision evidence integration

#you need to do a few things before being able to do this.

#We measured confirmatory and disconfirmatory evidence integration 
# as changes in confidence induced by post-decision evidence. We constructed 
# trial-by-trial linear models for every participant, separately 
# for correct and incorrect trials across data pooled across Tasks 1 and 2, 
# using post-decision evidence strength as a predictor 
# (confidence task = 0, low post-decision evidence = 1, high post-decision evidence = 2) 
# and confidence ratings as the dependent variable. 
# Individual beta weights for correct trials, indicating increases of confidence 
# due to post-decision evidence, were estimated as measures of 
# confirmatory evidence integration. Disconfirmatory evidence integration 
# was estimated as the beta weight on incorrect trials 
# (we reversed the sign of this beta weight in the figures such that 
#   higher values indicate greater disconfirmatory evidence integration).


#create df with only correct trials
correctTrials = subset(df2, df2$correct == 1) #subset the df into correct trials
correctTrials = subset(correctTrials, correctTrials$label == 'confidencerating') #keep only trials where label == confidencerating

correctTrials = correctTrials %>%
  mutate(postDecisionEvidenceStrength = case_when(correctTrials$current_trial_difficuly == '1.3' ~ 1,
                                                  correctTrials$current_trial_difficuly == '1' ~ 2,
                                                  correctTrials$Task_type == 'simpleperceptual' ~ 0))

#create df with only incorrect trials
incorrectTrials = subset(df2, df2$correct == 0) #subset the df into incorrect trials
incorrectTrials = subset(incorrectTrials, incorrectTrials$label == 'confidencerating') #keep only trials where label == confidencerating

incorrectTrials = incorrectTrials %>%
  mutate(postDecisionEvidenceStrength = case_when(incorrectTrials$current_trial_difficuly == '1.3' ~ 1,
                                                  incorrectTrials$current_trial_difficuly == '1' ~ 2,
                                                  incorrectTrials$Task_type == 'simpleperceptual' ~ 0))

correctTrials

write.csv(correctTrials, '../Data/correctTrials.csv', row.names = FALSE)

write.csv(incorrectTrials, '../Data/incorrectTrials.csv', row.names = FALSE)


#-----------------------------------------------------------------------------------------------------------

#calculate individual disconfirmation bias scores 
#(rejection of disconfirmatory evidence) automatically

discIntegration = ''

for (participant in levels(incorrectTrials$Participant.Public.ID)) {
  discDF = filter(incorrectTrials,Participant.Public.ID==participant)
  discIntegration[participant] = lm(discDF$confidenceRating ~ discDF$postDecisionEvidenceStrength, 
                                    data = discDF)
}

#drop first list item, which is empty
discIntegration = discIntegration[-1]

#convert list to dataframe
disconfirmatoryDF = data.frame(discIntegration)

#keep only coefficients, drop intercepts
disconfirmatoryDF = disconfirmatoryDF[2,]

#store coefficients as list, drop their labels
listDisconf = list(as.numeric(disconfirmatoryDF[1,]))

#create df with disconfirmatory evidence integrations scores and participant IDs
disconfirmatoryDF = data.frame(listDisconf,levels(incorrectTrials$Participant.Public.ID))

#change column names
colnames(disconfirmatoryDF) = c('Disconfirmatory evidence integration','Participant.Public.ID')

#change sign of disconfirmatory evidence integration so that higher values indicate
#greater integration of disconfirmatory evidence
disconfirmatoryDF$`Disconfirmatory evidence integration` = -disconfirmatoryDF$`Disconfirmatory evidence integration`

print(disconfirmatoryDF)


#----------------------------------------------------------------------------------------------------------------------------------------------

#calculate individual confirmation bias scores (endorsement of confirmatory evidence) automatically

confIntegration = ''

for (participant in levels(correctTrials$Participant.Public.ID)) {
  confDF = filter(correctTrials,Participant.Public.ID==participant)
  confIntegration[participant] = lm(confDF$confidenceRating ~ confDF$postDecisionEvidenceStrength, 
                                    data = confDF)
}

#drop first list item, which is empty
confIntegration = confIntegration[-1]

#convert list to dataframe
confirmatoryDF = data.frame(confIntegration)

#keep only coefficients, drop intercepts
confirmatoryDF = confirmatoryDF[2,]

#store coefficients as list, drop their labels
listConf = list(as.numeric(confirmatoryDF[1,]))

#create df with disconfirmatory evidence integrations scores and participant IDs
confirmatoryDF = data.frame(listConf,levels(correctTrials$Participant.Public.ID))

#change column names
colnames(confirmatoryDF) = c('Confirmatory evidence integration','Participant.Public.ID')

print(confirmatoryDF)

#merge confirmatoryDF and disconfirmatoryDF
merged_conf_bias = merge(disconfirmatoryDF,confirmatoryDF, by = "Participant.Public.ID")



#---------------------------------------------------------------------------------------------------------------------------------------------------



#merge df with confirmation and disconfirmation bias with 
#df with demographic data
demo_and_bias_df = merge(merged_conf_bias,demoAll, by = 'Participant.Public.ID')

#merge demo + conf bias df with values df (child-rearing values and economic-social values)
values = read.csv('../Data/values.csv')

#remove participants who revoked their consent for participation 
#in the task from the values df 
revoked_consent = c("5f869b703a15e41ee32f5b19", 
                    "60c17595bf53c3dff68487d3", 
                    "613b53e4038b14ab5bf1f889")

values = values[! values$Participant.Public.ID %in% revoked_consent,]

demo_and_bias_df = merge(demo_and_bias_df,values, by = 'Participant.Public.ID')

#remove the 3 participants who revoked their consent
demo_and_bias_df = subset(demo_and_bias_df, demo_and_bias_df$Sex != "CONSENT REVOKED")

# set DATA EXPIRED values in employment status and education to NA
demo_and_bias_df$Employment.Status = na_if(demo_and_bias_df$Employment.Status,"DATA EXPIRED")
demo_and_bias_df$Highest.education.level.completed = na_if(demo_and_bias_df$Highest.education.level.completed,
                                                           "DATA EXPIRED")  

#get rid of unused factor levels in categorical variables
demo_and_bias_df$Highest.education.level.completed = droplevels(demo_and_bias_df$Highest.education.level.completed)
demo_and_bias_df$Employment.Status = droplevels(demo_and_bias_df$Employment.Status)
demo_and_bias_df$Sex = droplevels(demo_and_bias_df$Sex)
demo_and_bias_df$Ethnicity = droplevels(demo_and_bias_df$Ethnicity)

#change reference level in the categorical variables

# set reference level to 'High school diploma/A-levels' for education
demo_and_bias_df$Highest.education.level.completed = relevel(demo_and_bias_df$Highest.education.level.completed,
                                                             'High school diploma/A-levels')
#employment status
demo_and_bias_df$Employment.Status = relevel(demo_and_bias_df$Employment.Status,
                                             "Full-Time")

#ethnicity
demo_and_bias_df$Ethnicity = relevel(demo_and_bias_df$Ethnicity,
                                     'White/Caucasian')

#restore the auth_score to row 215 where it is missing
demo_and_bias_df[215,13] = 1

write.csv(demo_and_bias_df,'../Data/demo_and_bias_df.csv', row.names = FALSE)
