#Chapter 2 - Cognitive Task

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")

library(ggplot2)
library(BayesFactor)
library(mcmcr)
library(rethinking)

# we recommend running this in a fresh R session or restarting your current session
install.packages("cmdstanr", 
                 repos = c('https://stan-dev.r-universe.dev', 
                           getOption("repos")))

#load data
demo_and_bias_df = read.csv('../Data/demo_and_bias_df.csv')
correctTrials = read.csv('../Data/correctTrials.csv')
incorrectTrials = read.csv('../Data/incorrectTrials.csv')
df4 = read.csv('../Data/df4.csv')

# Subset of column names to convert to factors
cols_to_convert <- c( "Participant.Public.ID",  
                      "switch",                              
                      "Current.U.S.state.of.residence",      
                      "Employment.Status",                    
                      "Ethnicity",                           
                      "Highest.education.level.completed",    
                      "Sex" )

# Convert subset of columns to factors using lapply
demo_and_bias_df[cols_to_convert] <- lapply(demo_and_bias_df[cols_to_convert], 
                                            factor)

cols_to_convert_2 = c( "Participant.Public.ID", "switch", "Task_type")

correctTrials[cols_to_convert_2] <- lapply(correctTrials[cols_to_convert_2], 
                                           factor)

incorrectTrials[cols_to_convert_2] <- lapply(incorrectTrials[cols_to_convert_2], 
                                             factor)

#------------------------------------------------------------------------------------------------

#linear model beta coefficients

#convert $postDecisionEvidenceStrength to factor

correctTrials$postDecisionEvidenceStrength = factor(correctTrials$postDecisionEvidenceStrength)

incorrectTrials$postDecisionEvidenceStrength = factor(incorrectTrials$postDecisionEvidenceStrength)

#split by group

correctTrialsSwitchers = subset(correctTrials, correctTrials$switch == 1 )

correctTrialsStickers = subset(correctTrials, correctTrials$switch == 0)

incorrectTrialsSwitchers = subset(incorrectTrials, incorrectTrials$switch == 1 )

incorrectTrialsStickers = subset(incorrectTrials, incorrectTrials$switch == 0)

#---------------------------------

# confirmatory evidence integration

#switchers
CB_Switchers = lm(correctTrialsSwitchers$confidenceRating ~
                    correctTrialsSwitchers$postDecisionEvidenceStrength, 
                  data = correctTrialsSwitchers)

summary(CB_Switchers)

CB_Switchers_beta = CB_Switchers$coefficients[3]

print(CB_Switchers_beta)

#stickers
CB_Stickers = lm(correctTrialsStickers$confidenceRating ~ 
                   correctTrialsStickers$postDecisionEvidenceStrength, 
                 data = correctTrialsStickers)

summary(CB_Stickers)

CB_Stickers_beta = CB_Stickers$coefficients[3]

print(CB_Stickers_beta)

#all
CB_all = lm(correctTrials$confidenceRating ~ 
              correctTrials$postDecisionEvidenceStrength, 
            data = correctTrials)

summary(CB_all)

CB_all_beta = CB_all$coefficients[3]

print(CB_all_beta)

#disconfirmatory evidence integration. The beta sign on this one is reversed 
#so that higher values indicate greater disconfirmatory evidence integration

#switchers
DCB_Switchers = lm(incorrectTrialsSwitchers$confidenceRating ~ 
                     incorrectTrialsSwitchers$postDecisionEvidenceStrength, 
                   data = incorrectTrialsSwitchers)

summary(DCB_Switchers)

DCB_Switchers_beta = -DCB_Switchers$coefficients[3]

print(DCB_Switchers_beta)

#stickers
DCB_Stickers = lm(incorrectTrialsStickers$confidenceRating ~ 
                    incorrectTrialsStickers$postDecisionEvidenceStrength, 
                  data = incorrectTrialsStickers)

summary(DCB_Stickers)

DCB_Stickers_beta = -DCB_Stickers$coefficients[3]

print(DCB_Stickers_beta)

#all
DCB_all = lm(incorrectTrials$confidenceRating ~ 
               incorrectTrials$postDecisionEvidenceStrength, 
             data = incorrectTrials)

DCB_all_beta = -DCB_all$coefficients[3]

print(DCB_all_beta)

#t-tests to see if the differences between switchers and stickers are significant

demo_and_bias_df_switchers = subset(demo_and_bias_df, demo_and_bias_df$switch == '1')

demo_and_bias_df_stickers = subset(demo_and_bias_df, demo_and_bias_df$switch == '0')

t.test(demo_and_bias_df_switchers$Disconfirmatory.evidence.integration,
       demo_and_bias_df_stickers$Disconfirmatory.evidence.integration)

t.test(demo_and_bias_df_switchers$Confirmatory.evidence.integration,
       demo_and_bias_df_stickers$Confirmatory.evidence.integration)

# run Bayesian t-test to garner evidence in support of the null results

ttestBF(formula = Disconfirmatory.evidence.integration ~ switch,
        data = demo_and_bias_df)

ttestBF(formula = Confirmatory.evidence.integration ~ switch,
        data = demo_and_bias_df)

#--------------------------------------------------------------------------------------------

#logistic regressions

#here we test our hypotheses and some exploratory analyses

#does disconfirmation bias (rejection of disconfirmatory evidence) 
#predict whether someone is going to switch or not?

simple_model = glm(switch ~ Disconfirmatory.evidence.integration,
                   demo_and_bias_df,
                   family=binomial (link = "logit"))

summary(simple_model)

full_model = glm(switch ~ Disconfirmatory.evidence.integration
                 + Confirmatory.evidence.integration
                 + cons_score
                 + auth_score
                 + Sex
                 + Employment.Status
                 + Highest.education.level.completed
                 + Ethnicity
                 + age,
                 demo_and_bias_df,
                 family=binomial (link = "logit"))

summary(full_model)

#do people high in auth update their beliefs less?
auth_confidence_updating = lm(Disconfirmatory.evidence.integration ~ auth_score,
                              demo_and_bias_df)

summary(auth_confidence_updating)

#extract p-value for individual predictors
summary(full_model)$coefficients[2,4]

#extract z-value from individual predictors
summary(full_model)$coefficients[1,3]

#model without disc or conf bias

nobias_model = glm(switch ~ cons_score
                   + auth_score
                   + Sex
                   + Employment.Status
                   + Highest.education.level.completed
                   + Ethnicity
                   + age,
                   demo_and_bias_df,
                   family=binomial (link = "logit"))

summary(nobias_model)

#model with auth and socio-demographic variables

auth_model = glm(switch ~ auth_score
                 + Sex
                 + Employment.Status
                 + Highest.education.level.completed
                 + Ethnicity
                 + age,
                 demo_and_bias_df,
                 family=binomial (link = "logit"))

summary(auth_model)

#model with conservatism and socio-demographic variables

conservatism_model = glm(switch ~ cons_score
                         + Sex
                         + Employment.Status
                         + Highest.education.level.completed
                         + Ethnicity
                         + age,
                         demo_and_bias_df,
                         family=binomial (link = "logit"))

summary(conservatism_model)


#model with confirmation bias as sole predictor

simple_model = glm(switch ~ Confirmatory.evidence.integration,
                   demo_and_bias_df,
                   family=binomial(link = "logit"))

summary(simple_model)


glm_disconfirmatory = glm(switch ~ Disconfirmatory.evidence.integration
                          + cons_score
                          + auth_score
                          + Sex
                          + Employment.Status
                          + Highest.education.level.completed
                          + Ethnicity
                          + age,
                          demo_and_bias_df,
                          family=binomial (link = "logit"))

summary(glm_disconfirmatory)


#disconf bias + demographic

disconfirmatory_model = glm(switch ~ Disconfirmatory.evidence.integration
                            + Sex
                            + Employment.Status
                            + Highest.education.level.completed
                            + Ethnicity
                            + age,
                            demo_and_bias_df,
                            family=binomial (link = "logit"))

summary(disconfirmatory_model)




glm_confirmatory = glm(switch ~ Confirmatory.evidence.integration
                       + cons_score
                       + auth_score
                       + Sex
                       + Employment.Status
                       + Highest.education.level.completed
                       + Ethnicity
                       + age,
                       demo_and_bias_df,
                       family=binomial (link = "logit"))

summary(glm_confirmatory)



#model with confirmation bias + socio-demographic variables

confirmatory_model = glm(switch ~ Confirmatory.evidence.integration
                         + Sex
                         + Employment.Status
                         + Highest.education.level.completed
                         + Ethnicity
                         + age,
                         demo_and_bias_df,
                         family=binomial (link = "logit"))

summary(confirmatory_model)



#model with only demographic variables

demo_model = glm(switch ~ Sex
                 + Employment.Status
                 + Highest.education.level.completed
                 + Ethnicity
                 + age,
                 demo_and_bias_df,
                 family=binomial (link = "logit"))

summary(demo_model)

#create table 2 (Chapter 2, p. 97 of the thesis)

table <- huxtable::huxreg(full_model, glm_disconfirmatory, glm_confirmatory,
                          coefs = c( 'Disconfirmation bias' = 'Disconfirmatory.evidence.integration',
                                     'Confirmation bias' = 'Confirmatory.evidence.integration',
                                     'Conservatism' = 'cons_score',
                                     'auth' = 'auth_score',
                                     'Sex' = 'SexMale',
                                     'Employment status: Part time' = 'Employment.StatusPart-Time',                                                  
                                     'Employment status: Other' = 'Employment.StatusOther',                                                        
                                     'Employment status: Unemployed' = 'Employment.StatusUnemployed (and job seeking)',                                 
                                     'Employment status: Due to start a new job' = 'Employment.StatusDue to start a new job within the next month',
                                     'Education: Technical/community college' = 'Highest.education.level.completedTechnical/community college',                  
                                     'Education: Undergraduate degree' = 'Highest.education.level.completedUndergraduate degree (BA/BSc/other)',        
                                     'Education: PhD' = 'Highest.education.level.completedDoctorate degree (PhD/other)',     
                                     'Education: Graduate degree' = 'Highest.education.level.completedGraduate degree (MA/MSc/MPhil/other)',
                                     'Education: No formal qualifications' = 'Highest.education.level.completedNo formal qualifications',
                                     'Ethnicity' = 'Ethnicity',                                             
                                     'Age' = 'age'),
                          statistics = NULL)

quick_docx(table, file = "huxtable-output.docx", borders = 0.4,
           open = interactive())


#-----------------------------------------------------------------------------------------------------------


#likelihood ratio tests

#full model vs demo model

lrt_full_model = anova(full_model,demo_model,test="Chisq")

full_model_deviance = round(lrt_full_model$Deviance[2], digits = 2)
full_model_df = lrt_full_model$Df[2]

#model with disc bias vs demo model

lrt_disconfirmatory_model = anova(disconfirmatory_model, demo_model, test = 'Chisq')

disconfirmatory_model_deviance = round(lrt_disconfirmatory_model$Deviance[2], digits = 2)
disconfirmatory_model_df = lrt_disconfirmatory_model$Df[2]

#model with conf bias vs demo model

lrt_confirmatory_model = anova(confirmatory_model, demo_model, test = 'Chisq')

confirmatory_model_deviance = round(lrt_confirmatory_model$Deviance[2], digits = 2)
confirmatory_model_df = lrt_confirmatory_model$Df[2]

#model with auth and conservatism vs model with only demo

lrt_nobias_model = anova(nobias_model, demo_model , test = 'Chisq')

nobias_model_deviance = round(lrt_nobias_model$Deviance[2], digits = 2)
nobias_model_df = lrt_nobias_model$Df[2]

#model with auth vs demo model

lrt_auth_model = anova(auth_model,demo_model,test = "Chisq")

auth_model_deviance = round(lrt_auth_model$Deviance[2], digits = 2)
auth_model_df = lrt_auth_model$Df[2]

#model with conservatism vs demo model

lrt_conservatism_model = anova(conservatism_model,demo_model, test = 'Chisq')

conservatism_model_deviance = round(lrt_conservatism_model$Deviance[2], digits = 2)
conservatism_model_df = lrt_conservatism_model$Df[2]


#-----------------------------------------------------------------------------------------------------------

#cross-pressured voters - quadrants

mean(demo_and_bias_df_stickers$auth_score)
mean(demo_and_bias_df_switchers$auth_score)

mean(demo_and_bias_df_stickers$cons_score)
mean(demo_and_bias_df_switchers$cons_score)

# Define custom labels for the legend
switch_labels <- c("Trump > Trump", "Trump > Biden")

# Plotting
quadrant = ggplot(data = demo_and_bias_df) + 
  aes(x = cons_score, y = auth_score, colour = switch) +  # Convert switch to factor to apply custom labels
  geom_jitter() + 
  geom_vline(xintercept = 10.35882) + 
  geom_hline(yintercept = 3.264706) +
  xlab('Conservatism') + 
  ylab('auth') + 
  theme_classic() +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),   # Custom colors for 0 and 1
                     labels = switch_labels)  # Custom labels for the legend

ggsave("quadrant.pdf", plot = quadrant)


#create subset with only the distinct group of switchers high in conservatism and low
#in authoritarianism to see if they present any distinct traits
switchers_low_auth_high_cons = subset(demo_and_bias_df_switchers, 
                                      demo_and_bias_df_switchers$cons_score > 20
                                      & demo_and_bias_df_switchers$auth_score < 3.264706)


#predict function
#adjust full_model so that there are no categorical predictors
#as predict() can't run with them

full_model2 = glm(switch ~ Disconfirmatory.evidence.integration
                  + Confirmatory.evidence.integration
                  + cons_score
                  + auth_score
                  + age,
                  demo_and_bias_df,
                  family=binomial (link = "logit"))

summary(full_model2)

#probability of switching for voters low in cons_score

newdata_cons = data.frame(Disconfirmatory.evidence.integration = mean(demo_and_bias_df$Disconfirmatory.evidence.integration, na.rm = TRUE), 
                          Confirmatory.evidence.integration = mean(demo_and_bias_df$Confirmatory.evidence.integration, na.rm = TRUE),
                          cons_score = 1,
                          auth_score = mean(demo_and_bias_df$auth_score, na.rm = TRUE),
                          age = mean(demo_and_bias_df$age, na.rm = TRUE)) 

newdata_auth = data.frame(Disconfirmatory.evidence.integration = mean(demo_and_bias_df$Disconfirmatory.evidence.integration, na.rm = TRUE), 
                          Confirmatory.evidence.integration = mean(demo_and_bias_df$Confirmatory.evidence.integration, na.rm = TRUE),
                          cons_score = mean(demo_and_bias_df$cons_score, na.rm = TRUE),
                          auth_score = 1,
                          age = mean(demo_and_bias_df$age, na.rm = TRUE))

predict(full_model2, newdata_cons, type = "response")
predict(full_model2, newdata_auth, type = "response")



#-----------------------------------------------------------------------------------------------------------


# METACOGNITIVE SENSITIVITY

#the below code was borrowed and adapted from a tutorial by Audrey Mazancieux, PhD
#source: https://github.com/metacoglab/HMeta-d/blob/master/R/example_metad_group.R

source('Function_trials2counts.R')  

#compare the metacognitive sensitivity of Switchers and Stickers
#following Audrey Mazancieux's tutorial step-by-step
#create the counts for each participant with trials2counts()

#the problem here was that one of the variables -rating-
#was of length = 0 because there was a mistake
#in the variable name: you were doing
#df2$confidence_rating which is the old varname
#when the correct one is df2$confidenceRating

#error found by looking up the lengths of these inputs 
#as the error message said that one of them was of length = 0

dfSwitchers = subset(df4, df4$switch == 1)
dfStickers = subset(df4, df4$switch == 0)

stimIDSwitchers =    dfSwitchers$StimID
responseSwitchers =  dfSwitchers$Response
ratingSwitchers =    dfSwitchers$confidenceRating
nRatings = as.numeric(9)

stimIDStickers =    dfStickers$StimID
responseStickers =  dfStickers$Response
ratingStickers =    dfStickers$confidenceRating
nRatings = as.numeric(9)

newlistSwitchers = trials2counts(stimIDSwitchers,responseSwitchers,ratingSwitchers,nRatings)

newlistStickers = trials2counts(stimIDStickers,responseStickers,ratingStickers,nRatings)

nR_S1_switchers = newlistSwitchers[1]
nR_S2_switchers = newlistSwitchers[2]

nR_S1_stickers = newlistStickers[1]
nR_S2_stickers = newlistStickers[2]

#convert the nR_S1 and nR_S2 lists to dataframes so that metad_group can run on them
#otherwise you'll get the "Error: $ operator is invalid for atomic vectors"
nR_S1_switchers <- list(data.frame(nR_S1_switchers))
nR_S2_switchers <- list(data.frame(nR_S2_switchers))

nR_S1_stickers <- list(data.frame(nR_S1_stickers))
nR_S2_stickers <- list(data.frame(nR_S2_stickers))

#fit the data and pass it to metad_group()
source("Function_metad_group.R")

output_switchers <- metad_group(nR_S1 = nR_S1_switchers, nR_S2 = nR_S2_switchers)

output_stickers = metad_group(nR_S1 = nR_S1_stickers, nR_S2 = nR_S2_stickers)

summary(output_stickers)
summary(output_switchers)

# Values 
Value_stickers <- summary(output_stickers)
stat_stickers <- data.frame(mean = Value_stickers$statistics[,"Mean"])
stat_stickers %<>%
  rownames_to_column(var = "name")

Value_switchers <- summary(output_switchers)
stat_switchers <- data.frame(mean = Value_switchers$statistics[,"Mean"])
stat_switchers %<>%
  rownames_to_column(var = "name")

# Rhat 
Value_stickers <- gelman.diag(output_stickers, confidence = 0.95)
Rhat_stickers <- data.frame(conv = Value_stickers$psrf)

Value_switchers <- gelman.diag(output_switchers, confidence = 0.95)
Rhat_switchers <- data.frame(conv = Value_switchers$psrf)

# HDI 
HDI_stickers <- data.frame(HPDinterval(output_stickers, prob = 0.95))
HDI_stickers %<>%
  rownames_to_column(var = "name")

HDI_switchers <- data.frame(HPDinterval(output_switchers, prob = 0.95))
HDI_switchers   %<>%
  rownames_to_column(var = "name")

# All values in the same dataframe
Fit_stickers <- stat_stickers %>%
  cbind(lower = HDI_stickers$lower,
        upper = HDI_stickers$upper,
        Rhat = Rhat_stickers[,1])

Fit_switchers <- stat_switchers %>%
  cbind(lower = HDI_switchers$lower,
        upper = HDI_switchers$upper,
        Rhat = Rhat_switchers[,1])

# Plot trace mcmc
traceplot(output_stickers)
traceplot(output_switchers)

# store mcmc values in df for plot posterior distributions
mcmc.sample_stickers <- ggs(output_stickers)
mcmc.sample_switchers <- ggs(output_switchers)

# Plot posterior distribution for mu Mratio value

Mratio_plot_stickers <- mcmc.sample_stickers %>%
  filter(Parameter == "mu_logMratio") %>% 
  ggplot(aes(exp(value))) +
  xlim(0,2.5)+
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = exp(stat_stickers$mean[stat_stickers$name == "mu_logMratio"]),linetype="dashed", size = 1.5) +
  geom_segment(aes(x = exp(HDI_stickers$lower[HDI_stickers$name == "mu_logMratio"]), y = 50, 
                   xend = exp(HDI_stickers$upper[HDI_stickers$name == "mu_logMratio"]), yend = 50), colour = "white", linewidth = 2.5) +
  ylab("Sample count") +
  xlab(expression(paste(mu , "log meta-d'/d'"))) #+
# ggtitle("Stickers - mu_logMratio")

Mratio_plot_switchers <- mcmc.sample_switchers %>%
  filter(Parameter == "mu_logMratio") %>% 
  ggplot(aes(exp(value))) +
  xlim(0,2.5)+
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = exp(stat_switchers$mean[stat_switchers$name == "mu_logMratio"]),linetype="dashed", size = 1.5) +
  geom_segment(aes(x = exp(HDI_switchers$lower[HDI_switchers$name == "mu_logMratio"]), y = 50, 
                   xend = exp(HDI_switchers$upper[HDI_switchers$name == "mu_logMratio"]), yend = 50), colour = "white", size = 2.5) +
  ylab("Sample count") +
  xlab(expression(paste(mu , "log meta-d'/d'"))) #+
#ggtitle("Switchers - mu_logMratio")

Mratio_plot_stickers
Mratio_plot_switchers


#plotting the difference between the two posterior distributions of meta-d'

mu_logMratio_stickers <- numeric(0)
mu_logMratio_switchers <- numeric(0)

# Extract 'mu_logMratio' from each 'mcmc' object in large_mcmc_list_1
for (i in seq_along(output_stickers)) {
  mu_logMratio_stickers <- c(mu_logMratio_stickers, output_stickers[[i]][, "mu_logMratio"])
}

# Extract 'mu_logMratio' from each 'mcmc' object in large_mcmc_list_2
for (i in seq_along(output_switchers)) {
  mu_logMratio_switchers <- c(mu_logMratio_switchers, output_switchers[[i]][, "mu_logMratio"])
}

output_difference = mu_logMratio_stickers - mu_logMratio_switchers

# Create a data frame for ggplot
df_output_difference <- data.frame(output_difference)

#create df with all posteriors for both switchers and stickers

mu_logMratio_stickers_df = as.data.frame(mu_logMratio_stickers)

mu_logMratio_stickers_df = mu_logMratio_stickers_df %>%
  mutate(switch = 0)

mu_logMratio_switchers_df = as.data.frame(mu_logMratio_switchers)

mu_logMratio_switchers_df = mu_logMratio_switchers_df %>%
  mutate(switch = 1)

#have same colname for value mu_logMratio column
colnames(mu_logMratio_stickers_df) = c('mu_logMratio','switch')
colnames(mu_logMratio_switchers_df) = c('mu_logMratio','switch')

mu_logMratio_df = rbind(mu_logMratio_stickers_df, mu_logMratio_switchers_df)

mu_logMratio_df$switch = as.factor(mu_logMratio_df$switch)

# Plot the histogram with a horizontal error bar for the 95% confidence interval

ggplot(df_output_difference, aes(x=output_difference)) +
  geom_histogram(color="black", fill="blue") +
  geom_rect(aes(xmin=-1.203485, xmax=1.322692, ymin=0, ymax=2000), fill="white", alpha=0.5) +
  ylab("Posterior frequency") +
  xlab('Difference between groups')+
  theme(axis.title.x = element_text(size=10)) +
  theme(axis.title.y = element_text(size=10))

HPDI(output_difference, prob = 0.95)
