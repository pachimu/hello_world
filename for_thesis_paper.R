library(openxlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(psych)
library(stringr)
library(lme4)
install.packages("broom") # only needed if you didn't ever install broom
install.packages("tidyr")
install.packages('effsize')
install.packages("esvis")
#install.packages("devtools")
devtools::install_github("DJAnderson07/esvis")

install.packages("lsr")
library(lsr)
library(tidyr)
library(broom)

library(ez)
library(effsize)
library(esvis)

#for confidence interval calculation

install.packages('Rmisc')
library(Rmisc)

#Loading accuracy file
CMT_acc_a <- read.xlsx("/Users/valentinabachurina/Desktop/ET for transfer/CMT_accuracy_all_adults_10_19.xlsx")

CMT_acc_a <- as.data.table(CMT_accuracy_by_level)

#rename 56
CMT_acc_a[RECORDING_SESSION_LABEL == "56nik26m"]$RECORDING_SESSION_LABEL <- "57nik26m"

#LOADING DATA
#ADULTS BLINKS AND SACCADES---------------------------------------------------------------------------------------------------------------------
#loading saccades data for adults only
ball_new_sac_a_with_baseline <- read.delim("/Users/valentinabachurina/Documents/october_adults_ball/Output/ml_saccades_balloons.txt")
clown_new_sac_a_with_baseline <- read.delim("/Users/valentinabachurina/Documents/october_clowns/Output/ml_saccades_clowns.txt")

clown_new_sac_a_with_baseline  <- as.data.table(clown_new_sac_a_with_baseline)
ball_new_sac_a_with_baseline <- as.data.table(ball_new_sac_a_with_baseline)

clown_new_sac_a_with_baseline<- clown_new_sac_a_with_baseline[, condition := "clowns"]
ball_new_sac_a_with_baseline <- ball_new_sac_a_with_baseline[, condition := "balloons"]

new_CMT_sac_with_baseline <- full_join(clown_new_sac_a_with_baseline,ball_new_sac_a_with_baseline)
new_CMT_sac_with_baseline <- as.data.table(new_CMT_sac_with_baseline)
new_CMT_sac <- new_CMT_sac_with_baseline[!(new_CMT_sac_with_baseline$expected == "any"),]

new_CMT_sac$demand <- ifelse(new_CMT_sac$condition == 'balloons', as.numeric(new_CMT_sac$level) + 1, as.numeric(new_CMT_sac$level) + 2)

new_CMT_sac <- new_CMT_sac[, blink := ifelse(CURRENT_SAC_CONTAINS_BLINK == "true", 1, 0)]

new_CMT_sac[RECORDING_SESSION_LABEL == "56nik26m"]$RECORDING_SESSION_LABEL <- "57nik26m"

correct_new_CMT_sac <- new_CMT_sac[(new_CMT_sac$ACCURACY == 1 ),]
correct_new_CMT_sac <- as.data.table(correct_new_CMT_sac)


#ONLY CORRECT TRIALS ARE USED FOR BLINKS AND SACCADES RATE
#RTs------------------------------------------------------------------------------
RT_CMT_subj_level <- correct_new_CMT_sac%>%
  group_by(RECORDING_SESSION_LABEL,level, condition, demand)%>%
  summarize_at(vars(RESPONSE_TIME),funs(mean, sd(., na.rm=TRUE)))

names(RT_CMT_subj_level) <- c("RECORDING_SESSION_LABEL","level", "condition","demand", "RT_mean", "RT_sd")

#BLINKS---------------------------------------------------------------------------
BLINKS_CMT_subj_level_trial <- correct_new_CMT_sac %>%
  group_by(RECORDING_SESSION_LABEL,level,condition,trial, demand)%>%
  summarize_at(vars(blink),funs(sum(., na.rm=TRUE)))

BLINKS_CMT_subj_level_trial <- as.data.table(BLINKS_CMT_subj_level_trial)
BLINKS_CMT_subj_level_trial$blink_per_sec <- BLINKS_CMT_subj_level_trial$blink/3

BLINKS_CMT_subj_level <- BLINKS_CMT_subj_level_trial %>%
  group_by(RECORDING_SESSION_LABEL,level,condition, demand)%>%
  summarize_at(vars(blink_per_sec),funs(mean(., na.rm=TRUE)))

#SACCADES
SACCADES_CMT_subj_level <- correct_new_CMT_sac%>%
  group_by(RECORDING_SESSION_LABEL,level, condition, demand)%>%
  summarize_at(vars(TRIAL_SACCADE_TOTAL, CURRENT_SAC_PEAK_VELOCITY),funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)))

#names(SACCADES_CMT_all_subj_level) <- c("RECORDING_SESSION_LABEL","level","condition", "group", "TRIAL_SACCADE_TOTAL_mean", "TRIAL_SACCADE_TOTAL_sd")


#make a full full table 

adults_CMT <- full_join(RT_CMT_subj_level,SACCADES_CMT_subj_level)
adults_CMT <- full_join(adults_CMT,BLINKS_CMT_subj_level)

#adults_CMT <- full_join(adults_CMT,trials_count)

adults_CMT <- full_join(adults_CMT, CMT_acc_a)
adults_CMT <- as.data.table(adults_CMT)

adults_CMT <- adults_CMT[!(is.na(RT_mean)),]

adults_CMT <- adults_CMT %>% 
  separate(RECORDING_SESSION_LABEL, into = c('RECORDING_SESSION_LABEL', 'gender'), sep = 7)

adults_CMT$accuracy <- adults_CMT$hits/28*100

adults_CMT <- adults_CMT[!(is.na(accuracy)),]


#M-scores
adults_CMT$passed <- ifelse(adults_CMT$accuracy <70, 0, 1)
m_scores_adults <- adults_CMT[passed == 1,]
m_scores_adults$level <- as.numeric(m_scores_adults$level)

m_scores_adults <- m_scores_adults%>%
  group_by(RECORDING_SESSION_LABEL, condition)%>%
  summarize_at(vars(level),funs(max(., na.rm=TRUE)))

m_scores_adults$m_score <- ifelse(m_scores_adults$condition == 'balloons', m_scores_adults$level +1,  m_scores_adults$level +2)
m_scores_adults$level_passed <- m_scores_adults$level
m_scores_adults$level <- NULL

adults_CMT <- full_join(adults_CMT, m_scores_adults)
adults_CMT <- as.data.table(adults_CMT)

#remove  08aid20f 55lil23f due to technical problems (very few eye-tracking trials)
adults_CMT <- adults_CMT[!(RECORDING_SESSION_LABEL == "08aid20f" | RECORDING_SESSION_LABEL == "55lil23f"),] 
#remove 06 and 23 (let's say same reason)
adults_CMT <- adults_CMT[!(RECORDING_SESSION_LABEL == "06ann20f" | RECORDING_SESSION_LABEL == "23ann18f"),] 
#4 people removed

#cleaning up the table 
adults_CMT$RT_sd <- NULL
adults_CMT$hits <- NULL
adults_CMT$misses <- NULL
adults_CMT$TRIAL_SACCADE_TOTAL_sd <- NULL
adults_CMT$passed <- NULL
adults_CMT$CURRENT_SAC_PEAK_VELOCITY_sd <- NULL
adults_CMT$group <- NULL

#DESCRIPTIVES AGE AND GENDER
adults_CMT <- adults_CMT %>% 
  separate(RECORDING_SESSION_LABEL, into = c('RECORDING_SESSION_LABEL', 'age'), sep = 5)
adults_CMT$age <- as.numeric(adults_CMT$age)

adults_CMT$gender[adults_CMT$RECORDING_SESSION_LABEL == "16eka"] <- 'f'

#ages
ages <- adults_CMT%>%
  summarize_at(vars(age),funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)))

age_genders <- adults_CMT%>%
  group_by(RECORDING_SESSION_LABEL, gender)%>% 
  summarize_at(vars(age),funs(mean(., na.rm=TRUE)))

aggregate(cbind(count = RECORDING_SESSION_LABEL) ~ gender, 
          data = age_genders, 
          FUN = function(x){NROW(x)})

#MEANS BY DEMAND

demand_summary <- adults_CMT%>%
  group_by(level, condition, demand)%>%
  summarize_at(vars(accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, CURRENT_SAC_PEAK_VELOCITY_mean),funs(mean, sd(., na.rm=TRUE)))


#create by subject everything
subject_summary <- adults_CMT%>%
  group_by(RECORDING_SESSION_LABEL)%>%
  summarize_at(vars(accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, CURRENT_SAC_PEAK_VELOCITY_mean, m_score),funs(mean(., na.rm=TRUE)))


#by condition by subject everything 
condition_subject_summary <- adults_CMT%>%
  group_by(RECORDING_SESSION_LABEL, condition)%>%
  summarize_at(vars(accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, m_score, CURRENT_SAC_PEAK_VELOCITY_mean),funs(mean(., na.rm=TRUE)))



#density plots for subject data
ggplot(subject_summary, aes(x=RT_mean)) + 
  geom_density()

ggplot(subject_summary, aes(x=accuracy)) + 
  geom_density()

ggplot(subject_summary, aes(x=blink_per_sec)) + 
  geom_density()

#PLOTS BY DEMAND

ggplot(demand_summary, aes(x = demand, y= blink_per_sec_mean, group= condition, color= condition)) +
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = blink_per_sec_mean - (blink_per_sec_sd/sqrt(92)), ymax = blink_per_sec_mean + (blink_per_sec_sd/sqrt(92))), 
                position=position_dodge(0.1)) +
  theme_minimal()+
  labs(y = "Number of blinks per second", x = "MA demand")+
  scale_x_continuous(breaks=c(2,3,4,5,6,7,8))


ggplot(demand_summary, aes(x = demand, y= accuracy_mean, group= condition, color= condition)) +
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = accuracy_mean - (accuracy_sd/sqrt(61)), ymax = accuracy_mean + accuracy_sd/sqrt(61)), 
                position=position_dodge(0.1)) +
  labs(y = "Percent of correct trials", x = "MA demand")+
  theme_minimal()+
  scale_x_continuous(breaks=c(2,3,4,5,6,7,8))

ggplot(demand_summary, aes(x = demand, y= RT_mean_mean, group= condition, color= condition)) +
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = RT_mean_mean - (RT_mean_sd/sqrt(61)), ymax = RT_mean_mean + RT_mean_sd/sqrt(61)), 
                position=position_dodge(0.1)) +
  theme_minimal()+
  labs(y = "RT (in msec)", x = "MA demand")+
  scale_x_continuous(breaks=c(2,3,4,5,6,7,8))


#saccades
ggplot(demand_summary, aes(x = demand, y=TRIAL_SACCADE_TOTAL_mean_mean, group= condition, color= condition)) +
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = TRIAL_SACCADE_TOTAL_mean_mean - (TRIAL_SACCADE_TOTAL_mean_sd/sqrt(61)), ymax = TRIAL_SACCADE_TOTAL_mean_mean + TRIAL_SACCADE_TOTAL_mean_sd/sqrt(61)), 
                position=position_dodge(0.1)) +
  labs(title = "Saccade rate by task demand", y = "Number of saccades per trial", x = "MA task demand") +
  theme_minimal()+
  scale_x_continuous(breaks=c(2,3,4,5,6,7,8))

#velocity
ggplot(demand_summary, aes(x = demand, y=CURRENT_SAC_PEAK_VELOCITY_mean_mean, group= condition, color= condition)) +
  geom_line() +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = CURRENT_SAC_PEAK_VELOCITY_mean_mean - (CURRENT_SAC_PEAK_VELOCITY_mean_sd/sqrt(61)), ymax = CURRENT_SAC_PEAK_VELOCITY_mean_mean + CURRENT_SAC_PEAK_VELOCITY_mean_sd/sqrt(61)), 
                position=position_dodge(0.1)) +
  labs(y = "Average trial peak saccade velocity ", x = "MA demand") +
  theme_minimal()+
  scale_x_continuous(breaks=c(2,3,4,5,6,7,8))


#analylysis - anovas and t-tests for effect of demand

#m_score effect of condition
avo_M_score <- aov(m_score ~ condition, data = condition_subject_summary)
summary(avo_M_score)
etaSquared(avo_demand_sacc)
#great, no significant difference, it would seem, which is as it should be


#need to do apa tables before moving on to look for stuff
demand_summary_accuracy_RT <- adults_CMT%>%
  group_by(demand, condition)%>%
  summarize_at(vars(accuracy,RT_mean, m_score),funs(round(mean(., na.rm=TRUE),2), round(sd(., na.rm=TRUE),2)))

demand_summary_saccades <- adults_CMT%>%
  group_by(condition, demand)%>%
  summarize_at(vars(TRIAL_SACCADE_TOTAL_mean,CURRENT_SAC_PEAK_VELOCITY_mean),funs(round(mean(., na.rm=TRUE),2), round(sd(., na.rm=TRUE),2)))

demand_summary_blink <- adults_CMT%>%
  group_by(condition, demand)%>%
  summarize_at(vars(blink_per_sec),funs(round(mean(., na.rm=TRUE),2), round(sd(., na.rm=TRUE),2)))

write.xlsx(demand_summary_saccades, "demand_summary_saccades.xlsx")
write.xlsx(demand_summary_blink, "demand_summary_blink.xlsx")

#repeated measures two-way ANOVA
#

#unfortunately, we'll have to take only five levels of demand (3,4,5,6,7), 
#because otherwise the data is unbalanced

adults_CMT_repeated <- adults_CMT[!(demand == 2 | demand == 8),]
adults_CMT_repeated$demand <- as.factor(adults_CMT_repeated$demand)

#remove 32ego, due to lack of clowns 
adults_CMT_repeated <- adults_CMT_repeated[!(RECORDING_SESSION_LABEL == "32ego"),]
#remove 57nik due to missing data (no idea how that happened)
adults_CMT_repeated <- adults_CMT_repeated[!(RECORDING_SESSION_LABEL == "57nik"),]

rep_aov_demand_acc <- ezANOVA(data = adults_CMT_repeated,
                              wid = RECORDING_SESSION_LABEL,
                              within = .(demand, condition),
                              dv = accuracy,
                              type = 3)

rep_aov_demand_acc

#intepretation notes - if Macuhly's test for spericity p-value returns LESS than 0.001 (!), then choose 
#the p-value from the corrections below - in this case te p[GG]

#okay, so for CMT-balloons effect of demand on accuracy was hghly significant, 
#repeated measures two-way ANOVA results: F(4,236) = 188.49, p < 0.001, ??2 = 0.42, 
#Greenhouse Geiser correction for sphericity on p. We found no effect of condition and no inreaction effects.

pairw <- tidy(pairwise.t.test(adults_CMT_repeated$accuracy, adults_CMT_repeated$demand, paired=TRUE, p.adjust.method="bonferroni"))
pairw$sig <- ifelse(pairw$p.value < 0.05, 1,0)

t.test(adults_CMT_repeated$accuracy[adults_CMT_repeated$demand == "3"], adults_CMT_repeated$accuracy[adults_CMT_repeated$demand == "4"], paired = TRUE)
cohen.d(adults_CMT_repeated$accuracy[adults_CMT_repeated$demand == "3"], adults_CMT_repeated$accuracy[adults_CMT_repeated$demand == "4"])

cohen.d(adults_CMT_repeated$accuracy[adults_CMT_repeated$demand == "3"], adults_CMT_repeated$accuracy[adults_CMT_repeated$demand == "4"], paired = TRUE)

adults_CMT_repeated <- as.data.table(adults_CMT_repeated)

??ohendz <- tidy(cohen.d(adults_CMT_repeated$accuracy, adults_CMT_repeated$accuracy, paired = TRUE))

hedg_g(adults_CMT_repeated, accuracy ~ demand) 

#okay, so. everything is significant for accuracy, as expected. No on to scary stuff

#velocity rep measures
rep_aov_demand_vel <- ezANOVA(data = adults_CMT_repeated,
                              wid = RECORDING_SESSION_LABEL,
                              within = .(demand, condition),
                              dv = CURRENT_SAC_PEAK_VELOCITY_mean,
                              type = 3)


rep_aov_demand_vel

pairw <- tidy(pairwise.t.test(adults_CMT_repeated$CURRENT_SAC_PEAK_VELOCITY_mean, adults_CMT_repeated$demand, paired=TRUE, p.adjust.method="bonferroni"))
pairw$sig <- ifelse(pairw$p.value < 0.05, 1,0)


#saccade rate rep measures
rep_aov_demand_sacc <- ezANOVA(data = adults_CMT_repeated,
                              wid = RECORDING_SESSION_LABEL,
                              within = .(demand, condition),
                              dv = TRIAL_SACCADE_TOTAL_mean,
                              type = 3)

rep_aov_demand_sacc

pairw <- tidy(pairwise.t.test(adults_CMT_repeated$TRIAL_SACCADE_TOTAL_mean, adults_CMT_repeated$demand, paired=TRUE, p.adjust.method="bonferroni"))
pairw$sig <- ifelse(pairw$p.value < 0.05, 1,0)


#blink rate rep measures
rep_aov_demand_blink <- ezANOVA(data = adults_CMT_repeated,
                               wid = RECORDING_SESSION_LABEL,
                               within = .(demand, condition),
                               dv = blink_per_sec,
                               type = 3)

rows <- count(adults_CMT_repeated, RECORDING_SESSION_LABEL)

rep_aov_demand_blink

pairw <- tidy(pairwise.t.test(adults_CMT_repeated$blink_per_sec, adults_CMT_repeated$demand, paired=TRUE, p.adjust.method="bonferroni"))
pairw$sig <- ifelse(pairw$p.value < 0.05, 1,0)

#RT rep measures
rep_aov_demand_RT <- ezANOVA(data = adults_CMT_repeated,
                                wid = RECORDING_SESSION_LABEL,
                                within = .(demand, condition),
                                dv = RT_mean,
                                type = 3)

rep_aov_demand_RT

pairw <- tidy(pairwise.t.test(adults_CMT_repeated$RT_mean, adults_CMT_repeated$demand, paired=TRUE, p.adjust.method="bonferroni"))
pairw$sig <- ifelse(pairw$p.value < 0.05, 1,0)

#An ???? = 0.0099 ??? 0.01 is a small effect. An ???? = 0.0588 ??? 0.06 is a medium effect. 
#And, an ???? = 0.1379 ??? 0.14 is a large effect.



#-----------------------------------------------------------------------------------------------------
#let's try to look for eye-movement perfromance correlations for one last time (hopefully)

#RT and eye-tracking by demand 

rt_velocity_CMT <- adults_CMT%>%
  group_by(demand)%>%
  do(tidy(cor.test(.$RT_mean, .$blink_per_sec, method = 'pearson')))

#nothign for velocity-RT (except for demand 5 and 7), nothing for saccade rate-rt, 
#nothing for blink rate and RT (except for demand 5 and 7). Can't really explain 5 and 7


#let's try accoutning for m-capacity

adults_capacity_top <- adults_CMT[level == level_passed,] #took only the last level that were within capacity
#assuming people performed at the top of their capacity here

#now let's try correlations by conditions
rt_ET <- adults_capacity_top%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$RT_mean, .$blink_per_sec, method = 'pearson')))
#very weak and not significant, but better than expected

#No correlations were found for RT and eye-tracking indices for individualised top-capacity performance
#code removed - to have less clutter

#accuracy
acc_ET <- adults_capacity_top%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$accuracy, .$blink_per_sec, method = 'pearson')))

#significant for blinks for clowns (not at all for balloons - but for clowns significant (effect is very weak though))
#the others were not significant


#now let's see if m-score has any correlation with eye-trackign indices at the top capacity level
m_score_ET <- adults_capacity_top%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$m_score, .$CURRENT_SAC_PEAK_VELOCITY_mean, method = 'pearson')))
#nope, nothing

#looked at by demand and at at the m_capacity limit
#no for subject means 

m_score_RT <- condition_subject_summary%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$m_score, .$RT_mean, method = 'pearson')))

#! accidentally found a fun thing - mean RT (to correct trials) very significantly negatively correlates with M-scores
#the higher the score the faster the RT - r is higher for clowns

#for eye-tracking
m_score_ET <- condition_subject_summary%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$m_score, .$blink_per_sec, method = 'pearson')))
#nothing for m-scores, blinks-velocity-sac

RT_ET <- condition_subject_summary%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$RT_mean, .$TRIAL_SACCADE_TOTAL_mean, method = 'pearson')))
#nothign for RT, blink-velocity-sac

acc_ET <- condition_subject_summary%>%
  group_by(condition)%>%
  do(tidy(cor.test(.$accuracy, .$CURRENT_SAC_PEAK_VELOCITY_mean, method = 'pearson')))
#nothing for accuracy, blink-velocity-sac

#finally, let's try looking at differences between passed and non-passed levels
adults_CMT$passed <- ifelse(!(adults_CMT$accuracy < 70), 1,0)

subject_summary_passed <- adults_CMT%>%
  group_by(RECORDING_SESSION_LABEL, passed)%>%
  summarize_at(vars(accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, CURRENT_SAC_PEAK_VELOCITY_mean, m_score),funs(mean(., na.rm=TRUE)))

#now we have a table with means for passed and failed levels (although some people don't have any failed)
run_cor <- subject_summary_passed %>%
  group_by(passed)%>%
  do(tidy(cor.test(.$RT_mean, .$CURRENT_SAC_PEAK_VELOCITY_mean, method = 'pearson')))

run_cor <- subject_summary_passed %>%
  group_by(passed)%>%
  do(tidy(cor.test(.$RT_mean, .$blink_per_sec, method = 'pearson')))

#Results:
#a significant correlation between RT and blink rate for failed levels (p = 0.03, r(53) = -0.3)
#a significant correlation between RT and peak saccade velocity for failed levels (p = 0.01, r(53) = -0.32)
#

subject_summary_passed_condition <- adults_CMT%>%
  group_by(RECORDING_SESSION_LABEL, passed, condition)%>%
  summarize_at(vars(accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, CURRENT_SAC_PEAK_VELOCITY_mean, m_score),funs(mean(., na.rm=TRUE)))

run_cor <- subject_summary_passed_condition %>%
  group_by(passed, condition)%>%
  do(tidy(cor.test(.$RT_mean, .$CURRENT_SAC_PEAK_VELOCITY_mean, method = 'pearson')))

run_cor <- subject_summary_passed_condition %>%
  group_by(passed, condition)%>%
  do(tidy(cor.test(.$RT_mean, .$blink_per_sec, method = 'pearson')))

#no significant correlations between accuracy and and ET indices
#NOTE: we predicted a significant correlation between accuracy and ET indices, based on the hypothesis
#that storing items in WM requires efficient processing of the visual stimuly
#and people with more efifficient scanning (saccade rate) would have higher accuracy
#there appears to be no correlation of this kind

#however, velocity and blink rate decrease as a function of difficulty. We found a negative correlation between
#RT and blink rate and peak velocity for failed levels of CMT, so the slower an individuals's rt (=increase),
#the lower their blink rate and peak velocity, indicating that individuals with slower RT 
#also show increase in subjective difficulty, This could be related to the construct of executive efficiency.

#let's integrate with effort response
#our hypotheses here are that there will be a significant correlation between demand and effort response,
#showing that the contruct accurately represents measure cognitive load

#for eye-tracking indices - we expect to see correlations between individual effort scores
#and eye-tracking indices
#people with higher effort scores will show decrease in peak velocity and blink rate and increase in RT

#effort_CMT_adults_table <- read.csv('effort_CMT_adults_table.csv')

#see in "effort_extraction_attempt" file - the correlation between effort response and task demand 
#is highly significant and very high (r(54) = 0.86, p < 0.001 and the lines are almost identical
#for balloons and clown)
effort_CMT_adults_table$demand <- ifelse(effort_CMT_adults_table$condition == "ball", as.numeric(effort_CMT_adults_table$level) + 1, as.numeric(effort_CMT_adults_table$level) + 2)

#create by demand table (with level and condition)
effort_CMT_adults_demand <- effort_CMT_adults_table %>%
  group_by(RECORDING_SESSION_LABEL, level, demand, condition)%>%
  summarize_at(vars(EFFORT_RESPONSE),funs(mean(., na.rm=TRUE)))

#prepare tablae for join
effort_CMT_adults_demand$condition<- ifelse(effort_CMT_adults_demand$condition == 'ball', 'balloons', 'clowns')

effort_CMT_adults_demand <- effort_CMT_adults_demand %>% 
  separate(RECORDING_SESSION_LABEL, into = c('RECORDING_SESSION_LABEL', 'gender'), sep = 7)

effort_CMT_adults_demand <- effort_CMT_adults_demand %>% 
  separate(RECORDING_SESSION_LABEL, into = c('RECORDING_SESSION_LABEL', 'age'), sep = 5)

effort_CMT_adults_demand$age <- as.numeric(effort_CMT_adults_demand$age)
effort_CMT_adults_demand$gender[effort_CMT_adults_demand$RECORDING_SESSION_LABEL == "16eka"] <- 'f'
effort_CMT_adults_demand$level <- as.character(effort_CMT_adults_demand$level)
#effort_CMT_adults_demand$demand <- as.numeric(effort_CMT_adults_demand$demand)

adults_CMT_eff<- full_join(adults_CMT,effort_CMT_adults_demand)

adults_CMT_eff <- adults_CMT_eff[!(is.na(adults_CMT_eff$RT_mean)),]

#running correlations

run_cor <- adults_CMT_eff %>%
  group_by(demand, condition)%>%
  do(tidy(cor.test(.$CURRENT_SAC_PEAK_VELOCITY_mean, .$EFFORT_RESPONSE, method = 'pearson')))

ggplot(adults_CMT_eff , aes(y = CURRENT_SAC_PEAK_VELOCITY_mean, x = EFFORT_RESPONSE , group = condition, color = condition)) +
  geom_jitter() + geom_smooth(method=lm) + facet_wrap(~demand)

#Results
#there is no correlation between individual's accuracy in a level of task demand and their subjective effort assessement
#there is also no correlation between RT and effort_response within a level of demand
#withing demand analysis shows no cors for ET indices and effort

#let's try to do subject means

subject_summary_adults_CMT_eff <-adults_CMT_eff %>%
  group_by(RECORDING_SESSION_LABEL, condition)%>%
  summarize_at(vars(EFFORT_RESPONSE, accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, m_score, CURRENT_SAC_PEAK_VELOCITY_mean),funs(mean(., na.rm=TRUE)))

subject_summary_adults_CMT_eff <- subject_summary_adults_CMT_eff[subject_summary_adults_CMT_eff$CURRENT_SAC_PEAK_VELOCITY_mean < 600,]
subject_summary_adults_CMT_eff <- subject_summary_adults_CMT_eff[!(is.na(subject_summary_adults_CMT_eff$EFFORT_RESPONSE)),]

#run cors
#blink per sec
run_cor <- subject_summary_adults_CMT_eff  %>%
  group_by(condition)%>%
  do(tidy(cor.test(.$blink_per_sec, .$EFFORT_RESPONSE, method = 'pearson')))

ggplot(subject_summary_adults_CMT_eff  , aes(y = blink_per_sec, x = EFFORT_RESPONSE , group = condition, color = condition)) +
  geom_jitter() + geom_smooth(method=lm)

#velocity
run_cor <- subject_summary_adults_CMT_eff  %>%
  group_by(condition)%>%
  do(tidy(cor.test(.$CURRENT_SAC_PEAK_VELOCITY_mean, .$EFFORT_RESPONSE, method = 'pearson')))

#Results:
#No correlations between behavioural (RT, accuracy and M-score) and effort 
#For eye-tracking: nothing for peak saccade velocity and saccade rate, both CMT conditions significant
# for blink rate (p = 0.04), but very weak (r = -0.27) - at least the direction is as expected

subject_summary_adults_CMT_eff <-adults_CMT_eff %>%
  group_by(RECORDING_SESSION_LABEL)%>%
  summarize_at(vars(EFFORT_RESPONSE, accuracy, TRIAL_SACCADE_TOTAL_mean, RT_mean, blink_per_sec, m_score, CURRENT_SAC_PEAK_VELOCITY_mean),funs(mean(., na.rm=TRUE)))

#excluding outliers
subject_summary_adults_CMT_eff <- subject_summary_adults_CMT_eff[subject_summary_adults_CMT_eff$CURRENT_SAC_PEAK_VELOCITY_mean < 600,]
subject_summary_adults_CMT_eff <- subject_summary_adults_CMT_eff[!(is.na(subject_summary_adults_CMT_eff$EFFORT_RESPONSE)),]

#run cors
cor.test(subject_summary_adults_CMT_eff$blink_per_sec, subject_summary_adults_CMT_eff $EFFORT_RESPONSE, method = 'pearson')
cor.test(subject_summary_adults_CMT_eff$CURRENT_SAC_PEAK_VELOCITY_mean, subject_summary_adults_CMT_eff $EFFORT_RESPONSE, method = 'pearson')

ggplot(subject_summary_adults_CMT_eff  , aes(y = blink_per_sec, x = EFFORT_RESPONSE)) +
  geom_jitter() + geom_smooth(method=lm) + theme_light() + labs(x = "Mental effort response", y = "Blink rate (per second)", title = "Relationship between blink rate and effort response")

ggplot(subject_summary_adults_CMT_eff  , aes(y = CURRENT_SAC_PEAK_VELOCITY_mean, x = EFFORT_RESPONSE)) +
  geom_jitter() + geom_smooth(method=lm) + theme_light() + labs(x = "Mental effort response", y = "Peak saccade velocity", title = "Relationship between peak saccade velocity and effort response")


#Results
#with CMT averages (no condition)
#cor with blink rate - p = 0.006, r = -0.37
#cor with PEAK velocity ! p = 0.02, r = -0.30
#so the results are weak - but what we expected! yay


#means by demand for effort scores
summary_demand_eff <-adults_CMT_eff %>%
  group_by(condition, demand)%>%
  summarize_at(vars(EFFORT_RESPONSE),funs(round(mean(., na.rm=TRUE),2), round(sd(., na.rm=TRUE), 2)))

write.xlsx(summary_demand_eff, "summary_demand_eff.xlsx")


#---------------------------------------------------------------------
#wanna try correlations between demand and eye movements
adults_CMT

run_cor <- adults_CMT  %>%
  group_by(condition)%>%
  do(tidy(cor.test(.$CURRENT_SAC_PEAK_VELOCITY_mean, .$demand, method = 'pearson')))

#Pearson correlation between peak velocity and MA demand is significant by veeeery low (negative, which is good)


run_cor <- adults_CMT  %>%
  group_by(condition)%>%
  do(tidy(cor.test(.$blink_per_sec, .$demand, method = 'pearson')))

#pearson correlation between blink rate and MA demand is meh significant (considering number of items)
# and extremely low (the direction is expected - negative)



