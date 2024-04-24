# script created by Santiago Castiello de Obeso 
# date: 20/03/2024
# scripts found in socialHallucinations repository: https://github.com/santiagocdo/socialHallucinations


# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))



# # # # # # # # # # libraries and functions # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
source("functions_gitHub.R")
loadPackages(c("ggplot2","reshape2","ggpubr","png","lmerTest","dplyr"))

library(ggplot2,reshape2)
library(ggpubr)
library(png)
library(lmerTest)
library(dplyr)



# # # # # # # # # # import data # # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
remove_invalid <- 1
print_figure <- 1


# # # # read behaviour # # # # 
# experiment 1 (detect-chase)
exp1 <- read.csv("data/behaviour/exp1-chase.csv")
exp1$setting <- "between"
exp1$subjectId <- exp1$workerId

# experiment 2 (detect-chase-confidence)
exp2 <- read.csv("data/behaviour/exp6-chase-confidence.csv")
exp2$setting <- "between"
# exp2$subjectId <- exp2$participantId # no needed, but becarefull it contains 2 PROLIFIC_PID


# experiment 3[wolf]
exp3wolf <- read.csv("data/behaviour/exp2-wolf.csv")
exp3wolf$setting <- "between"
# experiment 3[sheep]
exp3sheep <- read.csv("data/behaviour/exp3-sheep.csv")
exp3sheep$setting <- "between"


# experiment 3
exp3 <- rbind(exp3wolf,exp3sheep)
exp3$subjectId <- exp3$PROLIFIC_PID


# experiment 4a
exp4a <- read.csv("data/behaviour/exp4-sheep-and-wolf.csv")
exp4a$setting <- "within"
exp4a$subjectId <- exp4a$workerId

# experiment 4b
exp4b <- read.csv("data/behaviour/exp5-sheep-and-wolf.csv")
exp4b$setting <- "within"
exp4b$subjectId <- exp4b$workerId



# # # # read questionnaires # # # # 
# experiment 1
quest1 <- read.csv("data/questionnaire/wideFormat_exp1.csv")
quest1$subjectId <- quest1$workerId
# experiment 2
quest2 <- read.csv("data/questionnaire/wideFormat_exp2.csv")
# quest2$subjectId <- quest2$participantId # no needed, but be carefull it contains 2 PROLIFIC_PID
# experiment 3, 4a, and 4b
quests <- read.csv("data/questionnaire/wideFormat_exps.csv")
quests$setting <- ifelse(quests$version == "detect-sheep-and-wolf","within","between")
quests$subjectId <- ifelse(quests$workerId == "", quests$PROLIFIC_PID, quests$workerId)






# # # # # # # # # # combine data sets # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
if (remove_invalid == 1) {
  # remove invalid trials and participants 
  # exclusion criteria already aplied when cleaning data
  quest1 <- quest1[quest1$valid_id == T,]
  quest2 <- quest2[quest2$valid_id == T,]
  exp1 <- exp1[exp1$valid_id == T & exp1$valid_trials ==  T,]
  exp2 <- exp2[exp2$valid_id == T & exp2$valid_trials ==  T,]
  
  # sensitivity analysis only with good participants for study 4a
  also_4a <- 0
  if (also_4a == 1) {
    exp4a$valid_id <- F
    quests$valid_id <- ifelse(quests$experiment == 4,F,T)
    for (i in 1:length(unique(exp4a$subjectId))) {
      temp <- exp4a[exp4a$subjectId == unique(exp4a$subjectId)[i],]
      temp <- temp[temp$condition=="chase",]
      pVal <- binom.test(sum(temp$correct), nrow(temp), p = 1/8, alternative = "greater")$p.value
      if (pVal < 0.1) {
        exp4a$valid_id[exp4a$workerId == unique(exp4a$subjectId)[i]] <- T
        quests$valid_id[quests$workerId == unique(exp4a$subjectId)[i]] <- T
      }
    }
    # keep valid rows
    exp4a <- exp4a[exp4a$valid_id == T,]; exp4a$valid_id <- NULL
    quests <- quests[quests$valid_id == T,]; quests$valid_id <- NULL
  }
  
  
  exp4b$valid_id <- F
  quests$valid_id <- ifelse(quests$experiment == 5,F,T)
  for (i in 1:length(unique(exp4b$subjectId))) {
    temp <- exp4b[exp4b$subjectId == unique(exp4b$subjectId)[i],]
    temp <- temp[temp$condition=="chase",]
    pVal <- binom.test(sum(temp$correct), nrow(temp), p = 1/8, alternative = "greater")$p.value
    if (pVal < 0.1) {
      exp4b$valid_id[exp4b$workerId == unique(exp4b$subjectId)[i]] <- T
      quests$valid_id[quests$workerId == unique(exp4b$subjectId)[i]] <- T
    }
  }
  # keep valid rows
  exp4b <- exp4b[exp4b$valid_id == T,]; exp4b$valid_id <- NULL
  quests <- quests[quests$valid_id == T,]; quests$valid_id <- NULL
}



# # # # # # # # Experiment 1 # # # # # # # # 
# # # # add behaviour to questionnaires # # # #
quest1exp1 <- addBehaviour(to=quest1,from=exp1)
quest1exp1$paranoia <- ifelse(quest1exp1$rgpts_para=="high",1,0)

# # # # add questionnaires to behaviour # # # #
scores <- c("rgpts_pers","rgpts_para")
exp1Quest1 <- addQuestionnaires(to=exp1,from=quest1,scores)


# # # # # # # # Experiment 2 # # # # # # # # 
# # # # add behaviour to questionnaires # # # #
quest2exp2 <- addBehaviour(to=quest2,from=exp2)
quest2exp2$paranoia <- ifelse(quest2exp2$rgpts_para=="high",1,0)

# # # # add questionnaires to behaviour # # # #
scores <- c("rgpts_refe","rgpts_pers","rgpts_para","bpe","caps_total",
            "caps_distress","caps_distracting","caps_hardly")
exp2Quest2 <- addQuestionnaires(to=exp2,from=quest2,scores)


# # # # # # # # Experiment 3 and 4 # # # # # # # # 
# # # # add behaviour to questionnaires # # # #
# get only within
within <- quests[quests$version == "detect-sheep-and-wolf",]
# remove within
quests <- quests[quests$version != "detect-sheep-and-wolf",]
# add the addecuate task type
quests$task <- ifelse(quests$version == "detect-wolf","wolf","sheep")
# duplicate within one for each task
within <- rbind(data.frame(within,task="wolf"),
                data.frame(within,task="sheep"))
# combine with between
quests <- rbind(quests,within); rm(within)
# create a unique ID (including the task)
quests$subjectId <- paste0(quests$subjectId,"_",quests$task)
# combine long formats
exps <- rbind(exp3,exp4a,exp4b)
# create the same unique ID
exps$subjectId <- paste0(exps$subjectId,"_",exps$task)
# now! finally, add behaviour to each task and participant
questsExps <- addBehaviour(to=quests,from=exps)
questsExps$paranoia <- ifelse(questsExps$rgpts_para == "high",1,0)

# remove this participant that revoked consent and it is duplicated!
# questsExps[questsExps$PROLIFIC_PID == "63222a1daf1fe6dcbc59b29a",]
questsExps <- questsExps[questsExps$PROLIFIC_PID != "63222a1daf1fe6dcbc59b29a",]



# # # # add questionnaires to behaviour # # # #
scores <- c("demo_sex","rgpts_refe","rgpts_pers","rgpts_para","bpe","caps_total",
            "caps_distress","caps_distracting","caps_hardly")
expsQuests <- addQuestionnaires(to=exps,from=quests,scores)






# # # # # # # # # # figures # # # # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# palette from Daniel Barreto: https://www.dbarreto.com/all/diurnalidad-1
# c("#BF7379","#005378","#D9D3C7","#D99441","#BF4949")
# sheep <- "#F2B885"; wolf <- "#262014"; 
# pHigh <- "#F29199"; pLow <- "#1F5B73"
# tHigh <- "#F2921D"; tLow <- "#8EA676";

fig2 <- plotFigure2(quest1exp1,quest2exp2,
                    questsExps,exp1Quest1,
                    exp2Quest2,expsQuests)

figS3 <- plotFigureS3(quest1exp1,quest2exp2,
                    questsExps,exp1Quest1,
                    exp2Quest2,expsQuests)



# print figures
if (print_figure == 1) {
  ggsave("figures/figure2.pdf", fig2, dpi = 2400, scale = 1.1, units = "cm",
         width = 20, height = 20, bg = "white")
  ggsave("figures/figureS3.pdf", figS3, dpi = 2400, scale = 1.1, units = "cm",
         width = 12, height = 20, bg = "white")
}





# # # # # # # # # # Stats# # # # # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# paranoia as binary
exp1Quest1$paranoia <- ifelse(exp1Quest1$rgpts_para=="high",1,0)
exp2Quest2$paranoia <- ifelse(exp2Quest2$rgpts_para=="high",1,0)
# exp2Quest2$bpe_high <- ifelse(exp2Quest2$bpe<median(exp2Quest2$bpe),0,1)
expsQuests$paranoia <- ifelse(expsQuests$rgpts_para=="high",1,0) 
# expsQuests$bpe_high <- ifelse(expsQuests$bpe<=median(expsQuests$bpe,na.rm=T),0,1)


# combine both exp1 and exp2
# long format
cols <- c("experiment","subjectId","condition","paranoia","choice")
exp1and2 <- rbind(exp1Quest1[,cols],exp2Quest2[,cols])

# wide format
sameCols <- c("experiment","version","demo_age","demo_sex",
              "subjectId","bpe","paranoia","rgpts_pers")
allQuest <- rbind(quest2exp2[,sameCols],questsExps[,sameCols])
# teleology and paranoia are correlated
cor.test(allQuest$rgpts_pers, allQuest$bpe,method = "spearman")
ggplot(allQuest, aes(x=scale(rgpts_pers),y=scale(bpe))) + 
  labs(x="Persecution",y="Teleolgy") +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm",col="black") +
  stat_cor(method = "spearman") +
  theme_classic()

# sex is related with teleolgy or paranoia?
questsExps$sex <- ifelse(questsExps$demo_sex=="Prefer not to say",NA,questsExps$demo_sex)

ggplot(questsExps, aes(x=sex,y=paranoia)) + stat_summary()
chisq.test(table(questsExps$sex,questsExps$paranoia))
ggplot(questsExps, aes(x=sex,y=bpe)) + stat_summary()
t.test(bpe~sex, questsExps)


# # # # # # # # # # Stats: Figure 3 # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# mod_dec_chase <- glmer(correct ~ paranoia * bpe * task + (1|experiment/subjectId),
#                        family=binomial, data=expsQuests[expsQuests$condition == "chase" &
#                                                           expsQuests$setting == "within",])
# tab_mod_dec_chase <- modelEstimates(mod_dec_chase)
# 
# # remove participants with no bpe or paranoia measures
# temp <- expsQuests[!is.na(expsQuests$bpe) & !is.na(expsQuests$paranoia),]
# temp <- temp[temp$condition == "chase",]
# temp$bpe_high <- ifelse(temp$bpe<=quantile(temp$bpe,0.5,na.rm=T),"low","high")
# temp$para_high <- ifelse(temp$paranoia==0,"low","high")
# temp$group <- paste0("P",temp$para_high,"_T",temp$bpe_high)
# temp$group_conjunction <- ifelse(temp$para_high == temp$bpe_high, 1, 0)
# 
# ggplot(temp, aes(x=group,y=correct,col=task)) + 
#   labs(title = "high BPE 50th quantile") + stat_summary() + 
#   theme_classic() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
# mod <- glmer(correct ~ para_high * bpe_high * task + (1|experiment/subjectId),
#              family=binomial, data=temp)
# summary(mod)
# 
# temp <- temp[temp$group_conjunction==0,]
# ggplot(temp[,], aes(x=group,y=correct,col=task)) + 
#   labs(title = "high BPE 50th quantile") + stat_summary() + 
#   theme_classic()
# mod <- glmer(correct ~ group * task + (1|experiment/subjectId),family=binomial, 
#              data=temp[,])
# summary(mod)
# 
# 
# 
# 
# sheepDb <- expsQuests[expsQuests$task == "sheep",]
# mod_dec_sheep <- glmer(correct ~ condition * paranoia * bpe + (condition|experiment/subjectId),
#                        family=binomial, data=sheepDb)
# tab_mod_dec_sheep <- modelEstimates(mod_dec_sheep)
# 
# mod_dec_chase_sheep <- glmer(correct ~ paranoia * bpe + (1|experiment/subjectId),
#                              family=binomial, data=sheepDb[sheepDb$condition == "chase",])
# tab_mod_dec_chase_sheep <- modelEstimates(mod_dec_chase_sheep)
# 
# 
# 
# wolfDb <- expsQuests[expsQuests$task == "wolf",]
# mod_dec_wolf <- glmer(correct ~ condition * paranoia * bpe + (condition|experiment/subjectId),
#                       family=binomial, data=wolfDb)
# tab_mod_dec_wolf <- modelEstimates(mod_dec_wolf)
# 
# mod_dec_chase_wolf <- glmer(correct ~ paranoia * bpe + (1|experiment/subjectId),
#                              family=binomial, data=wolfDb[wolfDb$condition == "chase",])
# tab_mod_dec_chase_wolf <- modelEstimates(mod_dec_chase_wolf)






# # # # # # # # # # Stats: Figure 2 # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
mod_dec_Fig2C_sheep <- glmer(correct ~ condition * paranoia + (condition|experiment/subjectId),
                             family=binomial, data=expsQuests[expsQuests$task == "sheep",])
# tab_mod_dec_Fig2C_sheep <- modelEstimates(mod_dec_Fig2C_sheep)
tab_mod_dec_Fig2C_sheep <- report::report_table(mod_dec_Fig2C_sheep)

mod_dec_Fig2C_wolf <- glmer(correct ~ condition * paranoia + (condition|experiment/subjectId),
                            family=binomial, data=expsQuests[expsQuests$task == "wolf",])
# tab_mod_dec_Fig2C_wolf <- modelEstimates(mod_dec_Fig2C_wolf)
tab_mod_dec_Fig2C_wolf <- report::report_table(mod_dec_Fig2C_wolf)



mod_dec_Fig2D_sheep <- glmer(correct ~ condition * bpe + (condition|experiment/subjectId),
                             family=binomial, data=expsQuests[expsQuests$task == "sheep",])
# tab_mod_dec_Fig2D_sheep <- modelEstimates(mod_dec_Fig2D_sheep)
tab_mod_dec_Fig2D_sheep <- report::report_table(mod_dec_Fig2D_sheep)

mod_dec_Fig2D_wolf <- glmer(correct ~ condition * bpe + (condition|experiment/subjectId),
                            family=binomial, data=expsQuests[expsQuests$task == "wolf",])
# tab_mod_dec_Fig2D_wolf <- modelEstimates(mod_dec_Fig2D_wolf)
tab_mod_dec_Fig2D_wolf <- report::report_table(mod_dec_Fig2D_wolf)



mod_con_Fig2E_sheep <- lmer(confidence ~ condition * paranoia + (condition|experiment/subjectId),
                            REML=F, data=expsQuests[expsQuests$task == "sheep",])
# tab_mod_con_Fig2E_sheep <- modelEstimates(mod_con_Fig2E_sheep)
tab_mod_con_Fig2E_sheep <- report::report_table(mod_con_Fig2E_sheep)

mod_con_Fig2E_wolf <- lmer(confidence ~ condition * paranoia + (condition|experiment/subjectId),
                           REML=F, data=expsQuests[expsQuests$task == "wolf",])
# tab_mod_con_Fig2E_wolf <- modelEstimates(mod_con_Fig2E_wolf)
tab_mod_con_Fig2E_wolf <- report::report_table(mod_con_Fig2E_wolf)



mod_con_Fig2F_sheep <- lmer(confidence ~ condition * bpe + (condition|experiment/subjectId),
                            REML=F, data=expsQuests[expsQuests$task == "sheep",])
# tab_mod_con_Fig2F_sheep <- modelEstimates(mod_con_Fig2F_sheep)
tab_mod_con_Fig2F_sheep <- report::report_table(mod_con_Fig2F_sheep)

mod_con_Fig2F_wolf <- lmer(confidence ~ condition * bpe + (condition|experiment/subjectId),
                           REML=F, data=expsQuests[expsQuests$task == "wolf",])
# tab_mod_con_Fig2F_wolf <- modelEstimates(mod_con_Fig2F_wolf)
tab_mod_con_Fig2F_wolf <- report::report_table(mod_con_Fig2F_wolf)



# discussion: "This could have reflected relatively low statistical power, however, 
# since when collapsing across studies (Experiments 1 and 2), we did observe more 
# false-alarms, and the trend was in the expected direction even in the un-collapsed data."
summary(lm(correct_C~paranoia*task,questsExps[,]))
summary(lm(correct_C~bpe*task,questsExps[,]))
summary(lm(confidence_M~paranoia*task,questsExps[,]))
summary(lm(confidence_M~bpe*task,questsExps[,]))

# summary(glm(paranoia~fa_rate,family=binomial,quest2exp2[,]))
# summary(glm(paranoia~fa_rate,family=binomial,quest1exp1[,]))
# summary(lm(bpe~fa_rate,quest2exp2[,]))
# summary(lm(bpe~fa_rate,quest2exp2[,]))
# 
# m1<-glmer(choice~condition*paranoia+(condition|subjectId),family=binomial,
#          data=exp2Quest2); summary(m1)
# m2<-glmer(choice~condition*bpe+(condition|subjectId),family=binomial,
#          data=exp2Quest2); summary(m2)
# m3<-lmer(confidence~condition*paranoia+(condition|subjectId),REML=F,
#         data=exp2Quest2); summary(m3)



# # # # # # # # # # Stats: Figure S3, Sex Analysis# # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
temp <- expsQuests[expsQuests$demo_sex == "Male" | expsQuests$demo_sex == "Female",]
temp$sex <- ifelse(temp$demo_sex == "Male",0,1) 

mod_ide_FigS2B_sheep <- glmer(correct ~ condition * sex + (condition|experiment/subjectId),
                              family=binomial, data=temp[temp$task == "sheep",])
tab_mod_ide_FigS2B_sheep <- report::report_table(mod_ide_FigS2B_sheep)

mod_ide_FigS2B_wolf <- glmer(correct ~ condition * sex + (condition|experiment/subjectId),
                            family=binomial, data=temp[temp$task == "wolf",])
tab_mod_ide_FigS2B_wolf <- report::report_table(mod_ide_FigS2B_wolf)



mod_con_FigS2C_sheep <- lmer(confidence ~ condition * sex + (condition|experiment/subjectId),
                            REML=F, data=temp[temp$task == "sheep",])
tab_mod_con_FigS2C_sheep <- report::report_table(mod_con_FigS2C_sheep)

mod_con_FigS2C_wolf <- lmer(confidence ~ condition * sex + (condition|experiment/subjectId),
                           REML=F, data=temp[temp$task == "wolf",])
tab_mod_con_FigS2C_wolf <- report::report_table(mod_con_FigS2C_wolf)



# # # # # # # # # # Stats: Figure S2# # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # does questionnaires influence detection?# # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# exp. 1 - detection - Paranoia
md1paranoia <- glmer(choice~condition*paranoia+(condition|subjectId),family=binomial,
                     data=exp1Quest1)
tmd1paranoia <- modelEstimates(md1paranoia)
# ggplot(exp1Quest1, aes(x=condition,y=choice,col=as.factor(paranoia))) + stat_summary()

# exp. 2 - detection - Paranoia
md2paranoia <- glmer(choice~condition*paranoia+(condition|subjectId),family=binomial,
                     data=exp2Quest2)
tmd2paranoia <- modelEstimates(md2paranoia)
# ggplot(exp2Quest2, aes(x=condition,y=choice,col=as.factor(paranoia))) + stat_summary()

# exp. 2 - confidence - Paranoia 
mc2paranoia <- lmer(confidence~condition*paranoia+(condition|subjectId),REML=F,
                    data=exp2Quest2)
tmc2paranoia <- modelEstimates(mc2paranoia)
# ggplot(exp2Quest2, aes(x=condition,y=confidence,col=as.factor(paranoia))) + stat_summary()

# exp. 2 - detection - Teleology
md2teleology <- glmer(choice~condition*bpe+(condition|subjectId),family=binomial,
                     data=exp2Quest2)
tmd2teleology <- modelEstimates(md2teleology)
# ggplot(exp2Quest2, aes(x=condition,y=choice,col=as.factor(bpe_high))) + stat_summary()

# exp. 2 - confidence - Teleology
mc2teleology <- lmer(confidence~condition*bpe+(condition|subjectId),REML=F,
                    data=exp2Quest2)
tmc2teleology <- modelEstimates(mc2teleology)
# ggplot(exp2Quest2, aes(x=condition,y=confidence,col=as.factor(bpe_high))) + stat_summary()


# exp. 1 and 2 - detection - paranoia
md1and2paranoia <- glmer(choice~condition*paranoia+(condition|experiment/subjectId),
                         family=binomial, data=exp1and2)
tmd1and2paranoia <- modelEstimates(md1and2paranoia)



# # # # does task-type influence selection? (between and within)# # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# exp. 3 - select - task
md3task <- glmer(correct~condition*task+(condition|subjectId),family=binomial,
                 data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmd3task <- modelEstimates(md3task)
# no third order interaction
md3taskPara <- glmer(correct~condition*task*paranoia+(condition|subjectId),family=binomial,
                     data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmd3taskPara <- modelEstimates(md3taskPara)
md3taskParaWolf <- glmer(correct~condition*paranoia+(condition|subjectId),family=binomial,
                         data=expsQuests[(expsQuests$experiment==2|expsQuests$experiment==3)&
                                           expsQuests$task == "wolf",])
tmd3taskParaWolf <- modelEstimates(md3taskParaWolf)
md3taskParaSheep <- glmer(correct~condition*paranoia+(condition|subjectId),family=binomial,
                          data=expsQuests[(expsQuests$experiment==2|expsQuests$experiment==3)&
                                            expsQuests$task == "sheep",])
tmd3taskParaSheep <- modelEstimates(md3taskParaSheep)

# no third order interaction
md3taskTele <- glmer(correct~condition*task*bpe+(condition|subjectId),family=binomial,
                     data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmd3taskTele <- modelEstimates(md3taskTele)
md3taskTeleWolf <- glmer(correct~condition*bpe+(condition|subjectId),family=binomial,
                         data=expsQuests[(expsQuests$experiment==2|expsQuests$experiment==3)&
                                           expsQuests$task == "wolf",])
tmd3taskTeleWolf <- modelEstimates(md3taskTeleWolf)
md3taskTeleSheep <- glmer(correct~condition*bpe+(condition|subjectId),family=binomial,
                          data=expsQuests[(expsQuests$experiment==2|expsQuests$experiment==3)&
                                            expsQuests$task == "sheep",])
tmd3taskTeleSheep <- modelEstimates(md3taskTeleSheep)



# exp. 4a - select - task (NOTE: use workerId not subjectId)
md4aTask <- glmer(correct~condition*task+(condition|workerId),family=binomial,
                  data=expsQuests[expsQuests$experiment==4,])
tmd4aTask <- modelEstimates(md4aTask)
# significant third order interaction
md4aTaskPara <- glmer(correct~condition*task*paranoia+(condition|workerId),family=binomial,
                      data=expsQuests[expsQuests$experiment==4,])
tmd4aTaskPara <- modelEstimates(md4aTaskPara)


md4aTaskParaWolf <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                          data=expsQuests[expsQuests$experiment==4 &
                                            expsQuests$task == "wolf",])
tmd4aTaskParaWolf <- modelEstimates(md4aTaskParaWolf)
md4aTaskParaSheep <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                           data=expsQuests[expsQuests$experiment==4 &
                                             expsQuests$task == "sheep",])
tmd4aTaskParaSheep <- modelEstimates(md4aTaskParaSheep)

# no third order interaction
md4aTaskTele <- glmer(correct~condition*task*bpe+(condition|workerId),family=binomial,
                      data=expsQuests[expsQuests$experiment==4,])
tmd4aTaskTele <- modelEstimates(md4aTaskTele)

md4aTaskTeleWolf <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                          data=expsQuests[expsQuests$experiment==4 &
                                            expsQuests$task == "wolf",])
tmd4aTaskTeleWolf <- modelEstimates(md4aTaskTeleWolf)
md4aTaskTeleSheep <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                           data=expsQuests[expsQuests$experiment==4 &
                                             expsQuests$task == "sheep",])
tmd4aTaskTeleSheep <- modelEstimates(md4aTaskTeleSheep)
expsQuests$teleos <- as.factor(ifelse(expsQuests$bpe>median(expsQuests$bpe,na.rm=T),1,0))


# exp. 4b - select - task (NOTE: use workerId not subjectId)
md4bTask <- glmer(correct~condition*task+(condition|workerId),family=binomial,
                  data=expsQuests[expsQuests$experiment==5,])
tmd4bTask <- modelEstimates(md4bTask)
# no third order interaction
md4bTaskPara <- glmer(correct~condition*task*paranoia+(condition|workerId),family=binomial,
                      data=expsQuests[expsQuests$experiment==5,])
tmd4bTaskPara <- modelEstimates(md4bTaskPara)
md4bTaskParaWolf <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                          data=expsQuests[expsQuests$experiment==5 &
                                            expsQuests$task == "wolf",])
tmd4bTaskParaWolf <- modelEstimates(md4bTaskParaWolf)
md4bTaskParaSheep <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                           data=expsQuests[expsQuests$experiment==5 &
                                             expsQuests$task == "sheep",])
tmd4bTaskParaSheep <- modelEstimates(md4bTaskParaSheep)
# no third order interaction
md4bTaskTele <- glmer(correct~condition*task*bpe+(condition|workerId),family=binomial,
                      data=expsQuests[expsQuests$experiment==5,])
tmd4bTaskTele <- modelEstimates(md4bTaskTele)
md4bTaskTeleWolf <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                          data=expsQuests[expsQuests$experiment==5 &
                                            expsQuests$task == "wolf",])
tmd4bTaskTeleWolf <- modelEstimates(md4bTaskTeleWolf)
md4bTaskTeleSheep <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                           data=expsQuests[expsQuests$experiment==5 &
                                             expsQuests$task == "sheep",])
tmd4bTaskTeleSheep <- modelEstimates(md4bTaskTeleSheep)



# exp. 3, 4a, and 4b - select - task (NOTE: use workerId not subjectId)
md3and4task <- glmer(correct~condition*task+(condition|experiment/workerId),
                      family=binomial, data=expsQuests[,])
tmd3and4task <- modelEstimates(md3and4task)
# significant third order interaction
md3and4taskPara <- glmer(correct~condition*task*paranoia+(condition|experiment/workerId),
                         family=binomial, data=expsQuests[,])
tmd3and4taskPara <- modelEstimates(md3and4taskPara)
md3and4taskParaWolf <- glmer(correct~condition*paranoia+(condition|experiment/workerId),
                             family=binomial, data=expsQuests[expsQuests$task == "wolf",])
tmd3and4taskParaWolf <- modelEstimates(md3and4taskParaWolf)
md3and4taskParaSheep <- glmer(correct~condition*paranoia+(condition|experiment/workerId),
                              family=binomial, data=expsQuests[expsQuests$task == "sheep",])
tmd3and4taskParaSheep <- modelEstimates(md3and4taskParaSheep)
# no third order interaction
md3and4taskTele <- glmer(correct~condition*task*bpe+(condition|experiment/workerId),
                         family=binomial, data=expsQuests[,])
tmd3and4taskTele <- modelEstimates(md3and4taskTele)
md3and4taskTeleWolf <- glmer(correct~condition*bpe+(condition|experiment/workerId),
                             family=binomial, data=expsQuests[expsQuests$task == "wolf",])
tmd3and4taskTeleWolf <- modelEstimates(md3and4taskTeleWolf)
md3and4taskTeleSheep <- glmer(correct~condition*bpe+(condition|experiment/workerId),
                              family=binomial, data=expsQuests[expsQuests$task == "sheep",])
tmd3and4taskTeleSheep <- modelEstimates(md3and4taskTeleSheep)
# ggplot(expsQuests, aes(x=condition,y=correct,col=task)) + 
#   stat_summary() + facet_grid(experiment ~ .)
# summary(glmer(correct~task+(condition|experiment/workerId),family=binomial,
#               data=expsQuests[,]))
# summary(glmer(correct~condition+(condition|experiment/workerId),family=binomial,
#               data=expsQuests[expsQuests$task=="wolf",]))
# summary(glmer(correct~condition+(condition|experiment/workerId),family=binomial,
#               data=expsQuests[expsQuests$task=="sheep",]))
# ggplot(expsQuests, aes(x=task,y=correct)) + 
#   stat_summary() #+ facet_grid(. ~ experiment)



# # # # does task-type influence confidence? (between and within) # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# exp. 3 - confidence - task
mc3task <- lmer(confidence~condition*task+(condition|subjectId),REML=T,
                data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
# no third order interaction
# mc3task <- lmer(confidence~condition*task*paranoia+(condition|subjectId),REML=T,
#                 data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
# significant third order interaction
# mc3task <- lmer(confidence~condition*task*bpe+(condition|subjectId),REML=T,
#                 data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmc3task <- modelEstimates(mc3task)

# exp. 4a - confidence - task (NOTE: use workerId not subjectId)
mc4aTask <- lmer(confidence~condition*task+(condition|workerId),REML=T,
                 data=expsQuests[expsQuests$experiment==4,])
# no third order interaction
# mc4aTask <- lmer(confidence~condition*task*paranoia+(condition|workerId),REML=T,
#                  data=expsQuests[expsQuests$experiment==4,])
# no third order interaction
# mc4aTask <- lmer(confidence~condition*task*bpe+(condition|workerId),REML=T,
#                  data=expsQuests[expsQuests$experiment==4,])
tmc4aTask <- modelEstimates(mc4aTask)

# exp. 4b - confidence - task (NOTE: use workerId not subjectId)
mc4bTask <- lmer(confidence~condition*task+(condition|workerId),REML=T,
                 data=expsQuests[expsQuests$experiment==5,])
# no third order interaction
# mc4bTask <- lmer(confidence~condition*task*paranoia+(condition|workerId),REML=T,
#                  data=expsQuests[expsQuests$experiment==5,])
# no third order interaction
# mc4bTask <- lmer(confidence~condition*task*bpe+(condition|workerId),REML=T,
#                  data=expsQuests[expsQuests$experiment==5,])
tmc4bTask <- modelEstimates(mc4bTask)


# exp. 3, 4a, and 4b - confidence - task (NOTE: use workerId not subjectId)
mc3and4task <- lmer(confidence~condition*task+(condition|experiment/workerId),
                    REML=T, data=expsQuests[,])
# no third order interaction
# mc3and4task <- lmer(confidence~condition*task*paranoia+(condition|experiment/workerId),
#                     REML=T, data=expsQuests[,])
# no third order interaction
# mc3and4task <- lmer(confidence~condition*task*bpe+(condition|experiment/workerId),
#                     REML=T, data=expsQuests[,])
tmc3and4task <- modelEstimates(mc3and4task)




# # # # does questionnaires influence selection? (between and within) # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# exp. 3 - select - Paranoia
md3paranoia <- glmer(correct~condition*paranoia+(condition|subjectId),family=binomial,
                     data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmd3paranoia <- modelEstimates(md3paranoia)
# exp. 3 - select - Teleology
md3teleology <- glmer(correct~condition*bpe+(condition|subjectId),family=binomial,
                      data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmd3teleology <- modelEstimates(md3teleology)

# exp. 4a - select - Paranoia (NOTE: use workerId not subjectId)
md4aParanoia <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                      data=expsQuests[expsQuests$experiment==4,])
tmd4aParanoia <- modelEstimates(md4aParanoia)
# exp. 4a - select - Paranoia (NOTE: use workerId not subjectId)
md4aTeleology <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                       data=expsQuests[expsQuests$experiment==4,])
tmd4aTeleology <- modelEstimates(md4aTeleology)

# exp. 4b - select - Paranoia (NOTE: use workerId not subjectId)
md4bParanoia <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                    data=expsQuests[expsQuests$experiment==5,])
tmd4bParanoia <- modelEstimates(md4bParanoia)
# exp. 4b - select - Teleology (NOTE: use workerId not subjectId)
md4bTeleology <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                     data=expsQuests[expsQuests$experiment==5,])
tmd4bTeleology <- modelEstimates(md4bTeleology)


# exp. 3, 4a, and 4b - select - Paranoia (NOTE: use workerId not subjectId)
md3and4paranoia <- glmer(correct~condition*paranoia+(condition|experiment/workerId),
                        family=binomial, data=expsQuests[,])
tmd3and4paranoia <- modelEstimates(md3and4paranoia)
# exp. 3, 4a, and 4b - select - Teleology (NOTE: use workerId not subjectId)
md3and4teleology <- glmer(correct~condition*bpe+(condition|experiment/workerId),
                         family=binomial, data=expsQuests[,])
tmd3and4teleology <- modelEstimates(md3and4teleology)




# # # # does task type influence confidence? (between and within) # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# exp. 3 - confidence - Paranoia
mc3paranoia <- lmer(confidence~condition*paranoia+(condition|subjectId),REML=T,
                    data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmc3paranoia <- modelEstimates(mc3paranoia)
# exp. 3 - confidence - Teleology
mc3teleology <- lmer(confidence~condition*bpe+(condition|subjectId),REML=T,
                     data=expsQuests[expsQuests$experiment==2|expsQuests$experiment==3,])
tmc3teleology <- modelEstimates(mc3teleology)

# exp. 4a - confidence - Paranoia (NOTE: use workerId not subjectId)
mc4aParanoia <- lmer(confidence~condition*paranoia+(condition|workerId),REML=T,
                     data=expsQuests[expsQuests$experiment==4,])
tmc4aParanoia <- modelEstimates(mc4aParanoia)
# exp. 4a - confidence - Teleology (NOTE: use workerId not subjectId)
mc4aTeleology <- lmer(confidence~condition*bpe+(condition|workerId),REML=T,
                      data=expsQuests[expsQuests$experiment==4,])
tmc4aTeleology <- modelEstimates(mc4aTeleology)

# exp. 4b - confidence - Paranoia (NOTE: use workerId not subjectId)
mc4bParanoia <- lmer(confidence~condition*paranoia+(condition|workerId),REML=T,
                     data=expsQuests[expsQuests$experiment==5,])
tmc4bParanoia <- modelEstimates(mc4bParanoia)
# exp. 4b - confidence - Teleology (NOTE: use workerId not subjectId)
mc4bTeleology <- lmer(confidence~condition*bpe+(condition|workerId),REML=T,
                      data=expsQuests[expsQuests$experiment==5,])
tmc4bTeleology <- modelEstimates(mc4bTeleology)


# exp. 4b - confidence - Paranoia (NOTE: use workerId not subjectId)
mc3and4paranoia <- lmer(confidence~condition*paranoia+(condition|experiment/workerId),
                        REML=T, data=expsQuests[,])
tmc3and4paranoia <- modelEstimates(mc3and4paranoia)
# ggplot(expsQuests, aes(x=condition,y=confidence,col=as.factor(paranoia))) + stat_summary()
# exp. 4b - confidence - Teleology (NOTE: use workerId not subjectId)
mc3and4teleology <- lmer(confidence~condition*bpe+(condition|experiment/workerId),
                         REML=T, data=expsQuests[,])
tmc3and4teleology <- modelEstimates(mc3and4teleology)



# combine all models
tableS2 <- rbind(data.frame(exp="Study 1",mod="Paranoia",out="Detect",tmd1paranoia[2:4,c(1,3,4)]),
                 data.frame(exp="Study 2",mod="Paranoia",out="Detect",tmd2paranoia[2:4,c(1,3,4)]),
                 data.frame(exp="Study 2",mod="Teleology",out="Detect",tmd2teleology[2:4,c(1,3,4)]),
            data.frame(exp="Studies 1 and 2",mod="Paranoia",out="Detect",tmd1and2paranoia[2:4,c(1,3,4)]),
                 data.frame(exp="Study 2",mod="Paranoia",out="Confidence",tmc2paranoia[2:4,c(1,4,5)]),
                 data.frame(exp="Study 2",mod="Teleology",out="Confidence",tmc2teleology[2:4,c(1,4,5)]),
                 data.frame(exp="Study 3",mod="Task Type",out="Identification",tmd3task[2:4,c(1,3,4)]),
                 data.frame(exp="Study 4a",mod="Task Type",out="Identification",tmd4aTask[2:4,c(1,3,4)]),
                 data.frame(exp="Study 4b",mod="Task Type",out="Identification",tmd4bTask[2:4,c(1,3,4)]),
            data.frame(exp="Studies 3 and 4",mod="Task Type",out="Identification",tmd3and4task[2:4,c(1,3,4)]),
                 data.frame(exp="Study 3",mod="Task Type",out="Confidence",tmc3task[2:4,c(1,4,5)]),
                 data.frame(exp="Study 4a",mod="Task Type",out="Confidence",tmc4aTask[2:4,c(1,4,5)]),
                 data.frame(exp="Study 4b",mod="Task Type",out="Confidence",tmc4bTask[2:4,c(1,4,5)]),
            data.frame(exp="Studies 3 and 4",mod="Task Type",out="Confidence",tmc3and4task[2:4,c(1,4,5)]),
                 data.frame(exp="Study 3",mod="Paranoia",out="Identification",tmd3paranoia[2:4,c(1,3,4)]),
                 data.frame(exp="Study 4a",mod="Paranoia",out="Identification",tmd4aParanoia[2:4,c(1,3,4)]),
                 data.frame(exp="Study 4b",mod="Paranoia",out="Identification",tmd4bParanoia[2:4,c(1,3,4)]),
            data.frame(exp="Studies 3 and 4",mod="Paranoia",out="Identification",tmd3and4paranoia[2:4,c(1,3,4)]),
                 data.frame(exp="Study 3",mod="Teleology",out="Identification",tmd3teleology[2:4,c(1,3,4)]),
                 data.frame(exp="Study 4a",mod="Teleology",out="Identification",tmd4aTeleology[2:4,c(1,3,4)]),
                 data.frame(exp="Study 4b",mod="Teleology",out="Identification",tmd4bTeleology[2:4,c(1,3,4)]),
            data.frame(exp="Studies 3 and 4",mod="Teleology",out="Identification",tmd3and4teleology[2:4,c(1,3,4)]),
                 data.frame(exp="Study 3",mod="Paranoia",out="Confidence",tmc3paranoia[2:4,c(1,4,5)]),
                 data.frame(exp="Study 4a",mod="Paranoia",out="Confidence",tmc4aParanoia[2:4,c(1,4,5)]),
                 data.frame(exp="Study 4b",mod="Paranoia",out="Confidence",tmc4bParanoia[2:4,c(1,4,5)]),
            data.frame(exp="Studies 3 and 4",mod="Paranoia",out="Confidence",tmc3and4paranoia[2:4,c(1,4,5)]),
                 data.frame(exp="Study 3",mod="Teleology",out="Confidence",tmc3teleology[2:4,c(1,4,5)]),
                 data.frame(exp="Study 4a",mod="Teleology",out="Confidence",tmc4aTeleology[2:4,c(1,4,5)]),
                 data.frame(exp="Study 4b",mod="Teleology",out="Confidence",tmc4bTeleology[2:4,c(1,4,5)]),
            data.frame(exp="Studies 3 and 4",mod="Teleology",out="Confidence",tmc3and4teleology[2:4,c(1,4,5)]))



# get correct labeling for tableS2
tableS2$exp <- factor(tableS2$exp, levels = unique(tableS2$exp))
tableS2$coef <- rownames(tableS2)
tableS2$coef <- ifelse(grepl(":",tableS2$coef),"Interaction",tableS2$coef)
tableS2$coef <- ifelse(grepl("conditionmirror",tableS2$coef),"Condition[mirror]",tableS2$coef)
tableS2$coef <- ifelse(grepl("bpe",tableS2$coef),"Teleology",tableS2$coef)
tableS2$coef <- ifelse(grepl("paranoia",tableS2$coef),"Paranoia",tableS2$coef)
tableS2$coef <- ifelse(grepl("wolf",tableS2$coef),"Task Type[wolf]",tableS2$coef)
# tableS2$coef <- factor(tableS2$coef,labels=c("condition[mirror]","task","paranoia","teleology","interaction"))
tableS2$mod <- factor(tableS2$mod, levels = c("Task Type","Paranoia","Teleology"))
tableS2$out <- factor(tableS2$out, levels = c("Detect","Identification","Confidence"))
tableS2$sig <- as.factor(ifelse(tableS2$p.value > 0.05,"",
                               ifelse(tableS2$p.value < 0.05 & tableS2$p.value > 0.01,"*",
                                      ifelse(tableS2$p.value < 0.01 & tableS2$p.value > 0.001,
                                             "**","***"))))

# print table S2
if (print_figure == 1) {
  write.csv(tableS2, "figures/figureS2.csv", row.names = F)
}

# Table 2 became Figure S2
figureS2 <- ggplot(tableS2,aes(x=exp,y=coef,size=abs(Estimate),fill=Estimate,coll=Estimate)) +
  labs(x="Studies",y="Mixed Models Estimates",size="Estimate",fill="Estimate",col="Estimate") + 
  geom_point(shape=c(21)) +
  geom_text(aes(label=sig), size = 5) + 
  scale_size(range = c(3,12), breaks=seq(-4.2,1.8,by=1)) +
  scale_fill_gradient2(low="red",mid="white",high="green",
                       limits=c(-4.2, 1.8), breaks=seq(-4.2,1.8,by=1)) +
  guides(fill = guide_colourbar(barwidth = 1.2, barheight = 10, ticks = T), 
         size="none") + 
  facet_grid(mod~out, scales = "free", space = "free") +
  guides(size=element_blank()) +
  theme_classic() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
figureS2

if (print_figure == 1) {
  ggsave("figures/figureS2.pdf", figureS2, dpi = 2400, scale = 1, units = "cm",
         width = 13*1.618, height = 13, bg = "white")
}




# # # # # # # # # # Sensitivity Analysis Study 4a (N=86)# # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
wolf <- expsQuests[expsQuests$experiment==4 & expsQuests$task == "wolf",]
sheep <- expsQuests[expsQuests$experiment==4 & expsQuests$task == "sheep",]


# # # Identification # # #

# Paranoia
iden_wolf_para <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                        data=wolf)
t_iden_wolf_para <- modelEstimates(iden_wolf_para)
iden_sheep_para <- glmer(correct~condition*paranoia+(condition|workerId),family=binomial,
                         data=sheep)
t_iden_sheep_para <- modelEstimates(iden_sheep_para)

ggplot(expsQuests[expsQuests$experiment==4,],aes(x=condition,y=correct,col=as.factor(paranoia))) + 
  stat_summary() + facet_grid(.~task)

# Teleolgy
iden_wolf_tele <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                          data=wolf)
t_iden_wolf_tele <- modelEstimates(iden_wolf_tele)
iden_sheep_tele <- glmer(correct~condition*bpe+(condition|workerId),family=binomial,
                           data=sheep)
t_iden_sheep_tele <- modelEstimates(iden_sheep_tele)

expsQuests$teleos <- as.factor(ifelse(expsQuests$bpe>median(expsQuests$bpe,na.rm=T),1,0))
ggplot(expsQuests[expsQuests$experiment==4,],aes(x=condition,y=correct,col=teleos)) + 
  stat_summary() + facet_grid(.~task)
expsQuests$teleos <- NULL


# # # Confidence # # #

# Paranoia
conf_wolf_para <- lmer(confidence~condition*paranoia+(condition|workerId),REML=F,data=wolf)
t_conf_wolf_para <- modelEstimates(conf_wolf_para)
conf_sheep_para <- lmer(confidence~condition*paranoia+(condition|workerId),REML=F,data=sheep)
t_conf_sheep_para <- modelEstimates(conf_sheep_para)

ggplot(expsQuests[expsQuests$experiment==4,],aes(x=condition,y=confidence,col=as.factor(paranoia))) + 
  stat_summary() + facet_grid(.~task)

# Teleolgy
conf_wolf_tele <- lmer(confidence~condition*bpe+(condition|workerId),REML=F,data=wolf)
t_conf_wolf_tele <- modelEstimates(conf_wolf_tele)
conf_sheep_tele <- lmer(confidence~condition*bpe+(condition|workerId),REML=F,data=sheep)
t_conf_sheep_tele <- modelEstimates(conf_sheep_tele)

expsQuests$teleos <- as.factor(ifelse(expsQuests$bpe>median(expsQuests$bpe,na.rm=T),1,0))
ggplot(expsQuests[expsQuests$experiment==4,],aes(x=condition,y=confidence,col=teleos)) + 
  stat_summary() + facet_grid(.~task)
expsQuests$teleos <- NULL



# # # # # # # # # # Sensitivity Analysis Sex, Paranoia, and Teleology # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# if needed
# # exp. 3, 4a, and 4b - select - Paranoia (NOTE: use workerId not subjectId)
# md3and4paranoia <- glmer(correct~condition*paranoia*sex+(condition|experiment/workerId),
#                          family=binomial, data=expsQuests[,])
# tmd3and4paranoia <- modelEstimates(md3and4paranoia)
# # exp. 3, 4a, and 4b - select - Teleology (NOTE: use workerId not subjectId)
# md3and4teleology <- glmer(correct~condition*bpe*sex+(condition|experiment/workerId),
#                           family=binomial, data=expsQuests[,])
# tmd3and4teleology <- modelEstimates(md3and4teleology)




# # # # # # # # # # Within Subject Analysis # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# exp4 <- questsExps[questsExps$experiment == 5,]
exp4 <- questsExps[questsExps$setting == "within",]
# exp4 <- questsExps

anova(lm(correct_C ~ paranoia * bpe, data = exp4[exp4$task=="sheep",]))
anova(lm(correct_C ~ paranoia * bpe, data = exp4[exp4$task=="wolf",]))
anova(lm(correct_C ~ paranoia * bpe * task, data=exp4))

summary(lm(correct_M ~ paranoia * bpe * task, data=exp4))
summary(lm(confidence_C ~ paranoia * bpe * task, data=exp4))
summary(lm(confidence_M ~ paranoia * bpe * task, data=exp4))


wolf <- exp4[exp4$task=="wolf",]
colnames(wolf)[grepl("correct_",colnames(wolf))] <- 
  paste0("wolf_", colnames(wolf)[grepl("correct_",colnames(wolf))])
colnames(wolf)[grepl("confidence_",colnames(wolf))] <-
  paste0("wolf_", colnames(wolf)[grepl("confidence_",colnames(wolf))])

sheep <- exp4[exp4$task=="sheep",]
colnames(sheep)[grepl("correct_",colnames(sheep))] <- 
  paste0("sheep_", colnames(sheep)[grepl("correct_",colnames(sheep))])
colnames(sheep)[grepl("confidence_",colnames(sheep))] <-
  paste0("sheep_", colnames(sheep)[grepl("confidence_",colnames(sheep))])

temp <- intersect(wolf$workerId,sheep$workerId)
wolf$remove <- T
sheep$remove <- T
for (i in 1:length(temp)) {
  wolf$remove[wolf$workerId==temp[i]] <- F
  sheep$remove[sheep$workerId==temp[i]] <- F
}
wolf <- wolf[wolf$remove != T,]; wolf$remove <- NULL
sheep <- sheep[sheep$remove != T,]; sheep$remove <- NULL

# ssame order
wolf <- wolf[order(wolf$workerId),]
sheep <- sheep[order(sheep$workerId),]

sum(wolf$workerId==sheep$workerId)==length(temp)

within <- cbind(wolf[,c("workerId","wolf_correct_C","wolf_correct_M",
                        "wolf_confidence_C","wolf_confidence_M",
                        "rgpts_pers","rgpts_para","bpe","caps_hardly","caps_distracting",
                        "caps_total","caps_distress",sprintf("Q2.1_%d", 1:32))],
                sheep[,c("sheep_correct_C","sheep_correct_M",
                         "sheep_confidence_C","sheep_confidence_M",
                         "experiment")])
within$paranoia <- ifelse(within$rgpts_para == "high",1,0)



# # # # # # # # # # Partial Correlation Network (Explore) # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Figure 3D
# questionnaire z-scores difference
within$difPerBpe <- scale(within$rgpts_pers)[1:nrow(within)] - scale(within$bpe)[1:nrow(within)]
# performance difference
within$difCorChaSheepWolf <- within$sheep_correct_C - within$wolf_correct_C


cor.test(within$difCorChaSheepWolf,within$difPerBpe, method = "spearman")

t.test(within$difCorChaSheepWolf-within$difPerBpe, mu = 0, alternative = "two.sided")

anova(lm(difCorChaSheepWolf~paranoia*bpe,within))

anova(lm(difCorChaSheepWolf~paranoia,within))
ggplot(within, aes(x=paranoia,y=difCorChaSheepWolf)) + 
  geom_point() + geom_smooth(method = "lm") +
  stat_cor(method = "spearman")

anova(lm(difCorChaSheepWolf~bpe,within))
ggplot(within, aes(x=bpe,y=difCorChaSheepWolf)) + 
  geom_point() + geom_smooth(method = "lm") +
  stat_cor(method = "spearman")

figure3D <- ggplot(within, aes(x=difPerBpe,y=difCorChaSheepWolf)) + 
  labs(subtitle = "N = 189",
       y="p(Select Sheep|Chase) - p(Select Wolf|Chase)",
       x="scaled(Persecution) - scaled(Teleolgy)") +
  geom_hline(yintercept = 0, col = "black") +
  geom_vline(xintercept = 0, col = "black") +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "black") +
  stat_cor(method = "spearman") + 
  theme_classic()
figure3D
ggsave("figures/figure3D.png", figure3D, dpi = 1200, scale = 1.1, units = "cm",
       width = 8, height = 8, bg = "white")


cor.test(within$difPerBpe,within$difCorChaSheepWolf,method = "spearman")
cor.test(rank(within$difPerBpe),rank(within$difCorChaSheepWolf))
shapiro.test(within$difPerBpe)
shapiro.test(within$difCorChaSheepWolf)



# # # # # # # # # # Figure 3# # # # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
shapiro.test(within$caps_total)
shapiro.test(within$rgpts_pers)
shapiro.test(within$bpe)
shapiro.test(within$wolf_correct_C)
shapiro.test(within$sheep_correct_C)
cor.test(within$caps_total,within$rgpts_pers,method = "spearman")
cor.test(within$caps_total,within$bpe,method = "spearman")
cor.test(within$caps_total,within$wolf_correct_C,method = "spearman")
cor.test(within$caps_total,within$sheep_correct_C,method = "spearman")
summary(glm(paranoia ~ sheep_correct_C, binomial, within))
summary(glm(paranoia ~ wolf_correct_C, binomial, within))
summary(glm(paranoia ~ sheep_correct_C + wolf_correct_C, binomial, within))
summary(glm(paranoia ~ sheep_correct_C + wolf_correct_C + bpe, binomial, within))
summary(glm(paranoia ~ sheep_correct_C + wolf_correct_C + bpe + caps_total, binomial, within))
summary(lm(bpe ~ wolf_correct_C, within))
summary(lm(bpe ~ sheep_correct_C, within))
summary(lm(bpe ~ wolf_correct_C + sheep_correct_C, within))
summary(lm(bpe ~ wolf_correct_C + sheep_correct_C + paranoia, within))
summary(lm(bpe ~ wolf_correct_C + sheep_correct_C + paranoia + caps_total, within))



if (!require(BGGM)) {install.packages('BGGM')}; library(BGGM)
if (!require(qgraph)) {install.packages('qgraph')}; library(qgraph)
# fig3A
vec <- c("bpe","wolf_correct_C","sheep_correct_C","paranoia")
# fig3C
# vec <- c("bpe","wolf_correct_M","sheep_correct_M","paranoia")
dims <- within[,vec]# within[within$experiment == 5,vec]
for (i in 1:ncol(dims)) {
  dims[,i] <- scale(dims[,i])[1:nrow(dims)]
}
dims$paranoia <- within$paranoia#[within$experiment == 4]

# hypothesis probabilities
summary(select(explore(dims, mixed_type = c(0,0,0,1)), alternative = "exhaustive"))

# estimate BGGM
netA <- BGGM::estimate(dims, iter = 10000, type = "mixed", mixed_type = c(0,0,0,1))
summary(netA)
sumnetA <- summary(netA)
sel.netA <- BGGM::select(netA, cred = 0.95)
sel.netA$pcor_adj <- cbind(0,sel.netA$pcor_adj)
sel.netA$pcor_adj <- rbind(0,sel.netA$pcor_adj)
nodeNames <- c("Anomalous\nPerceptions","Teleology","Wolf","Sheep","Paranoia")
# nodeNames <- c("Teleology","Wolf","Sheep","Paranoia")
qgraph(sel.netA$pcor_adj, labels = nodeNames, borders = T)
qgraph(sel.netA$pcor_adj, labels = nodeNames, borders = T, #layout = "sping",
       filetype = "png", filename = "figures/figure3A",
       height = 15, width = 15, normalize = F,
       node.height = 5, node.width = 5,
       esize=120*exp(-5/90)+1)#, #15*exp(-nNodes/90)+1)

# fig3B
vec <- c("caps_total","bpe","wolf_correct_C","sheep_correct_C","paranoia")
dims <- within[,vec]
for (i in 1:ncol(dims)) {
  dims[,i] <- scale(dims[,i])[1:nrow(dims)]
}
dims$paranoia <- within$paranoia

# hypothesis probabilities
summary(select(explore(dims, mixed_type = c(0,0,0,0,1)), alternative = "exhaustive"))

# estimate BGGM
netB <- BGGM::estimate(dims, iter = 10000, type = "mixed", mixed_type = c(0,0,0,0,1))
summary(netB)
sel.netB <- BGGM::select(netB, cred = 0.95)
nodeNames <- c("Anomalous\nPerceptions","Teleology","Wolf","Sheep","Paranoia")
qgraph(sel.netB$pcor_adj, labels = nodeNames, borders = T)
qgraph(sel.netB$pcor_adj, labels = nodeNames, borders = T, #layout = "sping",
       filetype = "png", filename = "figures/figure3B",
       height = 15, width = 15, normalize = F,
       node.height = 5, node.width = 5,
       esize=120*exp(-5/90)+1)#, #15*exp(-nNodes/90)+1)






# # # # # # # # # # Partial Correlation Network (Inferences)# # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# data for experiment 3a
exp4a <- within[within$experiment == 4,]
vec <- c("bpe","wolf_correct_C","sheep_correct_C","paranoia")
dims4 <- exp4a[,vec]
for (i in 1:ncol(dims4)) {
  dims4[,i] <- scale(dims4[,i])[1:nrow(dims4)]
}
dims4$paranoia <- exp4a$paranoia
colnames(dims4) <- c("BPE","Wolf","Sheep","Paranoia")
# dims4$CAPS <- NULL

# data for experiment 3b
exp4a <- within[within$experiment == 5,]
vec <- c("bpe","wolf_correct_C","sheep_correct_C","paranoia")
dims5 <- exp4a[,vec]
for (i in 1:ncol(dims5)) {
  dims5[,i] <- scale(dims5[,i])[1:nrow(dims5)]
}
dims5$paranoia <- exp4a$paranoia
colnames(dims5) <- c("BPE","Wolf","Sheep","Paranoia")
# dims5$CAPS <- NULL

# fit both models
# compare experiment 3a and 3b. Ideally should not be different.
fit <- ggm_compare_estimate(dims4, dims5)
plot(summary(fit))
est4 <- estimate(dims4, type = "mixed", iter = 10000, mixed_type = c(0,0,0,1))
est5 <- estimate(dims5, type = "mixed", iter = 10000, mixed_type = c(0,0,0,1))
sumest4<-summary(est4)
sumest5<-summary(est5)
qgraph(est4$pcor_mat, labels = colnames(dims4), borders = T)
qgraph(est5$pcor_mat, labels = colnames(dims5), borders = T)
est4 <- select(est4, cred = 0.95)
est5 <- select(est5, cred = 0.95)
qgraph(est4$pcor_adj, labels = colnames(dims4), borders = T)
qgraph(est5$pcor_adj, labels = colnames(dims5), borders = T)



# supplementary information Table S2
relCols <- c("Relation","Post.mean","Cred.lb","Cred.ub")
tableS2 <- data.frame(sumest4$dat_results[,relCols],
                      sumest5$dat_results[,relCols],
                      sumnetA$dat_results[,relCols])
tableS2 <- rbind(data.frame(net="Expt. 4a",sumest4$dat_results[,relCols]),
                 data.frame(net="Expt. 4b",sumest5$dat_results[,relCols]),
                 data.frame(net="Expt. 4",sumnetA$dat_results[,relCols]))
print_csv <- 1
if (print_csv == 1) {
  write.csv(tableS2,"figures/tableS2.csv",row.names = F, na = "")
}






# # # # # # # # # Tables# # # # # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# remove duplicated participants
quests <- questsExps[!(questsExps$task == "sheep" &
                         questsExps$version == "detect-sheep-and-wolf"),]

# relevant columns
relevant_columns <- c("demo_age","demo_sex","survey_duration","rgpts_refe",
                      "rgpts_pers","rgpts_para","bpe","caps_total",
                      "caps_distress","caps_distracting","caps_hardly")
# add non existent columns
for (i in 1:length(relevant_columns)) {
  if (sum(colnames(quest1)==relevant_columns[i]) == 0) {
    quest1 <- cbind(quest1,NA)
    colnames(quest1)[ncol(quest1)] <- relevant_columns[i]
  }
  if (sum(colnames(quest2)==relevant_columns[i]) == 0) {
    quest2 <- cbind(quest2,NA)
    colnames(quest2)[ncol(quest2)] <- relevant_columns[i]
  }
  if (sum(colnames(quests)==relevant_columns[i]) == 0) {
    quests <- cbind(quests,NA)
    colnames(quests)[ncol(quests)] <- relevant_columns[i]
  }
}

# combine 
quest <- rbind(quest1[,c("experiment",relevant_columns)],
               quest2[,c("experiment",relevant_columns)],
               quests[,c("experiment",relevant_columns)])


# create descriptive guide skeleton
descrGuide <- matrix(c(relevant_columns,
                       1,0,1,1,1,0,1,1,1,1,1),ncol=2)

# library(dplyr)
# quest %>% group_by(version) %>%
#   dplyr::summarise(m_age=mean(demo_age,na.rm=T),sd_age=sd(demo_age,na.rm=T))
# built appendix (Table A1 and A3)

# produce appendix tables 
tabChase <- data.frame(version="detect-chase",f_suppTables(quest[quest$experiment==1,],descrGuide))
tabWolf <- data.frame(version="detect-wolf",f_suppTables(quest[quest$experiment==2,],descrGuide))
tabSheep <- data.frame(version="detect-sheep",f_suppTables(quest[quest$experiment==3,],descrGuide))
tabBothA <- data.frame(version="detect-sheep-and-wolfA",f_suppTables(quest[quest$experiment==4,],descrGuide))
tabBothB <- data.frame(version="detect-sheep-and-wolfB",f_suppTables(quest[quest$experiment==5,],descrGuide))
tabChaseConf <- data.frame(version="detect-chase-confidence",f_suppTables(quest[quest$experiment==6,],descrGuide))

# combine
tabAll <- rbind(tabChase,tabChaseConf,tabWolf,tabSheep,tabBothA,tabBothB)

print_csv <- 1
if (print_csv == 1) {
  write.csv(tabAll,"figures/tableS1.csv",row.names = F, na = "")
}



