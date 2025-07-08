# script created by Santiago Castiello de Obeso 
# date: 08/07/2025
# scripts found in socialHallucinations repository: https://github.com/santiagocdo/socialHallucinations

# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))


perceptionAndTime <- function (expQuest, subjectId, window_size = 10,
                               add_vars = NULL) {
  # ids (or participants)
  ids <- unique(expQuest[,subjectId])
  
  # trial type (chase or no chase) and response (chase or no chase)
  rel_cols <- c("condition","choice")
  
  # change condition to binary (1 for chase 0 for no chase)
  expQuest[,rel_cols[1]] <- ifelse(expQuest[,rel_cols[1]]=="chase",1,0)
  
  for (i in 1:length(ids)) {
    # read one id
    tmp <- expQuest[expQuest[,subjectId] == ids[i],]
    
    # create an empty data.frame to be filled in the windowing
    one_subj <- data.frame(from=NA,to=NA,p_ch=NA,p_r_ch=NA,ch=NA,r_ch=NA)
    for (j in 1:(nrow(tmp)-window_size)) {
      # fill every window jth
      one_subj <- rbind(one_subj,
                        data.frame(from=j, to=j+window_size-1,
                                   p_ch = mean(tmp[j:(j+window_size-1), rel_cols[1]]),
                                   p_r_ch = mean(tmp[j:(j+window_size-1), rel_cols[2]]),
                                   ch = tmp[j+window_size, rel_cols[1]],
                                   r_ch = tmp[j+window_size, rel_cols[2]]))
    }
    
    # combine participants
    if (i == 1) {
      output <- data.frame(ids[i],tmp[1,add_vars],one_subj)
    } else {
      output <- rbind(output, data.frame(ids[i],tmp[1,add_vars],one_subj))
    }
  }
  # name of the columns
  colnames(output) <- c("subjectId",add_vars,"from","to","p_ch","p_r_ch","ch","r_ch")
  # remove empty rows
  output <- output[!is.na(output$ch),]
  return(output)
}


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
remove_invalid <- 0
print_figure <- 1

# # # # read behaviour # # # # 
# experiment 1 (detect-chase)
exp1 <- read.csv("data/behaviour/exp1-chase.csv")
exp1$subjectId <- exp1$workerId

# experiment 2 (detect-chase-confidence)
exp2 <- read.csv("data/behaviour/exp6-chase-confidence.csv")
# exp2$subjectId <- exp2$participantId # no needed, but becarefull it contains 2 PROLIFIC_PID



# # # # read questionnaires # # # # 
# experiment 1
quest1 <- read.csv("data/questionnaire/wideFormat_exp1.csv")
quest1$subjectId <- quest1$workerId
# experiment 2
quest2 <- read.csv("data/questionnaire/wideFormat_exp2.csv")
# quest2$subjectId <- quest2$participantId # no needed, but be carefull it contains 2 PROLIFIC_PID



# # # # # # # # # # combine data sets # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
if (remove_invalid == 1) {
  # remove invalid trials and participants 
  # exclusion criteria already applied when cleaning data
  quest1 <- quest1[quest1$valid_id == T,]
  quest2 <- quest2[quest2$valid_id == T,]
  exp1 <- exp1[exp1$valid_id == T & exp1$valid_trials ==  T,]
  exp2 <- exp2[exp2$valid_id == T & exp2$valid_trials ==  T,]
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






# output1_10 <- perceptionAndTime(exp1Quest1,"subjectId", 10, add_vars="rgpts_para")
# output1_10 <- output1_10[output1_10$p_ch != 0 & output1_10$p_ch != 1,]
output1_6 <- perceptionAndTime(exp1Quest1,"subjectId", 6, add_vars="rgpts_para")

output1 <- output1_6

m1 <- glmer(r_ch ~ p_ch * rgpts_para + (1|subjectId), family = "binomial", output1)
summary(m1)
(p1 <- ggplot(output1, aes(x=p_ch,y=ch,col=rgpts_para)) + 
    labs(title="exp1",
         x="p(chase trial, last 6ts)",
         y="p(chase trial, t+1)") + 
    geom_smooth(method="lm",se=F) +
    stat_summary())
(p3 <- ggplot(output1, aes(x=p_ch,y=r_ch,col=rgpts_para)) + 
    labs(title="exp1",
         x="p(chase trial, last 6ts)",
         y="p(detecting chase, t+1)") + 
    geom_smooth(method="lm",se=F) +
    stat_summary())
(ggplot(output1, aes(x=p_ch,y=r_ch)) + 
    labs(title="exp1",
         x="p(chase trial, last 6ts)",
         y="p(detecting chase, t+1)") + 
    stat_summary())
m1 <- glmer(r_ch ~ p_r_ch * rgpts_para + (1|subjectId), family = "binomial", output1)
summary(m1)
(p5 <- ggplot(output1, aes(x=p_r_ch,y=r_ch,col=rgpts_para)) + 
    labs(title="exp1",
         x="p(detecting chase, last 6ts)",
         y="p(detecting chase, t+1)") + 
    geom_smooth(method="lm",se=F) +
    stat_summary())




# output2_10 <- perceptionAndTime(exp2Quest2,"subjectId", 10, add_vars="rgpts_para")
# output2_10 <- output2_10[output2_10$p_ch != 0 & output2_10$p_ch != 1,]
output2_6 <- perceptionAndTime(exp2Quest2,"subjectId", 6, add_vars="rgpts_para")

output2 <- output2_6

m2 <- glmer(r_ch ~ p_ch * rgpts_para + (1|subjectId), family = "binomial", output2)
summary(m2)
(p2 <- ggplot(output2, aes(x=p_ch,y=ch,col=rgpts_para)) + 
    labs(title="exp2",
         x="p(chase trial, last 6ts)",
         y="p(chase trial, t+1)") + 
    geom_smooth(method="lm",se=F) +
    stat_summary())
(p4 <- ggplot(output2, aes(x=p_ch,y=r_ch,col=rgpts_para)) + 
    labs(title="exp2",
         x="p(chase trial, last 6ts)",
         y="p(detecting chase, t+1)") + 
    geom_smooth(method="lm",se=F) +
    stat_summary())
(ggplot(output2, aes(x=p_ch,y=r_ch)) + 
    labs(title="exp2",
         x="p(chase trial, last 6ts)",
         y="p(detecting chase, t+1)") + 
    stat_summary())
m2 <- glmer(r_ch ~ p_r_ch * rgpts_para + (1|subjectId), family = "binomial", output2)
summary(m2)
(p6 <- ggplot(output2, aes(x=p_r_ch,y=r_ch,col=rgpts_para)) + 
    labs(title="exp2",
         x="p(detecting chase, last 6ts)",
         y="p(detecting chase, t+1)") + 
    geom_smooth(method="lm",se=F) +
    stat_summary())



ggarrange(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2,common.legend = T)
