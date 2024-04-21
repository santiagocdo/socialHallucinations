# # # # # # # # # # Other stuff # # # # # # # # # # 
loadPackages <- function(libraries) {
  # this function receives a character vector with libraries names and download 
  # them if necessary
  if (class(libraries)!="character") {warning("vector is not character")}
  # loop libraries vector
  for (i in 1:length(libraries)) {
    # Check if the package is already installed
    if (!requireNamespace(libraries[i], quietly = TRUE)) {
      # If not installed, install the package
      install.packages(libraries[i], dependencies = TRUE)
    } else {
      cat(paste("Package", libraries[i], "is already installed.\n"))
    }
    # require(libraries[i])
  }
}

# add behaviour (long format) to questionnaire (wide format)
addBehaviour <- function(to,from) {
  # experiment type
  exp <- unique(from$experiment)[1]
  # order subjects id
  to <- to[order(to$subjectId),]
  from <- from[order(from$subjectId),]
  # for looped
  for (i in 1:nrow(to)) {
    # get one subject
    temp <- from[from$subjectId == to$subjectId[i],]
    # if (exp == 6) {temp <- temp[temp$trial_num<=100,]}
    tempMod <- sdtModel(temp, c("Hit","FA","Ms","CR"))
    
    # in the case of experiment 1 (recorded as 1), confidence is null
    if (exp == 1) {
      if (i == 1) {
        wideFormat <- cbind(to[i,],data.frame(sensitivity=tempMod$sensitivity,
                                              response_criterion=tempMod$response_criterion,
                                              t(tempMod$sdtTable),
                                              hit_rate=tempMod$hit_rate,fa_rate=tempMod$fa_rate,
                                              correct=mean(temp$correct),
                                              detection=mean(temp$choice),
                                              reaction_time=mean(temp$reaction_time)))
      } else {
        wideFormat <- rbind(wideFormat,
                            cbind(to[i,],data.frame(sensitivity=tempMod$sensitivity,
                                                    response_criterion=tempMod$response_criterion,
                                                    t(tempMod$sdtTable),
                                                    hit_rate=tempMod$hit_rate,fa_rate=tempMod$fa_rate,
                                                    correct=mean(temp$correct),
                                                    detection=mean(temp$choice),
                                                    reaction_time=mean(temp$reaction_time))))
      } # end if subject i
      
    # in the case of experiment 2 (recorded as 6), confidence is present in detect-chase  
    } else if (exp == 6) {
      if (i == 1) {
        wideFormat <- cbind(to[i,],data.frame(sensitivity=tempMod$sensitivity,
                                              response_criterion=tempMod$response_criterion,
                                              t(tempMod$sdtTable),
                                              hit_rate=tempMod$hit_rate,fa_rate=tempMod$fa_rate,
                                              correct=mean(temp$correct),
                                              confidence_C=mean(temp$confidence[temp$condition=="chase"], na.rm = T),
                                              confidence_M=mean(temp$confidence[temp$condition=="mirror"], na.rm = T),
                                              detection=mean(temp$choice),
                                              reaction_time=mean(temp$reaction_time)))
      } else {
        wideFormat <- rbind(wideFormat,
                            cbind(to[i,],data.frame(sensitivity=tempMod$sensitivity,
                                                    response_criterion=tempMod$response_criterion,
                                                    t(tempMod$sdtTable),
                                                    hit_rate=tempMod$hit_rate,fa_rate=tempMod$fa_rate,
                                                    correct=mean(temp$correct),
                                                    confidence_C=mean(temp$confidence[temp$condition=="chase"], na.rm = T),
                                                    confidence_M=mean(temp$confidence[temp$condition=="mirror"], na.rm = T),
                                                    detection=mean(temp$choice),
                                                    reaction_time=mean(temp$reaction_time))))
      } # end if subject i
      
    } else { # otherwise, experiment 2 (between) and 3 (within)
      if (i == 1) {
        wideFormat <- cbind(to[i,],data.frame(sensitivity=tempMod$sensitivity,
                                              response_criterion=tempMod$response_criterion,
                                              t(tempMod$sdtTable),
                                              hit_rate=tempMod$hit_rate,fa_rate=tempMod$fa_rate,
                                              correct_C=mean(temp$correct[temp$condition=="chase"]),
                                              correct_M=mean(temp$correct[temp$condition=="mirror"]),
                                              confidence=mean(temp$confidence),
                                              confidence_C=mean(temp$confidence[temp$condition=="chase"]),
                                              confidence_M=mean(temp$confidence[temp$condition=="mirror"])))
      } else {
        wideFormat <- rbind(wideFormat,
                            cbind(to[i,],data.frame(sensitivity=tempMod$sensitivity,
                                                    response_criterion=tempMod$response_criterion,
                                                    t(tempMod$sdtTable),
                                                    hit_rate=tempMod$hit_rate,fa_rate=tempMod$fa_rate,
                                                    correct_C=mean(temp$correct[temp$condition=="chase"]),
                                                    correct_M=mean(temp$correct[temp$condition=="mirror"]),
                                                    confidence=mean(temp$confidence),
                                                    confidence_C=mean(temp$confidence[temp$condition=="chase"]),
                                                    confidence_M=mean(temp$confidence[temp$condition=="mirror"]))))
      } # end if subject i
    } # end for loop
    
  } # end loop
  return(wideFormat)
}

# add questionnaire (wide format) to behaviour (long format)
addQuestionnaires <- function(to,from,scores) {
  # order subjects id
  to <- to[order(to$subjectId),]
  from <- from[order(from$subjectId),]
  # add extra columns
  extra <- matrix(NA,nrow(to),ncol=length(scores))
  colnames(extra) <- scores
  to <- cbind(to,extra)
  # for looped
  for (i in 1:nrow(from)) {
    # get one subject and one score j
    for (j in 1:length(scores)) {
      to[to$subjectId == from$subjectId[i],scores[j]] <- from[i,scores[j]]
    }
  } # end loop
  longFormat <- to
  return(longFormat)
}

# use sdt classical metrics to a dataset
sdtModel <- function (data, events, confidence = NULL) {
  # NOTE: events must be ordered as follows: hit, FA, Ms, CR
  # SDT cell frequencies 
  sdtTable <- colSums(data$cells==t(matrix(rep(events,nrow(data)),ncol=nrow(data))))
  hit_rate <- sdtTable[1]/(sdtTable[1]+sdtTable[3]) # p(detection|signal)
  fa_rate <- sdtTable[2]/(sdtTable[2]+sdtTable[4]) # p(detection|noise)
  # http://wise.cgu.edu/wise-tutorials/tutorial-signal-detection-theory/signal-detection-d-defined-2/
  if (hit_rate == 0 | is.nan(hit_rate)) {
    hit_rate <- 1/nrow(data)
  } else if (hit_rate == 1) {
    hit_rate <- (nrow(data)-1)/nrow(data)
  }
  if (fa_rate == 0 | is.nan(fa_rate)) {
    fa_rate <- 1/nrow(data)
  } else if (fa_rate == 1) {
    fa_rate <- (nrow(data)-1)/nrow(data)
  }
  # estimate inverse variance of signal and noise distribution using confidence
  if (!is.null(confidence)) {
    # normalize (divided by the subjective confidence mean) confidence
    data[,confidence] <- data[,confidence]/mean(data[,confidence])
    library(dplyr)
    temp <- as.data.frame(data %>% group_by(cells) %>%
      summarise(confidence=mean(confidence,na.rm=T)))
    # assuming all cells has at least one trial/observation
    if (length(intersect(temp$cells,events))!=length(events)) {
      # assuming one event is missing, then add row with it and 0
      temp <- rbind(temp,data.frame(cells=outersect(temp$cells,events),
                                    confidence=NA))
    }
    temp <- temp[order(temp$cells),]
    # hetocedasticity. Variance is the confidence-inverse and confidence is precision
    var_signal <- 1/mean(temp$confidence[temp$cells=="Hit"|temp$cells=="Ms"],na.rm=T)
    var_noise <- 1/mean(temp$confidence[temp$cells=="FA"|temp$cells=="CR"],na.rm=T)
  } else {
    # homocedasticity 
    var_signal <- 1
    var_noise <- 1
  }
  
  # sensitivity (d')
  sensitivity <- qnorm(hit_rate,0,var_signal) - qnorm(fa_rate,0,var_noise)
  # response criterion
  response_criterion  <- -1*(qnorm(hit_rate,0,var_signal) + qnorm(fa_rate,0,var_noise)) / 2
  # prepare output
  names(sdtTable) <- events
  # return list
  return(list(sensitivity=sensitivity,response_criterion=response_criterion,
              sdtTable=sdtTable,hit_rate=hit_rate,fa_rate=fa_rate,
              var=data.frame(signal=var_signal,noise=var_noise)))
}

# more info information https://statistics.berkeley.edu/computing/r-t-tests
meanDifference <- function(sample1, sample2, paired){
  
  alpha <- 0.05 
  
  norm1 <- shapiro.test(sample1) # Shapiro-Wilk normality test
  norm2 <- shapiro.test(sample2) # Shapiro-Wilk normality test
  
  homoce <- var.test(sample1,sample2) # F test to compare two variances
  
  if (homoce$p.value > alpha) {var_equal = 1} else {var_equal = 0} 
  # there is a difference in t test depending homocedasticity. 
  # For var_equal: 1 = Two Sample t-test; 0 = Welch Two Sample t-test
  if (norm1$p.value > alpha  & norm2$p.value > alpha) {
    test <- t.test(sample1,sample2, paired = paired, var.equal = var_equal)
  } else {
    test <- wilcox.test(sample1,sample2, paired = paired)
  }
  return(test)
}


plotFigure2 <- function (quest1exp1,quest2exp2,questsExps,
                         exp1Quest1,exp2Quest2,expsQuests) {
  # # # colors # # #
  paraColour <- c("#F29199","#1F5B73")
  teleColour <- c("#8EA676","#F2921D")
  wolfSheep <- c("#F2B885","#262014")
  
  # # # read images # # #
  chase_img <- readPNG("figures/icons/chaseDB.png")
  wolf_img <- readPNG("figures/icons/wolfDB.png")
  sheep_img <- readPNG("figures/icons/sheepDB.png")
  
  # # # custom layer for ggplot2 # # #
  # https://stackoverflow.com/questions/44688623/adding-custom-images-to-ggplot-facets
  require(ggplot2); require(grid); require(png); require(RCurl)
  annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
    layer(data = data, stat = StatIdentity, position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
  }
  
  
  # # # # EXPERIMENT 1 and 2 # # # #
  # figure 2A based on all the classic chase detection SDT
  quest1exp1$confidence_M <- quest1exp1$confidence_C <- NA
  sameCols <- c("experiment","version","demo_age","demo_sex",
                "subjectId","survey_duration","interview_date","interview_date_full",
                "sensitivity","response_criterion","Hit","FA","Ms","CR","hit_rate",
                "fa_rate","correct","confidence_C","confidence_M","detection",
                "reaction_time","paranoia")
  questDetect <- rbind(quest1exp1[,sameCols], quest2exp2[,sameCols])
  exp1_2_parameters <- c("hit_rate","fa_rate","sensitivity","response_criterion",
                         "correct","detection","reaction_time","confidence_M","confidence_C")
  # # # # Brian Scholl's lab meeting # # # #
  # questDetect <- rbind(quest1exp1[,sameCols])
  # exp1_2_parameters <- c("hit_rate","fa_rate","sensitivity","response_criterion",
  #                        "correct","detection","reaction_time")
  questDetectMelt <- reshape2::melt(questDetect, measure.vars = exp1_2_parameters)
  questDetectMelt <- questDetectMelt[!is.na(questDetectMelt$value),]
  
  # significance difference
  sig <- estimate <- means <- as.vector(rep(NA,length(exp1_2_parameters)))
  effect_size <- matrix(NA,nrow=length(exp1_2_parameters),ncol=3)
  for (i in 1:length(exp1_2_parameters)) {
    temp <- questDetectMelt$value[questDetectMelt$variable == exp1_2_parameters[i]]
    questDetectMelt$value[questDetectMelt$variable == exp1_2_parameters[i]] <- 
      scale(temp)[1:length(temp)]
    means[i] <- mean(scale(temp)[1:length(temp)], na.rm=T)
    temp2 <- glm(paranoia~., family = "binomial",
                 data = questDetect[,c("paranoia",exp1_2_parameters[i])])
    temp3 <- report::report_table(temp2)
    sig[i] <- temp3$p[2]
    estimate[i] <- temp3$Coefficient[2]
    effect_size[i,] <- unlist(temp3[2,c("Std_Coefficient","Std_Coefficient_CI_low",
                                 "Std_Coefficient_CI_high")])
  }
  print <- data.frame(exp1_2_parameters,means,
                      round(data.frame(estimate,sig,
                                       Std_Coefficient=effect_size[,1],
                                       Std_Coefficient_CI_low=effect_size[,2],
                                       Std_Coefficient_CI_high=effect_size[,3]),4))
  write.csv(print, "figures/fig2A.csv",row.names = F)
  # paste(round(mean(questDetect$hit_rate),2),"SD", round(sd(questDetect$hit_rate),2))
  # paste(round(mean(questDetect$fa_rate),2),"SD", round(sd(questDetect$fa_rate),2))
  # paste(round(mean(questDetect$sensitivity),2),"SD", round(sd(questDetect$sensitivity),2))
  # paste(round(mean(questDetect$response_criterion),2),"SD", round(sd(questDetect$response_criterion),2))
  # paste(round(mean(questDetect$correct),2),"SD", round(sd(questDetect$correct),2))
  # paste(round(mean(questDetect$detection),2),"SD", round(sd(questDetect$detection),2))
  # paste(round(mean(questDetect$reaction_time),2),"SD", round(sd(questDetect$reaction_time),2))
  # paste(round(mean(questDetect$confidence_M,na.rm=T),2),"SD", round(sd(questDetect$confidence_M,na.rm=T),2))
  # paste(round(mean(questDetect$confidence_C,na.rm=T),2),"SD", round(sd(questDetect$confidence_C,na.rm=T),2))
  # meanDifference(questDetect$hit_rate[questDetect$paranoia==0],
  #                questDetect$hit_rate[questDetect$paranoia==1],F)
  # meanDifference(questDetect$fa_rate[questDetect$paranoia==0],
  #                questDetect$fa_rate[questDetect$paranoia==1],F)
  # meanDifference(questDetect$sensitivity[questDetect$paranoia==0],
  #                questDetect$sensitivity[questDetect$paranoia==1],F)
  # meanDifference(questDetect$response_criterion[questDetect$paranoia==0],
  #                questDetect$response_criterion[questDetect$paranoia==1],F)
  # meanDifference(questDetect$correct[questDetect$paranoia==0],
  #                questDetect$correct[questDetect$paranoia==1],F)
  # meanDifference(questDetect$detection[questDetect$paranoia==0],
  #                questDetect$detection[questDetect$paranoia==1],F)
  # meanDifference(questDetect$reaction_time[questDetect$paranoia==0],
  #                questDetect$reaction_time[questDetect$paranoia==1],F)
  # meanDifference(questDetect$confidence_M[questDetect$paranoia==0],
  #                questDetect$confidence_M[questDetect$paranoia==1],F)
  # meanDifference(questDetect$confidence_C[questDetect$paranoia==0],
  #                questDetect$confidence_C[questDetect$paranoia==1],F)
  
  # create data frame for significant experiment 1
  temp4 <- data.frame(exp1_2_parameters,sig,dich=
                        ifelse(sig<0.001,"***",#p < 0.001
                               ifelse(sig>0.001&sig<0.01,"**",#p < 0.01
                                      ifelse(sig>0.01&sig<0.05,"*",""))))#p < 0.05
  questDetectMelt$paranoia1 <- factor(questDetectMelt$paranoia, levels = c("1","0"))
  fig2A <- ggplot(questDetectMelt, aes(x=variable,y=value,col=paranoia1,
                                       fill=paranoia1,shape=paranoia1)) +
    labs(shape="Paranoia:",fill="Paranoia:",col="Paranoia:",
         y = "Scaled Scores") +
    stat_summary(fun.data = mean_se, position = position_dodge(0.2)) +
    scale_color_manual(values = paraColour, labels = c("High","Low")) +
    scale_fill_manual(values = paraColour, labels = c("High","Low")) +
    scale_shape_manual(values = c(24,25), labels = c("High","Low")) +
    scale_y_continuous(breaks = c(-0.5,0,0.5)) +
    scale_x_discrete(labels = c('hit_rate'="Hit Rate",
                                'fa_rate'="False Alarm Rate",
                                'sensitivity'="Sensitivity: d'",
                                'response_criterion'="Response Criterion: C",
                                'correct'="Correctness",
                                'detection'="p(Detect Chase)",
                                'reaction_time'="Decision Time",
                                'confidence_M'="Mirror Confidence",
                                'confidence_C'="Chase Confidence")) +
    annotate("text",y=means, x=seq(1.2,9.2,1),label=temp4$dich,size=3.5) +
    coord_cartesian(ylim = c(-0.6,0.6)) +
    theme_classic() + theme(legend.position = "none",
                            legend.background = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x = element_text(angle = 30, hjust = 1)) +
    # annotation_raster(chase_img, ymin = 0.2, ymax = 0.6, xmin = 2, xmax = 3.6)
    annotation_raster(chase_img,
                      ymin = 0.18,ymax = 0.6,
                      xmin = 2, xmax = 3.7)
  # fig2A
  # brian1 <- ggplot(questDetectMelt, aes(x=variable,y=value,col=paranoia1,
  #                                     fill=paranoia1,shape=paranoia1)) +
  #   labs(shape="Paranoia:",fill="Paranoia:",col="Paranoia:",
  #        y = "Scaled Scores") +
  #   geom_hline(yintercept = 0, col="black", alpha = 0.5) +
  #   geom_point(position = position_dodge(0.2), alpha = 0.2,size=0.5) +
  #   stat_summary(fun.data = mean_se, position = position_dodge(0.2),size=1) +
  #   scale_color_manual(values = paraColour, labels = c("High","Low")) +
  #   scale_fill_manual(values = paraColour, labels = c("High","Low")) +
  #   scale_shape_manual(values = c(24,25), labels = c("High","Low")) +
  #   scale_y_continuous(breaks = seq(-3,3,by=1)) +
  #   scale_x_discrete(labels = c('hit_rate'="Hit Rate",
  #                               'fa_rate'="False Alarm Rate",
  #                               'sensitivity'="Sensitivity: d'",
  #                               'response_criterion'="Response Criterion: C",
  #                               'correct'="Correctness",
  #                               'detection'="p(Detect Chase)",
  #                               'reaction_time'="Decision Time")) +
  #   annotate("text",y=means, x=seq(1.3,7.3,1),label=temp3$dich,size=5) +
  #   # coord_cartesian(ylim = c(-0.6,0.6)) +
  #   theme_classic() + theme(legend.position = c(0.09,0.9),
  #                           legend.background = element_blank(),
  #                           axis.title.x = element_blank(),
  #                           axis.text.x = element_text(angle = 30, hjust = 1))
  #   # annotation_raster(chase_img, ymin = 0.2, ymax = 0.6, xmin = 2, xmax = 3.6)
  # # brian1
  
  
  
  # # # EXPERIMENT 2 # # # #
  # figure 2A based on all the classic chase detection SDT
  exp2_parameters <- c("hit_rate","fa_rate","sensitivity","response_criterion",
                       "correct","detection","reaction_time","confidence_M","confidence_C")
  quest2exp2melt <- reshape2::melt(quest2exp2, measure.vars = exp2_parameters)
  
  # significance difference
  sig <- estimate <- means <- as.vector(rep(NA,length(exp2_parameters)))
  effect_size <- matrix(NA,nrow=length(exp2_parameters),ncol=3)
  for (i in 1:length(exp2_parameters)) {
    temp <- quest2exp2melt$value[quest2exp2melt$variable == exp2_parameters[i]]
    quest2exp2melt$value[quest2exp2melt$variable == exp2_parameters[i]] <-
      scale(temp)[1:length(temp)]
    means[i] <- mean(  scale(temp)[1:length(temp)], na.rm=T)
    temp2 <- lm(bpe~., data = quest2exp2[,c("bpe",exp2_parameters[i])])
    temp3 <- report::report_table(temp2)
    sig[i] <- temp3$p[2]
    estimate[i] <- temp3$Coefficient[2]
    effect_size[i,] <- unlist(temp3[2,c("Std_Coefficient","Std_Coefficient_CI_low",
                                        "Std_Coefficient_CI_high")])
  }
  print <- data.frame(exp2_parameters,means,
                      round(data.frame(estimate,sig,
                                       Std_Coefficient=effect_size[,1],
                                       Std_Coefficient_CI_low=effect_size[,2],
                                       Std_Coefficient_CI_high=effect_size[,3]),4))
  write.csv(print, "figures/fig2B.csv",row.names = F)
  # shapiro.test(quest2exp2$bpe)
  # shapiro.test(quest2exp2$hit_rate)
  # shapiro.test(quest2exp2$fa_rate)
  # shapiro.test(quest2exp2$sensitivity)
  # shapiro.test(quest2exp2$response_criterion)
  # shapiro.test(quest2exp2$correct)
  # shapiro.test(quest2exp2$detection)
  # shapiro.test(quest2exp2$reaction_time)
  # shapiro.test(quest2exp2$confidence_M)
  # shapiro.test(quest2exp2$confidence_C)
  # cor.test(quest2exp2$hit_rate,quest2exp2$bpe,method = "spearman")
  # cor.test(quest2exp2$fa_rate,quest2exp2$bpe)
  # cor.test(quest2exp2$sensitivity,quest2exp2$bpe)
  # cor.test(quest2exp2$response_criterion,quest2exp2$bpe)
  # cor.test(quest2exp2$correct,quest2exp2$bpe)
  # cor.test(quest2exp2$detection,quest2exp2$bpe)
  # cor.test(quest2exp2$reaction_time,quest2exp2$bpe,method = "spearman")
  # cor.test(quest2exp2$confidence_M,quest2exp2$bpe)
  # cor.test(quest2exp2$confidence_C,quest2exp2$bpe,method = "spearman")
  
  # create data frame for significant experiment 1
  temp4 <- data.frame(exp2_parameters,sig,dich=
                        ifelse(sig<0.001,"***",#p < 0.001
                               ifelse(sig>0.001&sig<0.01,"**",#p < 0.01
                                      ifelse(sig>0.01&sig<0.05,"*",""))))#p < 0.05
  quest2exp2melt$bpe1 <- factor(ifelse(quest2exp2melt$bpe<median(quest2exp2melt$bpe),"0","1"),
                                levels = c("1","0"))
  teleolgy <- quest2exp2melt
  fig2B <- ggplot(quest2exp2melt, aes(x=variable,y=value,col=bpe1,
                                      fill=bpe1,shape=bpe1)) +
    labs(shape="Teleology:",fill="Teleology:",col="Teleology:",
         y = "Scaled Scores") +
    stat_summary(fun.data = mean_se, position = position_dodge(0.2)) +
    scale_color_manual(values = teleColour, labels = c("High","Low")) +
    scale_fill_manual(values = teleColour, labels = c("High","Low")) +
    scale_shape_manual(values = c(24,25),labels = c("High","Low")) +
    scale_y_continuous(breaks = c(-0.5,0,0.5)) +
    scale_x_discrete(labels = c('hit_rate'="Hit Rate",
                                'fa_rate'="False Alarm Rate",
                                'sensitivity'="Sensitivity: d'",
                                'response_criterion'="Response Criterion: C",
                                'correct'="Correctness",
                                'detection'="p(Detect Chase)",
                                'reaction_time'="Decision Time",
                                'confidence_M'="Mirror Confidence",
                                'confidence_C'="Chase Confidence")) +
    annotate("text",y=means, x=seq(1.2,9.2,1),label=temp4$dich,size=3.5) +
    coord_cartesian(ylim = c(-0.6,0.6)) + 
    theme_classic() +  theme(legend.position = "top",
                             legend.background = element_blank(),
                             axis.title.x = element_blank(),
                             axis.text.x = element_text(angle = 30, hjust = 1)) +
    # annotation_raster(chase_img, ymin = 0.2, ymax = 0.6, xmin = 0.4, xmax = 2)
    annotation_raster(chase_img, 
                      ymin = 0.18,ymax = 0.6,#ymin = 0.6*(1/3)*0.9, ymax = 0.6, 
                      xmin = 0.4, xmax = 2.1)#,xmin = 2*0.2*0.9, xmax = 2)
  # fig2B

  
  
  # condition to numeric for easy plot
  expsQuests$condition2 <- ifelse(expsQuests$condition == "chase",2,1)
  # expsQuests$condition2 <- ifelse(expsQuests$rgpts_para == "high",
  #                                 expsQuests$condition2 + 0.1,
  #           
  
  # side space from x margins to high low mirror and high chase
  sideSpace <- 0.4
  
  # filter data base?
  temp <- expsQuests[!is.na(expsQuests$rgpts_para),]
  temp$task <- ifelse(temp$task == "sheep","Sheep","Wolf")
  # temp$rgpts_para <- ifelse(temp$rgpts_para == "low","Low","High")
  
  p1 <- annotation_custom2(rasterGrob(wolf_img, interpolate=TRUE), xmin=0.3, xmax=1.3, ymin=0.55, ymax=1, data=temp[1,])
  p2 <- annotation_custom2(rasterGrob(sheep_img, interpolate=TRUE), xmin=0.3, xmax=1.3, ymin=0.55, ymax=1, data=temp[101,])
  fig2C <- ggplot(temp[,], aes(x=condition2,y=correct,col=rgpts_para,shape=rgpts_para,fill=rgpts_para)) + 
    labs(x="Condition",y="p(Correct Identification)",col="Paranoia:",shape="Paranoia:",fill="Paranoia:") +
    geom_hline(yintercept = 1/8, linetype = "dashed") +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "line", alpha=0.05) +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "point", alpha=0.05,stroke=0) +
    geom_smooth(method="lm", se=F, size=1.1) +
    stat_summary(fun="mean", geom="point", size=4) +
    scale_color_manual(values = paraColour,labels = c("High","Low")) +
    scale_fill_manual(values = paraColour,labels = c("High","Low")) +
    scale_shape_manual(values = c(24,25),labels = c("High","Low")) +
    scale_x_continuous(breaks = c(1,2), labels = c("Mirror","Chase"),
                       limits = c(1-sideSpace,2+sideSpace)) +
    scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1)) +
    facet_grid(.~task) +
    theme_classic() + theme(legend.position = "none",
                            axis.title.x = element_blank())
  fig2C <- fig2C + p1 + p2
  # fig2C
  fig2E <- ggplot(temp[,], aes(x=condition2,y=confidence,col=rgpts_para,
                            shape=rgpts_para,fill=rgpts_para)) + 
    labs(x="Condition",y="Confidence",col="Paranoia:",shape="Paranoia:",fill="Paranoia:") +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "line", alpha=0.05) +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "point", alpha=0.05,stroke=0) +
    geom_smooth(method="lm", se=F, size=1.1) +
    stat_summary(fun="mean", geom="point", size=4) +
    scale_color_manual(values = paraColour,labels = c("High","Low")) +
    scale_fill_manual(values = paraColour,labels = c("High","Low")) +
    scale_shape_manual(values = c(24,25),labels = c("High","Low")) +
    scale_x_continuous(breaks = c(1,2), labels = c("Mirror","Chase"),
                       limits = c(1-sideSpace,2+sideSpace)) +
    scale_y_continuous(breaks = c(1,3,5),limits = c(1,5)) +
    facet_grid(.~task) +
    theme_classic() + theme(legend.position = "none",
                            axis.title.x = element_blank())
  # fig2E
  
  
  
  # add categorical variable
  expsQuests$bpe_high <- ifelse(expsQuests$bpe < median(expsQuests$bpe,na.rm = T), 
                                "Low","High")
  
  # condition in numeric
  expsQuests$condition2 <- ifelse(expsQuests$condition == "chase",2,1)
  # expsQuests$condition2 <- ifelse(expsQuests$bpe_high == "High", 
  #                                 expsQuests$condition2 + 0.1,
  #                                 expsQuests$condition2 - 0.1)
  
  # side space from x margins to high low mirror and high chase
  sideSpace <- 0.4
  
  # filter data base?
  temp <- expsQuests[!is.na(expsQuests$bpe_high),]
  temp$task <- ifelse(temp$task == "sheep","Sheep","Wolf")
  
  p1 <- annotation_custom2(rasterGrob(wolf_img, interpolate=TRUE), xmin=0.3, xmax=1.3, ymin=0.55, ymax=1, data=temp[1,])
  p2 <- annotation_custom2(rasterGrob(sheep_img, interpolate=TRUE), xmin=0.3, xmax=1.3, ymin=0.55, ymax=1, data=temp[101,])
  fig2D <- ggplot(temp, aes(x=condition2,y=correct,col=bpe_high,
                            shape=bpe_high,fill=bpe_high)) + 
    labs(x="Condition",y="p(Correct Identification)",col="Teleology:",
         shape="Teleology:",fill="Teleology:") +
    geom_hline(yintercept = 1/8, linetype = "dashed") +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(bpe_high == "High",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "line", alpha=0.05) +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(bpe_high == "High",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "point", alpha=0.05,stroke=0) +
    geom_smooth(method="lm", se=F, size=1.1) +
    stat_summary(fun="mean", geom="point", size=4) +
    scale_color_manual(values = teleColour) +
    scale_fill_manual(values = teleColour) +
    scale_shape_manual(values = c(24,25)) +
    scale_x_continuous(breaks = c(1,2), labels = c("Mirror","Chase"),
                       limits = c(1-sideSpace,2+sideSpace)) +
    scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1)) +
    facet_grid(.~task) +
    theme_classic() + theme(legend.position = "none",
                            axis.title.x = element_blank())
  fig2D <- fig2D  + p1 + p2
  # fig2D
  
  fig2F <- ggplot(temp, aes(x=condition2,y=confidence,col=bpe_high,
                            shape=bpe_high,fill=bpe_high)) + 
    labs(x="Condition",y="Confidence",col="Teleology:",
         shape="Teleology:",fill="Teleology:") +
    geom_hline(yintercept = 1/8, linetype = "dashed") +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(bpe_high == "High",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "line", alpha=0.05) +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(bpe_high == "High",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "point", alpha=0.05,stroke=0) + #pch=21
    geom_smooth(method="lm", se=F, size=1.1) +
    stat_summary(fun="mean", geom="point", size=4) +
    scale_color_manual(values = teleColour) +
    scale_fill_manual(values = teleColour) +
    scale_shape_manual(values = c(24,25)) +
    scale_x_continuous(breaks = c(1,2), labels = c("Mirror","Chase"),
                       limits = c(1-sideSpace,2+sideSpace)) +
    scale_y_continuous(breaks = c(1,3,5),limits = c(1,5)) +
    facet_grid(.~task) +
    theme_classic() + theme(legend.position = "none",
                            axis.title.x = element_blank())
  # fig2F
  

  # combine figures
  bottomleft <- annotate_figure(ggarrange(fig2C, fig2E, nrow=2,align = "hv",labels = c("C","E"),
                                          common.legend = F),
                                top = text_grob("Studies 3, 4a, & 4b", color = "black",face = "bold", size = 12),
                                bottom = text_grob("Conditions", color = "black",face = "bold", size = 12))
  bottomright <- annotate_figure(ggarrange(fig2D, fig2F, nrow=2,align = "hv",labels = c("D","F"),
                                           common.legend = F),
                                 top = text_grob("Studies 3, 4a, & 4b", color = "black",face = "bold", size = 12),
                                 bottom = text_grob("Conditions", color = "black",face = "bold", size = 12))
  left <- annotate_figure(ggarrange(fig2A,bottomleft,nrow=2,labels = c("A",""),
                                    heights = c(3.1,4.9),
                                    common.legend = T),
                          top = text_grob("Studies 1 & 2", color = "black",face = "bold", size = 12))
  right <- annotate_figure(ggarrange(fig2B,bottomright,nrow=2,labels = c("B",""),
                                     heights = c(3.1,4.9),
                                     common.legend = T),
                           top = text_grob("Study 2", color = "black",face = "bold", size = 12))
  fig2 <- ggarrange(left,right)
  return(fig2)
}

plotFigureS3 <- function (quest1exp1,quest2exp2,questsExps,
                         exp1Quest1,exp2Quest2,expsQuests) {
  # # # colors # # #
  paraColour <- c("#F29199","#1F5B73")
  teleColour <- c("#8EA676","#F2921D")
  wolfSheep <- c("#F2B885","#262014")
  
  # # # # EXPERIMENT 1 and 2 # # # #
  # figure 2A based on all the classic chase detection SDT
  quest1exp1$confidence_M <- quest1exp1$confidence_C <- NA
  sameCols <- c("experiment","version","demo_age","demo_sex",
                "subjectId","survey_duration","interview_date","interview_date_full",
                "sensitivity","response_criterion","Hit","FA","Ms","CR","hit_rate",
                "fa_rate","correct","confidence_C","confidence_M","detection",
                "reaction_time","paranoia")
  questDetect <- rbind(quest1exp1[,sameCols], quest2exp2[,sameCols])
  questDetect$sex <- ifelse(questDetect$demo_sex == "Male",0,1)
  exp1_2_parameters <- c("hit_rate","fa_rate","sensitivity","response_criterion",
                         "correct","detection","reaction_time","confidence_M","confidence_C")
  # # # # Brian Scholl's lab meeting # # # #
  # questDetect <- rbind(quest1exp1[,sameCols])
  # exp1_2_parameters <- c("hit_rate","fa_rate","sensitivity","response_criterion",
  #                        "correct","detection","reaction_time")
  questDetectMelt <- reshape2::melt(questDetect, measure.vars = exp1_2_parameters)
  questDetectMelt <- questDetectMelt[!is.na(questDetectMelt$value),]
  
  # significance difference
  sig <- estimate <- means <- as.vector(rep(NA,length(exp1_2_parameters)))
  effect_size <- matrix(NA,nrow=length(exp1_2_parameters),ncol=3)
  for (i in 1:length(exp1_2_parameters)) {
    temp <- questDetectMelt$value[questDetectMelt$variable == exp1_2_parameters[i]]
    questDetectMelt$value[questDetectMelt$variable == exp1_2_parameters[i]] <- 
      scale(temp)[1:length(temp)]
    means[i] <- mean(scale(temp)[1:length(temp)], na.rm=T)
    temp2 <- glm(sex~., family = "binomial",
                 data = questDetect[,c("sex",exp1_2_parameters[i])])
    temp3 <- report::report_table(temp2)
    sig[i] <- temp3$p[2]
    estimate[i] <- temp3$Coefficient[2]
    effect_size[i,] <- unlist(temp3[2,c("Std_Coefficient","Std_Coefficient_CI_low",
                                        "Std_Coefficient_CI_high")])
  }
  print <- data.frame(exp1_2_parameters,means,
                      round(data.frame(estimate,sig,
                                       Std_Coefficient=effect_size[,1],
                                       Std_Coefficient_CI_low=effect_size[,2],
                                       Std_Coefficient_CI_high=effect_size[,3]),4))
  write.csv(print, "figures/figS3A.csv",row.names = F)
  
  # create data frame for significant experiment 1
  temp4 <- data.frame(exp1_2_parameters,sig,dich=
                        ifelse(sig<0.001,"***",#p < 0.001
                               ifelse(sig>0.001&sig<0.01,"**",#p < 0.01
                                      ifelse(sig>0.01&sig<0.05,"*",""))))#p < 0.05
  questDetectMelt$sex1 <- factor(questDetectMelt$sex, levels = c("1","0"))
  figS2A <- ggplot(questDetectMelt, aes(x=variable,y=value,col=sex1,
                                       fill=sex1,shape=sex1)) +
    labs(shape="Sex:",fill="Sex:",col="Sex:",
         y = "Scaled Scores") +
    stat_summary(fun.data = mean_se, position = position_dodge(0.2)) +
    scale_color_manual(values = c("pink","blue"), labels = c("Female","Male")) +
    scale_fill_manual(values = c("pink","blue"), labels = c("Female","Male")) +
    scale_shape_manual(values = c(21,22), labels = c("Female","Male")) +
    scale_y_continuous(breaks = c(-0.5,0,0.5)) +
    scale_x_discrete(labels = c('hit_rate'="Hit Rate",
                                'fa_rate'="False Alarm Rate",
                                'sensitivity'="Sensitivity: d'",
                                'response_criterion'="Response Criterion: C",
                                'correct'="Correctness",
                                'detection'="p(Detect Chase)",
                                'reaction_time'="Decision Time",
                                'confidence_M'="Mirror Confidence",
                                'confidence_C'="Chase Confidence")) +
    annotate("text",y=means, x=seq(1.2,9.2,1),label=temp4$dich,size=3.5) +
    coord_cartesian(ylim = c(-0.6,0.6)) +
    theme_classic() + theme(legend.position = "none",
                            legend.background = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x = element_text(angle = 30, hjust = 1))
  # figS2A
  # paste(round(mean(questDetect$hit_rate),2),"SD", round(sd(questDetect$hit_rate),2))
  # paste(round(mean(questDetect$fa_rate),2),"SD", round(sd(questDetect$fa_rate),2))
  # paste(round(mean(questDetect$sensitivity),2),"SD", round(sd(questDetect$sensitivity),2))
  # paste(round(mean(questDetect$response_criterion),2),"SD", round(sd(questDetect$response_criterion),2))
  # paste(round(mean(questDetect$correct),2),"SD", round(sd(questDetect$correct),2))
  # paste(round(mean(questDetect$detection),2),"SD", round(sd(questDetect$detection),2))
  # paste(round(mean(questDetect$reaction_time),2),"SD", round(sd(questDetect$reaction_time),2))
  # paste(round(mean(questDetect$confidence_M,na.rm=T),2),"SD", round(sd(questDetect$confidence_M,na.rm=T),2))
  # paste(round(mean(questDetect$confidence_C,na.rm=T),2),"SD", round(sd(questDetect$confidence_C,na.rm=T),2))
  # meanDifference(questDetect$hit_rate[questDetect$sex==0],
  #                questDetect$hit_rate[questDetect$sex==1],F)
  # meanDifference(questDetect$fa_rate[questDetect$sex==0],
  #                questDetect$fa_rate[questDetect$sex==1],F)
  # meanDifference(questDetect$sensitivity[questDetect$sex==0],
  #                questDetect$sensitivity[questDetect$sex==1],F)
  # meanDifference(questDetect$response_criterion[questDetect$sex==0],
  #                questDetect$response_criterion[questDetect$sex==1],F)
  # meanDifference(questDetect$correct[questDetect$sex==0],
  #                questDetect$correct[questDetect$sex==1],F)
  # meanDifference(questDetect$detection[questDetect$sex==0],
  #                questDetect$detection[questDetect$sex==1],F)
  # meanDifference(questDetect$reaction_time[questDetect$sex==0],
  #                questDetect$reaction_time[questDetect$sex==1],F)
  # meanDifference(questDetect$confidence_M[questDetect$sex==0],
  #                questDetect$confidence_M[questDetect$sex==1],F)
  # meanDifference(questDetect$confidence_C[questDetect$sex==0],
  #                questDetect$confidence_C[questDetect$sex==1],F)
  
  
  
  # condition to numeric for easy plot
  expsQuests$condition2 <- ifelse(expsQuests$condition == "chase",2,1)
  # expsQuests$condition2 <- ifelse(expsQuests$rgpts_para == "high",
  #                                 expsQuests$condition2 + 0.1,
  #           
  
  # side space from x margins to high low mirror and high chase
  sideSpace <- 0.4
  
  # filter data base?
  temp <- expsQuests[expsQuests$demo_sex == "Male" | expsQuests$demo_sex == "Female",]
  temp <- temp[!is.na(temp$task),]
  temp$task <- ifelse(temp$task == "sheep","Sheep","Wolf")
  # temp$rgpts_para <- ifelse(temp$rgpts_para == "low","Low","High")
  
  figS2B <- ggplot(temp[,], aes(x=condition2,y=correct,
                                                     col=demo_sex,shape=demo_sex,fill=demo_sex)) + 
    labs(x="Condition",y="p(Correct Identification)",col="Sex:",shape="Sex:",fill="Sex:") +
    geom_hline(yintercept = 1/8, linetype = "dashed") +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "line", alpha=0.05) +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "point", alpha=0.05,stroke=0) +
    geom_smooth(method="lm", se=F, size=1.1) +
    stat_summary(fun="mean", geom="point", size=4) +
    scale_color_manual(values = c("pink","blue"),labels = c("Female","Male")) +
    scale_fill_manual(values = c("pink","blue"),labels = c("Female","Male")) +
    scale_shape_manual(values = c(21,22),labels = c("Female","Male")) +
    scale_x_continuous(breaks = c(1,2), labels = c("Mirror","Chase"),
                       limits = c(1-sideSpace,2+sideSpace)) +
    scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1)) +
    facet_grid(.~task) +
    theme_classic() + theme(legend.position = "none",
                            axis.title.x = element_blank())
  # figS2B
  figS2C <- ggplot(temp[,], aes(x=condition2,y=confidence,
                               col=demo_sex,shape=demo_sex,fill=demo_sex)) + 
    labs(x="Condition",y="Confidence",col="Sex:",shape="Sex:",fill="Sex:") +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "line", alpha=0.05) +
    stat_summary(fun = "mean", aes(group=subjectId,
                                   x=ifelse(rgpts_para == "high",
                                            condition2 + 0.1,
                                            condition2 - 0.1)), 
                 geom = "point", alpha=0.05,stroke=0) +
    geom_smooth(method="lm", se=F, size=1.1) +
    stat_summary(fun="mean", geom="point", size=4) +
    scale_color_manual(values = c("pink","blue"),labels = c("Female","Male")) +
    scale_fill_manual(values = c("pink","blue"),labels = c("Female","Male")) +
    scale_shape_manual(values = c(21,22),labels = c("Female","Male")) +
    scale_x_continuous(breaks = c(1,2), labels = c("Mirror","Chase"),
                       limits = c(1-sideSpace,2+sideSpace)) +
    scale_y_continuous(breaks = c(1,3,5),limits = c(1,5)) +
    facet_grid(.~task) +
    theme_classic() + theme(legend.position = "none",
                            axis.title.x = element_blank())
  # figS2C
  

  
  # combine figures
  bottomleft <- annotate_figure(ggarrange(figS2B, figS2C, nrow=2,align = "hv",labels = c("B","C"),
                                          common.legend = F),
                                top = text_grob("Studies 3, 4a, & 4b", color = "black",face = "bold", size = 12),
                                bottom = text_grob("Conditions", color = "black",face = "bold", size = 12))
  # bottomright <- annotate_figure(ggarrange(fig2D, fig2F, nrow=2,align = "hv",labels = c("D","F"),
  #                                          common.legend = F),
  #                                top = text_grob("Studies 3, 4a, & 4b", color = "black",face = "bold", size = 12),
  #                                bottom = text_grob("Conditions", color = "black",face = "bold", size = 12))
  left <- annotate_figure(ggarrange(figS2A,bottomleft,nrow=2,labels = c("A",""),
                                    heights = c(3.1,4.9),
                                    common.legend = T),
                          top = text_grob("Studies 1 & 2", color = "black",face = "bold", size = 12))
  # right <- annotate_figure(ggarrange(fig2B,bottomright,nrow=2,labels = c("B",""),
  #                                    heights = c(3.1,4.9),
  #                                    common.legend = T),
  #                          top = text_grob("Study 2", color = "black",face = "bold", size = 12))
  # fig2 <- ggarrange(left,right)
  
  return(left)
}

modelEstimates <- function (mod) {
  print(summary(mod))
  tmod <- as.data.frame(summary(mod)$coefficients)
  colnames(tmod)[ncol(tmod)] <- "p.value"
  colnames(tmod)[ncol(tmod)-1] <- "score.value"
  tmod$sig <- as.factor(ifelse(tmod$p.value > 0.05,"",
                               ifelse(tmod$p.value < 0.05 & tmod$p.value > 0.01,"*",
                                      ifelse(tmod$p.value < 0.01 & tmod$p.value > 0.001,
                                             "**","***"))))
  return(tmod)
}



# prepare descriptive table
f_suppTables <- function (quest,descrGuide) {
  # change nature of the next variables
  quest$demo_sex <- factor(quest$demo_sex, levels = c("Female","Male","Prefer not to say"))
  quest$demo_age <- as.numeric(quest$demo_age)
  
  # create continuous variables 
  for (i in 1:nrow(descrGuide)) {
    if (descrGuide[i,2] == 1) {
      quest[,descrGuide[i,1]] <- as.numeric(quest[,descrGuide[i,1]])
    }
  }
  
  # summarized
  for (i in 1:nrow(descrGuide)) {
    # if the variable is continuous
    if (descrGuide[i,2] == 1) {
      temp <- f_descrContinuous(quest[,descrGuide[i,1]])
      temp <- matrix(c(descrGuide[i,1],descrGuide[i,2],rep(NA,3),temp),nrow=1)
    } else { # or the variable is categorical
      temp <- f_descrCategorical(quest[,descrGuide[i,1]])
      temp <- cbind(descrGuide[i,1],descrGuide[i,2],temp,matrix(NA,ncol=5,nrow=nrow(temp)))
    }
    if (i == 1) {
      outputTable <- temp
    } else {
      outputTable <- rbind(outputTable,temp)
    }
  }
  # add column names
  colnames(outputTable) <- c("var","type","factor","frequency","percentage","N","mean","sd","min","max")
  
  # function output
  return(outputTable)
}
# categorical descriptive variable
f_descrCategorical <- function(vec) {return(matrix(c(levels(as.factor(vec)),table(vec),(table(vec) / length(vec)) * 100),
                                                   nrow=length(levels(as.factor(vec)))))}
# continuous descriptive variable
f_descrContinuous <- function(vec) {return(c(sum(!is.na(vec)),mean(vec, na.rm = T),sd(vec, na.rm = T),range(vec, na.rm = T)))}
