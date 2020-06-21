########################################################################################
# Replication File for: Party Animals? Extreme Partisan Polarization and Dehumanization
# Political Behavior
# James Martherus, Andy Martinez, Paul Piff, and Alexander Theodoridis
########################################################################################
rm(list=ls())
library(ggplot2)
library(plyr)
library(stargazer)
library(gridExtra)
library(lmtest)
library(coefplot)
library(stringr)
library(psych)
library(foreign)
library(survey)
library(boot)
library(gplots)
library(car)
library(Hmisc)

cces14_data <- read.csv("cces14.csv")
cces18_data <- read.csv("cces18.csv")
ssi18_data <- read.csv("ssi18.csv")


# Distributions 

############
# Figure 2a
############

ggplot(ssi18_data, aes(x = hum_blatant)) +  
  geom_density() +
  xlab("Blatant Dehumanization") +
  xlim(c(-30,101)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure 2b
############

ggplot(cces18_data, aes(x = hum_blatant)) +  
  geom_density() +
  xlab("Blatant Dehumanization") +
  xlim(c(-30,101)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure 2c
############

ggplot(ssi18_data[!is.na(ssi18_data$hum_subtle),], aes(x = hum_subtle)) +  
  geom_density() +
  xlab("Subtle Dehumanization") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure 2d
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=hum_blatant, y=hum_subtle, color=party_binary)) +
  geom_point() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Blatant Dehumanization") + 
  ylab("Subtle Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

# Dehumanization and Party Strength

############
# Figure 3a
############

ggplot(cces18_data[is.na(cces18_data$party_strength)==F,]) +
  geom_jitter(aes(x=factor(party_strength, levels=c("1","2","3"), labels=c("Leaners","Not-so-strong","Strong")), y=hum_blatant, color=pid3lean)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=party_strength, y=hum_blatant, color=pid3lean)) +
  xlab("Party Strength") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())+
  ylim(c(-83,100))

############
# Figure 3b
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,]) +
  geom_jitter(aes(x=factor(party_strength, levels=c("1","2","3"), labels=c("Leaners","Not-so-strong","Strong")), y=hum_blatant, color=party_binary)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=party_strength, y=hum_blatant, color=party_binary)) +
  xlab("Party Strength") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

############
# Figure 3c
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,]) +
  geom_jitter(aes(x=factor(party_strength, levels=c("1","2","3"), labels=c("Leaners","Not-so-strong","Strong")), y=hum_subtle, color=party_binary)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=party_strength, y=hum_subtle, color=party_binary)) +
  xlab("Party Strength") + 
  ylab("Subtle Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

# Dehumanization and Party Closeness

############
# Figure 4a
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,]) +
  geom_jitter(aes(x=factor(close_recode, levels=c("1","2","3"), labels=c("Not very close","Somewhat close","Very close")), y=hum_blatant, color=party_binary)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=close_recode, y=hum_blatant, color=party_binary)) +
  xlab("Party Closeness") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

############
# Figure 4b
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,]) +
  geom_jitter(aes(x=factor(close_recode, levels=c("1","2","3"), labels=c("Not very close","Somewhat close","Very close")), y=hum_subtle, color=party_binary)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=close_recode, y=hum_subtle, color=party_binary)) +
  xlab("Party Closeness") + 
  ylab("Subtle Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

#Dehumanization and Affective Polarization

############
# Figure 5a
############

ggplot(cces18_data[is.na(cces18_data$ft_diff)==F,]) +
  geom_jitter(aes(x=ft_diff, y=hum_blatant, color=pid3lean)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=ft_diff, y=hum_blatant, color=pid3lean)) +
  xlab("Affective Polarization") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend()) +
  ylim(c(-83,100))

############
# Figure 5b
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=ft_diff, y=hum_blatant, color=party_binary)) +
  geom_point() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Affective Polarization") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())


############
# Figure 5c
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=ft_diff, y=hum_subtle, color=party_binary)) +
  geom_point() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Affective Polarization") + 
  ylab("Subtle Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

# Dehumanization and Worldview

############
# Figure 6a
############

ggplot(cces18_data[is.na(cces18_data$authscale)==F,]) +
  geom_jitter(aes(x=authscale, y=hum_blatant, color=pid3lean)) +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5, aes(x=authscale, y=hum_blatant, color=pid3lean)) +
  xlab("Fixed Worldview") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())+
  ylim(c(-83,100))

############
# Figure 6b
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=authscale, y=hum_blatant, color=party_binary)) +
  geom_jitter() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Fixed Worldview") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

############
# Figure 6c
############

ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=authscale, y=hum_subtle, color=party_binary)) +
  geom_jitter() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Fixed Worldview") + 
  ylab("Subtle Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

# Dehumanization and Partisan Motivated Reasoning

############
# Figure 7a
############

ssi18_data$hum_blatant_quantile[ssi18_data$hum_blatant > wtd.quantile(ssi18_data$hum_blatant, probs=.666666667, na.rm=TRUE)] <- 3
ssi18_data$hum_blatant_quantile[ssi18_data$hum_blatant > wtd.quantile(ssi18_data$hum_blatant, probs=.33333333, na.rm=TRUE) & ssi18_data$hum_blatant <= wtd.quantile(ssi18_data$hum_blatant, probs=.666666667, na.rm=TRUE)] <- 2
ssi18_data$hum_blatant_quantile[ssi18_data$hum_blatant <= wtd.quantile(ssi18_data$hum_blatant, probs=.33333333, na.rm=TRUE) & ssi18_data$hum_blatant >= 0] <- 1

Variables <- data.frame(depvar=ssi18_data$scandal, hum_blatant_quantile=ssi18_data$hum_blatant_quantile, scandal_out=ssi18_data$scandal_out, pid6=(ssi18_data$pid7-4), party_binary=ssi18_data$party_binary)

# Creating Null vectors
lowDehumBias<-NULL
medDehumBias<-NULL
highDehumBias<-NULL

# Bootstrapping Bias
for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T)
  
  Partisans <- Variables[bsrows,]
  
  # Blatant Dehumanization
  
  lowDehum <- subset(Partisans, subset=hum_blatant_quantile==1)
  medDehum <- subset(Partisans, subset=hum_blatant_quantile==2)
  highDehum <- subset(Partisans, subset=hum_blatant_quantile==3)
  
  lowDehumIn <-subset(lowDehum, subset=scandal_out==-1)
  lowDehumOut <-subset(lowDehum, subset=scandal_out==1)
  medDehumIn <-subset(medDehum, subset=scandal_out==-1)
  medDehumOut <-subset(medDehum, subset=scandal_out==1)
  highDehumIn <-subset(highDehum, subset=scandal_out==-1)
  highDehumOut <-subset(highDehum, subset=scandal_out==1)
  
  lowDehumBias <- c(lowDehumBias, mean(lowDehumIn$depvar, na.rm=T)-mean(lowDehumOut$depvar, na.rm=T))
  medDehumBias <- c(medDehumBias, mean(medDehumIn$depvar, na.rm=T)-mean(medDehumOut$depvar, na.rm=T))
  highDehumBias <- c(highDehumBias, mean(highDehumIn$depvar, na.rm=T)-mean(highDehumOut$depvar, na.rm=T))
  
}

#Plot
hh <- c(mean(lowDehumBias),mean(medDehumBias),mean(highDehumBias))
mybarcol <- "gray20"
ci.l <- c(quantile(lowDehumBias,.025), quantile(medDehumBias,.025), quantile(highDehumBias,.025))
ci.u <- c(quantile(lowDehumBias,.975), quantile(medDehumBias,.975), quantile(highDehumBias,.975))
pdf("bias_dehum.pdf", width=9, height=9, pointsize=22)
barplot2(hh, beside = TRUE, horiz=FALSE,
         names.arg=c("Low", "Medium", "High"),
         xlab = "Blatant Partisan Dehumanization",
         ylab = "Motivated Processing: Outparty - Inparty",
         cex.lab = 1.3,
         ci.lwd = 3, 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE)
dev.off()


############
# Figure 7b
############

ssi18_data$hum_subtle_quantile[ssi18_data$hum_subtle > wtd.quantile(ssi18_data$hum_subtle, probs=.666666667, na.rm=TRUE)] <- 3
ssi18_data$hum_subtle_quantile[ssi18_data$hum_subtle > wtd.quantile(ssi18_data$hum_subtle, probs=.33333333, na.rm=TRUE) & ssi18_data$hum_subtle <= wtd.quantile(ssi18_data$hum_subtle, probs=.666666667, na.rm=TRUE)] <- 2
ssi18_data$hum_subtle_quantile[ssi18_data$hum_subtle <= wtd.quantile(ssi18_data$hum_subtle, probs=.33333333, na.rm=TRUE) & ssi18_data$hum_subtle >= 0] <- 1

Variables <- data.frame(depvar=ssi18_data$scandal, hum_subtle_quantile=ssi18_data$hum_subtle_quantile, scandal_out=ssi18_data$scandal_out, pid6=(ssi18_data$pid7-4), party_binary=ssi18_data$party_binary)

# Creating Null vectors
lowDehumBias<-NULL
medDehumBias<-NULL
highDehumBias<-NULL

# Bootstrapping Bias
for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T)
  
  Partisans <- Variables[bsrows,]

  lowDehum <- subset(Partisans, subset=hum_subtle_quantile==1)
  medDehum <- subset(Partisans, subset=hum_subtle_quantile==2)
  highDehum <- subset(Partisans, subset=hum_subtle_quantile==3)
  
  lowDehumIn <-subset(lowDehum, subset=scandal_out==-1)
  lowDehumOut <-subset(lowDehum, subset=scandal_out==1)
  medDehumIn <-subset(medDehum, subset=scandal_out==-1)
  medDehumOut <-subset(medDehum, subset=scandal_out==1)
  highDehumIn <-subset(highDehum, subset=scandal_out==-1)
  highDehumOut <-subset(highDehum, subset=scandal_out==1)
  
  lowDehumBias <- c(lowDehumBias, mean(lowDehumIn$depvar, na.rm=T)-mean(lowDehumOut$depvar, na.rm=T))
  medDehumBias <- c(medDehumBias, mean(medDehumIn$depvar, na.rm=T)-mean(medDehumOut$depvar, na.rm=T))
  highDehumBias <- c(highDehumBias, mean(highDehumIn$depvar, na.rm=T)-mean(highDehumOut$depvar, na.rm=T))
}

# Plot
hh <- c(mean(lowDehumBias),mean(medDehumBias),mean(highDehumBias))
mybarcol <- "gray20"
ci.l <- c(quantile(lowDehumBias,.025), quantile(medDehumBias,.025), quantile(highDehumBias,.025))
ci.u <- c(quantile(lowDehumBias,.975), quantile(medDehumBias,.975), quantile(highDehumBias,.975))
pdf("bias_dehum_subtle.pdf", width=9, height=9, pointsize=22)
barplot2(hh, beside = TRUE, horiz=FALSE,
         names.arg=c("Low", "Medium", "High"),
         xlab = "Subtle Partisan Dehumanization",
         ylab = "Motivated Processing: Outparty - Inparty",
         ylim = c(-.2,.6),
         cex.lab = 1.3,
         ci.lwd = 3, 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE) 
dev.off()


# Dehumanization Experiment

############
# Figure 9a
############

Variables <- data.frame(depvar=cces14_data$animalistic_mean, treat=cces14_data$Shame_treat, pid=cces14_data$pid3lean, weight=cces14_data$wait)
attach(Variables)

weighted<-svydesign(id=~0, weights=~weight, data=Variables)
descriptives_matrix <- svyby(~depvar,~pid+treat, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
descriptives_matrix$minci <- descriptives_matrix$depvar -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar +(1.96  * descriptives_matrix$se)
descriptives_matrix$treat <- recode(descriptives_matrix$treat,"2='Democratic'; 3='Republican'")
descriptives_matrix$pid <- recode(descriptives_matrix$pid,"-1='Democrats'; 1='Republicans'")
pd <- position_dodge(.4)

ggplot(descriptives_matrix, aes(x = as.factor(pid), y = depvar, colour=as.factor(treat))) + scale_colour_manual(values = c("Democratic" = "darkblue", "Republican" = "firebrick1"), name = "Treatment:") + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.0, size=1.1, position=pd) + geom_point(size=3, position=pd)  + theme_bw() + labs(x = "Respondent Party ID (including leaners)", y = "Animalistic Dehumanization", color = "Condition") + theme(text = element_text(size=18), legend.position="top") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(legend.key = element_blank())


############
# Figure 9b
############

Variables <- data.frame(depvar=cces14_data$robotic_mean, treat=cces14_data$Shame_treat, pid=cces14_data$pid3lean, weight=cces14_data$wait)
attach(Variables)

weighted<-svydesign(id=~0, weights=~weight, data=Variables)
descriptives_matrix <- svyby(~depvar,~pid+treat, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
descriptives_matrix$minci <- descriptives_matrix$depvar -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar +(1.96  * descriptives_matrix$se)
descriptives_matrix$treat <- recode(descriptives_matrix$treat,"2='Democratic'; 3='Republican'")
descriptives_matrix$pid <- recode(descriptives_matrix$pid,"-1='Democrats'; 1='Republicans'")

pd <- position_dodge(.4)
ggplot(descriptives_matrix, aes(x = as.factor(pid), y = depvar, colour=as.factor(treat))) + scale_colour_manual(values = c("Democratic" = "darkblue", "Republican" = "firebrick1"), name = "Treatment:") + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.0, size=1.1, position=pd) + geom_point(size=3, position=pd)  + theme_bw() + labs(x = "Respondent Party ID (including leaners)", y = "Mechanistic Dehumanization", color = "Condition") + theme(text = element_text(size=18),legend.position="top") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(legend.key = element_blank())



############################################################################################
############################################################################################
######################################### Appendix #########################################
############################################################################################
############################################################################################



############
# Table I
############

strong_partisans <- ssi18_data[ssi18_data$party_strength==3,]
model <- lm(hum_blatant ~ authscale + Republican + attention_num, data=strong_partisans)

stargazer(model,
          type="text",
          header=FALSE,
          dep.var.labels=c("Blatant Dehumanization"),
          title="Analysis of Strong Partisans",
          covariate.labels = c("Authoritarianism", "Republican","Attention to Politics"))

############
# Figure I
############

data_barcharts <- cces14_data
data_barcharts <- data_barcharts %>% filter(Shame_treat != 1) %>% select(c(Shame_treat, pid3lean, Grid_Human_1, Grid_Human_2, Grid_Human_3, Grid_Human_4))

data_barcharts$Grid1_recode <- data_barcharts$Grid_Human_1
data_barcharts$Grid1_recode[data_barcharts$Grid_Human_1==5] <- 4
data_barcharts$Grid1_recode[data_barcharts$Grid_Human_1==6] <- 5

data_barcharts$Grid2_recode <- data_barcharts$Grid_Human_2
data_barcharts$Grid2_recode[data_barcharts$Grid_Human_2==5] <- 4
data_barcharts$Grid2_recode[data_barcharts$Grid_Human_2==6] <- 5

data_barcharts$Grid3_recode <- data_barcharts$Grid_Human_3
data_barcharts$Grid3_recode[data_barcharts$Grid_Human_3==5] <- 4
data_barcharts$Grid3_recode[data_barcharts$Grid_Human_3==6] <- 5

data_barcharts$Grid4_recode <- data_barcharts$Grid_Human_4
data_barcharts$Grid4_recode[data_barcharts$Grid_Human_4==5] <- 4
data_barcharts$Grid4_recode[data_barcharts$Grid_Human_4==6] <- 5

data_barcharts$Shame_treat <- factor(data_barcharts$Shame_treat, levels=c(2,3), labels = c("Dem Treat","Rep Treat"))
data_barcharts$pid3lean <- factor(data_barcharts$pid3lean, levels=c(-1,1), labels = c("Dem Respondent","Rep Respondent"))

ggplot(data_barcharts, aes(x = Grid1_recode)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_grid(as.character(pid3lean) ~ as.character(Shame_treat)) +
  xlab("These people are like animals") +
  ylab("% of Sample") + 
  scale_y_continuous(labels = scales::percent, breaks=c(0,.05,.1)) +
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("Strongly disagree","","","","Strongly agree")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure II
############

ggplot(data_barcharts, aes(x = Grid2_recode)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_grid(as.character(pid3lean) ~ as.character(Shame_treat)) +
  xlab("These people are uncivilized") +
  ylab("% of Sample") + 
  scale_y_continuous(labels = scales::percent, breaks=c(0,.05,.1)) +
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("Strongly disagree","","","","Strongly agree")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure III
############

ggplot(data_barcharts, aes(x = Grid3_recode)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_grid(as.character(pid3lean) ~ as.character(Shame_treat)) +
  xlab("These people are like robots") +
  ylab("% of Sample") + 
  scale_y_continuous(labels = scales::percent, breaks=c(0,.05,.1)) +
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("Strongly disagree","","","","Strongly agree")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure IV
############

ggplot(data_barcharts, aes(x = Grid4_recode)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_grid(as.character(pid3lean) ~ as.character(Shame_treat)) +
  xlab("These people have no feelings") +
  ylab("% of Sample") + 
  scale_y_continuous(labels = scales::percent, breaks=c(0,.05,.1)) +
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("Strongly disagree","","","","Strongly agree")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

############
# Figure V
############

plot_pid7 <- qplot(factor(ssi18_data$pid7,levels=c(1,2,3,4,5,6,7), labels=c("Democrat","2","3","4","5","6","Republican")), xlab="Party Identification") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black") ,axis.text.x=element_text(size=8))
plot_gender <- qplot(ssi18_data[!is.na(ssi18_data$Gender),]$Gender, xlab="Gender") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot_age <- qplot(ssi18_data$Age, xlab="Age") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot_race <- qplot(ssi18_data[!is.na(ssi18_data$race),]$race, xlab="Race")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text.x=element_text(size=7)) 
grid.arrange(plot_race,plot_pid7,plot_gender,plot_age) 


############
# Figure VI
############

inparty <- ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=ft_in, y=hum_blatant, color=party_binary)) +
  geom_point() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Affective Polarization") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())
outparty <- ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=ft_out, y=hum_blatant, color=party_binary)) +
  geom_point() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Affective Polarization") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())
difference <- ggplot(ssi18_data[is.na(ssi18_data$party_binary)==F,], aes(x=ft_diff, y=hum_blatant, color=party_binary)) +
  geom_point() +
  scale_color_manual(values=c("darkblue", "darkred")) +
  geom_smooth(method = "loess", size = 1.5) +
  xlab("Affective Polarization") + 
  ylab("Blatant Dehumanization") +
  labs(color="Party ID") +
  theme_bw() +
  theme(text = element_text(size=18), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="bottom") +
  scale_fill_discrete(guide = guide_legend())

grid.arrange(inparty, outparty, difference, nrow=3)

############
# Figure VII
############

ssi18_data$ap_quantile[ssi18_data$ft_diff > wtd.quantile(ssi18_data$ft_diff, probs=.666666667, na.rm=TRUE)] <- 3
ssi18_data$ap_quantile[ssi18_data$ft_diff > wtd.quantile(ssi18_data$ft_diff, probs=.33333333, na.rm=TRUE) & ssi18_data$ft_diff <= wtd.quantile(ssi18_data$ft_diff, probs=.666666667, na.rm=TRUE)] <- 2
ssi18_data$ap_quantile[ssi18_data$ft_diff <= wtd.quantile(ssi18_data$ft_diff, probs=.33333333, na.rm=TRUE) & ssi18_data$ft_diff >= 0] <- 1

Variables <- data.frame(depvar=ssi18_data$scandal, ap_quantile=ssi18_data$ap_quantile, scandal_out=ssi18_data$scandal_out, pid6=(ssi18_data$pid7-4))

# Creating Null vectors
lowAPBias<-NULL
medAPBias<-NULL
highAPBias<-NULL

# Bootstrapping Bias
for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T)
  Partisans <- Variables[bsrows,]
  
  lowAP <- subset(Partisans, subset=ap_quantile==1)
  medAP <- subset(Partisans, subset=ap_quantile==2)
  highAP <- subset(Partisans, subset=ap_quantile==3)
  
  lowAPIn <-subset(lowAP, subset=scandal_out==-1)
  lowAPOut <-subset(lowAP, subset=scandal_out==1)
  medAPIn <-subset(medAP, subset=scandal_out==-1)
  medAPOut <-subset(medAP, subset=scandal_out==1)
  highAPIn <-subset(highAP, subset=scandal_out==-1)
  highAPOut <-subset(highAP, subset=scandal_out==1)
  
  lowAPBias <- c(lowAPBias, mean(lowAPIn$depvar, na.rm=T)-mean(lowAPOut$depvar, na.rm=T))
  medAPBias <- c(medAPBias, mean(medAPIn$depvar, na.rm=T)-mean(medAPOut$depvar, na.rm=T))
  highAPBias <- c(highAPBias, mean(highAPIn$depvar, na.rm=T)-mean(highAPOut$depvar, na.rm=T))
  
}

#Plotting Bias
hh <- c(mean(lowAPBias),mean(medAPBias),mean(highAPBias))
mybarcol <- "gray20"
ci.l <- c(quantile(lowAPBias,.025), quantile(medAPBias,.025), quantile(highAPBias,.025))
ci.u <- c(quantile(lowAPBias,.975), quantile(medAPBias,.975), quantile(highAPBias,.975))
pdf("bias_ap.pdf", width=9, height=9, pointsize=22)
barplot2(hh, beside = TRUE, horiz=FALSE,
         names.arg=c("Low", "Medium", "High"),
         xlab = "Affective Polarization",
         ylab = "Motivated Processing: Outparty - Inparty",
         cex.lab = 1.3,
         ci.lwd = 3, 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE) 
dev.off()

############
# Figure VIII
############

Variables <- data.frame(depvar=ssi18_data$scandal, party_strength=ssi18_data$party_strength, scandal_out=ssi18_data$scandal_out, pid6=(ssi18_data$pid7-4))

# Creating Null vectors
lowStrengthBias<-NULL
medStrengthBias<-NULL
highStrengthBias<-NULL

# Bootstrapping Bias
for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T)
  Partisans <- Variables[bsrows,]
  
  lowStrength <- subset(Partisans, subset=party_strength==1)
  medStrength <- subset(Partisans, subset=party_strength==2)
  highStrength <- subset(Partisans, subset=party_strength==3)
  
  lowStrengthIn <-subset(lowStrength, subset=scandal_out==-1)
  lowStrengthOut <-subset(lowStrength, subset=scandal_out==1)
  medStrengthIn <-subset(medStrength, subset=scandal_out==-1)
  medStrengthOut <-subset(medStrength, subset=scandal_out==1)
  highStrengthIn <-subset(highStrength, subset=scandal_out==-1)
  highStrengthOut <-subset(highStrength, subset=scandal_out==1)
  
  lowStrengthBias <- c(lowStrengthBias, mean(lowStrengthIn$depvar, na.rm=T)-mean(lowStrengthOut$depvar, na.rm=T))
  medStrengthBias <- c(medStrengthBias, mean(medStrengthIn$depvar, na.rm=T)-mean(medStrengthOut$depvar, na.rm=T))
  highStrengthBias <- c(highStrengthBias, mean(highStrengthIn$depvar, na.rm=T)-mean(highStrengthOut$depvar, na.rm=T))
  
}

#Plotting Bias
hh <- c(mean(lowStrengthBias),mean(medStrengthBias),mean(highStrengthBias))
mybarcol <- "gray20"
ci.l <- c(quantile(lowStrengthBias,.025), quantile(medStrengthBias,.025), quantile(highStrengthBias,.025))
ci.u <- c(quantile(lowStrengthBias,.975), quantile(medStrengthBias,.975), quantile(highStrengthBias,.975))
pdf("bias_strength.pdf", width=9, height=9, pointsize=22)
barplot2(hh, beside = TRUE, horiz=FALSE,
         names.arg=c("Leaners", "Weak", "Strong"),
         xlab = "Party Strength",
         ylab = "Motivated Processing: Outparty - Inparty",
         cex.lab = 1.3,
         ci.lwd = 3, 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE) 
dev.off()


############
# Table II
############

auth <- lm(hum_blatant ~ authscale, data=ssi18_data)
auth_dem <- lm(hum_blatant ~ authscale, data=ssi18_data[ssi18_data$pid=="Democrat",])
auth_rep <- lm(hum_blatant ~ authscale, data=ssi18_data[ssi18_data$pid=="Republican",])
black_auth_dem <- lm(hum_blatant ~ authscale, data=ssi18_data[ssi18_data$race=="Black" & ssi18_data$pid=="Democrat",])
white_auth_dem <- lm(hum_blatant ~ authscale, data=ssi18_data[ssi18_data$race=="White" & ssi18_data$pid=="Democrat",])
black_auth_rep <- lm(hum_blatant ~ authscale, data=ssi18_data[ssi18_data$race=="Black" & ssi18_data$pid=="Republican",])
white_auth_rep <- lm(hum_blatant ~ authscale, data=ssi18_data[ssi18_data$race=="White" & ssi18_data$pid=="Republican",])

stargazer(auth, auth_dem, auth_rep, black_auth_dem, white_auth_dem, black_auth_rep, white_auth_rep,
          type="text",
          header=FALSE,
          dep.var.labels=c("Blatant Dehumanization"),
          title="Relationship Between Authoritarianism and Blatant Partisan Dehumanization",
          covariate.labels = c("Authoritarianism"),
          column.labels=c("Overall","Democrats","Republicans","Black Dems","White Dems","Black Reps","White Reps"),
          omit.stat=c("f","ser"))

############
# Table III
############

auth <- lm(hum_blatant ~ authscale, data=cces18_data)
auth_dem <- lm(hum_blatant ~ authscale, data=cces18_data[cces18_data$pid3lean=="Democrat",])
auth_rep <- lm(hum_blatant ~ authscale, data=cces18_data[cces18_data$pid3lean=="Republican",])
black_auth_dem <- lm(hum_blatant ~ authscale, data=cces18_data[cces18_data$race=="Black" & cces18_data$pid3lean=="Democrat",])
white_auth_dem <- lm(hum_blatant ~ authscale, data=cces18_data[cces18_data$race=="White" & cces18_data$pid3lean=="Democrat",])
black_auth_rep <- lm(hum_blatant ~ authscale, data=cces18_data[cces18_data$race=="Black" & cces18_data$pid3lean=="Republican",])
white_auth_rep <- lm(hum_blatant ~ authscale, data=cces18_data[cces18_data$race=="White" & cces18_data$pid3lean=="Republican",])

stargazer(auth, auth_dem, auth_rep, black_auth_dem, white_auth_dem, black_auth_rep, white_auth_rep,
          type="text",
          header=FALSE,
          dep.var.labels=c("Blatant Dehumanization"),
          title="Relationship Between Authoritarianism and Blatant Partisan Dehumanization",
          covariate.labels = c("Authoritarianism"),
          column.labels=c("Overall","Democrats","Republicans","Black Dems","White Dems","Black Reps","White Reps"),
          omit.stat=c("f","ser"))










