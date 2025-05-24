#################################################################
# clean the Environment
rm(list = ls())
.vsc.attach()

#load librairies
library(dplyr)
setwd(dir= "C:/Users/mfbpe/Desktop/DATA/2023_Social_Influence/results/") 

outlier <- function(dat) {
  stop <- 0
  while (stop != 1) {

    outlierp <- quantile(dat, .75, na.rm = T) + (2 * IQR(dat, na.rm = T))
    outlierm <- quantile(dat, .25, na.rm = T) - (2 * IQR(dat, na.rm = T))
    idx <- which(dat <= outlierm | dat >= outlierp)

    if (length(idx) > 0) {
      dat <- replace(dat, idx, NA)
      stop <- 1
    } else {
      stop <- 1
    }
  }
  return(dat)
}


df <- read.csv("Social_Influence_SIV2.csv", sep=";", header=TRUE)
df$Gender[df$Participant==736] <- 'f'
df$Gender <- replace(df$Gender, df$Gender%in%c("H",'h'), 'm')

mean(df$Age[df$Country=="Belgium"])
sd(df$Age[df$Country=="Belgium"])
mean(df$Age[df$Country=="Cambodia"])
sd(df$Age[df$Country=="Cambodia"])
mean(df$Age[df$Country=="Rwanda"])
sd(df$Age[df$Country=="Rwanda"])

sum(df$Gender[df$Country=="Rwanda"]=="m")
sum(df$Gender[df$Country=="Rwanda"]=="f")
length(df$Gender[df$Country=="Rwanda"])

dfeeg <- read.csv("SIV2_Cambodia_eeg_lmer_005sign_base500ms_1500ms_type_trial.csv", sep=";", header=TRUE)
dfeeg <- dfeeg[dfeeg$RT>.35,]
dfeeg <- dfeeg[!(is.na(dfeeg$Participant)),]
dfeeg$answer <- ifelse(dfeeg$answer=="yes","Yes","No")
dfeeg$Influence[dfeeg$Influence=="authority"] <- "Author"
dfeeg$Influence[dfeeg$Influence=="group"] <- "Group"
dfeeg$Influence[dfeeg$Influence=="indiv"] <- "Indiv"
dfeeg <- dfeeg[!(is.na(dfeeg$Participant)),]
dfeeg$Participant <- factor(dfeeg$Participant)
dfeeg$answer <- factor(dfeeg$answer, levels=c('No','Yes'))
dfeeg$Forced_free <- factor(dfeeg$Forced_free, levels=c('forced','free'))
dfeeg$Influence <- factor(dfeeg$Influence, levels=c('Indiv','Group','Author'))
dfeeg$Type_trial <- factor(dfeeg$Type_trial)

dfbeh_long <- read.csv("Social_Influence_lmer_SIV2.csv", sep=";", header=TRUE)
for (part_i in unique(dfeeg$Participant)){
  dfeeg$Country[dfeeg$Participant==part_i] <- dfbeh_long$Country[dfbeh_long$Participant==part_i][1]
}
dfeeg$Country[dfeeg$Participant=="729"] <- "Cambodia"
dfeeg$Country[dfeeg$Participant=="177"] <- "Belgium"

dfeeg$Country <- factor(dfeeg$Country, levels=c('Belgium','Cambodia','Rwanda'))

dfbeh_long$Trial <- dfbeh_long$Trial+1 #because start to 0 
dfbeh_long$Gender <- replace(dfbeh_long$Gender, dfbeh_long$Gender%in%c("H",'h'), 'm')
dfbeh_long$Gender[dfbeh_long$Participant==736] <- 'f'
dfbeh_long$Gender <- factor(dfbeh_long$Gender, levels=c('m','f'))
dfbeh_long$Participant <- factor(dfbeh_long$Participant)
dfbeh_long$answer <- factor(dfbeh_long$answer, levels=c('No','Yes'))
dfbeh_long$Influence <- factor(dfbeh_long$Influence, levels=c('Indiv','Group','Author'))
dfbeh_long$Country <- factor(dfbeh_long$Country, levels=c('Belgium','Cambodia','Rwanda'))
dfbeh_long$Forced_free <- factor(dfbeh_long$Forced_free, levels=c('forced','free'))
dfbeh_long$Answer_similar <- factor(dfbeh_long$Answer_similar)
dfbeh_long$Favorite_answer <- factor(dfbeh_long$Favorite_answer)



dfbeh_long <- dfbeh_long[order(dfbeh_long$Participant),]
dfeeg <- dfeeg[order(dfeeg$Participant),]



rejection_per_participant<-c()
rejection_per_participant_fmtheta<-c()
for (i in unique(dfbeh_long$Participant)){
  length_rt_participant_i <- nrow(dfbeh_long[dfbeh_long$Participant==i,])
  rejection_participant_i_iqr <- df$Rejection_iqr[df$Participant==i][1]
  rejection_participant_i_slow <- df$Rejection_slow[df$Participant==i][1]
  percentage_removed <- (1-(length_rt_participant_i / ( length_rt_participant_i / ((1 - rejection_participant_i_slow/100) * (1- rejection_participant_i_iqr/100)))))*100
  rejection_per_participant <- c(rejection_per_participant, percentage_removed)

  percentage_removed_fmtheta <- (sum(dfeeg$Outlier[dfeeg$Participant==i]==0)/nrow(dfeeg[dfeeg$Participant==i,]))*100
  rejection_per_participant_fmtheta <- c(rejection_per_participant_fmtheta, percentage_removed_fmtheta)

}
mean(rejection_per_participant)
sd(rejection_per_participant)
mean(df$Rejection_slow)
sd(df$Rejection_slow)
mean(df$Rejection_iqr)
sd(df$Rejection_iqr)
mean(rejection_per_participant_fmtheta,na.rm=T)
sd(rejection_per_participant_fmtheta,na.rm=T)

emm_options(opt.digits = FALSE)
options(width=400)



write.table(dfbeh_long, file = paste(c("dfbeh_long.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)
write.table(dfeeg, file = paste(c("dfeeg.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)
