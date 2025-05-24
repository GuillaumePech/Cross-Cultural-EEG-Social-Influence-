#################################################################
# clean the Environment
rm(list = ls())

#load librairies
library(plyr)

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
setwd(dir= "C:/Users/mfbpe/Desktop/DATA/2023_Social_Influence/beh/") # nolint # nolint
df <- data.frame()
df3 <- data.frame()

#select the path of all the files with a specific name (here Pilot_Deliberation_Participant_*.csv, where * can be replace by everything)
files_social_Influence <- Sys.glob("*V2*")

for (i in seq_len(length(files_social_Influence))){
df1 <- read.csv(files_social_Influence[i], sep=";", header=TRUE)
df2 <- df1[1, 1:3]

df1$Country <- 'Rwanda'

if (length(colnames(df1))==10 | is.na(df1$RT[1])){

df1$Country <- 'Belgium'

if (is.na(df1$RT[1])){
  df1 <- df1[,-10]
  df1$Country <- 'Cambodia'
  }
colnames(df1) <- c("Participant" ,"Age" ,"Gender", "Scenario" ,"Trial" , "Duo_presented", "Forced_free", "choice_individu", "RT", 'Country')

# list_duo<- [[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[0,2],[0,3], [0,4],[0,5], [1,2],[1,3], [1,4],[1,5],[2,4],[2,5],[3,4],[3,5]] 
## 0 auth yes / 1 auth No / 2 Group yes / 3 Group No / 4 Individu Yes / 5 Individu No

list_names_duo <- c('Author-Yes__Author-Yes','Author-No__Author-No','Group-Yes__Group-Yes','Group-No__Group-No', 'Indiv-Yes__Indiv-Yes','Indiv-No__Indiv-No','Author-Yes__Group-Yes', 'Author-Yes__Group-No','Author-Yes__Indiv-Yes','Author-Yes__Indiv-No','Author-No__Group-Yes', 'Author-No__Group-No','Author-No__Indiv-Yes','Author-No__Indiv-No','Group-Yes__Indiv-Yes','Group-Yes__Indiv-No', 'Group-No__Indiv-Yes', 'Group-No__Indiv-No')
for (i in seq(unique(df1$Duo_presented))){
  df1$Duo_presented <- replace(df1$Duo_presented, df1$Duo_presented==i-1, list_names_duo[i])
}

list_names_forced <- c('forced','free')
for (i in seq(unique(df1$Forced_free))){
  df1$Forced_free <- replace(df1$Forced_free, df1$Forced_free==i-1, list_names_forced[i])
}

list_choice_individu <- c('Author-Yes','Author-No','Group-Yes','Group-No','Indiv-Yes','Indiv-No')
for (i in seq(unique(df1$choice_individu))){
  df1$choice_individu <- replace(df1$choice_individu, df1$choice_individu==i-1, list_choice_individu[i])
}

# group choice: 6 [0,2] YES Auth Group / 8 [0,4] YES auth Ind / 14 [2,4] YES Group Ind
# group choice :11 [1,3] No Auth Group / 13 [1,5] No auth Ind / 17 [3,5] NO Group Ind

# Response Group choice : 7 [0,3] YES Auth - NO Group / 9 [0, 5] YES Auth - NO Ind / 10 [1, 2] NO Auth /  Yes Group
# Response Group choice : 12 [1,4] NO Auth - YES Group / 15 [2, 5] YES Group - NO Ind / 16 [3, 4] NO Group - YES Ind

}

df1$RT_outlier <- df1$RT
df1$RT_outlier <- replace(df1$RT_outlier, df1$RT_outlier<.35, NA)
df2$Rejection_slow <- (sum(is.na(df1$RT_outlier))/nrow(df1))*100


df1 = df1[complete.cases(df1),]

df1$RT_outlier <- outlier(df1$RT_outlier)
df2$Rejection_iqr <- (sum(is.na(df1$RT_outlier))/nrow(df1))*100

df2$RT_mean <- mean(df1$RT_outlier, na.rm=T)
df2$RT_std <- sd(df1$RT_outlier, na.rm=T)

df2$Country <- df1$Country[1]

df1 <- tidyr::separate_wider_delim(df1, choice_individu, delim = '-', names=c('group','answer'))
df1 <- tidyr::separate_wider_delim(df1, Duo_presented, delim = '__', names=c('Single1','Single2'))
df1 <- tidyr::separate_wider_delim(df1, Single1, delim = '-', names=c('Indiv1','Answer1'))
df1 <- tidyr::separate_wider_delim(df1, Single2, delim = '-', names=c('Indiv2','Answer2'))

names(df1)[names(df1)=="group"] <- "Influence"

df1$Answer_similar <- ifelse(df1$Answer1 == df1$Answer2, 1, 0)
df1$Influence <- replace(df1$Influence, df1$Influence=='Indv', 'Indiv')
df1$Influence <- replace(df1$Influence, df1$Influence==' Indiv', 'Indiv')

df2$Selection_Group <- (sum(df1$Influence[df1$Forced_free=='free']=='Group')/length(df1$Influence[df1$Forced_free=='free']))*100
df2$Selection_Author <- (sum(df1$Influence[df1$Forced_free=='free']=='Author')/length(df1$Influence[df1$Forced_free=='free']))*100
df2$Selection_Indiv <- (sum(df1$Influence[df1$Forced_free=='free']=='Indiv')/length(df1$Influence[df1$Forced_free=='free']))*100

df2$Selection_Yes <- (sum(df1$answer[df1$Forced_free=='free' & df1$Answer_similar=='0']=='Yes')/length(df1$Influence[df1$Forced_free=='free' & df1$Answer_similar=='0']))*100
df2$Selection_No <- (sum(df1$answer[df1$Forced_free=='free' & df1$Answer_similar=='0']=='No')/length(df1$Influence[df1$Forced_free=='free' & df1$Answer_similar=='0']))*100

df2$Favorite_answer <- ifelse(df2$Selection_Yes > df2$Selection_No, 'Yes', 'No')
df1$Favorite_answer <- ifelse(df1$answer == df2$Favorite_answer, 1, 0)

df2$RT_forced_Group <- mean(df1$RT_outlier[df1$Forced_free=='forced' & df1$Influence=='Group'], na.rm=T)
df2$RT_forced_Author <- mean(df1$RT_outlier[df1$Forced_free=='forced' & df1$Influence=='Author'], na.rm=T)
df2$RT_forced_Indiv <- mean(df1$RT_outlier[df1$Forced_free=='forced' & df1$Influence=='Indiv'], na.rm=T)

df2$RT_forced_Yes <- mean(df1$RT_outlier[df1$Forced_free=='forced' & df1$answer=='Yes'], na.rm=T)
df2$RT_forced_No <- mean(df1$RT_outlier[df1$Forced_free=='forced' & df1$answer=='No'], na.rm=T)

df2$RT_free_Group <- mean(df1$RT_outlier[df1$Forced_free=='free' & df1$Influence=='Group'], na.rm=T)
df2$RT_free_Author <- mean(df1$RT_outlier[df1$Forced_free=='free' & df1$Influence=='Author'], na.rm=T)
df2$RT_free_Indiv <- mean(df1$RT_outlier[df1$Forced_free=='free' & df1$Influence=='Indiv'], na.rm=T)

df2$RT_free_Yes <- mean(df1$RT_outlier[df1$Forced_free=='free' & df1$answer=='Yes'], na.rm=T)
df2$RT_free_No <- mean(df1$RT_outlier[df1$Forced_free=='free' & df1$answer=='No'], na.rm=T)

df <- rbind.fill(df, df2)
df3 <- rbind.fill(df3, df1)
}
df <- df[order(df$Participant),]
df3 <- df3[order(df3$Participant),]


setwd(dir= "C:/Users/mfbpe/Desktop/DATA/2023_Social_Influence/results/") 

write.table(df, file = paste(c("Social_Influence_SIV2.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)
write.table(df3, file = paste(c("Social_Influence_lmer_SIV2.csv"), collapse = ""), sep=";", dec = ".", row.names = FALSE)

