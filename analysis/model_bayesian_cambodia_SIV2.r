#################################################################
# clean the Environment
rm(list = ls())
.vsc.attach()


inferential_test <- function(test_list, name_test_list, actual_sd){

for (test_i in 1:length(test_list)){

  test_on <- (gather_emmeans_draws(test_list[[test_i]])$.value)
  print(name_test_list[test_i], quote = FALSE)
  print( c("median: ", median(test_on)), quote = FALSE) 
  print ( bayestestR::pd(test_on), quote = FALSE)
  print ( HDInterval::hdi(test_on, .89)[1:2], quote = FALSE)

  max_range_density <- round(ifelse(actual_sd*2 > max(abs(test_on)), actual_sd*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
  
  min_range_density <- - max_range_density

  density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
  density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
  probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x))] # find the value to 0

  density_prior <- density(rnorm(4e4,0, actual_sd), from = min_range_density, to = max_range_density, n = 1001)
  density_prior$y <- density_prior$y / sum(density_prior$y)
  probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x))] # find the value to 0

  ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior 
  print(c("Ratio H10 :", ratio_H10), quote = FALSE)
  print(c("Actual_sd :", actual_sd), quote = FALSE)
  favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")

  lower_range_list <- pracma::logseq(actual_sd, actual_sd/1000, 100)
  upper_range_list <- pracma::logseq(actual_sd, actual_sd*1000, 100)
  actual_hypothesis <- favorite_hypothesis
  original_hypothesis <- favorite_hypothesis
  range_check <- ifelse(actual_hypothesis=="unconclusive", 2, 2)
  upper_range_value <- NA; lower_range_value <- NA
  stop_H0 = F; stop_H1 = F
  for (ite in 1:length(lower_range_list)){

    if ((favorite_hypothesis != "H0") & stop_H0 == F){
      #increase sd for upper range to test range H1 or unconclusive 
      upper_range_value = upper_range_list[ite]
          
      max_range_density <- round(ifelse(upper_range_value*2 > max(abs(test_on)), upper_range_value*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
      min_range_density <- - max_range_density

      density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
      density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
      probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x))] # find the value to 0

      density_prior <- density(rnorm(4e4,0, upper_range_value), from = min_range_density, to = max_range_density, n = 1001)      
      density_prior$y <- density_prior$y / sum(density_prior$y)
      probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x))] 
      ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior
      favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")
      if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H0 <- T; favorite_hypothesis <- "unconclusive"}
    }
    if (favorite_hypothesis != "H1" & stop_H1 ==F){
      #increase sd for upper range to test range H1 or unconclusive 
      lower_range_value = lower_range_list[ite]
      
      max_range_density <- round(ifelse(lower_range_value*2 > max(abs(test_on)), lower_range_value*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
      min_range_density <- - max_range_density
      
      density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
      density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
      probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x))] # find the value to 0

      density_prior <- density(rnorm(4e4,0, lower_range_value), from = min_range_density, to = max_range_density, n = 1001)
      density_prior$y <- density_prior$y / sum(density_prior$y)
      probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x))] 
      ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior
      favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")
      if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H1 <- T; favorite_hypothesis <- "unconclusive"}

    }
    if (range_check==0){break}
  }

    lower_range_value <- ifelse(original_hypothesis=="H1", actual_sd, lower_range_value)
    upper_range_value <- ifelse(original_hypothesis=="H0", actual_sd, upper_range_value)
    print(c("Hypothesis favored originally is :", original_hypothesis), quote = FALSE)
    print(c("range giving the same results is :", lower_range_value, upper_range_value), quote = FALSE)
    cat("\n\n\n\n\n\n")
  }

}


inferential_test_logit <- function(test_list, name_test_list, actual_sd){

for (test_i in 1:length(test_list)){

  test_on <- exp(gather_emmeans_draws(test_list[[test_i]])$.value)
  print(name_test_list[test_i], quote = FALSE)
  print( c("median: ", median(test_on)), quote = FALSE) 
  print ( bayestestR::pd(test_on, null=1), quote = FALSE)
  print ( HDInterval::hdi(test_on, .89)[1:2], quote = FALSE)

  max_range_density <- round(ifelse(actual_sd*2 > max(abs(test_on)), actual_sd*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
  
  min_range_density <- -max_range_density

  density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
  density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
  probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x - 1))] # find the value to 0

  density_prior <- density(rnorm(4e4, 1, actual_sd), from = min_range_density, to = max_range_density, n = 1001)
  density_prior$y <- density_prior$y / sum(density_prior$y)
  probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x - 1))] 

  ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior 
  print(c("Ratio H10 :", ratio_H10), quote = FALSE)
  print(c("Actual_sd :", actual_sd), quote = FALSE)
  favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")

  lower_range_list <- pracma::logseq(actual_sd, actual_sd/1000, 100)
  upper_range_list <- pracma::logseq(actual_sd, actual_sd*1000, 100)
  actual_hypothesis <- favorite_hypothesis
  original_hypothesis <- favorite_hypothesis
  range_check <- ifelse(actual_hypothesis=="unconclusive", 2, 2)
  upper_range_value <- NA; lower_range_value <- NA
  stop_H0 = F; stop_H1 = F
  for (ite in 1:length(lower_range_list)){

    if ((favorite_hypothesis != "H0") & stop_H0 == F){
      #increase sd for upper range to test range H1 or unconclusive 
      upper_range_value = upper_range_list[ite]
          
      max_range_density <- round(ifelse(upper_range_value*2 > max(abs(test_on)), upper_range_value*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
      min_range_density <- - max_range_density

      density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
      density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
      probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x - 1))] # find the value to 1

      density_prior <- density(rnorm(4e4, 1, upper_range_value), from = min_range_density, to = max_range_density, n = 1001)      
      density_prior$y <- density_prior$y / sum(density_prior$y)
      probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x - 1))] 
      ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior
      favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")
      if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H0 <- T; favorite_hypothesis <- "unconclusive"}
    }
    if (favorite_hypothesis != "H1" & stop_H1 ==F){
      #increase sd for upper range to test range H1 or unconclusive 
      lower_range_value = lower_range_list[ite]
      
      max_range_density <- round(ifelse(lower_range_value*2 > max(abs(test_on)), lower_range_value*4,  max(abs(test_on))*4 )) #to define the range of the density based on the maximum between the prior and the posterior range
      min_range_density <- - max_range_density

      density_posterior_extraction <-density(test_on, from = min_range_density, to = max_range_density, n = 1001) # calculate the density of the posterior
      density_posterior_extraction$y <- density_posterior_extraction$y / sum(density_posterior_extraction$y) # normalize to sum to 1
      probabilty_H0_posterior <- density_posterior_extraction$y[which.min(abs(density_posterior_extraction$x - 1))] # find the value to 1
      
      density_prior <- density(rnorm(4e4, 1, lower_range_value), from = min_range_density, to = max_range_density, n = 1001)
      density_prior$y <- density_prior$y / sum(density_prior$y)
      probabilty_H0_prior <- density_prior$y[which.min(abs(density_prior$x - 1))] 
      ratio_H10 <- probabilty_H0_prior / probabilty_H0_posterior
      favorite_hypothesis <- ifelse(ratio_H10 > 1/3, ifelse(ratio_H10 < 3, "unconclusive","H1"), "H0")
      if (favorite_hypothesis != actual_hypothesis){range_check <- range_check-1; stop_H1 <- T; favorite_hypothesis <- "unconclusive"}

    }
    if (range_check==0){break}
  }

    lower_range_value <- ifelse(original_hypothesis=="H1", actual_sd, lower_range_value)
    upper_range_value <- ifelse(original_hypothesis=="H0", actual_sd, upper_range_value)
    print(c("Hypothesis favored originally is :", original_hypothesis), quote = FALSE)
    print(c("range giving the same results is :", lower_range_value, upper_range_value), quote = FALSE)
    cat("\n\n\n\n\n\n")
  }

}

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

#load librairies
library(brms); library(emmeans); library(ggplot2); library(httpgd); library(dplyr); library(MASS); library(tidybayes)

df_questionnaire <- read.csv("V2_CrossCultural_SocialInfluence.csv", sep=";", header=TRUE)

df_questionnaire <- data.frame(Participant=rep(df_questionnaire$Participant,6), Country = rep(df_questionnaire$Country,6), Influence = rep(c("Authority","Group"),each=nrow(df_questionnaire)*3),Score = c(df_questionnaire[,3],df_questionnaire[,6],df_questionnaire[,7],df_questionnaire[,4],df_questionnaire[,5],df_questionnaire[,8]), Items=rep(seq(6),each=nrow(df_questionnaire)))


df_questionnaire$Participant <- factor(df_questionnaire$Participant)
df_questionnaire$Score <- factor(df_questionnaire$Score, ordered=T)
df_questionnaire$Country <- factor(df_questionnaire$Country, levels=c('Belgium','Cambodia','Rwanda'))
df_questionnaire$Influence <- factor(df_questionnaire$Influence, levels=c('Group', 'Authority'))
df_questionnaire$Items <- factor(df_questionnaire$Items)
contrasts(df_questionnaire$Country) <- contr.sdif(3)
contrasts(df_questionnaire$Influence) <- contr.sdif(2)

dfbeh_long <- read.csv("dfbeh_long.csv", sep = ";", header = TRUE)
dfeeg <- read.csv("dfeeg.csv", sep = ";", header = TRUE)


dfbeh_long$Gender <- factor(dfbeh_long$Gender, levels=c('m','f'))
dfbeh_long$Participant <- factor(dfbeh_long$Participant)
dfbeh_long$answer <- factor(dfbeh_long$answer, levels=c('No','Yes'))
dfbeh_long$Influence <- factor(dfbeh_long$Influence, levels=c('Indiv','Group','Author'))
dfbeh_long$Country <- factor(dfbeh_long$Country, levels=c('Belgium','Cambodia','Rwanda'))
dfbeh_long$Forced_free <- factor(dfbeh_long$Forced_free, levels=c('forced','free'))
dfbeh_long$Answer_similar <- factor(dfbeh_long$Answer_similar)
dfbeh_long$Favorite_answer <- factor(dfbeh_long$Favorite_answer)

contrasts(dfbeh_long$Influence) <- contr.sdif(3)
contrasts(dfbeh_long$Country) <- contr.sdif(3)
contrasts(dfbeh_long$answer) <- contr.sdif(2)
contrasts(dfbeh_long$Forced_free) <- contr.sdif(2)

dfeeg$Participant <- factor(dfeeg$Participant)
dfeeg$answer <- factor(dfeeg$answer, levels=c('No','Yes'))
dfeeg$Forced_free <- factor(dfeeg$Forced_free, levels=c('forced','free'))
dfeeg$Influence <- factor(dfeeg$Influence, levels=c('Indiv','Group','Author'))

dfeeg$Country <- factor(dfeeg$Country, levels=c('Belgium','Cambodia','Rwanda'))

contrasts(dfeeg$Influence) <- contr.sdif(3)
contrasts(dfeeg$Country) <- contr.sdif(3)
contrasts(dfeeg$answer) <- contr.sdif(2)
contrasts(dfeeg$Forced_free) <- contr.sdif(2)









###########################################################################
#####                                                           ###########
##### QUESTION 1 : DOES THEY CHOOSE MORE ONE GROUP THAN ANOTHER ###########
#####                                                           ###########
###########################################################################

df_selection <- dfbeh_long[dfbeh_long$Forced_free=="free",] %>%
  group_by(Participant, Country, answer)%>%
  summarise(Selection = c(sum(Influence=="Indiv"), sum(Influence=="Group"), sum(Influence=="Author")))

df_selection$Influence <- rep(c('Indiv','Group','Author'), nrow(df_selection)/3)

for (part_i in unique(df_selection$Participant)){
    df_selection$Selection[df_selection$Participant==part_i] <-  (df_selection$Selection[df_selection$Participant==part_i] / sum(df_selection$Selection[df_selection$Participant==part_i]))
}

df_selection$Influence <- factor(df_selection$Influence, levels=c('Indiv','Group','Author'))

df_part <- df_selection %>%
  group_by(Participant)%>%
  summarise(Selection = mean(Selection))


contrasts(df_selection$Influence) <- contr.sdif(3)
contrasts(df_selection$answer) <- contr.sdif(2)
str(df_selection)

prior_selection <- prior("normal(0, .5)", class="b") +  #plogis(-1.6 +- .5) +- 15% dif
prior("normal(-1.6, 0.1)", class="Intercept") +   #  -1.6 plogis(-1.6) ~ 16.667%    1/6 answer yes - no * indiv auth - group - indiv
prior("normal(0, .1)", class = "sd", group="Participant")+
prior("normal(0, .5)", class = "sd", coef = "answerNo", group="Participant")+
prior("normal(0, .5)", class = "sd", coef = "answerYes", group="Participant")+
prior("normal(0, .5)", class = "sd", coef = "Influence2M1", group="Participant")+
prior("normal(0, .5)", class = "sd", coef = "Influence3M2", group="Participant")+
prior("gamma(1.8, 0.02)", class = "phi", lb=0)+ #likely to be between 0-100 then decreasing
prior("beta(1, 1.5)", class = "zi", lb=0, ub=1)   # 1,1.5   more liekly to be 0 than 1, decreasing slowly

mSelection <- brm(Selection ~ Influence * answer * Country + (0+answer * Influence | Participant), data=df_selection, family = zero_inflated_beta, prior = prior_selection, iter=2000, warmup = 1000, chains=1, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mSelection, file="mSelection.Rdata")
load("mSelection.Rdata")

Group_Indiv <-data.frame(ranef(mSelection, pars="Influence2M1"))[,1]
Author_Group <-data.frame(ranef(mSelection, pars="Influence3M2"))[,1]
Author_Indiv <- Author_Group + Group_Indiv 


outlier(Group_Indiv)
outlier(Author_Group)
outlier(Author_Indiv)

data.frame(ranef(mSelection))[which(is.na(outlier(Group_Indiv))),]
data.frame(ranef(mSelection))[which(is.na(outlier(Author_Group))),]
data.frame(ranef(mSelection))[which(is.na(outlier(Author_Indiv))),]

mSelection_clean <- update(mSelection, newdata = mSelection$data[!(mSelection$data$Participant %in% c(176, 739, 941, 946, 952)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mSelection_clean, file = 'mSelection_clean.Rdata')
load("mSelection_clean.Rdata")

# Generate the summary of your model
summary_output <- summary(mSelection_clean)
prior_output <- mSelection_clean$prior

# Capture the summary output as text
summary_text <- c(capture.output(summary_output),capture.output(prior_output))


# Save the summary as a PDF
tiff("mSelection_clean_summary.tiff", width = 16, height = 24, units = "in", res = 300)  
plot.new()
text(0, 1, paste(summary_text, collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.8, font=2)  # Reduce text size with cex
dev.off()


estimated_means1 <- emmeans(mSelection_clean, ~ Country*Influence*answer)


actual_sd <- 0.5

#Antisocial No vs Prosocial Yes
 
contrasts_no_vs_yes <- contrast(estimated_means1,  list(c1 = c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,  -1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9))) 

inferential_test(list(contrasts_no_vs_yes), "no_vs_yes", actual_sd)
inferential_test_logit(list(contrasts_no_vs_yes), "no_vs_yes", actual_sd)

#Antisocial No vs Prosocial per country

contrasts_no_vs_yes_belgium <- contrast(estimated_means1,  list(c1 = c(1/3, 0, 0,1/3, 0, 0,1/3, 0, 0,  -1/3, 0, 0,-1/3, 0, 0,-1/3, 0, 0))) 
contrasts_no_vs_yes_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/3, 0, 0, 1/3, 0, 0, 1/3, 0,  0, -1/3, 0, 0, -1/3, 0, 0, -1/3, 0))) 
contrasts_no_vs_yes_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/3, 0, 0, 1/3, 0, 0, 1/3,  0, 0, -1/3, 0, 0, -1/3, 0, 0, -1/3))) 

test_list <- list(contrasts_no_vs_yes_belgium,contrasts_no_vs_yes_cambodia,contrasts_no_vs_yes_rwanda)
name_test <- c("contrasts_no_vs_yes_belgium","contrasts_no_vs_yes_cambodia","contrasts_no_vs_yes_rwanda")
inferential_test(test_list, name_test, actual_sd)
inferential_test_logit(test_list, name_test, actual_sd)


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/6, 1/6, 1/6, -1/6, -1/6, -1/6, 0, 0, 0,  1/6, 1/6, 1/6, -1/6, -1/6, -1/6, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/6, 1/6, 1/6, 0, 0, 0, -1/6, -1/6, -1/6,  1/6, 1/6, 1/6, 0, 0, 0, -1/6, -1/6, -1/6)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/6, 1/6, 1/6, -1/6, -1/6, -1/6,  0, 0, 0, 1/6, 1/6, 1/6, -1/6, -1/6, -1/6)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)
inferential_test_logit(test_list, name_test, actual_sd)

# test within Influence within Country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, -1/2, 0, 0, 0, 0, 0,  1/2, 0, 0, -1/2, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, 0, 0, 0, -1/2, 0, 0,  1/2, 0, 0, 0, 0, 0, -1/2, 0, 0))) 
contrasts_group_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, 0, -1/2, 0, 0,  0, 0, 0, 1/2, 0, 0, -1/2, 0, 0))) 

contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, -1/2, 0, 0, 0, 0,  0, 1/2, 0, 0, -1/2, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, 0, 0, 0, -1/2, 0,  0, 1/2, 0, 0, 0, 0, 0, -1/2, 0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, 0, 0, -1/2, 0,  0, 0, 0, 0, 1/2, 0, 0, -1/2, 0))) 

contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0, -1/2, 0, 0, 0,  0, 0, 1/2, 0, 0, -1/2, 0, 0, 0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0, 0, 0, 0, -1/2,  0, 0, 1/2, 0, 0, 0, 0, 0, -1/2))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 1/2, 0, 0, -1/2,  0, 0, 0, 0, 0, 1/2, 0, 0, -1/2))) 

test_list <- list(contrasts_indiv_vs_group_belgium,
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia,
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)
name_test <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium",
"contrasts_indiv_vs_group_cambodia",
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")
inferential_test(test_list, name_test, actual_sd)
inferential_test_logit(test_list, name_test, actual_sd)



# test within Influence BETWEEN Country

contrasts_indiv_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(1/2, -1/2, 0, 0, 0, 0, 0, 0, 0,     1/2, -1/2, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(1/2, 0, -1/2, 0, 0, 0, 0, 0, 0,     1/2, 0, -1/2, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 1/2, -1/2, 0, 0, 0, 0, 0, 0,     0, 1/2, -1/2, 0, 0, 0, 0, 0, 0))) 

contrasts_group_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, -1/2, 0, 0, 0, 0,     0, 0, 0, 1/2, -1/2, 0, 0, 0, 0))) 
contrasts_group_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, -1/2, 0, 0, 0,     0, 0, 0, 1/2, 0, -1/2, 0, 0, 0))) 
contrasts_group_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, -1/2, 0, 0, 0,     0, 0, 0, 0, 1/2, -1/2, 0, 0, 0))) 

contrasts_author_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, -1/2, 0,      0, 0, 0, 0, 0, 0, 1/2, -1/2, 0))) 
contrasts_author_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, 0, -1/2,      0, 0, 0, 0, 0, 0, 1/2, 0, -1/2))) 
contrasts_author_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 0, 1/2, -1/2,      0, 0, 0, 0, 0, 0, 0, 1/2, -1/2))) 

test_list <- list(contrasts_indiv_belgium_vs_cambodia,
contrasts_indiv_belgium_vs_rwanda,
contrasts_indiv_cambodia_vs_rwanda,
contrasts_group_belgium_vs_cambodia,
contrasts_group_belgium_vs_rwanda,
contrasts_group_cambodia_vs_rwanda,
contrasts_author_belgium_vs_cambodia,
contrasts_author_belgium_vs_rwanda,
contrasts_author_cambodia_vs_rwanda)
name_test <- c("contrasts_indiv_belgium_vs_cambodia",
"contrasts_indiv_belgium_vs_rwanda",
"contrasts_indiv_cambodia_vs_rwanda",
"contrasts_group_belgium_vs_cambodia",
"contrasts_group_belgium_vs_rwanda",
"contrasts_group_cambodia_vs_rwanda",
"contrasts_author_belgium_vs_cambodia",
"contrasts_author_belgium_vs_rwanda",
"contrasts_author_cambodia_vs_rwanda")
inferential_test_logit(test_list, name_test, actual_sd)




#Antisocial No


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/3, 1/3, 1/3, -1/3, -1/3, -1/3, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/3, 1/3, 1/3, 0, 0, 0, -1/3, -1/3, -1/3,  0, 0, 0, 0, 0, 0, 0, 0, 0)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3,  0, 0, 0, 0, 0, 0, 0, 0, 0)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test_logit(test_list, name_test, actual_sd)

# test Antisocial within Influence within Country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1,0,0,-1,0,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1, list(c1 = c(0,1,0,0,-1,0,0,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,   list(c1 = c(0,0,1,0,0,-1,0,0,0,    0,0,0,0,0,0,0,0,0))) 

contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(1,0,0,0,0,0,-1,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,1,0,0,0,0,0,-1,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,1,0,0,0,0,0,-1,    0,0,0,0,0,0,0,0,0))) 

contrasts_group_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,1,0,0,-1,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,1,0,0,-1,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,1,0,0,-1,    0,0,0,0,0,0,0,0,0))) 


test_list <- list(contrasts_indiv_vs_group_belgium, 
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia, 
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)

name_test_list <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium", 
"contrasts_indiv_vs_group_cambodia", 
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")

inferential_test_logit(test_list, name_test, actual_sd)

#Prosocial Yes


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   1/3, 1/3, 1/3, -1/3, -1/3, -1/3, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   1/3, 1/3, 1/3, 0, 0, 0, -1/3, -1/3, -1/3)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test_logit(test_list, name_test, actual_sd)

# test Prosocial within Influence within Country


contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,0,-1,0,0,0,0,0))) 
contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1, list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,0,0,-1,0,0,0,0))) 
contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,1,0,0,-1,0,0,0))) 

contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,0,0,0,0,-1,0,0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,0,0,0,0,0,-1,0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,1,0,0,0,0,0,-1))) 

contrasts_group_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,1,0,0,-1,0,0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,1,0,0,-1,0))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,0,1,0,0,-1))) 



test_list <- list(contrasts_indiv_vs_group_belgium, 
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia, 
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)

name_test_list <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium", 
"contrasts_indiv_vs_group_cambodia", 
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")
inferential_test_logit(test_list, name_test, actual_sd)
































#############################################################################
#####                                                             ###########
##### QUESTION 2 : DOES THEY CHOOSE FASTER ONE GROUP THAN ANOTHER ###########
#####                                                             ###########
#############################################################################


m18_RT <- glmer(RT_outlier ~ Influence * Forced_free  + Country + Influence:Country + (1|Participant), data=dfbeh_long,family=Gamma('identity')) 


mRT <- lmer(RT_outlier ~ Influence * answer * Country * Forced_free +(Influence+answer|Participant), data=dfbeh_long)


emmeans(mRT, pairwise~"Influence * answer * Country", adjust="none")

prior_RT <- get_prior(RT ~ Influence * answer * Country * Forced_free +(Influence+answer|Participant), data=dfbeh_long, family=shifted_lognormal())


dfbeh_long$RT <- dfbeh_long$RT_outlier

exp(-.1+.8)+.35
prior_RT <- prior("normal(0, .15)", class="b") + # 300ms of effect maximally expected
prior("normal(-.1, .125)", class="Intercept") +  # exp(-.1) = 900ms (median of the datasat)
prior("normal(0, .4)", class="sd") +
prior("normal(0,.15)", class="sd", coef="answer2M1", group="Participant") + # difference of 300ms of effect between participant
prior("normal(0,.15)", class="sd", coef="Influence2M1", group="Participant") + # difference of 300ms of effect between participant
prior("normal(0,.15)", class="sd", coef="Influence3M2", group="Participant") + # difference of 300ms of effect between participant
prior("normal(.35, .125)", class="ndt", lb=0) +
prior("lkj(1)", class="cor") +
prior("normal(.25, .125)", class="sigma", lb=0)

mRT <- brm(RT ~ Influence * answer * Country * Forced_free +(Influence*answer|Participant), data=dfbeh_long, family=shifted_lognormal(), prior = prior_RT, ,iter = 2000, chains=1, warmup=1000, backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mRT, file="mRT.Rdata")
load("mRT.Rdata")


Group_Indiv <-data.frame(ranef(mRT, pars="Influence2M1"))[,1]
Author_Group <-data.frame(ranef(mRT, pars="Influence3M2"))[,1]
Author_Indiv <- Author_Group + Group_Indiv 
Yes_No <-data.frame(ranef(mRT, pars="answer2M1"))[,1]


data.frame(ranef(mRT))[which(is.na(outlier(Group_Indiv))),]
data.frame(ranef(mRT))[which(is.na(outlier(Author_Group))),]
data.frame(ranef(mRT))[which(is.na(outlier(Author_Indiv))),]
data.frame(ranef(mRT))[which(is.na(outlier(Yes_No))),]

mRT_clean <- update(mRT, newdata = mRT$data[!(mRT$data$Participant %in% c(170, 735, 946)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")


save(mRT_clean, file = 'mRT_clean.Rdata')
load("mRT_clean.Rdata")

# Generate the summary of your model
summary_output <- summary(mRT_clean)
prior_output <- mRT_clean$prior

options(width=400)
# Capture the summary output as text
summary_text <- c(capture.output(summary_output),capture.output(prior_output))


# Save the summary as a PDF
tiff("mRT_clean_summary.tiff", width = 16, height = 24, units = "in", res = 300)  
plot.new()
text(0, 1, paste(summary_text, collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.8, font=2)  # Reduce text size with cex
dev.off()



estimated_means1 <- emmeans(mRT_clean, ~ Country*Influence*answer)
estimated_means2 <- emmeans(mRT_clean, pairwise~ answer)

actual_sd <- .15

#Antisocial No vs Prosocial Yes

contrasts_no_vs_yes <- contrast(estimated_means1,  list(c1 = c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,  -1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9))) 

inferential_test(list(contrasts_no_vs_yes), "no_vs_yes", actual_sd)

#Antisocial No vs Prosocial per country

contrasts_no_vs_yes_belgium <- contrast(estimated_means1,  list(c1 = c(1/3, 0, 0,1/3, 0, 0,1/3, 0, 0,  -1/3, 0, 0,-1/3, 0, 0,-1/3, 0, 0))) 
contrasts_no_vs_yes_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/3, 0, 0, 1/3, 0, 0, 1/3, 0,  0, -1/3, 0, 0, -1/3, 0, 0, -1/3, 0))) 
contrasts_no_vs_yes_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/3, 0, 0, 1/3, 0, 0, 1/3,  0, 0, -1/3, 0, 0, -1/3, 0, 0, -1/3))) 

test_list <- list(contrasts_no_vs_yes_belgium,contrasts_no_vs_yes_cambodia,contrasts_no_vs_yes_rwanda)
name_test <- c("contrasts_no_vs_yes_belgium","contrasts_no_vs_yes_cambodia","contrasts_no_vs_yes_rwanda")
inferential_test(test_list, name_test, actual_sd)

# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/6, 1/6, 1/6, -1/6, -1/6, -1/6, 0, 0, 0,  1/6, 1/6, 1/6, -1/6, -1/6, -1/6, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/6, 1/6, 1/6, 0, 0, 0, -1/6, -1/6, -1/6,  1/6, 1/6, 1/6, 0, 0, 0, -1/6, -1/6, -1/6)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/6, 1/6, 1/6, -1/6, -1/6, -1/6,  0, 0, 0, 1/6, 1/6, 1/6, -1/6, -1/6, -1/6)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)


# test within Influence within Country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, -1/2, 0, 0, 0, 0, 0,  1/2, 0, 0, -1/2, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, 0, 0, 0, -1/2, 0, 0,  1/2, 0, 0, 0, 0, 0, -1/2, 0, 0))) 
contrasts_group_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, 0, -1/2, 0, 0,  0, 0, 0, 1/2, 0, 0, -1/2, 0, 0))) 

contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, -1/2, 0, 0, 0, 0,  0, 1/2, 0, 0, -1/2, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, 0, 0, 0, -1/2, 0,  0, 1/2, 0, 0, 0, 0, 0, -1/2, 0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, 0, 0, -1/2, 0,  0, 0, 0, 0, 1/2, 0, 0, -1/2, 0))) 

contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0, -1/2, 0, 0, 0,  0, 0, 1/2, 0, 0, -1/2, 0, 0, 0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0, 0, 0, 0, -1/2,  0, 0, 1/2, 0, 0, 0, 0, 0, -1/2))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 1/2, 0, 0, -1/2,  0, 0, 0, 0, 0, 1/2, 0, 0, -1/2))) 

test_list <- list(contrasts_indiv_vs_group_belgium,
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia,
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)
name_test <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium",
"contrasts_indiv_vs_group_cambodia",
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")
inferential_test(test_list, name_test, actual_sd)


# test within Influence BETWEEN Country
contrasts_indiv_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(1/2, -1/2, 0, 0, 0, 0, 0, 0, 0,     1/2, -1/2, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(1/2, 0, -1/2, 0, 0, 0, 0, 0, 0,     1/2, 0, -1/2, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 1/2, -1/2, 0, 0, 0, 0, 0, 0,     0, 1/2, -1/2, 0, 0, 0, 0, 0, 0))) 

contrasts_group_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, -1/2, 0, 0, 0, 0,     0, 0, 0, 1/2, -1/2, 0, 0, 0, 0))) 
contrasts_group_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, -1/2, 0, 0, 0,     0, 0, 0, 1/2, 0, -1/2, 0, 0, 0))) 
contrasts_group_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, -1/2, 0, 0, 0,     0, 0, 0, 0, 1/2, -1/2, 0, 0, 0))) 

contrasts_author_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, -1/2, 0,      0, 0, 0, 0, 0, 0, 1/2, -1/2, 0))) 
contrasts_author_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, 0, -1/2,      0, 0, 0, 0, 0, 0, 1/2, 0, -1/2))) 
contrasts_author_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 0, 1/2, -1/2,      0, 0, 0, 0, 0, 0, 0, 1/2, -1/2))) 

test_list <- list(contrasts_indiv_belgium_vs_cambodia,
contrasts_indiv_belgium_vs_rwanda,
contrasts_indiv_cambodia_vs_rwanda,
contrasts_group_belgium_vs_cambodia,
contrasts_group_belgium_vs_rwanda,
contrasts_group_cambodia_vs_rwanda,
contrasts_author_belgium_vs_cambodia,
contrasts_author_belgium_vs_rwanda,
contrasts_author_cambodia_vs_rwanda)
name_test <- c("contrasts_indiv_belgium_vs_cambodia",
"contrasts_indiv_belgium_vs_rwanda",
"contrasts_indiv_cambodia_vs_rwanda",
"contrasts_group_belgium_vs_cambodia",
"contrasts_group_belgium_vs_rwanda",
"contrasts_group_cambodia_vs_rwanda",
"contrasts_author_belgium_vs_cambodia",
"contrasts_author_belgium_vs_rwanda",
"contrasts_author_cambodia_vs_rwanda")
inferential_test(test_list, name_test, actual_sd)



#Antisocial No


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/3, 1/3, 1/3, -1/3, -1/3, -1/3, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/3, 1/3, 1/3, 0, 0, 0, -1/3, -1/3, -1/3,  0, 0, 0, 0, 0, 0, 0, 0, 0)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3,  0, 0, 0, 0, 0, 0, 0, 0, 0)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)

# test Antisocial within Influence within Country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1,0,0,-1,0,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1, list(c1 = c(0,1,0,0,-1,0,0,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,   list(c1 = c(0,0,1,0,0,-1,0,0,0,    0,0,0,0,0,0,0,0,0))) 

contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(1,0,0,0,0,0,-1,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,1,0,0,0,0,0,-1,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,1,0,0,0,0,0,-1,    0,0,0,0,0,0,0,0,0))) 

contrasts_group_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,1,0,0,-1,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,1,0,0,-1,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,1,0,0,-1,    0,0,0,0,0,0,0,0,0))) 


test_list <- list(contrasts_indiv_vs_group_belgium, 
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia, 
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)

name_test_list <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium", 
"contrasts_indiv_vs_group_cambodia", 
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")

inferential_test(test_list, name_test_list, actual_sd)

#Prosocial Yes


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   1/3, 1/3, 1/3, -1/3, -1/3, -1/3, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   1/3, 1/3, 1/3, 0, 0, 0, -1/3, -1/3, -1/3)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)

# test Prosocial within Influence within Country


contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,0,-1,0,0,0,0,0))) 
contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1, list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,0,0,-1,0,0,0,0))) 
contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,1,0,0,-1,0,0,0))) 

contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,0,0,0,0,-1,0,0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,0,0,0,0,0,-1,0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,1,0,0,0,0,0,-1))) 

contrasts_group_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,1,0,0,-1,0,0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,1,0,0,-1,0))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,0,1,0,0,-1))) 



test_list <- list(contrasts_indiv_vs_group_belgium, 
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia, 
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)

name_test_list <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium", 
"contrasts_indiv_vs_group_cambodia", 
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")
inferential_test(test_list, name_test_list, actual_sd)























#############################################################################
#####                                                             ###########
##### QUESTION 3 : WHAT ABOUT THE FMtheta                         ###########
#####                                                             ###########
#############################################################################

windows()

prior_FMtheta <- get_prior(FMtheta ~ Type_trial * Country + (Type_trial|Participant), data=dfeeg)


median(dfeeg$FMtheta) # 5
options(width=400)

prior_FMtheta <- prior("normal(0, 2)", class="b") + 
prior("normal(5, 2)", class="Intercept") +   # 
prior("normal(0, 5)", class="sd")  +   # 
prior("normal(0,2)", class="sd", coef="answer2M1", group="Participant") +
prior("normal(0,2)", class="sd", coef="Influence2M1", group="Participant") +
prior("normal(0,2)", class="sd", coef="Influence3M2", group="Participant") +
prior("normal(0, 5)", class="sigma", lb=0) 

mFMtheta <- brm(FMtheta ~ Influence * answer * Country * Forced_free + (Influence * answer |Participant), data=dfeeg[dfeeg$Outlier==1,], prior = prior_FMtheta,  iter=2000, warmup = 1000, chains=1,  backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())

save(mFMtheta, file="mFMtheta.Rdata")

Group_Indiv <-data.frame(ranef(mFMtheta, pars="Influence2M1"))[,1]
Author_Group <-data.frame(ranef(mFMtheta, pars="Influence3M2"))[,1]
Author_Indiv <- Author_Group + Group_Indiv 
Yes_No <-data.frame(ranef(mFMtheta, pars="answer2M1"))[,1]

outlier(Group_Indiv)
outlier(Author_Group)
outlier(Author_Indiv)
outlier(Yes_No)


data.frame(ranef(mFMtheta))[which(is.na(outlier(Group_Indiv))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Author_Group))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Author_Indiv))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Yes_No))),]

load("mFMtheta.Rdata")
mFMtheta_clean <- update(mFMtheta, newdata = mFMtheta$data[!(mFMtheta$data$Participant %in% c(739, 946, 951)),] ,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mFMtheta_clean, file="mFMtheta_clean.Rdata")


load("mFMtheta_clean.Rdata")

# Generate the summary of your model
summary_output <- summary(mFMtheta_clean)
prior_output <- mFMtheta_clean$prior

options(width=400)
# Capture the summary output as text
summary_text <- c(capture.output(summary_output),capture.output(prior_output))


# Save the summary as a PDF
tiff("mFMtheta_clean_summary.tiff", width = 16, height = 24, units = "in", res = 300)  
plot.new()
text(0, 1, paste(summary_text, collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.8, font=2)  # Reduce text size with cex
dev.off()

actual_sd <- 2

confint(estimated_means1,level=.89)

#Antisocial No vs Prosocial Yes

contrasts_no_vs_yes <- contrast(estimated_means1,  list(c1 = c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,  -1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9,-1/9))) 

inferential_test(list(contrasts_no_vs_yes), "no_vs_yes", actual_sd)

#Antisocial No vs Prosocial per country

contrasts_no_vs_yes_belgium <- contrast(estimated_means1,  list(c1 = c(1/3, 0, 0,1/3, 0, 0,1/3, 0, 0,  -1/3, 0, 0,-1/3, 0, 0,-1/3, 0, 0))) 
contrasts_no_vs_yes_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/3, 0, 0, 1/3, 0, 0, 1/3, 0,  0, -1/3, 0, 0, -1/3, 0, 0, -1/3, 0))) 
contrasts_no_vs_yes_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/3, 0, 0, 1/3, 0, 0, 1/3,  0, 0, -1/3, 0, 0, -1/3, 0, 0, -1/3))) 

test_list <- list(contrasts_no_vs_yes_belgium,contrasts_no_vs_yes_cambodia,contrasts_no_vs_yes_rwanda)
name_test <- c("contrasts_no_vs_yes_belgium","contrasts_no_vs_yes_cambodia","contrasts_no_vs_yes_rwanda")
inferential_test(test_list, name_test, actual_sd)

# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/6, 1/6, 1/6, -1/6, -1/6, -1/6, 0, 0, 0,  1/6, 1/6, 1/6, -1/6, -1/6, -1/6, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/6, 1/6, 1/6, 0, 0, 0, -1/6, -1/6, -1/6,  1/6, 1/6, 1/6, 0, 0, 0, -1/6, -1/6, -1/6)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/6, 1/6, 1/6, -1/6, -1/6, -1/6,  0, 0, 0, 1/6, 1/6, 1/6, -1/6, -1/6, -1/6)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)


# test within Influence within Country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, -1/2, 0, 0, 0, 0, 0,  1/2, 0, 0, -1/2, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, 0, 0, 0, -1/2, 0, 0,  1/2, 0, 0, 0, 0, 0, -1/2, 0, 0))) 
contrasts_group_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, 0, -1/2, 0, 0,  0, 0, 0, 1/2, 0, 0, -1/2, 0, 0))) 

contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, -1/2, 0, 0, 0, 0,  0, 1/2, 0, 0, -1/2, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, 0, 0, 0, -1/2, 0,  0, 1/2, 0, 0, 0, 0, 0, -1/2, 0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, 0, 0, -1/2, 0,  0, 0, 0, 0, 1/2, 0, 0, -1/2, 0))) 

contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0, -1/2, 0, 0, 0,  0, 0, 1/2, 0, 0, -1/2, 0, 0, 0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0, 0, 0, 0, -1/2,  0, 0, 1/2, 0, 0, 0, 0, 0, -1/2))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 1/2, 0, 0, -1/2,  0, 0, 0, 0, 0, 1/2, 0, 0, -1/2))) 

test_list <- list(contrasts_indiv_vs_group_belgium,
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia,
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)
name_test <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium",
"contrasts_indiv_vs_group_cambodia",
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")
inferential_test(test_list, name_test, actual_sd)

# test within Influence BETWEEN Country

contrasts_indiv_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(1/2, -1/2, 0, 0, 0, 0, 0, 0, 0,     1/2, -1/2, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(1/2, 0, -1/2, 0, 0, 0, 0, 0, 0,     1/2, 0, -1/2, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 1/2, -1/2, 0, 0, 0, 0, 0, 0,     0, 1/2, -1/2, 0, 0, 0, 0, 0, 0))) 

contrasts_group_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, -1/2, 0, 0, 0, 0,     0, 0, 0, 1/2, -1/2, 0, 0, 0, 0))) 
contrasts_group_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, -1/2, 0, 0, 0,     0, 0, 0, 1/2, 0, -1/2, 0, 0, 0))) 
contrasts_group_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, -1/2, 0, 0, 0,     0, 0, 0, 0, 1/2, -1/2, 0, 0, 0))) 

contrasts_author_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, -1/2, 0,      0, 0, 0, 0, 0, 0, 1/2, -1/2, 0))) 
contrasts_author_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, 0, -1/2,      0, 0, 0, 0, 0, 0, 1/2, 0, -1/2))) 
contrasts_author_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 0, 1/2, -1/2,      0, 0, 0, 0, 0, 0, 0, 1/2, -1/2))) 

test_list <- list(contrasts_indiv_belgium_vs_cambodia,
contrasts_indiv_belgium_vs_rwanda,
contrasts_indiv_cambodia_vs_rwanda,
contrasts_group_belgium_vs_cambodia,
contrasts_group_belgium_vs_rwanda,
contrasts_group_cambodia_vs_rwanda,
contrasts_author_belgium_vs_cambodia,
contrasts_author_belgium_vs_rwanda,
contrasts_author_cambodia_vs_rwanda)
name_test <- c("contrasts_indiv_belgium_vs_cambodia",
"contrasts_indiv_belgium_vs_rwanda",
"contrasts_indiv_cambodia_vs_rwanda",
"contrasts_group_belgium_vs_cambodia",
"contrasts_group_belgium_vs_rwanda",
"contrasts_group_cambodia_vs_rwanda",
"contrasts_author_belgium_vs_cambodia",
"contrasts_author_belgium_vs_rwanda",
"contrasts_author_cambodia_vs_rwanda")
inferential_test(test_list, name_test, actual_sd)


#Antisocial No


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/3, 1/3, 1/3, -1/3, -1/3, -1/3, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/3, 1/3, 1/3, 0, 0, 0, -1/3, -1/3, -1/3,  0, 0, 0, 0, 0, 0, 0, 0, 0)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3,  0, 0, 0, 0, 0, 0, 0, 0, 0)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)

# test Antisocial within Influence within Country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1,0,0,-1,0,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1, list(c1 = c(0,1,0,0,-1,0,0,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,   list(c1 = c(0,0,1,0,0,-1,0,0,0,    0,0,0,0,0,0,0,0,0))) 

contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(1,0,0,0,0,0,-1,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,1,0,0,0,0,0,-1,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,1,0,0,0,0,0,-1,    0,0,0,0,0,0,0,0,0))) 

contrasts_group_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,1,0,0,-1,0,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,1,0,0,-1,0,    0,0,0,0,0,0,0,0,0))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,1,0,0,-1,    0,0,0,0,0,0,0,0,0))) 


test_list <- list(contrasts_indiv_vs_group_belgium, 
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia, 
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)

name_test_list <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium", 
"contrasts_indiv_vs_group_cambodia", 
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")

inferential_test(test_list, name_test_list, actual_sd)

#Prosocial Yes


# test within Influence 

contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   1/3, 1/3, 1/3, -1/3, -1/3, -1/3, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   1/3, 1/3, 1/3, 0, 0, 0, -1/3, -1/3, -1/3)))
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 1/3, 1/3, 1/3, -1/3, -1/3, -1/3)))

test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)

# test Prosocial within Influence within Country


contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,0,-1,0,0,0,0,0))) 
contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1, list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,0,0,-1,0,0,0,0))) 
contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,1,0,0,-1,0,0,0))) 

contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,0,0,0,0,-1,0,0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,0,0,0,0,0,-1,0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,1,0,0,0,0,0,-1))) 

contrasts_group_vs_author_belgium <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,1,0,0,-1,0,0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,1,0,0,-1,0))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,0,1,0,0,-1))) 



test_list <- list(contrasts_indiv_vs_group_belgium, 
contrasts_indiv_vs_author_belgium,
contrasts_group_vs_author_belgium,
contrasts_indiv_vs_group_cambodia, 
contrasts_indiv_vs_author_cambodia,
contrasts_group_vs_author_cambodia,
contrasts_indiv_vs_group_rwanda,
contrasts_indiv_vs_author_rwanda,
contrasts_group_vs_author_rwanda)

name_test_list <- c("contrasts_indiv_vs_group_belgium",
"contrasts_indiv_vs_author_belgium",
"contrasts_group_vs_author_belgium", 
"contrasts_indiv_vs_group_cambodia", 
"contrasts_indiv_vs_author_cambodia",
"contrasts_group_vs_author_cambodia",
"contrasts_indiv_vs_group_rwanda",
"contrasts_indiv_vs_author_rwanda",
"contrasts_group_vs_author_rwanda")
inferential_test(test_list, name_test_list, actual_sd)



##### test between country

# antisocial no
contrasts_belgium_vs_cambodia_indiv <- contrast(estimated_means1,  list(c1 = c(1,-1,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_belgium_vs_rwanda_indiv <- contrast(estimated_means1,    list(c1 = c(1,0,-1,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_cambodia_vs_rwanda_indiv <- contrast(estimated_means1,   list(c1 = c(0,1,-1,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 

contrasts_belgium_vs_cambodia_group <- contrast(estimated_means1,   list(c1 = c(0,0,0,1,-1,0,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_belgium_vs_rwanda_group <- contrast(estimated_means1,     list(c1 = c(0,0,0,1,0,-1,0,0,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_cambodia_vs_rwanda_group <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,1,-1,0,0,0,   0,0,0,0,0,0,0,0,0))) 

contrasts_belgium_vs_cambodia_author <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,1,-1,0,   0,0,0,0,0,0,0,0,0))) 
contrasts_belgium_vs_rwanda_author <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,1,0,-1,   0,0,0,0,0,0,0,0,0))) 
contrasts_cambodia_vs_rwanda_author <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,1,-1,   0,0,0,0,0,0,0,0,0))) 

test_list <- list(
contrasts_belgium_vs_cambodia_indiv,
contrasts_belgium_vs_rwanda_indiv,
contrasts_cambodia_vs_rwanda_indiv,
contrasts_belgium_vs_cambodia_group,
contrasts_belgium_vs_rwanda_group,
contrasts_cambodia_vs_rwanda_group,
contrasts_belgium_vs_cambodia_author,
contrasts_belgium_vs_rwanda_author,
contrasts_cambodia_vs_rwanda_author)


name_test_list <- c(
"contrasts_belgium_vs_cambodia_indiv",
"contrasts_belgium_vs_rwanda_indiv",
"contrasts_cambodia_vs_rwanda_indiv",
"contrasts_belgium_vs_cambodia_group",
"contrasts_belgium_vs_rwanda_group",
"contrasts_cambodia_vs_rwanda_group",
"contrasts_belgium_vs_cambodia_author",
"contrasts_belgium_vs_rwanda_author",
"contrasts_cambodia_vs_rwanda_author")

inferential_test(test_list, name_test_list, actual_sd)

# prosocial yes 

contrasts_belgium_vs_cambodia_indiv <- contrast(estimated_means1,  list(c1 =  c(0,0,0,0,0,0,0,0,0,    1,-1,0,0,0,0,0,0,0))) 
contrasts_belgium_vs_rwanda_indiv <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     1,0,-1,0,0,0,0,0,0))) 
contrasts_cambodia_vs_rwanda_indiv <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,1,-1,0,0,0,0,0,0))) 



contrasts_belgium_vs_cambodia_group <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,1,-1,0,0,0,0))) 
contrasts_belgium_vs_rwanda_group <- contrast(estimated_means1,     list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,1,0,-1,0,0,0))) 
contrasts_cambodia_vs_rwanda_group <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,1,-1,0,0,0))) 

contrasts_belgium_vs_cambodia_author <- contrast(estimated_means1,  list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,0,0,1,-1,0))) 
contrasts_belgium_vs_rwanda_author <- contrast(estimated_means1,    list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,0,0,1,0,-1))) 
contrasts_cambodia_vs_rwanda_author <- contrast(estimated_means1,   list(c1 = c(0,0,0,0,0,0,0,0,0,     0,0,0,0,0,0,0,1,-1))) 


test_list <- list(
contrasts_belgium_vs_cambodia_indiv,
contrasts_belgium_vs_rwanda_indiv,
contrasts_cambodia_vs_rwanda_indiv,
contrasts_belgium_vs_cambodia_group,
contrasts_belgium_vs_rwanda_group,
contrasts_cambodia_vs_rwanda_group,
contrasts_belgium_vs_cambodia_author,
contrasts_belgium_vs_rwanda_author,
contrasts_cambodia_vs_rwanda_author)

inferential_test(test_list, name_test_list, actual_sd)


#Individual comparison
contrasts_indiv_vs_group <- contrast(estimated_means1,  list(c1 = c(1/6,1/6,1/6, -1/6,-1/6,-1/6, 0, 0, 0,  1/6,1/6,1/6, -1/6,-1/6,-1/6, 0, 0, 0))) 
contrasts_indiv_vs_author <- contrast(estimated_means1,  list(c1 = c(1/6,1/6,1/6,  0, 0, 0,-1/6,-1/6,-1/6,  1/6,1/6,1/6, 0, 0, 0,-1/6,-1/6,-1/6))) 
contrasts_group_vs_author <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 1/6,1/6,1/6, -1/6,-1/6,-1/6,   0, 0, 0, 1/6,1/6,1/6, -1/6,-1/6,-1/6))) 


test_list <- list(contrasts_indiv_vs_group,contrasts_indiv_vs_author,contrasts_group_vs_author)
name_test <- c("contrasts_indiv_vs_group","contrasts_indiv_vs_author","contrasts_group_vs_author")
inferential_test(test_list, name_test, actual_sd)

#Individual comparisons within country

contrasts_indiv_vs_group_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, -1/2, 0 , 0, 0, 0, 0,    1/2, 0, 0, -1/2, 0, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(1/2, 0, 0, 0, 0 , 0, -1/2, 0, 0,    1/2, 0, 0, 0, 0, 0, -1/2, 0, 0))) 
contrasts_group_vs_author_belgium <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0 , 0, -1/2, 0, 0,    0, 0, 0, 1/2, 0, 0, -1/2, 0, 0))) 


contrasts_indiv_vs_group_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, -1/2 , 0, 0, 0, 0,    0, 1/2, 0, 0, -1/2, 0, 0, 0, 0))) 
contrasts_indiv_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 1/2, 0, 0, 0 , 0, 0, -1/2, 0,    0, 1/2, 0, 0, 0, 0, 0, -1/2, 0))) 
contrasts_group_vs_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2 , 0, 0, -1/2, 0,    0, 0, 0, 0, 1/2, 0, 0, -1/2, 0))) 

contrasts_indiv_vs_group_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0 , -1/2, 0, 0, 0,    0, 0, 1/2, 0, 0, -1/2, 0, 0, 0))) 
contrasts_indiv_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 1/2, 0, 0 , 0, 0, 0, -1/2,    0, 0, 1/2, 0, 0, 0, 0, 0, -1/2))) 
contrasts_group_vs_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 0, 1/2, 0, 0, -1/2,    0, 0, 0, 0, 0, 1/2, 0, 0, -1/2))) 


test_list <- list(contrasts_indiv_vs_group_belgium,contrasts_indiv_vs_author_belgium,contrasts_group_vs_author_belgium, contrasts_indiv_vs_group_cambodia, contrasts_indiv_vs_author_cambodia,contrasts_group_vs_author_cambodia, contrasts_indiv_vs_group_rwanda, contrasts_indiv_vs_author_rwanda, contrasts_group_vs_author_rwanda)
name_test <- c("contrasts_indiv_vs_group_belgium","contrasts_indiv_vs_author_belgium","contrasts_group_vs_author_belgium", "contrasts_indiv_vs_group_cambodia", "contrasts_indiv_vs_author_cambodia", "contrasts_group_vs_author_cambodia", "contrasts_indiv_vs_group_rwanda", "contrasts_indiv_vs_author_rwanda", "contrasts_group_vs_author_rwanda")
inferential_test(test_list, name_test, actual_sd)


# test within Influence BETWEEN Country

contrasts_indiv_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(1/2, -1/2, 0, 0, 0, 0, 0, 0, 0,     1/2, -1/2, 0, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(1/2, 0, -1/2, 0, 0, 0, 0, 0, 0,     1/2, 0, -1/2, 0, 0, 0, 0, 0, 0))) 
contrasts_indiv_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 1/2, -1/2, 0, 0, 0, 0, 0, 0,     0, 1/2, -1/2, 0, 0, 0, 0, 0, 0))) 

contrasts_group_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, -1/2, 0, 0, 0, 0,     0, 0, 0, 1/2, -1/2, 0, 0, 0, 0))) 
contrasts_group_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 1/2, 0, -1/2, 0, 0, 0,     0, 0, 0, 1/2, 0, -1/2, 0, 0, 0))) 
contrasts_group_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c(0, 0, 0, 0, 1/2, -1/2, 0, 0, 0,     0, 0, 0, 0, 1/2, -1/2, 0, 0, 0))) 

contrasts_author_belgium_vs_cambodia <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, -1/2, 0,      0, 0, 0, 0, 0, 0, 1/2, -1/2, 0))) 
contrasts_author_belgium_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 1/2, 0, -1/2,      0, 0, 0, 0, 0, 0, 1/2, 0, -1/2))) 
contrasts_author_cambodia_vs_rwanda <- contrast(estimated_means1,  list(c1 = c( 0, 0, 0, 0, 0, 0, 0, 1/2, -1/2,      0, 0, 0, 0, 0, 0, 0, 1/2, -1/2))) 

test_list <- list(contrasts_indiv_belgium_vs_cambodia,
contrasts_indiv_belgium_vs_rwanda,
contrasts_indiv_cambodia_vs_rwanda,
contrasts_group_belgium_vs_cambodia,
contrasts_group_belgium_vs_rwanda,
contrasts_group_cambodia_vs_rwanda,
contrasts_author_belgium_vs_cambodia,
contrasts_author_belgium_vs_rwanda,
contrasts_author_cambodia_vs_rwanda)
name_test <- c("contrasts_indiv_belgium_vs_cambodia",
"contrasts_indiv_belgium_vs_rwanda",
"contrasts_indiv_cambodia_vs_rwanda",
"contrasts_group_belgium_vs_cambodia",
"contrasts_group_belgium_vs_rwanda",
"contrasts_group_cambodia_vs_rwanda",
"contrasts_author_belgium_vs_cambodia",
"contrasts_author_belgium_vs_rwanda",
"contrasts_author_cambodia_vs_rwanda")
inferential_test(test_list, name_test, actual_sd)













































#############################################################################
#####                                                             ###########
##### QUESTION 4 : What they report Questionnaire                 ###########
#####                                                             ###########
#############################################################################

prior_Qst <- get_prior(Score ~ Influence + Influence : Country + (Influence|Participant) + (1|Items), data=df_questionnaire, family=cumulative(probit))


prior_Qst <- prior("normal(0, 1)", class="b") + 
prior("normal(-0.967, .5", class="Intercept", coef=1) +   # 
prior("normal(-0.431, .5", class="Intercept", coef=2) +   # 
prior("normal(0, .5", class="Intercept", coef=3) +   # 
prior("normal(0.431, .5", class="Intercept", coef=4) +   # 
prior("normal(0.967, .5", class="Intercept", coef=5) +   # 
prior("exponential(1)", class = "sd")   

mQst <- brm(Score ~ Influence * Country  + (Influence|Participant) + (1|Items),  family=cumulative(logit),  data=df_questionnaire, prior = prior_Qst,  iter=2000, warmup = 1000, chains=1,  backend = "cmdstanr", threads=threading(parallel::detectCores()), core=parallel::detectCores())
fixef(mQst)["Influence2M1", -2]

mQst 
pp_check(mQst)
a <- emmeans(mQst, ~"Influence:Country", type="response")
pairs(a)
save(mQst, file="mQst.Rdata")
ranef(mQst)
Author_Group <-data.frame(ranef(mQst, pars="Influence2M1"))[,1]


outlier(Author_Group)


data.frame(ranef(mQst))[which(is.na(outlier(Author_Group))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Author_Group))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Author_Indiv))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Yes_No))),]
data.frame(ranef(mFMtheta))[which(is.na(outlier(Forced_free))),]
mQ <- mQst

load("mQst.Rdata")

mQst_clean <- update(mQst,iter = 12000, chains=4, warmup=2000, threads=threading(parallel::detectCores()), core=parallel::detectCores(), backend = "cmdstanr")
save(mQst_clean, file="mQst_clean.Rdata")

load("mQst_clean.Rdata")

# Generate the summary of your model
summary_output <- summary(mQst_clean)
prior_output <- mQst_clean$prior

# Capture the summary output as text
summary_text <- c(capture.output(summary_output),capture.output(prior_output))


# Save the summary as a PDF
tiff("mQst_clean_summary.tiff", width = 16, height = 24, units = "in", res = 300)  
plot.new()
text(0, 1, paste(summary_text, collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.8, font=2)  # Reduce text size with cex
dev.off()

estimated_means1 <- emmeans(mQst_clean, ~ Country*Influence)

actual_sd <- 1

confint(estimated_means1,level=.89, type=)

#Group vs authority

contrasts_group_author <- contrast(estimated_means1,  list(c1 = c(1/3,1/3,1/3, -1/3,-1/3,-1/3))) 

inferential_test(list(contrasts_group_author), "group_author", actual_sd)

#Group vs authority within Country

contrasts_group_author_belgium <- contrast(estimated_means1,  list(c1 = c(1,0,0, -1,0,0))) 
contrasts_group_author_cambodia <- contrast(estimated_means1,  list(c1 = c(0,1,0, 0,-1,0))) 
contrasts_group_author_rwanda <- contrast(estimated_means1,  list(c1 = c(0,0,1, 0,0,-1))) 

test_list <- list(contrasts_group_author_belgium,contrasts_group_author_cambodia,contrasts_group_author_rwanda)
name_test <- c("contrasts_group_author_belgium","contrasts_group_author_cambodia","contrasts_group_author_rwanda")
inferential_test(test_list, name_test, actual_sd)

#Group between Country

contrasts_group_belgium_cambodia <- contrast(estimated_means1,  list(c1 = c(1,-1,0, 0,0,0))) 
contrasts_group_rwanda_belgium <- contrast(estimated_means1,  list(c1 = c(1,0,-1, 0,0,0))) 
contrasts_group_cambodia_rwanda <- contrast(estimated_means1,  list(c1 = c(0,1,-1, 0,0,0))) 

test_list <- list(contrasts_group_belgium_cambodia,contrasts_group_rwanda_belgium,contrasts_group_cambodia_rwanda)
name_test <- c("contrasts_group_belgium_cambodia","contrasts_group_rwanda_belgium","contrasts_group_cambodia_rwanda")
inferential_test(test_list, name_test, actual_sd)

#Authority between Country

contrasts_authority_belgium_cambodia <- contrast(estimated_means1,  list(c1 = c(0,0,0, 1,-1,0))) 
contrasts_authority_rwanda_belgium <- contrast(estimated_means1,  list(c1 = c(0,0,0, 1,0,-1))) 
contrasts_authority_cambodia_rwanda <- contrast(estimated_means1,  list(c1 = c(0,0,0, 0,1,-1))) 

test_list <- list(contrasts_authority_belgium_cambodia,contrasts_authority_rwanda_belgium,contrasts_authority_cambodia_rwanda)
name_test <- c("contrasts_authority_belgium_cambodia","contrasts_authority_rwanda_belgium","contrasts_authority_cambodia_rwanda")
inferential_test(test_list, name_test, actual_sd)

















































