library(lmerTest); library(brms); library(emmeans); library(ggplot2); library(httpgd); library(dplyr); library(MASS); library(tidybayes)
setwd(dir= "C:/Users/mfbpe/Desktop/DATA/2023_Social_Influence/results/") 


load("mFMtheta_clean.Rdata")
load("mSelection_clean.Rdata")
load("mRT_clean.Rdata")
load("mQst_clean.Rdata")

# https://cran.r-project.org/web/packages/scico/index.html  check colorblind palette

windows()


df_selection <- mSelection_clean$data
violin_box_plot <- ggplot(df_selection, aes(answer, Selection, fill=answer))+
  geom_line(aes(answer, Selection, group=Participant),color='#4d4d4d',lwd=1,alpha=.2, show.legend=F, inherit.aes = F)+
  # geom_violin(lwd=1,trim=T)+
  theme(
   text=element_text(size=20,face='bold',family='Times New Roman')
  #  panel.grid.major.y =element_line(linewidth=.5),
  #  panel.grid.minor.y =element_line(linewidth=.5)
  )
violin_box_plot

mQst_clean$data$Score <- as.integer(mQst_clean$data$Score)

b <- mQst_clean$data %>%
    group_by(Participant,Influence, Country)%>%
    summarise(Score = mean(Score,na.rm=T))

windows()

p <- ggplot(b, aes(Influence, Score, fill=Influence))+
  geom_jitter(aes(color=Influence),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  ) + 
  scale_y_continuous(breaks=seq(6), labels=seq(0,5,1))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Group", "Authority"), values=c("#B664EC", "#001683"))+
  scale_color_manual(labels = c("Group", "Authority"), values=c("#B664EC", "#001683"))+
  stat_slab(aes(y=Score, Fill=Influence), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill=rep(rep(c( "#B664EC", "#001683"),each=501),3)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=12,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.85,.10),
    legend.direction="horizontal"

  )+
  facet_grid(~Country)

tiff("plot.tiff", width = 10, height = 5, units = 'in', res = 300, compression = "lzw", family = "CMYK")

print(p)
dev.off()
b <- mRT_clean$data %>%
    group_by(Participant,Influence, Country)%>%
    summarise(RT = mean(RT,na.rm=T))

windows()

ggplot(b, aes(Influence, RT, fill=Influence))+
  geom_jitter(aes(color=Influence),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  # scale_y_continuous(breaks=seq(40,100, 10))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Individual", "Group", "Authority"), values=c("#d70966", "#B664EC", "#001683"))+
  scale_color_manual(labels = c("Individual", "Group", "Authority"), values=c("#d70966", "#B664EC", "#001683"))+
  stat_slab(aes(y=RT, Fill=Influence), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill=rep(rep(c("#d70966", "#B664EC", "#001683"),each=501),3)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=12,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.93,.85)
  )+
  facet_grid(~Country)

b <- mRT_clean$data %>%
    group_by(Participant,answer)%>%
    summarise(RT = mean(RT,na.rm=T))


ggplot(b, aes(answer, RT, fill=answer))+
  geom_jitter(aes(color=answer),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  # scale_y_continuous(breaks=seq(40,100, 10))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Antisocial","Prosocial"), values=c("#d20f0f", "#0bcf39"))+
  scale_color_manual(labels = c("Antisocial","Prosocial"), values=c("#d20f0f", "#0bcf39"))+
  stat_slab(aes(y=RT, Fill=answer), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill=rep(rep(c("#d20f0f","#0bcf39"),each = 501),1)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=20,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.9,.5)
  )


b <- mSelection_clean$data %>%
    group_by(Participant,Influence, Country)%>%
    summarise(Selection = mean(Selection,na.rm=T))

windows()

ggplot(b, aes(Influence, Selection, fill=Influence))+
  geom_jitter(aes(color=Influence),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  # scale_y_continuous(breaks=seq(40,100, 10))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Individual", "Group", "Authority"), values=c("#d70966", "#B664EC", "#001683"))+
  scale_color_manual(labels = c("Individual", "Group", "Authority"), values=c("#d70966", "#B664EC", "#001683"))+
  stat_slab(aes(y=Selection, Fill=Influence), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill=rep(rep(c("#d70966", "#B664EC", "#001683"),each=501),3)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=12,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.93,.85)
  )+
  facet_grid(~Country)



load("mSelection_clean.Rdata")


b <- mSelection_clean$data %>%
    group_by(Participant,answer)%>%
    summarise(Selection = mean(Selection,na.rm=T))


ggplot(b, aes(answer, Selection, fill=answer))+
  geom_jitter(aes(color=answer),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  # scale_y_continuous(breaks=seq(40,100, 10))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Antisocial","Prosocial"), values=c("#d20f0f", "#0bcf39"))+
  scale_color_manual(labels = c("Antisocial","Prosocial"), values=c("#d20f0f", "#0bcf39"))+
  stat_slab(aes(y=Selection, Fill=answer), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill=rep(rep(c("#d20f0f","#0bcf39"),each = 501),1)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=20,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.9,.5)
  )


load("mFMtheta_clean.Rdata")




a<-data.frame(emmeans(mFMtheta_clean,"Influence"))
a <- mFMtheta_clean$data
b <- mFMtheta_clean$data %>%
    group_by(Participant,Influence, Country)%>%
    summarise(FMtheta = mean(FMtheta,na.rm=T))

windows()



ggplot(b, aes(Influence, FMtheta, fill=Influence))+
  geom_jitter(aes(color=Influence),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  # scale_y_continuous(breaks=seq(40,100, 10))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Individual", "Group", "Authority"), values=c("#d70966", "#B664EC", "#001683"))+
  scale_color_manual(labels = c("Individual", "Group", "Authority"), values=c("#d70966", "#B664EC", "#001683"))+
  stat_slab(aes(y=FMtheta, Fill=Influence), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill= rep(rep(c("#d70966", "#B664EC", "#001683"),each=501),3)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=20,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.58,.85)
  )+  facet_grid(~Country)


b <- mFMtheta_clean$data %>%
    group_by(Participant,answer, Country)%>%
    summarise(FMtheta = mean(FMtheta,na.rm=T))


ggplot(b, aes(answer, FMtheta, fill=answer))+
  geom_jitter(aes(color=answer),width=.2, size=2)+
  geom_boxplot(
    alpha=.8,
    width = .9,
    size = 1.5, outlier.shape = NA,
    position=position_dodge(),
  )  +
  # scale_y_continuous(breaks=seq(40,100, 10))+   
  scale_x_discrete(breaks=NULL)+
  labs(
    x = NULL,
    y = NULL,
  ) +
  scale_fill_manual(labels = c("Antisocial","Prosocial"), values=c("#d20f0f", "#0bcf39"))+
  scale_color_manual(labels = c("Antisocial","Prosocial"), values=c("#d20f0f", "#0bcf39"))+
  stat_slab(aes(y=FMtheta, Fill=answer), 
  inherit.aes = F, adjust = 1, width = 3, linewidth = 1.5, alpha=.8, color="black", justification=-1.5, fill=rep(rep(c("#d20f0f","#0bcf39"),each = 501),3)) +
  theme(
   axis.text=element_text(size=30,face='bold'),
   legend.title = element_blank(),
   legend.text = element_text(size=20,face='bold'),
   legend.background = element_rect(colour = NA, fill = NA),
   legend.key = element_rect(colour = NA, fill = NA),
   legend.key.size = unit(2, 'cm'),
   legend.position = c(.83,.1),
   legend.direction="horizontal"
  )+
  facet_grid(~Country)






ggplot(b, aes(Influence, FMtheta, color=Influence, fill=Influence))+
    # stat_slab(trim=F,  adjust=1, width= .4,justification = -0,fill = NA)
    stat_halfeye(width=.1, position=position_dodge())

ggplot(b, aes(FMtheta, fill=Influence))+
    stat_slab(aes(fill=Influence),adjust=1,justification=-.2,size=2,alpha = .5) +
    geom_boxplot(linewidth=1,aes(fill=Influence),width=.15, position=position_dodge(.2),outliers=F)+
  scale_y_continuous(breaks = NULL)



ggplot(b, aes(FMtheta, color=Influence, fill=Influence))+
    stat_slab(trim=F,  adjust=1, width= .4,justification = -0,fill = NA)
    # stat_halfeye(width=3, position=position_dodge(),aes(fill = after_stat(level)))+
    #   scale_fill_brewer(na.translate = FALSE) +
    scale_y_continuous(limits = c(-.5,.65), breaks=NULL)


    # geom_boxplot(outliers =F, width=.4, inherit.aes = T, size=1)

windows()

ggplot(b, aes(FMtheta, color=Influence, fill=Influence))+
    stat_slab(size=2,alpha = .7,justification=-.15,linewidth=1.5) +
  scale_color_manual(values=rep("black",3))+
  # stat_pointinterval(stroke=2.5, linewidth=rep(c(20,5),3),.width=c(.66,.89), position = position_dodge(width = .5, preserve = "single")) +
  geom_boxplot(color="black",alpha = .7,linewidth=1,aes(fill=Influence),width=.15, position=position_dodge(.2),outliers=F)+
   labs(
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values=c("#f72585", "#B664EC", "#001683"))+
  scale_y_continuous(breaks = NULL)+ theme(
   text=element_text(size=30,face='bold'),
   legend.key.size = unit(2, 'cm'))
