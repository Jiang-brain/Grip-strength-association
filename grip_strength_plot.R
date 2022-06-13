## assocaition between strength and 30 cognition
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/strength_cognition_result.mat')
mydata$result[,2]=p.adjust(mydata$result[,2],method='fdr',n=length(mydata$result[,2]))
significance=-log10(mydata$result[,2])
result=data.frame(values=significance,
                  cognition_name=c('Fluid intelligence','Prospective memory','Reaction time','Numeric memory','Trail making: part A','Trail making: part B',
                                   'Symbol-digit matching: attempted','Symbol-digit matching: corrected','Matrix pattern completion','Tower rearraging',
                                   'Verbal declarative memory','Pairs-matching 1: error made','Pairs-matching 2: error made','Pairs-matching 3: error made',
                                   'Pairs-matching 1: time','Pairs-matching 2: time','Pairs-matching 3: time','Neuroticism','Depression symptom','Anxiety symptom',
                                   'Happiness: general','Happiness with own health','Belief that life is meaningful','Satisfication: health','Satisfication: family relationship',
                                   'Satisfaction: friendship','Satisfaction: financial situation','Satisfaction: job/work','Happiness','CIDI-depression'),
                  category=c(rep('Cognition function',17),rep('Depression/Anxiety',3),rep('Subjective well-being',3),rep('Mental health',6),'Depression/Anxiety'))

library(ggpubr)
#behavior_order<-order(result$values)
ggdotchart(result, x = 'cognition_name', y = 'values',
           color = 'category', # ????????????
           #palette = c("#00AFBB", "#E7B800", "#FC4E07",'darkgreen'),
           sorting = "descending", 
           #order=result$cognition_name[behavior_order],
           add = "segments",                             # ????????????
           add.params = list(color = "grey", size = 2.5),#??????????????????
           rotate = TRUE,                                
           dot.size = 4.7,                                 # ??????????????????
           label = round(result$values,1),                       # ??????label
           font.label = list(color = "white", size = 6.8, vjust = 0.5),               # ??????label??????
           ggtheme = theme_test(),                        # ????????????
           ylab="-log(p-value)",xlab=""
)


## association between strength and behavior for males and females
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/association_strength_behavior_female.mat')
mydata$result[,2]=p.adjust(mydata$result[,2],method='fdr',n=length(mydata$result[,2]))
significance=-log10(mydata$result[,2])
result=data.frame(values=significance,
                  cognition_name=c('Fluid intelligence','Prospective memory','Reaction time','Numeric memory','Trail making: part A','Trail making: part B',
                                   'Symbol-digit matching: attempted','Symbol-digit matching: corrected','Matrix pattern completion','Tower rearraging',
                                   'Verbal declarative memory','Pairs-matching 1: error made','Pairs-matching 2: error made','Pairs-matching 3: error made',
                                   'Pairs-matching 1: time','Pairs-matching 2: time','Pairs-matching 3: time','Neuroticism','Depression symptom','Anxiety symptom',
                                   'Happiness: general','Happiness with own health','Belief that life is meaningful','Satisfication: health','Satisfication: family relationship',
                                   'Satisfaction: friendship','Satisfaction: financial situation','Satisfaction: job/work','Happiness','CIDI-depression'),
                  category=c(rep('Cognition function',17),rep('Depression/Anxiety',3),rep('Subjective well-being',3),rep('Mental health',6),'Depression/Anxiety'))

library(ggpubr)
ggdotchart(result, x = 'cognition_name', y = 'values',
           color = 'category', # ????????????
           #palette = c("#00AFBB", "#E7B800", "#FC4E07",'darkgreen'),
           sorting = "descending",
           add = "segments",                             # ????????????
           add.params = list(color = "grey", size = 2.5),#??????????????????
           rotate = TRUE,                                
           dot.size = 4.7,                                 # ??????????????????
           label = round(result$values,1),                       # ??????label
           font.label = list(color = "white", size = 6.8, vjust = 0.5),               # ??????label??????
           ggtheme = theme_test(),                        # ????????????
           ylab="-log(p-value)",xlab=""
)



roi<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/139_roi_name.csv',header=T)
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/correlation_strength_139_gm.mat')
result<- data.frame(roi_name=roi$ROIs,t_value=mydata$result.height[,4],
                    category=c(rep('Cortical',96),rep('Subcortical',15),rep('Cerebellum',28)))
library(ggpubr)
ggdotchart(result, x = 'roi_name', y = 't_value',
           color = 'category', # ????????????
           palette = c("#E7B800", "#E7B800", "#E7B800"),
           sorting = "descending",                  
           add = "segments",                             # ????????????
           add.params = list(size = 0.5),#??????????????????
           #rotate = TRUE,                                
           dot.size = 1.0,                                 # ??????????????????
           ggtheme = theme_test(),                        # ????????????
           ylab="T-values",xlab=""
)+rotate_x_text(45)


roi<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/139_roi_name.csv',header=T)
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/cognition_gm_result.mat')
mydata=mydata$result.height.icv[[4]]
result<- data.frame(roi_name=roi$ROIs,t_value=mydata[,7],
                    category=c(rep('Cortical',96),rep('Subcortical',15),rep('Cerebellum',28)))
library(ggpubr)
ggdotchart(result, x = 'roi_name', y = 't_value',
           color = 'category', # ????????????
           palette = c("#E7B800", "#E7B800", "#E7B800"),
           sorting = "descending",                  
           add = "segments",                             # ????????????
           add.params = list(size = 0.5),#??????????????????
           #rotate = TRUE,                                
           dot.size = 1.2,                                 # ??????????????????
           ggtheme = theme_test(),                        # ????????????
           ylab="T-values",xlab=""
)

#### write grip strength-associated brain regions to Table
roi<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/139_roi_name.csv',header=T)
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/correlation_strength_139_gm.mat')
result<- data.frame(roi_name=roi$ROIs,t_value=mydata$result.icv.height[,4],p_value=mydata$result.icv.height[,2],
                    n=mydata$result.icv.height[,7],r_value=mydata$result.icv.height[,1],
                    lower=mydata$result.icv.height[,5],upper=mydata$result.icv.height[,6])
result$p_value=p.adjust(result$p_value,method='fdr',n=length(result$p_value))
index=which(result$p_value<0.01)
result=result[index,]
result$t_value=round(result$t_value,2)
result$r_value=round(result$r_value,3)
result$lower=round(result$lower,3)
result$upper=round(result$upper,3)
result<-result[order(abs(result$t_value),decreasing = TRUE),]
write.csv(result,'/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/significant_correlated_roi_top_30.csv')

### write grip strength-associated brain regions to Table for males and females
roi<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/139_roi_name.csv',header=T)
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/association_gmv_strength_female.mat')
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/association_gmv_strength_male.mat')

result<- data.frame(roi_name=roi$ROIs,t_value=mydata$result[,4],p_value=mydata$result[,2],
                    n=mydata$result[,7],r_value=mydata$result[,1],
                    lower=mydata$result[,5],upper=mydata$result[,6])
result$p_value=p.adjust(result$p_value,method='fdr',n=length(result$p_value))
index=which(result$p_value<0.01)
result=result[index,]
result$t_value=round(result$t_value,2)
result$r_value=round(result$r_value,3)
result$lower=round(result$lower,3)
result$upper=round(result$upper,3)
result<-result[order(abs(result$t_value),decreasing = TRUE),]



## correlation between two T-maps, with or without ICV as a covariate
mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/correlation_strength_139_gm.mat')
result<- data.frame(roi_name=c(seq(1:139)),t_value_no_control=mydata$result.icv.height[,4],
                    t_value_control=mydata$result.height.icv.icv2[,4],
                    category=c(rep('Cortical',96),rep('Subcortical',15),rep('Cerebellum',28)))
ggplot(result,aes(x=t_value_no_control,y=t_value_control,label = roi_name))+
  geom_point(aes(colour=as.factor(category)),size=3.8,shape=18)+geom_smooth(method='lm',alpha=0.25)+
  geom_text(size=2.5,hjust=-0.5,vjust=0.5,color='gray60')+theme_classic()+
  scale_color_manual(values=c("#4DACB9", "#E0BA3E", "#E95B2B"))


## T-map correlation
library(ggpubr)
mydata<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/t_map_correlation.csv',header=T)
cognition_name=c('Fluid intelligence','Prospective memory','Reaction time','Numeric memory','Trail making: part A','Trail making: part B',
                 'Symbol-digit matching: attempted','Symbol-digit matching: corrected','Matrix pattern completion','Tower rearraging',
                 'Verbal declarative memory','Pairs-matching 1: error made','Pairs-matching 2: error made','Pairs-matching 3: error made',
                 'Pairs-matching 1: time','Pairs-matching 2: time','Pairs-matching 3: time','Neuroticism','Depression symptom','Anxiety symptom',
                 'Happiness: general','Happiness with own health','Belief that life is meaningful','Satisfication: health','Satisfication: family relationship',
                 'Satisfaction: friendship','Satisfaction: financial situation','Satisfaction: job/work','Happiness','CIDI-depression')
result=data.frame(t_correlation=(mydata$t_map_no_control_r),t_p=mydata$t_map_no_control_p,cognition=cognition_name)
result=data.frame(t_correlation=(mydata$t_map_control_r),t_p=mydata$t_map_control_p,cognition=cognition_name)
result=data.frame(t_correlation=(mydata$t_map_control_no_control_r),t_p=mydata$t_map_control_no_control_p,cognition=cognition_name)
result<-result[order(result$t_correlation),]

# Spectral for negative-positive r, palette(limits=c(-1,1)) direction=1
ggplot(data=result, aes(x=cognition, y=t_correlation,fill=t_correlation)) +
  geom_bar(stat="identity",width=0.7,alpha=0.7)+scale_fill_distiller(palette = "Spectral")+
  scale_x_discrete(limits=rev(result$cognition))+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6,size = 9, hjust = 1,colour="black"))

scale_fill_continuous(low="blue", high="red")
scale_fill_gradient(low = "yellow", high = "red")
scale_fill_viridis_c(option="trubo")

ggdotchart(result, x = 'cognition', y = 't_correlation',
           sorting = "ascending",                  
           add = "segments",                             # ????????????
           add.params = list(color = "grey", size = 3.5),#??????????????????
           rotate = TRUE,                                
           dot.size = 5.5,                                 # ??????????????????
           label = round(result$t_correlation,2),                       # ??????label
           font.label = list(color = "white", size = 7.2, vjust = 0.5),               # ??????label??????
           ggtheme = theme_test(),                        # ????????????
           ylab="T-map correlation",xlab="")


##
memory.limit(90000)
library(mediation)
#the first component, the second component
#mydata <- R.matlab::readMat('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/test1.mat')
mydata<-R.matlab::readMat('C:/Users/rj463/OneDrive/rtjiang/G/working_data/2022-UKB/mediation_all_features_removed_outliers.mat')
a<-matrix(nrow=30,ncol=24)
for (i in 1:30){
  cognition_score<-mydata$cognition[,i]
  
  strength<-mydata$strength
  brains<-mydata$mean.gmv[,1]
  covariate_variable=mydata$covariate.variable[,c(1,2,3,4,5,6,7,8)]
  
  combined_data<- data.frame(stren=strength,brain=brains,cog=cognition_score,covariates=covariate_variable)
  number_na<-rowSums(is.na(combined_data))
  index_nonNA<-which(number_na==0)
  combined_data<-combined_data[index_nonNA,]
  rm(strength)
  rm(brains)
  rm(cognition_score)
  rm(covariate_variable)
  rm(index_nonNA)
  
  
  combined_data$covariates.2=as.factor(combined_data$covariates.2)
  combined_data$covariates.3=as.factor(combined_data$covariates.3)
  combined_data$covariates.5=as.factor(combined_data$covariates.5)
  
  b <- lm( brain~ stren+covariates.1+covariates.2+covariates.3+covariates.4+covariates.5+covariates.6+covariates.7+covariates.8,data=combined_data)
  c <- lm(cog~ brain+stren+covariates.1+covariates.2+covariates.3+covariates.4+covariates.5+covariates.6+covariates.7+covariates.8, data=combined_data)
  contcont <- mediate(b, c, sims=5000, treat="stren", mediator="brain")
  a[i,1]=contcont$d0
  a[i,2]=contcont$d0.ci[[1]]
  a[i,3]=contcont$d0.ci[[2]]
  a[i,4]=contcont$d0.p#significance of indirect effect
  a[i,5]=contcont$tau.coef
  a[i,6]=contcont$tau.ci[[1]]
  a[i,7]=contcont$tau.ci[[2]]
  a[i,8]=contcont$tau.p
  a[i,9]=contcont$z0
  a[i,10]=contcont$z0.ci[[1]]
  a[i,11]=contcont$z0.ci[[2]]
  a[i,12]=contcont$z0.p
  b_ci=confint(b, 'stren', level=0.95)
  a[i,13]=b$coefficients[2]
  a[i,14]=b_ci[[1]]
  a[i,15]=b_ci[[2]]
  a[i,16]=summary(b)$coefficients[2,4]
  
  c_ci=confint(c, 'brain', level=0.95)
  a[i,17]=c$coefficients[2]
  a[i,18]=c_ci[[1]]
  a[i,19]=c_ci[[2]]
  a[i,20]=summary(c)$coefficients[2,4]
  
  a[i,21]=contcont$n0
  a[i,22]=contcont$n0.ci[[1]]
  a[i,23]=contcont$n0.ci[[2]]
  a[i,24]=contcont$n0.p
  rm(b)
  rm(c)
  print(c(i,a[i,1],a[i,2],a[i,3]))
}

###################################female-specific mediation
mydata<-R.matlab::readMat('C:/Users/rj463/OneDrive/rtjiang/G/working_data/2022-UKB/mediation_all_features_removed_outliers.mat')
a<-matrix(nrow=30,ncol=24)
for (i in 1:30){
  cognition_score<-mydata$cognition[,i]
  
  strength<-mydata$strength
  brains<-mydata$mean.gmv.female[,1]
  covariate_variable=mydata$covariate.variable[,c(1,2,3,4,5,6,7,8)]
  
  combined_data<- data.frame(stren=strength,brain=brains,cog=cognition_score,covariates=covariate_variable)
  number_na<-rowSums(is.na(combined_data))
  index_nonNA<-which(number_na==0)
  combined_data<-combined_data[index_nonNA,]
  rm(strength)
  rm(brains)
  rm(cognition_score)
  rm(covariate_variable)
  rm(index_nonNA)
  
  female_index=which(combined_data$covariates.2==0)
  combined_data=combined_data[female_index,]
  
  #combined_data$covariates.2=as.factor(combined_data$covariates.2)
  combined_data$covariates.3=as.factor(combined_data$covariates.3)
  combined_data$covariates.5=as.factor(combined_data$covariates.5)
  
  b <- lm( brain~ stren+covariates.1+covariates.3+covariates.4+covariates.5+covariates.6+covariates.7+covariates.8,data=combined_data)
  c <- lm(cog~ brain+stren+covariates.1+covariates.3+covariates.4+covariates.5+covariates.6+covariates.7+covariates.8, data=combined_data)
  contcont <- mediate(b, c, sims=5000, treat="stren", mediator="brain")
  a[i,1]=contcont$d0
  a[i,2]=contcont$d0.ci[[1]]
  a[i,3]=contcont$d0.ci[[2]]
  a[i,4]=contcont$d0.p#significance of indirect effect
  a[i,5]=contcont$tau.coef
  a[i,6]=contcont$tau.ci[[1]]
  a[i,7]=contcont$tau.ci[[2]]
  a[i,8]=contcont$tau.p
  a[i,9]=contcont$z0
  a[i,10]=contcont$z0.ci[[1]]
  a[i,11]=contcont$z0.ci[[2]]
  a[i,12]=contcont$z0.p
  b_ci=confint(b, 'stren', level=0.95)
  a[i,13]=b$coefficients[2]
  a[i,14]=b_ci[[1]]
  a[i,15]=b_ci[[2]]
  a[i,16]=summary(b)$coefficients[2,4]
  
  c_ci=confint(c, 'brain', level=0.95)
  a[i,17]=c$coefficients[2]
  a[i,18]=c_ci[[1]]
  a[i,19]=c_ci[[2]]
  a[i,20]=summary(c)$coefficients[2,4]
  
  a[i,21]=contcont$n0
  a[i,22]=contcont$n0.ci[[1]]
  a[i,23]=contcont$n0.ci[[2]]
  a[i,24]=contcont$n0.p
  rm(b)
  rm(c)
  print(c(i,a[i,1],a[i,2],a[i,3]))
}
write.csv(a,'C:/Users/rj463/OneDrive/rtjiang/G/working_data/2022-UKB/datas/mediation_female_mean_gmv_female.csv')
rm(list=ls())

## males mediation
mydata<-R.matlab::readMat('C:/Users/rj463/OneDrive/rtjiang/G/working_data/2022-UKB/mediation_all_features_removed_outliers.mat')
a<-matrix(nrow=30,ncol=24)
for (i in 1:30){
  cognition_score<-mydata$cognition[,i]
  
  strength<-mydata$strength
  brains<-mydata$mean.gmv.male[,1]
  covariate_variable=mydata$covariate.variable[,c(1,2,3,4,5,6,7,8)]
  
  combined_data<- data.frame(stren=strength,brain=brains,cog=cognition_score,covariates=covariate_variable)
  number_na<-rowSums(is.na(combined_data))
  index_nonNA<-which(number_na==0)
  combined_data<-combined_data[index_nonNA,]
  rm(strength)
  rm(brains)
  rm(cognition_score)
  rm(covariate_variable)
  rm(index_nonNA)
  
  female_index=which(combined_data$covariates.2==1)
  combined_data=combined_data[female_index,]
  
  #combined_data$covariates.2=as.factor(combined_data$covariates.2)
  combined_data$covariates.3=as.factor(combined_data$covariates.3)
  combined_data$covariates.5=as.factor(combined_data$covariates.5)
  
  b <- lm( brain~ stren+covariates.1+covariates.3+covariates.4+covariates.5+covariates.6+covariates.7+covariates.8,data=combined_data)
  c <- lm(cog~ brain+stren+covariates.1+covariates.3+covariates.4+covariates.5+covariates.6+covariates.7+covariates.8, data=combined_data)
  contcont <- mediate(b, c, sims=5000, treat="stren", mediator="brain")
  a[i,1]=contcont$d0
  a[i,2]=contcont$d0.ci[[1]]
  a[i,3]=contcont$d0.ci[[2]]
  a[i,4]=contcont$d0.p#significance of indirect effect
  a[i,5]=contcont$tau.coef
  a[i,6]=contcont$tau.ci[[1]]
  a[i,7]=contcont$tau.ci[[2]]
  a[i,8]=contcont$tau.p
  a[i,9]=contcont$z0
  a[i,10]=contcont$z0.ci[[1]]
  a[i,11]=contcont$z0.ci[[2]]
  a[i,12]=contcont$z0.p
  b_ci=confint(b, 'stren', level=0.95)
  a[i,13]=b$coefficients[2]
  a[i,14]=b_ci[[1]]
  a[i,15]=b_ci[[2]]
  a[i,16]=summary(b)$coefficients[2,4]
  
  c_ci=confint(c, 'brain', level=0.95)
  a[i,17]=c$coefficients[2]
  a[i,18]=c_ci[[1]]
  a[i,19]=c_ci[[2]]
  a[i,20]=summary(c)$coefficients[2,4]
  
  a[i,21]=contcont$n0
  a[i,22]=contcont$n0.ci[[1]]
  a[i,23]=contcont$n0.ci[[2]]
  a[i,24]=contcont$n0.p
  rm(b)
  rm(c)
  print(c(i,a[i,1],a[i,2],a[i,3]))
}
write.csv(a,'C:/Users/rj463/OneDrive/rtjiang/G/working_data/2022-UKB/datas/mediation_male_mean_gmv_male.csv')


## display mediation results
mydata<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/mediation_mean_gmv_06_10.csv',header=T)
mydata<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/mediation_male_mean_gmv_male.csv',header=T)
mydata<- read.csv('/Users/andyjiang/OneDrive/rtjiang/G/working_data/2022-UKB/datas/mediation_female_mean_gmv_female.csv',header=T)

mydata<-mydata[,c(22,23,24,25,26)]

cognition_name=c('Fluid intelligence','Prospective memory','Reaction time','Numeric memory','Trail making: part A','Trail making: part B',
                 'Symbol-digit matching: attempted','Symbol-digit matching: corrected','Matrix pattern completion','Tower rearraging',
                 'Verbal declarative memory','Pairs-matching 1: error made','Pairs-matching 2: error made','Pairs-matching 3: error made',
                 'Pairs-matching 1: time','Pairs-matching 2: time','Pairs-matching 3: time','Neuroticism','Depression symptom','Anxiety symptom',
                 'Happiness: general','Happiness with own health','Belief that life is meaningful','Satisfication: health','Satisfication: family relationship',
                 'Satisfaction: friendship','Satisfaction: financial situation','Satisfaction: job/work','Happiness','CIDI-depression')
base_data <- tibble(mean  = mydata$V21+0.2, lower = mydata$V22+0.2,upper = mydata$V23+0.2,
                    study = cognition_name,mediated_value = as.character(mydata$V21),
                    N=as.factor(mydata$N),p_value=mydata$V24)

base_data=base_data[-c(1,6,17),]## all_subjects:remove non-significantly correlated behaviors
base_data=base_data[-c(4,6,7,8,10,11,13,16,17,20),]## males:remove non-significantly correlated behaviors
base_data=base_data[-c(14),]## females:remove non-significantly correlated behaviors
base_data<-base_data[order(base_data$mean),]

base_data$p_value=p.adjust(base_data$p_value,method='fdr',n=length(base_data$p_value))
base_data$p_value=as.factor(base_data$p_value)
header <- tibble(study = 'Cognition',mediated_value = "mediated_value",N='N',p_value='p_value')
cochrane_output_df <- bind_rows(header,base_data)
cochrane_output_df %>% 
  forestplot(labeltext = c(study, mediated_value,N,p_value),
             graph.pos =2,boxsize=0.45,zero=0.2,
             is.summary = FALSE,clip = c(0, 0.60), xlog = FALSE,graphwidth = unit(.4,"npc"),
             col = fpColors(box = "darkgreen",line = "black",summary = "grey"),
             mar=unit(rep(1.0, times = 4), "cm"),
             txt_gp=fpTxtGp(label=gpar(cex=0.7), ticks=gpar(cex=0.8), xlab=gpar(cex = 0.8), title=gpar(cex = 0.8)),
             xlab = "ACME",lwd.xaxis = 1.2)
