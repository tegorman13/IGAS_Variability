
source('IGAS_ProcessFunctions.R')

#rm(list=ls())
###### data loading and organization #######


e2 <- readRDS('data/igas_e2_cleanedData-final.b.rds')%>% mutate(initialVelocityX=X_Velocity,initialVelocityY=Y_Velocity)


trainingStages <- c(0,1,2,3,4,5,6)
goodSubjectsVaried <- e2 %>% filter(conditType=="varied") %>% select(sbjCode,conditType) %>% unique()%>% as.data.frame()
goodSubjects <- e2 %>% select(sbjCode,conditType) %>% unique()
goodSubjects=goodSubjects$sbjCode
goodSubjects2 <- e2 %>% select(sbjCode,conditType) %>% unique() %>% as.data.frame() # won't work unless sbjcode is int - df3 needs to be read in

transfer = e2 %>% filter(stage=="Transfer") %>% group_by(positionX) %>% mutate(scaledAbsDist=scale(AbsDistFromCenter)) %>% ungroup()
byConditPosition <- group_by(transfer,conditType,sbjCode,positionX,condit2)
avgDist <- summarise(byConditPosition,scaledDev=mean(scaledAbsDist,trim=.05))
transfer <-spread(avgDist,positionX,scaledDev)
colnames(transfer)[4:9] = c("sp400","sp500","sp625","sp675","sp800","sp900") 

transfer2= filter(e2,stage=="Transfer")
byConditPosition2 <- group_by(transfer2,conditType,sbjCode,positionX,condit2)
avgDist2 <- summarise(byConditPosition2,deviation=mean(AbsDistFromCenter,trim=.05))
transfer2 <-spread(avgDist2,positionX,deviation) %>% ungroup()
colnames(transfer2)[4:9] = c("400","500","625","675","800","900") 
transfer2= transfer2 %>% select(-condit2,-conditType)

transfer=merge(transfer,transfer2,by="sbjCode")

# standardize training performance within each group
train = e2 %>% filter(stage !="Transfer") %>% group_by(condit2)%>% mutate(scaledAbsDist=scale(AbsDistFromCenter)) # training performance scaled within group
train <- train %>% group_by(sbjCode,condit2) %>%summarise(trainAvg=mean(scaledAbsDist,trim=.05)) %>%ungroup() %>% select(-condit2)
allTrain = e2%>% filter(stage !="Transfer",initialVelocityX<0,initialVelocityY<0,initialVelocityX>-465,initialVelocityY>-685) %>%
  select(sbjCode,condit2,conditType,initialVelocityX,initialVelocityY,stage,trialType,AbsDistFromCenter,trial) # each individual training throw

solutionSpace <- filter(e2,trialType==11,stage=="Transfer") # all solutions from final stage

sol400 <-read.csv("data/sol400.csv") %>% select(initialVelocityX,initialVelocityY) %>% mutate(X_Velocity=initialVelocityX,Y_Velocity=initialVelocityY)
sol500 <-read.csv("data/sol500.csv")%>% select(initialVelocityX,initialVelocityY)%>% mutate(X_Velocity=initialVelocityX,Y_Velocity=initialVelocityY)
sol800 <-read.csv("data/sol800.csv")%>% select(initialVelocityX,initialVelocityY)%>% mutate(X_Velocity=initialVelocityX,Y_Velocity=initialVelocityY)
sol900 <-read.csv("data/sol900.csv")%>% select(initialVelocityX,initialVelocityY)%>% mutate(X_Velocity=initialVelocityX,Y_Velocity=initialVelocityY)
sol625 <-read.csv("data/sol625.csv")%>% select(initialVelocityX,initialVelocityY)%>% mutate(X_Velocity=initialVelocityX,Y_Velocity=initialVelocityY)
sol675 <-read.csv("data/sol675.csv")%>% select(initialVelocityX,initialVelocityY)%>% mutate(X_Velocity=initialVelocityX,Y_Velocity=initialVelocityY)

# best fitting generalization parameters. NM - original sim computation, expt 2

c=.0158
cv=.0164
cc=.0198

#### Compute 3 Similarity scores - original sim method - may take 20 secs to run - decrease sol size above to make it go faster#############
e2_c = cSim(c) %>% group_by(sbjCode,conditType,pos,dev,scaleDev) %>% select(-scalePos)# c optimized across all subjects 
e2_cvcc = cvSim(cv,cc)%>% group_by(sbjCode,conditType,pos,dev,scaleDev)%>% select(-scalePos)  # c optimized within each group
e2_sim=merge(e2_c,e2_cvcc,by=c("sbjCode","conditType","pos","dev","scaleDev"))
colnames(e2_sim)[6:7]=c("sim_c","sim_cvcc")


factorNames <- c("sbjCode","pos","conditType")
e2_c[,factorNames] <- lapply(e2_c[,factorNames],factor)
e2_cvcc[,factorNames] <- lapply(e2_cvcc[,factorNames],factor)
e2_sim[,factorNames] <- lapply(e2_sim[,factorNames],factor)

#saveRDS(e2_sim,"data/IGAS_Similarity-Performance.rds")

######## Simple Linear Regression Models  #########

########## Linear Regression Models
#summary(lm(dev~trainAvg,data=d))
summary(lm(dev~conditType,data=e2_sim))
summary(lm(dev~sim_c,data=e2_sim))
summary(lm(dev~sim_cvcc,data=e2_sim))

summary(lm(dev~sim_c+conditType,data=e2_sim))
summary(lm(dev~sim_cvcc+conditType,data=e2_sim))

summary(lm(dev~pos+sim_c+conditType,data=e2_sim))
summary(lm(dev~pos+sim_cvcc+conditType,data=e2_sim))

c_Only = lm(dev~pos+sim_c,data=e2_sim)  # sim computed from single c
c_Condit = lm(dev~pos+sim_c+conditType,data=e2_sim)
cv_Only = lm(dev~pos+sim_cvcc,data=e2_sim) # sim computed from cv cc
cv_Condit = lm(dev~pos+sim_cvcc+conditType,data=e2_sim)

c_Only %>% summary() #linear model 1
c_Condit %>% summary() #linear model 2
cv_Only %>% summary() #linear model 3
cv_Condit %>% summary() #linear model 4 
################


########Anova and BIC Model Comparisons - Used in Original JEP:HPP Submission #########

#additive linear models

anova(c_Only,c_Condit) # 2nd model adds conditType, sig. improvement
anova(cv_Only,cv_Condit) # 2nd model adds conditType, No sig. improvement

BIC(c_Only,cv_Only)
BIC(c_Condit,cv_Condit)


nrow(e2_sim)*(log(2*pi)+1+log((sum(c_Only$residuals^2)/nrow(e2_sim))))+((length(c_Only$coefficients)+1)*2) # computes AIC
nrow(e2_sim)*(log(2*pi)+1+log((sum(cv_Only$residuals^2)/nrow(e2_sim))))+((length(cv_Only$coefficients)+1)*2)
##############






