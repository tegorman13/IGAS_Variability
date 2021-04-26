# for use with updated dataframe with nicer column names - 02/13/21

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




###### Using Ancova ######
library(MuMIn)
library(HH)
acs.condit.r=aov(dev~conditType,data=e2_c)
acs.condit=ancova(dev~conditType+sim,data=e2_c,cex=2)
summary.lm( acs.condit )
anova(acs.condit.r,acs.condit)
acv.condit=ancova(dev~conditType+sim,data=e2_cvcc,cex=2)
summary.lm( acv.condit )
BIC(acs.condit, acv.condit) # very small advantage for cvcc
####################

###### Basic Lmer with random intercepts #########
mm.c_Only <- e2_c %>% lmer(dev~sim +(1|sbjCode) + (1|pos),data=.,REML=FALSE)
mm.c_Condit<- e2_c %>% lmer(dev~conditType+sim +(1|sbjCode) + (1|pos),data=.,REML=FALSE)
mm.cv_Only <- e2_cvcc %>% lmer(dev~sim +(1|sbjCode) + (1|pos),data=.,REML=FALSE)
mm.cv_Condit<- e2_cvcc %>% lmer(dev~conditType+sim +(1|sbjCode) + (1|pos),data=.,REML=FALSE)

anova(mm.c_Only,mm.c_Condit) # sig. improvement of condit, p=.028
anova(mm.cv_Only,mm.cv_Condit) # no sig imprvment, p=.079

BIC(mm.c_Only,mm.cv_Only)
BIC(mm.c_Condit,mm.cv_Condit)


################ lmer(dev~(pos*conditType)+sim +(1|sbjCode) ####################
lcs.condit.pa <- e2_c %>% lmer(dev~(pos+conditType)+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcs.condit.pa,type=3)
summary(lcs.condit.pa)


lcv.condit.pa <- e2_cvcc %>% lmer(dev~(pos+conditType)+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcv.condit.pa,type=3)
summary(lcv.condit.pa)

lc.pa <- e2_c %>% lmer(dev~pos+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lc.pa,type=3)
summary(lc.pa)

lcv.pi <- e2_cvcc %>% lmer(dev~pos+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcv.pa,type=3)
summary(lcv.pa)

##################################



################ lmer(dev~(pos*conditType)+sim +(1|sbjCode) ####################
lcs.condit.pi <- e2_c %>% lmer(dev~(pos*conditType)+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcs.condit.pi,type=3)
summary(lcs.condit.pi)


lcv.condit.pi <- e2_cvcc %>% lmer(dev~(pos*conditType)+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcv.condit.pi,type=3)
summary(lcv.condit.pi)



lc.pi <- e2_c %>% lmer(dev~pos+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lc.pi,type=3)
summary(lc.pi)


lcv.pi <- e2_cvcc %>% lmer(dev~pos+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcv.pi,type=3)
summary(lcv.pi)


anova(lc.pi,lcs.condit.pi)
anova(lcv.pi,lcv.condit.pi)
BIC(lc.pi,lcv.pi)

##################################





###################lmer(dev~conditType+sim +(1|sbjCode) ###############
lcs.condit <- e2_c %>% lmer(dev~conditType+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)

lcv.condit <- e2_cvcc %>% lmer(dev~conditType+sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcv.condit,type=3)
summary(lcv.condit)

lc <- e2_c %>% lmer(dev~sim +(1|sbjCode),data=.,REML=FALSE)
anova(lc,type=3)
summary(lc)

lcv <- e2_cvcc %>% lmer(dev~sim +(1|sbjCode),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)


anova(lc,lcs.condit)
anova(lcv,lcv.condit)

BIC(lc,lcv)
##################################

################# lmer(dev~conditType+sim+(1|pos)+(1|sbjCode) ##################3

lcs.condit <- e2_c %>% lmer(dev~conditType+sim+(1|pos)+(1|sbjCode),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)
MuMIn::r.squaredGLMM(lcs.condit)

lcvs.condit <- e2_cvcc %>% lmer(dev~conditType+sim+(1|pos)+(1|sbjCode),data=.,REML=FALSE)
anova(lcvs.condit,type=3)
summary(lcvs.condit)
MuMIn::r.squaredGLMM(lcvs.condit)

lcs <- e2_c %>% lmer(dev~sim+(1|pos)+(1|sbjCode),data=.,REML=FALSE)
anova(lcs,type=3)
summary(lcs)
MuMIn::r.squaredGLMM(lcs)

lcv <- e2_cvcc %>% lmer(dev~sim+(1|pos)+(1|sbjCode),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)
MuMIn::r.squaredGLMM(lcv)

anova(lcs,lcs.condit)
anova(lcv,lcvs.condit)
BIC(lcs,lcv)

###################################3



################ lmer(dev~(sim*conditType)+(1|sbjCode)+(1|pos) ####################
lcs.condit <- e2_c %>% lmer(dev~(sim*conditType)+(1|sbjCode)+(1|pos),data=.,REML=FALSE)
anova(lcs.condit.pi,type=3)
summary(lcs.condit.pi)


lcv.condit <- e2_cvcc %>% lmer(dev~(sim*conditType)+(1|sbjCode)+(1|pos),data=.,REML=FALSE)
anova(lcv.condit.pi,type=3)
summary(lcv.condit.pi)

lc <- e2_c %>% lmer(dev~(sim)+(1|sbjCode)+(1|pos),data=.,REML=FALSE)
anova(lc.pi,type=3)
summary(lc.pi)

lcv <- e2_cvcc %>% lmer(dev~(sim)+(1|sbjCode)+(1|pos),data=.,REML=FALSE)
anova(lcv.pi,type=3)
summary(lcv.pi)


anova(lc,lcs.condit)
anova(lcv,lcv.condit)
BIC(lc,lcv)

##################################









################ lmer(dev~(sim*conditType)+(1|sbjCode)+(1|pos) ####################
lcs.condit <- e2_c %>% lmer(dev~(sim*conditType)+(1|sbjCode)+(1+conditType|pos),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)


lcv.condit <- e2_cvcc %>% lmer(dev~(sim*conditType)+(1|sbjCode)+(1+conditType|pos),data=.,REML=FALSE)
anova(lcv.condit.pi,type=3)
summary(lcv.condit.pi)

lc <- e2_c %>% lmer(dev~(sim)+(1|sbjCode)+(1+conditType|pos),data=.,REML=FALSE)
anova(lc.pi,type=3)
summary(lc.pi)

lcv <- e2_cvcc %>% lmer(dev~(sim)+(1|sbjCode)+(1+conditType|pos),data=.,REML=FALSE)
anova(lcv.pi,type=3)
summary(lcv.pi)

anova(lc,lcs.condit)
anova(lcv,lcv.condit)
BIC(lc,lcv)

####################################################################





################ Slope without Int;  lmer(dev~conditType+sim+(1|sbjCode)+(0+conditType|pos) ####################
lcs.condit <- e2_c %>% lmer(dev~conditType+sim+(1|sbjCode)+(0+conditType|pos),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)


lcv.condit <- e2_cvcc %>% lmer(dev~conditType+sim+(1|sbjCode)+(0+conditType|pos),data=.,REML=FALSE)
anova(lcv.condit,type=3)
summary(lcv.condit)

lc <- e2_c %>% lmer(dev~sim+(1|sbjCode)+(0+conditType|pos),data=.,REML=FALSE)
anova(lc,type=3)
summary(lc.pi)

lcv <- e2_cvcc %>% lmer(dev~(sim)+(1|sbjCode)+(1+conditType|pos),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)

anova(lc,lcs.condit)
anova(lcv,lcv.condit)
BIC(lc,lcv)

####################################################################


################# fit a different slope on similarity?  pos+(1+sim|conditType) ##################3

lcs.condit <- e2_c %>% lmer(dev~conditType+pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)

lcvs.condit <- e2_cvcc %>% lmer(dev~conditType+pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcvs.condit,type=3)
summary(lcvs.condit)
MuMIn::r.squaredGLMM(lcvs.condit)

lcs <- e2_c %>% lmer(dev~pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcs,type=3)
summary(lcs)
MuMIn::r.squaredGLMM(lcs)

lcv <- e2_cvcc %>% lmer(dev~pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)
MuMIn::r.squaredGLMM(lcv)

anova(lcs,lcs.condit)
anova(lcv,lcvs.condit)

BIC(lcs,lcv)


###################################3







################# fit a different slope on similarity?  pos+(1+sim|conditType) ##################3

lcs.condit <- e2_c %>% lmer(dev~conditType+pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)

lcvs.condit <- e2_cvcc %>% lmer(dev~conditType+pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcvs.condit,type=3)
summary(lcvs.condit)
MuMIn::r.squaredGLMM(lcvs.condit)

lcs <- e2_c %>% lmer(dev~pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcs,type=3)
summary(lcs)
MuMIn::r.squaredGLMM(lcs)

lcv <- e2_cvcc %>% lmer(dev~pos+(1+sim|conditType),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)
MuMIn::r.squaredGLMM(lcv)

anova(lcs,lcs.condit)
anova(lcv,lcvs.condit)

BIC(lcs,lcv)


###################################3




#################lmer(dev~conditType+sim+(1+conditType|pos) ##################3


lcs.condit <- e2_c %>% lmer(dev~conditType+sim+(1+conditType|pos),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)

lcvs.condit <- e2_cvcc %>% lmer(dev~conditType+sim+(1+conditType|pos),data=.,REML=FALSE)
anova(lcvs.condit,type=3)
summary(lcvs.condit)


lcs <- e2_c %>% lmer(dev~sim+(1+conditType|pos),data=.,REML=FALSE)
anova(lcs,type=3)
summary(lcs)
MuMIn::r.squaredGLMM(lcs)

lcv <- e2_cvcc %>% lmer(dev~sim+(1+conditType|pos),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)
MuMIn::r.squaredGLMM(lcv)

anova(lcs,lcs.condit)
anova(lcv,lcvs.condit)

BIC(lcs,lcv)


###################################3



#################lmer(dev~conditType+sim+(1+conditType+sim|pos) ##################3


lcs.condit <- e2_c %>% lmer(dev~conditType+sim+(1+conditType+sim|pos),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)

lcvs.condit <- e2_cvcc %>% lmer(dev~conditType+sim+(1+conditType+sim|pos),data=.,REML=FALSE)
anova(lcvs.condit,type=3)
summary(lcvs.condit)


lcs <- e2_c %>% lmer(dev~sim+(1+conditType+sim|pos),data=.,REML=FALSE)
anova(lcs,type=3)
summary(lcs)
MuMIn::r.squaredGLMM(lcs)

lcv <- e2_cvcc %>% lmer(dev~sim+(1+conditType+sim|pos),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)
MuMIn::r.squaredGLMM(lcv)

anova(lcs,lcs.condit)
anova(lcv,lcvs.condit)

BIC(lcs,lcv)


###################################3








################# fit a different slope on similarity?  sim|pos ##################3

lcs.condit <- e2_c %>% lmer(dev~conditType+sim+(1+sim|pos),data=.,REML=FALSE)
anova(lcs.condit,type=3)
summary(lcs.condit)

lcvs.condit <- e2_cvcc %>% lmer(dev~conditType+sim+(1+sim|pos),data=.,REML=FALSE)
anova(lcvs.condit,type=3)
summary(lcvs.condit)


lcs <- e2_c %>% lmer(dev~sim+(1+sim|pos),data=.,REML=FALSE)
anova(lcs,type=3)
summary(lcs)
MuMIn::r.squaredGLMM(lcs)

lcv <- e2_cvcc %>% lmer(dev~sim+(1+sim|pos),data=.,REML=FALSE)
anova(lcv,type=3)
summary(lcv)
MuMIn::r.squaredGLMM(lcv)

anova(lcs,lcs.condit)
anova(lcv,lcvs.condit)

BIC(lc,lcv)


###################################3




# anova(lm1,type=3)



#scaledDev
c_Only2 = lm(scaleDev~sim_c,data=e2_sim)  # sim computed from single c
c_Condit2 = lm(scaleDev~sim_c+conditType,data=e2_sim)
cv_Only2 = lm(scaleDev~sim_cvcc,data=e2_sim) # sim computed from cv cc
cv_Condit2 = lm(scaleDev~sim_cvcc+conditType,data=e2_sim)





#### looking at similarity by itself####
s400 = e2_sim %>% filter(pos=="p400") 
cor(s400$dev,s400$sim_c)

s500 = e2_sim %>% filter(pos=="p500") 
cor(s500$dev,s500$sim_c)

s625 = e2_sim %>% filter(pos=="p625") 
cor(s625$dev,s625$sim_c)

s675 = e2_sim %>% filter(pos=="p675") 
cor(s675$dev,s675$sim_c)

s800 = e2_sim %>% filter(pos=="p800") 
cor(s800$dev,s800$sim_c)

s900 = e2_sim %>% filter(pos=="p900") 
cor(s900$dev,s900$sim_c)

###### OLDER Model comparison stuff ###########
qplot(e2_sim$sim_cvcc,e2_sim$dev,col=e2_sim$pos)
qplot(e2_sim$sim_cvcc,e2_sim$scaleDev,col=e2_sim$pos)

cor(e2_sim$scaleDev,e2_sim$dev)
cor(e2_sim$scaleDev,e2_sim$sim_cvcc)
cor(e2_sim$dev,e2_sim$sim_cvcc)



