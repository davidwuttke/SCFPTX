# clear workspace
rm(list = ls())

pc.wd = "Q:\\Research\\Data\\Data Files\\03 SCFPTX"
github.dir = "Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\"
artwork.dir = paste(github.dir,"03 Figures\\",sep="")
models.dir = paste(github.dir,"02 Models\\",sep="")


inArtDir <- function(file.name) return(paste(artwork.dir,file.name,sep=""))
inModelsDir <- function(file.name) return(paste(models.dir,file.name,sep=""))

if(dir.exists(file.path(pc.wd, "."))) setwd(pc.wd)


#load packages

library(plm)
library(lmtest)
library(ivreg)


# load data 
load("04 RData\\scf.RData")
load("04 RData\\buyers.RData")


#### IV - test relevance assumption, p. 291 Greene

summary(lm.IV.country.step.1.lm.fm <- lm(priorTerms ~ mean.priorTerms.others.country, data= scf.df))

summary(lm.IV.country.step.2.plm.fm <- plm(
  extension ~  
    as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + mode
  + posteriorTermsAreMode
  + ln.buyer.experience.count
  + priorTerms.std | . -priorTerms.std + mean.priorTerms.others.country,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    mutate(priorTerms.std = scale(priorTerms,T,F))
))

(rse.iv.prior.reg.plm <- coeftest(lm.IV.country.step.2.plm.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))



summary(lm.IV.ind.step.1.lm.fm <- lm(priorTerms ~ mean.priorTerms.others.ind, data= scf.df))

summary(lm.IV.ind.step.2.plm.fm <- plm(
  extension ~   as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + mode
  + posteriorTermsAreMode
  + ln.buyer.experience.count
  + priorTerms.std | . -priorTerms.std + mean.priorTerms.others.ind,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    mutate(priorTerms.std = scale(priorTerms,T,F))
))

(rse.iv.prior.reg.plm <- coeftest(lm.IV.ind.step.2.plm.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))

save(lm.IV.country.step.1.lm.fm,  file = "lm.IV.country.step.1.lm.fm.RData" %>% inModelsDir())
save(lm.IV.country.step.2.plm.fm, file = "lm.IV.country.step.2.plm.fm.RData" %>% inModelsDir())
save(lm.IV.ind.step.1.lm.fm,      file = "lm.IV.ind.step.1.lm.fm.RData" %>% inModelsDir())
save(lm.IV.ind.step.2.plm.fm,     file = "lm.IV.ind.step.2.plm.fm.RData" %>% inModelsDir())
