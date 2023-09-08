# clear workspace
rm(list = ls())

pc.wd = "Q:\\Research\\Data\\Data Files\\03 SCFPTX"
github.dir = "Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\"
artwork.dir = paste(github.dir,"03 Figures\\",sep="")
models.dir = paste(github.dir,"02 Models\\",sep="")


inArtDir <- function(file.name) return(paste(artwork.dir,file.name,sep=""))
inModelsDir <- function(file.name) return(paste(models.dir,file.name,sep=""))

if(dir.exists(file.path(pc.wd, "."))) setwd(pc.wd)


# call external function
#source('R/00_scf-functions.R')

#load packages

library(pastecs)
library(Hmisc)
library(xlsx)
library(robustHD)
library(graphics)
library(plm)
library(car)
require(stats)
require(lme4)
require(pglm)
require(lmtest)
library(rockchalk)
library(DescTools)
library(sampleSelection)
library(ssmrob)

library(dplyr)
library(mfx)

# load data 

load("04 RData\\scf_full.RData")


# count number of adopters and non-adopters in extended sample

plyr::count(scf_full.df$adopter)




summary(lsdv.effects.select.step1.fm <- glm(adopter ~ priorTerms.std + ln.annual.spend.std + ln.discount.rate.std + ln.annual.spend.std + as.factor(BID), family = binomial(link = "probit"), 
                                            data = scf_full.df%>% 
                                              subset(BID.single.obs == F) %>%
                                              mutate(priorTerms.std = scale(priorTerms))))




summary(lsdv.effects.select.step2.fm<-selection(
  selection = adopter ~ priorTerms.std + ln.revenue.std + ln.discount.rate.std + ln.annual.spend.std,
  outcome   =   extension ~  
    as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + posteriorTermsAreMode
  + ln.buyer.experience.count
  + as.factor(BID),
    data = scf_full.df %>% 
          subset(BID.single.obs == F) %>%
          mutate(priorTerms.std = scale(priorTerms)) %>%
          mutate(large.ISCF = scale((ln.discount.rate.std > quantile(ln.discount.rate.std,0.5))*1,T,F)) %>% 
          mutate(priorTerms.std = scale(priorTerms,T,F)),
    method="2step"))


save(lsdv.effects.select.step1.fm, file = "lsdv.effects.select.step1.fm.RData" %>% inModelsDir())
save(lsdv.effects.select.step2.fm, file = "lsdv.effects.select.step2.fm.RData" %>% inModelsDir())
