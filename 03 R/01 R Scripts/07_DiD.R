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
library(Hmisc)
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
library(multcomp)
library(dplyr)

# load data 
load("04 RData\\scf.RData")
load("04 RData\\buyers.RData")
load("04 RData\\libor.RData")



########################################################################################
########################################################################################

# TIME SERIES AND Diff-in-Diff approach.
# import libor data as time varying data.

libor.yearly.df <- libor.df %>% 
  mutate(year=lubridate::year(DATE)) %>%
  mutate(USD3MTD156N = as.numeric(USD3MTD156N )) %>%
  group_by(year) %>%
  dplyr::summarize(mean(USD3MTD156N,na.rm=T))  %>%
  ungroup()

colnames(libor.yearly.df) = c("year","libor")

# reshape dataset accordingly.


did.df <- scf.df %>% 
  mutate(TREAT = priorTerms > mean.prior.terms) %>%
  mutate(adoptionYear = time) %>%
  mutate(POST_2010 = (2010 >= adoptionYear)) %>%
  mutate(POST_2011 = (2011 >= adoptionYear)) %>%
  mutate(POST_2012 = (2012 >= adoptionYear)) %>%
  mutate(POST_2013 = (2013 >= adoptionYear)) %>%
  mutate(POST_2014 = (2014 >= adoptionYear)) %>%
  mutate(POST_2015 = (2015 >= adoptionYear)) %>%
  mutate(POST_2016 = (2016 >= adoptionYear)) %>%
  mutate(POST_2017 = (2017 >= adoptionYear)) %>%
  mutate(POST_2018 = (2018 >= adoptionYear)) %>%
  mutate(POST_2019 = (2019 >= adoptionYear)) %>%
  mutate(POST_2020 = (2020 >= adoptionYear)) %>% 
  dplyr::select(contains("POST_") | c("SID","TREAT", "S_IND__" , "S_COUNTRY__", "ln.annual.spend.std"  ,  "ln.revenue.std" , "ln.buyer.experience.count", "ln.libor.std", "ln.discount.rate.std", "priorTerms", "posteriorTerms", "mode", "posteriorTermsAreMode","BID")) %>% 
  tidyr::pivot_longer(
    cols = contains("POST_"), 
    names_to = "year",
    values_to = "adopted"
  ) %>%
  mutate(paymentTerms = adopted * posteriorTerms +(1-adopted)*priorTerms) %>% dplyr::select(!c("posteriorTerms","priorTerms")) %>%
  mutate(year = substr(year,6,9) %>% as.numeric) %>%
  dplyr::rename("treatment"="TREAT") %>% 
  merge(libor.yearly.df,by="year")





summary(plm.did.controls.fm <- plm(paymentTerms ~  
                                 as.factor(time)
                               + S_IND__ 
                               + S_COUNTRY__
                               + ln.discount.rate.std
                               + ln.libor.std
                               + ln.annual.spend.std 
                               + ln.revenue.std 
                               + mode
                               + posteriorTermsAreMode
                               + ln.buyer.experience.count,
                               index=c("SID"),
                               model="random",
                               data=did.df))
  
summary(plm.did.main.fm <- plm(paymentTerms ~  
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
                                + adopted
                                + treatment,
                                index=c("SID"),
                                model="random",
                                data=did.df))

summary(plm.did.full.fm <- plm(paymentTerms ~  
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
                               + adopted
                               + adopted*treatment,
                               index=c("SID"),
                               model="random",
                               data=did.df))


save(plm.did.controls.fm, file = "plm.did.controls.fm.RData" %>% inModelsDir())
save(plm.did.main.fm, file = "plm.did.main.fm.RData" %>% inModelsDir())
save(plm.did.full.fm, file = "plm.did.full.fm.RData" %>% inModelsDir())


(rse.plm.did.basic.fm <- coeftest(plm.did.basic.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))
