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
require(lmtest)
library(dplyr)
library(modelsummary)
library(ivreg)


# load data 
load("04 RData\\scf.RData")
load("04 RData\\buyers.RData")



#### Analysis for paper


# H1: The adoption of reverse factoring is associated with a reduction of different levels of payment terms.

t.test(scf.df$posteriorTerms,
       scf.df$priorTerms,
       paired=T)

wilcox.test(scf.df$posteriorTerms,
            scf.df$priorTerms,
            paired=T)


x <- rnorm(10,2)
y <- rnorm(10,3)

t.test(x,y,matched=T)

# more firms reduce the number of different terms than to increase it.
prop.test(x = 
            c(
              sum(buyers.df$n.posteriorTerms < buyers.df$n.priorTerms),
              sum(buyers.df$n.posteriorTerms > buyers.df$n.priorTerms)
            ),
          n= 
            c(nrow(buyers.df),
              nrow(buyers.df)
            ),
          alternative = "two.sided",
          correct = TRUE)

# on average, the number of different terms goes down:


t.test(log(buyers.df$n.priorTerms),
       log(buyers.df$n.posteriorTerms),
       paired=T)

wilcox.test(buyers.df$n.priorTerms,
            buyers.df$n.posteriorTerms, 
            paired = T)



mean(buyers.df$coef.var.priorTerms,na.rm=T)
mean(buyers.df$coef.var.posteriorTerms,na.rm=T)

t.test(buyers.df$coef.var.priorTerms,
       buyers.df$coef.var.posteriorTerms,
       paired=T)

wilcox.test(buyers.df$coef.var.priorTerms,
       buyers.df$coef.var.posteriorTerms,
       paired=T)

t.test(buyers.df$coef.var.posteriorTerms)
wilcox.test(buyers.df$coef.var.posteriorTerms)


# Main panel models

summary(plm.controls.fm <- plm(
  extension ~  as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std ,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F))
))

# (rse.plm.controls.fm <- coeftest(plm.controls.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))

summary(plm.basic.fm <- plm(
  extension ~  as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    mutate(large.ISCF = ln.discount.rate.std>quantile(ln.discount.rate.std,0.5)) %>%
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F))
))

# (rse.plm.main.fm <- coeftest(plm.main.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))

summary(plm.main.fm <- plm(
  extension ~  as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + posteriorTermsAreMode
  + ln.buyer.experience.count,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    mutate(large.ISCF = ln.discount.rate.std>quantile(ln.discount.rate.std,0.5)) %>%
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F))
))



summary(plm.full.fm <- plm(
  extension ~  as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + mode:priorTerms.std
  + posteriorTermsAreMode
  + ln.buyer.experience.count,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    mutate(priorTerms.std = scale(priorTerms,T,F)) %>%
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(dependence = scale(log(annualSpend/supplier.revenue)))
))

(rse.plm.full.fm <- coeftest(plm.full.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))


save(plm.controls.fm, file = "plm.controls.fm.RData" %>% inModelsDir())
save(plm.basic.fm, file = "plm.basic.fm.RData"%>% inModelsDir())
save(plm.main.fm, file = "plm.main.fm.RData"%>% inModelsDir())
save(plm.full.fm, file = "plm.full.fm.RData"%>% inModelsDir())

# better model fit?
plm::pFtest(plm.basic.fm, plm.controls.fm)
plm::pFtest(plm.main.fm, plm.basic.fm)
plm::pFtest(plm.full.fm, plm.main.fm)



# coefficient of variation goes down.

t.test(buyers.df$coef.var.priorTerms,buyers.df$coef.var.posteriorTerms, paired = T)

wilcox.test(buyers.df$coef.var.priorTerms,buyers.df$coef.var.posteriorTerms, paired = T)

# harmonization mainly for larger programs
summary(harmonization.probit.controls.fm <- glm(harmonization ~ B_COUNTRY__ + B_IND__ + buyer.revenue + buyer.cogs, family = binomial(link = "probit"), 
                                                data = buyers.df %>%
                                                mutate(buyer.revenue = scale(log(buyer.revenue)),
                                                       buyer.cogs = scale(log(buyer.cogs)),
                                                       RF.program.size = scale(log(RF.program.size)),
                                                       ln.mean.discountRate = scale(log(mean.discountRate)))))

summary(harmonization.probit.full.fm <- glm(harmonization ~ B_COUNTRY__ + B_IND__ + buyer.revenue + buyer.cogs +   RF.program.size , family = binomial(link = "probit"), 
                                            data = buyers.df %>%
                                            mutate(buyer.revenue = scale(log(buyer.revenue)),
                                                   buyer.cogs = scale(log(buyer.cogs)),
                                                   RF.program.size = scale(log(RF.program.size)),
                                                   ln.mean.discountRate = scale(log(mean.discountRate)))))

(rse.harmonization.probit.controls.fm <- coeftest(harmonization.probit.controls.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))
(rse.harmonization.probit.full.fm <- coeftest(harmonization.probit.full.fm, vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))

save(harmonization.probit.controls.fm, file = "harmonization.probit.controls.fm.RData"%>% inModelsDir())
save(harmonization.probit.full.fm,     file = "harmonization.probit.full.fm.RData"%>% inModelsDir())





###
#
#   Robustness tests
#

# old sample only



summary(plm.old.main.fm <- plm(
  extension ~   as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + posteriorTermsAreMode
  + ln.buyer.experience.count,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    subset(scf.df$period=="old") %>%
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F))
))



summary(plm.old.full.fm <- plm(
  extension ~   as.factor(time)
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + mode:priorTerms.std
  + posteriorTermsAreMode
  + ln.buyer.experience.count,
  index=c("BID","SID"),
  model="within",
  data=scf.df %>% 
    subset(scf.df$period=="old") %>%
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F))
))


save(plm.old.main.fm, file = "plm.old.main.fm.RData" %>% inModelsDir())
save(plm.old.full.fm, file = "plm.old.full.fm.RData" %>% inModelsDir())


# random effects

summary(plm.main.re.fm <- plm(
  extension ~  as.factor(time)
  + B_IND__
  + B_COUNTRY__
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + posteriorTermsAreMode
  + ln.buyer.experience.count,
  index=c("BID","SID"),
  model="random",
  data=scf.df %>% 
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F))
))

phtest(plm.main.re.fm,plm.main.fm)



summary(plm.full.re.fm <- plm(
  extension ~  as.factor(time)
  + B_IND__
  + B_COUNTRY__
  + S_IND__ 
  + S_COUNTRY__
  + ln.discount.rate.std
  + ln.libor.std
  + ln.annual.spend.std 
  + ln.revenue.std 
  + priorTerms.std
  + mode
  + mode:priorTerms.std
  + posteriorTermsAreMode
  + ln.buyer.experience.count,
  index=c("BID","SID"),
  model="random",
  data=scf.df %>% 
    mutate(mode = scale(as.numeric(mode),T,F)) %>%
    mutate(priorTerms.std = scale(priorTerms,T,F)) %>%
    mutate(dependence = scale(log(annualSpend/supplier.revenue)))
))

phtest(plm.full.re.fm,plm.full.fm)


save(plm.main.re.fm, file = "plm.main.re.fm.RData" %>% inModelsDir())
save(plm.full.re.fm, file = "plm.full.re.fm.RData" %>% inModelsDir())
