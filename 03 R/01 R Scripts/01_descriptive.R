# clear workspace
rm(list = ls())

pc.wd = "Q:\\Research\\Data\\Data Files\\03 SCFPTX"
if(dir.exists(file.path(pc.wd, "."))) setwd(pc.wd)

# call external function
source('Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\01 R Scripts\\00_scf-functions.R')

#load packages

library(pastecs)
library(Hmisc)
library(xlsx)
library(robustHD)
library(graphics)


# load data 

load(file="04 RData\\buyers.RData")
load(file="04 RData\\scf.RData")


###### 
# 
# Factor variables - Table 1A
#

print.table.1<-function(){
  createTable(list(scf.df$S_COUNTRY__,buyers.df$B_COUNTRY__,as.factor(scf.df$mode)),
              c("Supplier country","Buyer country","Mode"),
              list(scf.df$S_IND__,buyers.df$B_IND__,as.factor(scf.df$time)),
              c("Supplier industry","Buyer industry","Year"))
}

print.table.1()


###### 
# 
# Cont. variables - Table 1B
#

print.string = paste(
  print.tex.effectVar(scf.df$extension,"payment terms extensions (days)"),
  print.tex.effectVar(scf.df$priorTerms,"initial payment terms (days)"),
  print.tex.effectVar(scf.df$posteriorTerms,"RF payment terms (days)"),
  print.tex.effectVar(scf.df$posteriorTermsAreMode,"standard terms (1=yes)"),
  print.tex.effectVar(scf.df$discountRate/1E4,"RF interest rate"),
  print.tex.effectVar(scf.df$libor/1E4,"risk free rate"),
  print.tex.effectVar(scf.df$annualSpend/1E6,"annual spend (mn USD)"),
  print.tex.effectVar(scf.df$supplier.revenue/1E9,"supplier revenue (bn USD)"),
  print.tex.effectVar(buyers.df$buyer.revenue/1E9,"buyer revenue (bn USD)"),
  print.tex.effectVar(buyers.df$buyer.cogs/1E9,"buyer costs of goods sold (bn USD)"),
  print.tex.effectVar(buyers.df$RF.program.size,"RF program size (num. of suppliers)"),
  print.tex.effectVar(scf.df$Buyer.experience.count,"buyer experience (count)")
)

cat(print.string)

  


