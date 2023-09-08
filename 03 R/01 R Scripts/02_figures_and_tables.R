# clear workspace
rm(list = ls())

pc.wd = "Q:\\Research\\Data\\Data Files\\03 SCFPTX"
github.dir = "Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\"
artwork.dir = paste(github.dir,"03 Figures\\",sep="")

inArtDir <- function(file.name) return(paste(artwork.dir,file.name,sep=""))


if(dir.exists(file.path(pc.wd, "."))) setwd(pc.wd)

# call external function
source('Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\01 R Scripts\\00_scf-functions.R')
source('Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\01 R Scripts\\00_loadFonts.R')



#load packages

library(pastecs)
library(Hmisc)
library(xlsx)
library(robustHD)
library(graphics)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(dplyr)
library(tidyr)



# load data 

load("04 RData\\scf.RData")
load("04 RData\\buyers.RData")


# functions
se <- function(x) sd(x)/sqrt(length(x))


###### 
# 
# Correlation Table -- Table 3
#


print.table.3<-function(){
  
  corr.matrix <- scf.df %>%
    select(c("extension",
      "priorTerms",
      "posteriorTerms",
      "ln.annual.spend.std",
      "ln.revenue.std",
      "discountRate",
      "ln.libor.std",
      "mode",
      "posteriorTermsAreMode",
      "ln.buyer.experience.count",
      "buyer.cogs",
      "RF.program.size")) %>%
    mutate(buyer.cogs = scale(log(buyer.cogs)),
           RF.program.size = scale(log(RF.program.size))) %>%
    plyr::rename(., c("extension"="payment term extensions",
                      "priorTerms"="initial payment terms",
                      "posteriorTerms"="RF payment terms",
                      "discountRate"="RF interest rate",
                      "ln.libor.std"="risk free rate",
                      "ln.annual.spend.std"="annual spend",
                      "ln.buyer.experience.count" = "buyer experience",
                      "ln.revenue.std" ="supplier revenue",
                      "posteriorTermsAreMode" ="standard terms",
                      "buyer.cogs" = "buyer costs of goods sold",
                      "RF.program.size"= "RF Program size (num. of suppliers)"
    )) %>%
    mutate(mode = as.numeric(mode)) %>%
    as.matrix %>%
    apply(., 2., as.numeric) %>%
    rcorr(., type = "pearson")
    
  
  dim = sqrt(length(corr.matrix$r))
  out.matrix <- matrix(rep("",dim*(dim-1)),ncol=dim-1,nrow=dim)
  dimnames(out.matrix)<-list(paste(1:dim,colnames(corr.matrix$r),sep=". "), paste(1:(dim-1),".",sep=""))
  
  for (row in 1:dim){
    for (col in 1:dim-1){
      if(row>col)
        out.matrix[row,col]<-paste(sprintf("%.2f",round(corr.matrix$r[row,col],2)),apply(corr.matrix$P,2,star.symbol.corr)[row,col],sep="")
      if(row==col)
        out.matrix[row,col]<-""
    }
  }
  
  out.df <- data.frame(out.matrix)
  out.df$variables <- rownames(out.df)
  out.df<-unname(out.df)
  out.df <- out.df[c(length(out.df),1:length(out.df)-1)]
  
  for (row in 1:length(out.df[,1])){
    for (col in 1:length(out.df[row,])){
      cat(paste(as.matrix(out.df[row,col]),"&&"))
    }
    cat("\\\\[2pt]\n")
  }
}

print.table.3()




################################
#
#
#   FIGURES
#
#
################################


cbPalette <- c( "#F9F9F6", "#D5D5D5","#BABABA", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######
#
# H1
#
######

payment.terms.summary.df <- 
    rbind(
      scf.df %>% 
        dplyr::summarize(payment.terms = mean(priorTerms),
                         std.err = se(priorTerms),
                         sd = sd(priorTerms),
                         time = "before"),
      scf.df %>% 
        dplyr::summarize(payment.terms = mean(posteriorTerms),
                       std.err = se(posteriorTerms),
                       sd = sd(priorTerms),
                       time = "after")
    ) %>%
  mutate(time = factor(time,levels=unique(time)))

print(p <- 
        ggplot(payment.terms.summary.df, aes(x=time, y=payment.terms)) + 
        geom_bar(stat="identity", color="black",position=position_dodge(),alpha=0.5) +
        geom_errorbar(aes(ymin=payment.terms-std.err, ymax=payment.terms+std.err), width=.2, position=position_dodge(.9)) +
        scale_fill_manual(values=cbPalette)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        ggtitle("average payment terms")+# for the main title
        xlab("")+ # for the x axis label
        ylab("payment terms")+ # for the y axis label
        labs(fill="time relative to reverse factoring adoption")+
        theme(plot.title = element_text(size=26,face="bold"))+
        theme(legend.position="top")+
        theme(text=element_text(size=24,  family="latex"))
)

ggsave("bar.chart.payment.terms.pdf" %>% inArtDir, width = 20, height = 20, units = "cm")





######
#
# H2
#
######


N = nrow(buyers.df)

p.decrease     = sum(buyers.df$n.posteriorTerms < buyers.df$n.priorTerms) / N
sd.decrease    = sqrt((p.decrease*(1-p.decrease)))
se.decrease    = sd.decrease / sqrt(N)

p.increase     = sum(buyers.df$n.posteriorTerms > buyers.df$n.priorTerms) / N
sd.increase    = sqrt((p.increase*(1-p.increase)))
se.increase    = sd.increase / sqrt(N)

effect.summary.df <-
  rbind(
    data.frame(p.decrease,sd.decrease,se.decrease,effect="decrease") %>%
      dplyr::rename(c("p"="p.decrease",
                      "sd"="sd.decrease",
                      "se"="se.decrease")),
    data.frame(p.increase,sd.increase,se.increase,effect="increase") %>%
      dplyr::rename(c("p"="p.increase",
                      "sd"="sd.increase",
                      "se"="se.increase"))
  )


print(p <- 
        ggplot(effect.summary.df, aes(x=effect, y=p)) + 
        geom_bar(stat="identity", color="black",position=position_dodge(),alpha=0.5) +
        geom_errorbar(aes(ymin=p-se, ymax=p+se), width=.2, position=position_dodge(.9)) +
        scale_fill_manual(values=cbPalette)+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        ggtitle("changes in the number of different payment
terms per buyer due to RF adoption")+# for the main title
        xlab("")+ # for the x axis label
        ylab("relative frequency")+ # for the y axis label
        labs(fill="time relative to reverse factoring adoption")+
        theme(plot.title = element_text(size=26,face="bold"))+
        theme(legend.position="top")+
        theme(text=element_text(size=24,  family="latex"))
)

ggsave("bar.chart.programs.pdf" %>% inArtDir, width = 20, height = 20, units = "cm")



# import colors from Mathematica

color.1 <- cbPalette[1]
color.3 <- cbPalette[2]
color.4 <- cbPalette[3]
color.6 <- rgb(0,0,0)


# 23

bid = 23

dhat = 90
LB = 25 # lower bound - upto here increasing
UB = 70 # upper bound - after this increasing again
maxD = 90 # some largest value for prior terms, where the plot ends -- can be anything "to the right"
maxY = 185 # some largest value for posterior terms, where the plot ends -- can be anything "to the right"
LINEWIDTH = 1.2
FONT.SMALL = 4*3
LINESPACE = 0.3

single.bid.data.df <- scf.df %>% 
  subset(BID == bid) %>%
  select(priorTerms, posteriorTerms) %>%
  dplyr::count(priorTerms,posteriorTerms)

single.bid.ext.data.df <- scf.df %>% 
  subset(BID == bid) %>%
  select(priorTerms, extension) %>%
  dplyr::count(priorTerms,extension)


# estimate the slope from 0 to lower bound LB

single.bid.full.lm.fm <- lm(
  posteriorTerms ~ priorTerms,
  data = scf.df %>% 
    subset(BID == bid)
)

estimated.intercept = coefficients(single.bid.full.lm.fm)["(Intercept)"] %>% unname
estimated.slope = coefficients(single.bid.full.lm.fm)["priorTerms"] %>% unname


single.bid.lb.lm.fm <- lm(
  I(posteriorTerms-dhat) ~ 0 + I(priorTerms-LB),
  data = scf.df %>% 
    subset(BID == bid) %>% 
    subset(priorTerms <= LB)
)


b0 = single.bid.lb.lm.fm$coefficients %>% unname
y0 = dhat - LB * b0

# estimate the slope from UB to maxD

single.bid.ub.lm.fm <- lm(
  I(posteriorTerms-dhat) ~ 0 + I(priorTerms-UB),
  data = scf.df %>% 
    subset(BID == bid) %>% 
    subset(priorTerms >= UB)
)


b1 = single.bid.ub.lm.fm$coefficients %>% unname
y1 = b1 * (maxD - UB) + dhat

print(
  ggplot.23.posterior <- ggplot(single.bid.data.df, aes(x=priorTerms, y=posteriorTerms, size=n)) +
  geom_rect(
    mapping=aes(
      xmin = 0,
      xmax = LB,
      ymin = y0,
      ymax = maxY
    ),
    fill=color.4
  )+
  geom_rect(
    mapping=aes(
      xmin = LB,
      xmax = UB,
      ymin = y0,
      ymax = maxY
    ),
    fill=color.3
  )+
  geom_rect(
    mapping=aes(
      xmin = UB,
      xmax = maxD,
      ymin = y0,
      ymax = maxY
    ),
    fill=color.1
  )+
  geom_point(color="black")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x='initial payment terms', y='RF payment terms', title="")+#paste('Reverse factoring payment terms (buyer ID = ',bid,")",sep=""))+
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))+
  geom_segment(aes(x = 0, y = y0, xend = LB, yend = dhat),linewidth=LINEWIDTH,colour="black")+
  geom_segment(aes(x = LB, y = dhat, xend = UB, yend = dhat),linewidth=LINEWIDTH,colour="black")+
  geom_segment(aes(x = UB, y = dhat, xend = maxD, yend = y1),linewidth=LINEWIDTH,colour="black")+ 
  geom_segment(aes(x = 0, y = estimated.intercept, xend = maxD, yend = estimated.intercept + estimated.slope * maxD),linewidth=0.3*LINEWIDTH,colour=color.6,linetype ="dashed")+ 
  theme(legend.position="none") +
  theme(text=element_text(size=FONT.SMALL*8,  family="latex"))+
  geom_label(
    label="RF payment terms\n below reference", 
    x=12.5,
    y=55,
    label.size = NA,
    color = rgb(0.2,0.2,0.2),
    family="latex",
    alpha=0,
    lineheight = LINESPACE,
    size=FONT.SMALL
  )+
  geom_label(
    label="RF payment terms\n equal reference", 
    x=50,
    y=55,
    label.size = NA,
    color = rgb(0.2,0.2,0.2),
    family="latex",
    alpha=0,
    lineheight = LINESPACE,
    size=FONT.SMALL
  )+
    geom_label(
      label="RF payment terms\n above reference", 
      x=80,
      y=55,
      label.size = NA,
      color = rgb(0.2,0.2,0.2),
      family="latex",
      alpha=0,
      lineheight = LINESPACE,
      size=FONT.SMALL
    )
  )


single.bid.full.ext.lm.fm <- lm(
  extension ~ priorTerms,
  data = scf.df %>% 
    subset(BID == bid)
)

estimated.extension.intercept = coefficients(single.bid.full.ext.lm.fm)["(Intercept)"] %>% unname
estimated.extension.slope = coefficients(single.bid.full.ext.lm.fm)["priorTerms"] %>% unname


print(ggplot.23.extension <- ggplot(single.bid.ext.data.df, aes(x=priorTerms, y=extension, size=n)) +
  geom_rect(
    mapping=aes(
      xmin = 0,
      xmax = LB,
      ymin = 0,
      ymax = maxY
    ),
    fill=color.4
  )+
  geom_rect(
    mapping=aes(
      xmin = LB,
      xmax = UB,
      ymin = 0,
      ymax = maxY
    ),
    fill=color.3
  )+
  geom_rect(
    mapping=aes(
      xmin = UB,
      xmax = maxD,
      ymin = 0,
      ymax = maxY
    ),
    fill=color.1
  )+
  geom_point(color="black")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x='initial payment terms', y='payment term extensions', title="")+#title=paste('Payment terms extensions (buyer ID = ',bid,")",sep=""))+
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))+
  geom_segment(aes(x = 0, y = y0, xend = LB, yend = dhat-LB),linewidth=LINEWIDTH,colour="black")+
  geom_segment(aes(x = LB, y = dhat-LB, xend = UB, yend = dhat-UB),linewidth=LINEWIDTH,colour="black")+
  geom_segment(aes(x = UB, y = dhat-UB, xend = maxD, yend = y1-maxD),linewidth=LINEWIDTH,colour="black")+ 
  geom_segment(aes(x = 0, y = estimated.extension.intercept, xend = maxD, yend = estimated.extension.intercept +  estimated.extension.slope* maxD),linewidth=0.3*LINEWIDTH,colour=color.6,linetype ="dashed")+ 
  theme(legend.position="none")+
  theme(text=element_text(size=FONT.SMALL*8,  family="latex"))+
    geom_label(
      label="RF payment terms\n below reference", 
      x=12.5,
      y=5,
      label.size = NA,
      color = rgb(0.2,0.2,0.2),
      family="latex",
      alpha=0,
      lineheight = LINESPACE,
      size=FONT.SMALL
    )+
    geom_label(
      label="RF payment terms\n equal reference", 
      x=50,
      y=5,
      label.size = NA,
      color = rgb(0.2,0.2,0.2),
      family="latex",
      alpha=0,
      lineheight = LINESPACE,
      size=FONT.SMALL
    )+
    geom_label(
      label="RF payment terms\n above reference", 
      x=80,
      y=5,
      label.size = NA,
      color = rgb(0.2,0.2,0.2),
      family="latex",
      alpha=0,
      lineheight = LINESPACE,
      size=FONT.SMALL
    )
)


ggsave(
  paste("bid_23_posterior.png" %>% inArtDir,sep=""),
  plot = ggplot.23.posterior,
  device = "png",
  width = 20,
  height = 20,
  units = "cm")


ggsave(
  paste("bid_23_extension.png" %>% inArtDir,sep=""),
  plot = ggplot.23.extension,
  device = "png",
  width = 20,
  height = 20,
  units = "cm")



#

#######
#
# Create a histogram as on payment terms
#


hist.23.df <- scf.df %>% 
  subset(BID==bid) %>%
  select(c(priorTerms,posteriorTerms,BID)) %>%
  pivot_longer(., cols=-c("BID")) %>%
  dplyr::rename(c("paymentTerms"="value", "terms"="name")) %>%
  mutate(terms = forcats::fct_recode(terms, "initial"="priorTerms"),
         terms = forcats::fct_recode(terms, "RF"="posteriorTerms"))



(hist.plot <- ggplot(hist.23.df, aes(x=paymentTerms, color=terms, fill=terms)) +
  geom_density(alpha=0.2,adjust=3) +
  geom_histogram(aes(y=..density..), position=position_dodge(2.5), alpha=0.5,binwidth=5)+
  theme_bw() + 
  xlim(c(0,140))+
  scale_fill_manual(values=c("#333333", "#AAAAAA"))+
  scale_color_manual(values=c("#333333", "#AAAAAA"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title = element_text(size=26,face="bold"))+
  theme(legend.position=c(0.25,0.9), legend.spacing.y = unit(0.2, 'cm'), legend.spacing.x = unit(0.2, 'cm'))+
  theme(text=element_text(size=FONT.SMALL*8,  family="latex"))+
  labs(x='payment terms', y='density', title=""))#title=paste('Payment terms extensions (buyer ID = ',bid,")",sep=""))+



ggsave("hist.pt.png"  %>% inArtDir,
       device = "png",
       width = 20,
       height = 20,
       units = "cm")







