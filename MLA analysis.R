library(metafor)

mlabfun <- function(text, sens) {
  list(bquote(paste(.(text),
                    " (Q = ", .(formatC(sens$QE, digits=2, format="f")),
                    ", df = ", .(sens$k - sens$p),
                    ", p ", .(metafor:::.pval(sens$QEp, digits=2, showeq=TRUE, sep=" ")), "; ",
                    I^2, " = ", .(formatC(sens$I2, digits=1, format="f")), "%, ",
                    tau^2, " = ", .(formatC(sens$tau2, digits=2, format="f")), ")")))
}


#data cleaning
Dat$N<-Dat$`Sample size`

#AUC 90 days

dat1<- Dat[complete.cases(Dat[, c("AUC90m","CIlower90", "CIupper90" )]),]


dat1$auc <- log(dat1$AUC90m/(1-dat1$AUC90m))
dat1$se<- (log(dat1$CIupper90/(1-dat1$CIupper90))- log(dat1$CIlower90/(1-dat1$CIlower90)))/2*1.96


predict(res1, transf = transf.ilogit)
summary(res1)
res1<-rma(yi=auc, sei = se, method = "REML", data = dat1 )

forest.rma(res1,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", transf = transf.ilogit,
           mlab = mlabfun("RE Model", res1),xlab=" AUC 90 days (95% CI)", slab = dat1$Author)


#AUC 1 year

dat2<- Dat[complete.cases(Dat$AUC1m),]

dat2$auc <- log(dat2$AUC1m/(1-dat2$AUC1m))
dat2$se<- (log(dat2$CIupper1/(1-dat2$CIupper1))- log(dat2$CIlower1/(1-dat2$CIlower1)))/2*1.96




res2<-rma(yi=auc, sei = se, method = "REML", data = dat2 )
print(res2)

forest.rma(res2,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", transf = transf.ilogit,
           mlab = mlabfun("RE Model", res2),xlab=" AUC 1 year (95% CI)", slab = dat2$Author)


###Publication Bias:
funnel(res2)
funnel(res1)
ranktest(res2)
regtest(res2)
ranktest(res1)
regtest(res1)
reporter()
#Mortality 90 days
mort90<- Dat[complete.cases(Dat$m90),]

mort90<-escalc(xi = m90, ni =N , measure = "PR",  data = mort90 )
res3<-rma(yi, vi, method = "REML",data = mort90)

forest.rma(res3,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res3),xlab=" Mortality 90 days (95% CI)", slab = mort90$Author)


#Mortality 6 months
mort6<- Dat[complete.cases(Dat$m6),]

mort6<-escalc(xi = m6, ni =N , measure = "PR",  data = mort6 )
res4<-rma(yi, vi, method = "REML",data = mort6)

forest.rma(res4,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res4),xlab=" Mortality 6 months (95% CI)", slab = mort6$Author)

#Mortality 1 year
mort1<- Dat[complete.cases(Dat$m1),]

mort1<-escalc(xi = m1, ni =N , measure = "PR",  data = mort1 )
res5<-rma(yi, vi, method = "REML",data = mort1)

forest.rma(res5,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res5),xlab=" Mortality 1 Year (95% CI)", slab = mort1$Author)



#####Calibration:####
#Log O:E 90
oe90<- Dat[complete.cases(Dat$oe90),]



oe90$se<- (oe90$oe90u-oe90$oe90l)/2*1.96


res6<-rma(yi= oe90, sei = se, method = "REML",slab =paste(Author),data = oe90)

forest.rma(res6,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", atransf = exp,
           mlab = mlabfun("RE Model", res6),xlab="  O:E ratio Calibration : 90 days (95% CI)")


forest.rma(res6,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res6),xlab="  log O:E ratio Calibration :90 days (95% CI)")

#Log O:E 1 
oe1<- Dat[complete.cases(Dat$oe1),]




oe1$se<- (oe1$oe1u-oe1$oe1l)/2*1.96


res7<-rma(yi= oe1, sei = se, method = "REML",slab =paste(Author),data = oe1)

forest.rma(res7,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", atransf = exp,
           mlab = mlabfun("RE Model", res7),xlab="  O:E ratio Calibration: 1 year (95% CI)")


forest.rma(res7,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res7),xlab="  log O:E ratio Calibration: 1 year (95% CI)")



#Slope 1 
cal1<- Dat[complete.cases(Dat$cal1),]

cal1$se<- (cal1$cal1u-cal1$cal1l)/2*1.96


res8<-rma(yi= cal1, sei = se, method = "REML",slab =paste(Author),data = cal1)

forest.rma(res8,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res8),xlab="Calibration Slope : 1 year (95% CI)")


#Slope 90 
cal90<- Dat[complete.cases(Dat$cal90),]

cal90$se<- (cal90$cal90u-cal90$cal90l)/2*1.96


res9<-rma(yi= cal90, sei = se, method = "REML",slab =paste(Author),data = cal90)

forest.rma(res9,addpred = TRUE , header = TRUE, cex = 1
           , showweights = TRUE, refline = FALSE, 
           shade = "zebra2", 
           mlab = mlabfun("RE Model", res9),xlab="Calibration Slope : 90 days (95% CI)")




reporter(res9, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res9", format = "word")


reporter(res8, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res8", format = "word")


reporter(res7, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res7", format = "word")


reporter(res6, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res9", format = "word")


reporter(res5, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res9", format = "word")


reporter(res4, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res9", format = "word")


reporter(res3, funnel = FALSE, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res9", format = "word")



reporter(res2, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res2", format = "word")



reporter(res1, forest = FALSE, 
         dir = "C:/college/research/Systematic Reviews/Machine learning and Survival", 
         filename = "res1", format = "word")

