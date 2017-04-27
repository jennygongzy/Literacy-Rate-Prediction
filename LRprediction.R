######################### Data Preperation ###########################
#Read Data
x = read.csv('input.csv',header = TRUE,stringsAsFactors = FALSE,na.strings = "NA",sep = '\t')
head(x)
tail(x)
dim(x)

#Change Variable Type
x$IncomeGroup = as.character(x$IncomeGroup)
x$GDPPC = as.numeric(x$GDPPC)
x$GEOG = as.numeric(x$GEOG)
x$GEOGPS = as.numeric(x$GEOGPS)
x$EEOGE = as.numeric(x$EEOGE)
x$Unemployment = as.numeric(x$Unemployment)
x$Poverty = as.numeric(x$Poverty)
x$GINI = as.numeric(x$GINI)

x$Literacy = x$Literacy/100
x$Literacy3 = x$Literacy^3

summary(x[,4:13])

# Boxplot of income group
table(x$IncomeGroup)

boxplot(Literacy~IncomeGroup,data=x,col='pink', ylab = "Literacy Rate(%)",
        names=c('Low Income',"Lower Middle Income","Upper Middle Income","High Income"),
        main = 'Literacy Rate Distribution by Income Groups',
        cex.lab=1.5,cex.axis=1.2,cex.main=1.8)



#Histograms 
par(mfrow=c(1,2))
hist(x$Literacy, las = TRUE, main='Literacy Rate(100%)', 
     col="cadetblue", xlab="Literacy Rate(100%)", cex.lab=1.5, cex.axis=1.2, cex.main=1.8,breaks = 20)
hist(x$Literacy3,las = TRUE, main='Literacy Rate^3 (100%)^3', 
     col="cadetblue", xlab="Literacy Rate^3 (100%)^3", cex.lab=1.5, cex.axis=1.2, cex.main=1.8,breaks = 20)

##
par(mfrow=c(2,5))
hist(x$GDP, las = TRUE, main='GDP (Billion$)', 
         col="cadetblue", xlab="GDP (B$)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$GDPPC, las = TRUE, main='GDP per capita ($)', 
     col="cadetblue", xlab="GDPPC ($)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$GEOG, las = TRUE, main='Gov Expenditure on Education \ntotal(% of GDP)', 
     col="cadetblue", xlab="GEOG(%)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$GEOGPS, las = TRUE, main='Gov Expenditure per student\n(% of GDP per capita)', 
     col="cadetblue", xlab="GEOGPS(%))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$EEOGE, las = TRUE, main='Expenditure on Education\nas % of Gov Expenditure(%)', 
     col="cadetblue", xlab="EEOGE(%)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LaborForce, las = TRUE, main='Labor Force,total\nin Million People', 
     col="cadetblue", xlab="LaborForce(Million People)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LaborForcePC, las = TRUE, main='Labor Force in 100 people (%)', 
     col="cadetblue", xlab="LaborForcePC(%)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$Unemployment, las = TRUE, main='Unemployment, total\n(% of total Labor Force)', 
     col="cadetblue", xlab="Unemployment(%)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$Poverty, las = TRUE, main='Poverty headcount ratio\n(% of population)', 
     col="cadetblue", xlab="Poverty(%)", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$GINI, las = TRUE, main='GINI index', 
     col="cadetblue", xlab="GINI", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)

#Transform 
x$LGDP = log(x$GDP)
x$LGDPPC = log(x$GDPPC)
x$LLaborForce = log(x$LaborForce)
x$LUnemployment = log(x$Unemployment)
x$LPoverty = log(x$Poverty)
x$LLaborForcePC = log(x$LaborForcePC)

#Histogram after Transformation 
par(mfrow=c(2,3))
hist(x$LGDP, las = TRUE, main='Log GDP', 
     col="cadetblue", xlab="LGDP (log(B$))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LGDPPC, las = TRUE, main='Log GDP per capita', 
     col="cadetblue", xlab="LGDPPC (log($))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LLaborForce, las = TRUE, main='Log Labor Force,total', 
     col="cadetblue", xlab="LLaborForce(Log(Million People))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LUnemployment, las = TRUE, main='Log Unemployment, total\n(% of total Labor Force)', 
     col="cadetblue", xlab="LUnemployment(Log(%))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LPoverty, las = TRUE, main='Log Poverty headcount ratio\n(% of population)', 
     col="cadetblue", xlab="LPoverty(Log(%))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
hist(x$LLaborForcePC, las = TRUE, main='Log Labor Force,per capita(Log%)', 
         col="cadetblue", xlab="LLaborForcePC(Log(%))", cex.lab=1.0, cex.axis=1.0, cex.main=1.2,breaks = 20)
########################## Variable Selection + Model Building ###############################
#correlation of all variables 
dim(x)
cor(x[,3:20],use='pairwise.complete.obs')

#initially select variables
colnames(x)
x2 = x[,c("Country.Name","IncomeGroup","Literacy3","GEOG","LaborForcePC","LLaborForcePC","LGDPPC","LUnemployment")]
x2 = x2[complete.cases(x2),]
nrow(x2)

#scatter plot with color code
col.vector <- rep("black", times=nrow(x2))
col.vector[x2$IncomeGroup=="2"] <- "cadetblue"
col.vector[x2$IncomeGroup=="3"] <- "firebrick"
col.vector[x2$IncomeGroup=="4"] <- "orange"
pairs(x2[,3:8], las=TRUE, pch=19, col=col.vector,lwd=0.3)


####### Find Best model for LGDPPC ########

require(leaps)
forward.step <- regsubsets(Literacy3 ~ GEOG + LaborForcePC + LGDPPC + LUnemployment, data=x2, method="forward")
summary(forward.step)

# compare C_p values
round(summary(forward.step)$cp, digits=1)

# compare two models
lm.3 = lm(Literacy3 ~ LaborForcePC + LGDPPC + LUnemployment, data=x2)
lm.4 = lm(Literacy3 ~ GEOG + LaborForcePC + LGDPPC + LUnemployment, data=x2)

anova(lm.3,lm.4)

# compare LaborForcePC and LLaborForcePC
lm.log = lm(Literacy3 ~ LLaborForcePC + LGDPPC + LUnemployment, data=x2)
summary(lm.3)
summary(lm.log)

require(qpcR)
press.3 = PRESS(lm.3)$stat
press.log = PRESS(lm.log)$stat

# sqrt MSE_jackknife
round(sqrt(press.3/(nrow(x2)-2)), digits=3)
round(sqrt(press.log/(nrow(x2)-5)), digits=3)

# RMSE
round(summary(lm.3)$sigma, digits=3)
round(summary(lm.log)$sigma, digits=3)

# Best Model for LGDPPC

lm.gdp = lm.3
summary(lm.gdp)

#residual plot 
plot(lm.gdp$fitted.values,lm.gdp$residuals,pch = 19, main="Best model for lm.gdp\nResiduals vs. Fitted Values", xlab="fitted values (%)", ylab="residuals (%)",
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(lm.gdp)$sigma*2, -summary(lm.gdp)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 

######################### Find Best model for IncomeGroup ######################
forward.step <- regsubsets(Literacy3 ~ IncomeGroup + GEOG+ LaborForcePC+ LUnemployment, data=x2, method="forward")

summary(forward.step)

# compare C_p values
round(summary(forward.step)$cp, digits=1)

lm.3 = lm(Literacy3~IncomeGroup + LaborForcePC + LUnemployment, data=x2)
lm.4 = lm(Literacy3~IncomeGroup + GEOG + LaborForcePC + LUnemployment, data=x2)
summary(lm.3)
summary(lm.4)

anova(lm.3,lm.4)

# compare LaborForcePC and LLaborForcePC
lm.log = lm(Literacy3 ~ LLaborForcePC + IncomeGroup + LUnemployment, data=x2)
summary(lm.3)
summary(lm.log)

press.3 = PRESS(lm.3)$stat
press.log = PRESS(lm.log)$stat

# sqrt MSE_jackknife
round(sqrt(press.3/(nrow(x2)-2)), digits=3)
round(sqrt(press.log/(nrow(x2)-5)), digits=3)

# RMSE
round(summary(lm.3)$sigma, digits=3)
round(summary(lm.log)$sigma, digits=3)

#Best model for IncomeGroup
lm.income <- lm.3
summary(lm.income)

#residual plot 
col.vector <- rep("black", times=nrow(x2))
col.vector[x2$IncomeGroup=="2"] <- "cadetblue"
col.vector[x2$IncomeGroup=="3"] <- "firebrick"
col.vector[x2$IncomeGroup=="4"] <- "orange"
pch.vector <- rep('1',times=nrow(x2))
pch.vector[x2$IncomeGroup=='2'] <- '2'
pch.vector[x2$IncomeGroup=='3'] <- '3'
pch.vector[x2$IncomeGroup=='4'] <- '4'

plot(lm.income$fitted.values,lm.income$residuals,pch = pch.vector,col = col.vector, 
     main="Best model for lm.income\nResiduals vs. Fitted Values", 
     xlab="fitted values (%)", ylab="residuals (%)",cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(lm.income)$sigma*2, -summary(lm.income)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 
legend("topleft", legend=c("Low Income", "Lower Middle Income","Upper Middle Income","High Income"), 
       pch=c("1", "2","3","4"), 
       col=c("black", "cadetblue","firebrick","orange"), bty="n", cex=1.0)


############## Compare lm.gdp with lm.income ################

PRESS(lm.gdp)$stat
PRESS(lm.income)$stat

# sqrt MSE_jackknife
round(sqrt(press.gdp/(nrow(x2)-2)), digits=3)
round(sqrt(press.income/(nrow(x2)-5)), digits=3)

# RMSE
round(summary(lm.gdp)$sigma, digits=3)
round(summary(lm.income)$sigma, digits=3)

#################### Check Assumptions ####################

par(mfrow=c(2,2))

col.vector <- rep("black", times=nrow(x2))
col.vector[x2$IncomeGroup=="2"] <- "cadetblue"
col.vector[x2$IncomeGroup=="3"] <- "firebrick"
col.vector[x2$IncomeGroup=="4"] <- "orange"
pch.vector <- rep('1',times=nrow(x2))
pch.vector[x2$IncomeGroup=='2'] <- '2'
pch.vector[x2$IncomeGroup=='3'] <- '3'
pch.vector[x2$IncomeGroup=='4'] <- '4'


### Residual vs. Fitted Values
plot(lm.income$fitted, lm.income$residuals, pch=pch.vector, col=col.vector, las=TRUE,
     main="(A)Residuals vs. Fitted Values", xlab="fitted values (%)", ylab="residuals (%)",
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(lm.income)$sigma*2, -summary(lm.income)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 

### Residual vs. LaborForcePC

plot(x2$LaborForcePC, lm.income$residuals, pch=pch.vector, col=col.vector, las=TRUE,
     main="(B)Residuals vs. LaborForcePC", xlab="LaborForcePC (%)", ylab="residuals (%)",
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(lm.income)$sigma*2, -summary(lm.income)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 

### Residual vs. LUnemployment
plot(x2$LUnemployment, lm.income$residuals, pch=pch.vector, col=col.vector, las=TRUE,
     main="(C)Residuals vs. LUnemployment", xlab="LUnemployment(Log(%))", ylab="residuals (%)",
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(lm.income)$sigma*2, -summary(lm.income)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) 

### Normal QQ plot
qqnorm(lm.income$residuals, pch=19, las=TRUE, cex.main=1.4, cex.lab=1.4, cex.axis=1.4,
       main="(D) Normal Quantile Plot of Residuals")
qqline(lm.income$residuals)	

### Muticollinarity
vif(x2[,c('IncomeGroup','LaborForcePC','LUnemployment')])

### Influential of outliers  
influence.measures(lm.income)
