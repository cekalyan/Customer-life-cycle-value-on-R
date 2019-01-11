library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)

setwd("E:\\ivy\\R\\R Project")
project <- read.csv("ksr.csv")
str(project)
summary(project)

boxplot(project$Customer_Lifetime_Value)

quantile(project$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))
project2 <- project[project$Customer_Lifetime_Value <40000, ]
boxplot(project2$Customer_Lifetime_Value)

quantile(project2$Customer_Lifetime_Value, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.96,0.97,0.98,0.99,0.995,1))
project3 <- project[project$Customer_Lifetime_Value <14400, ]
boxplot(project3$Customer_Lifetime_Value)

nrow(project)-nrow(project3)

project<- project3
sapply(project, function(x) sum(is.na(x)))


nrow(project)
names(project)


library(caTools)
require(caTools)
set.seed(101) 
sample = sample.split(project$Customer_Lifetime_Value, SplitRatio = .70)
development = subset(project, sample == TRUE)
validation  = subset(project, sample == FALSE)

fit<- lm(Customer_Lifetime_Value ~ Customer+ State + Response + Coverage+ Education +	
Effective.To.Date+ EmploymentStatus+ Gender+ Income+ Location.Code+
Marital.Status+ Monthly.Premium.Auto+  Months.Since.Last.Claim+ Months.Since.Policy.Inception+
Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+
Renew.Offer.Type+ Sales.Channel+  Total.Claim.Amount+ Vehicle.Class+
Vehicle.Size, data=development)
summary(fit)


fit<- lm(Customer_Lifetime_Value ~ Coverage+ Effective.To.Date+
EmploymentStatus+ Income+ Monthly.Premium.Auto+ Number.of.Open.Complaints+
Number.of.Policies+ Renew.Offer.Type+ Vehicle.Class, data=development)
summary(fit)

fit<- lm(Customer_Lifetime_Value ~  I(Effective.To.Date == "27-01-2011")+ I(EmploymentStatus == "Medical Leave")+
I(EmploymentStatus == "Retired")+ I(EmploymentStatus == "Unemployed")+Income+ 
Monthly.Premium.Auto+ Number.of.Open.Complaints+
Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class == "Luxury SUV")+ I(Vehicle.Class == "Luxury Car")
+I(Vehicle.Class == "SUV"), data=development)
summary(fit)

vif(fit)

fit<- lm(Customer_Lifetime_Value ~  I(Effective.To.Date == "27-01-2011")
+Income+ Number.of.Open.Complaints+
Number.of.Policies+ Renew.Offer.Type+ I(Vehicle.Class == "Luxury SUV")+
I(Vehicle.Class == "Luxury Car")+I(Vehicle.Class == "SUV"), data=development)
summary(fit)

vif(fit)

dwt(fit)

bptest(fit)

resids <- fit$residuals

ad.test(resids)

fitted(fit)

development$clv_pred<- fitted(fit)

attach(development)
(sum((abs(Customer_Lifetime_Value-clv_pred))/Customer_Lifetime_Value))/nrow(development)
write.csv(development, "part10.csv")

################################ validation part ##############################

fit2<- lm(Customer_Lifetime_Value ~ Customer+ State + Response + Coverage+ Education +	
Effective.To.Date+ EmploymentStatus+ Gender+ Income+ Location.Code+
Marital.Status+ Monthly.Premium.Auto+  Months.Since.Last.Claim+ Months.Since.Policy.Inception+
Number.of.Open.Complaints+ Number.of.Policies+ Policy.Type+ Policy+
Renew.Offer.Type+ Sales.Channel+  Total.Claim.Amount+ Vehicle.Class+
Vehicle.Size, data=validation)
summary(fit2)

fit2<- lm(Customer_Lifetime_Value ~ I(Coverage == "Premium")+
I(Effective.To.Date == "03-01-2011")+I(Effective.To.Date == "18-01-2011")+
I(Effective.To.Date == "24-02-2011")
+I(Effective.To.Date == "25-01-2011")+
I(Effective.To.Date == "25-02-2011")+I(Effective.To.Date == "29-01-2011")+
Income+Monthly.Premium.Auto+Number.of.Policies+
Renew.Offer.Type+ Vehicle.Class,
data=validation)
summary(fit2)


fit2<- lm(Customer_Lifetime_Value ~ I(Coverage == "Premium")+
I(Effective.To.Date == "03-01-2011")+I(Effective.To.Date == "18-01-2011")+
I(Effective.To.Date == "24-02-2011")
+I(Effective.To.Date == "25-01-2011")+
I(Effective.To.Date == "25-02-2011")+I(Effective.To.Date == "29-01-2011")+
Income+ Monthly.Premium.Auto+ Number.of.Policies+ Renew.Offer.Type+
I(Vehicle.Class == "Luxury SUV")+ I(Vehicle.Class == "Luxury Car"), data=validation)
summary(fit2)

vif(fit2)

fitted(fit2)

validation$clv_pred2<- fitted(fit2)

attach(validation)
(sum((abs(Customer_Lifetime_Value-clv_pred2))/Customer_Lifetime_Value))/nrow(validation)
write.csv(validation, "part20.csv")