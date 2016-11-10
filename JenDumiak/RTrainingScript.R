# Jen Dumiak
# November 9, 2016
# Linear Regression Training, IRS RAAS

#load in the data file 
loans.file <- read.csv("C:/Users/jdumiak/Desktop/R/loansData_clean.csv", header = TRUE)
#look at the data structure
str(loans.file)
View(loans.file)
#Interest.Rate.Clean is our dependent variable for regression

#look at the hist of the dependent variable-- is it approx normal?
hist(loans.file$Interest.Rate.Clean, main="Interest Rate Distribution", xlab="Interest Rates", prob=TRUE) #not really normal but kind of 
# overlay normal distribution
x<-seq(min(loans.file$Interest.Rate.Clean),max(loans.file$Interest.Rate.Clean),length=201)
curve(dnorm(x, mean=mean(loans.file$Interest.Rate.Clean), sd=sd(loans.file$Interest.Rate.Clean)), add=TRUE, lty=2,col='firebrick1',lwd=4)

# Basic multiple regression
# Monthly.Income not significant with three below also decreses R2
# Significant Loan.Length.Clean, FICO.Score, Amount.Requested, with these three the R2 is .745
#with Inquiries.in.the.Last.6.Months .755

reg1 <- lm(Interest.Rate.Clean ~ Loan.Length.Clean + FICO.Score + Amount.Requested + Inquiries.in.the.Last.6.Months 
           , data = loans.file)
reg1  # Gives the coefficients only
summary(reg1)  # Gives coefficient significance
str(summary(reg1)) #structure of summary 


# More detailed summaries
anova(reg1)
coef(reg1)  # Or coefficients(reg1)
confint(reg1)  # CI for coefficients
res <- resid(reg1)  # Residuals case-by-case
hist(residuals(reg1), main="Histogram Residuals", xlab="Residuals")  # Histogram of residuals

#overlay normal distribution 
plot(density(res),main="Residuals",lwd=5,type='l')
#Superimpose the normal distribution on density 
xfit <- seq(min(res),max(res),length=201) # generates a sequence of equally spaced points from min to max with length 201
yfit <- dnorm(xfit,mean=mean(res),sd=sd(res)) # dnorm computes the density for a normal distribution with mean and sd
lines(xfit,yfit,lty=2,col='firebrick1',lwd=3)

#Diagnostics 
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
plot(reg1)
