# Jen Dumiak
# November 10, 2016
# Logigstic Regression Training, IRS RAAS

# Potential packages to use here; load in the packages
library(aod)
library(ggplot2)
library(Rcpp)
library('corrplot') #package corrplot

#load in the data file 
loans.file <- read.csv("C:/Users/jdumiak/Desktop/R/loansData_clean.csv", header = TRUE)
# View the data file; can use head() to look at the first few rows
View(loans.file)

# Interest.Rate.Clean is our dependent variable for regression
# Need to convert to a dummy variable-- loop through data, if interest rate < 12 <- 0 if >= 12 <- 1
for (i in 1:length(loans.file$Interest.Rate.Clean))
{
  if(loans.file$Interest.Rate.Clean[i] < .12) {
    loans.file$Interest.Rate.Binary[i] <- 0
  } else {
    loans.file$Interest.Rate.Binary[i] <- 1
  }
}
loans.file$Interest.Rate.Binary <- factor(loans.file$Interest.Rate.Binary)
#look at the histogram of log transform
transform <- log(loans.file$Interest.Rate.Clean)
hist(transform, main="Log Transform Interest Rate Distribution", xlab="Log Transform Interest Rates", prob=TRUE) #not really normal but kind of 
# overlay normal distribution
x<-seq(min(transform),max(transform),length=201) 
curve(dnorm(x, mean=mean(transform), sd=sd(transform)), add=TRUE, lty=2, col='firebrick1',lwd=4)  

# look at correlation matrix of independent variables to see if there is any correlation between them
corrmat <- data.frame(Loan.Length = loans.file$Loan.Length.Clean, 
                      FICO.Score = loans.file$FICO.Score, 
                      Amount.Requested = loans.file$Amount.Requested)
correlation <- cor(corrmat)
pairs(corrmat) # gives a scatterplot of the data frame
png("file2.png", width = 480,height = 600)
corrplot(correlation, method = "circle", title="Correlation Between Regression Variables") #plot matrix
dev.off()

#all variables correlations are less < 90%, logistic assumption met

# Logistic regression
mylogit <- glm(Interest.Rate.Binary ~ Loan.Length.Clean + FICO.Score + Amount.Requested
               , data = loans.file, binomial(link='logit'))

# Interpret the results
# All Variables are statistically significant
# Look at the summary stats of logistic regression
mylogit
summary(mylogit)
str(summary(mylogit)) #structure of summary 

anova(mylogit, test="Chisq")
# the difference between the null deviance and residual deviance shows how our model is doing versus the null model. The greater the gap, the better 
#deviance drops each variable we add--deviance drops greatly when FICO Score is added

#No R2 here but we can use the McFaddens R2 to assess the model fit 
# value of .5779 is very good (usually .2-.4 is highly satisfactory for a model)
#can be interpreted as an approximate variance in the outcome accounted for by the model
library(pscl)
pR2(mylogit)
#For every one unit change in loan length, the log odds of interest >= 12 (versus < 12) increases by 0.114111.

# Look at probabilities, p = odds / 1 + odds

odds <- exp(coef(mylogit))
# odds ratios and 95% CI
CI <- exp(cbind(OR = coef(mylogit), confint(mylogit)))
#Loan length & amount requested > 1 i.e. odds increase, FICO < 1 odds decrease
# for a one unit (month) increase in loan length , the odds of having >= 12% interest rate (versus < 12) increase by a factor of 1.12
prob <- odds / (1 + odds)

# Measure of model fit is the significance of the overall model. This test asks whether the model with predictors fits significantly better than a model with just an intercept (i.e., a null model). The test statistic is the difference between the residual deviance for the model with predictors and the null model. The test statistic is distributed chi-squared with degrees of freedom equal to the differences in degrees of freedom between the current and the null model 
pvalue <- with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#pvalue of 0 aka our model fits better than the null model
# Residuals
resids <- residuals(mylogit, type="pearson")
pred <- predict(mylogit)

#Diagnostics 
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page 
plot(mylogit)
