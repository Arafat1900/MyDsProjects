load(file = "killersandmotives.Rdata")

View(killersandmotives)

x= 14

createsample(14)

View(mysample)

save(mysample, file = 'killersandmotives_ara.Rdata')
save(mysample, file = 'mysample.Rdata')
save(mysample_cleaned, file = 'mysample_cleaned.Rdata')
nrow(mysample_cleaned)

summary(mysample)

# Data cleaning: 
# Use R to clean the mysample data set by removing any rows with missing observations for either age at first kill (missing numerical values are recorded 
# as 99999) or motive (missing motives are recorded as NA). Also remove any rows containing the data of killers who first killed before the year 1900. In your report, you
# should note the number (and percentage) of observations that you removed for each reason. Add a new variable to the data set, called CareerDuration, defined as number 
# of years between first and last kill. (See the Week 1 practical class.)

nrow(mysample[mysample$AgeFirstKill == 99999, ]) # 9 records with missing data for age of first kill
nrow(mysample[is.na(mysample$Motive), ]) # 6 records with missing data for motives
nrow(mysample[(mysample$YearBorn + mysample$AgeFirstKill) < 1900, ]) # 8 records for killers who killed before the 1900
nrow(mysample) # Total number of records before data cleaning in mysample - 566

nrow(mysample[mysample$AgeFirstKill == 99999 | is.na(mysample$Motive) | (mysample$YearBorn + mysample$AgeFirstKill) < 1900,  ]) # Total number of 
# of filter data based on the conditions provided above

#Data cleaning step by step

mysample_cleaned <- mysample[which(mysample$AgeFirstKill != 99999),  ]
mysample_cleaned <- mysample_cleaned[!is.na(mysample_cleaned$Motive),  ]
mysample_cleaned <- mysample_cleaned[(mysample_cleaned$YearBorn + mysample_cleaned$AgeFirstKill) >= 1900, ]

# Verifying the dataset mysample_cleaned

nrow(mysample_cleaned[mysample_cleaned$AgeFirstKill == 99999 | is.na(mysample_cleaned$Motive) | (mysample_cleaned$YearBorn + mysample_cleaned$AgeFirstKill) < 1900,  ])

nrow(mysample_cleaned) # count in mysample_cleaned 543

mysample_cleaned$CareerDuration <-  mysample_cleaned$AgeLastKill - mysample_cleaned$AgeFirstKill # Adding a new variable CareerDuration

summary(mysample_cleaned)

mysample_cleaned[mysample_cleaned$CareerDuration != (mysample_cleaned$AgeLastKill - mysample_cleaned$AgeFirstKill), ]

nrow(is.na(mysample_cleaned$AgeFirstKill))
nrow(is.na(mysample_cleaned$AgeLastKill))
nrow(is.na(mysample_cleaned$CareerDuration))

##########Data exploration:#####################

save(mysample_cleaned, file = "mysample_cleaned.Rdata")

# Numerically and graphically summarise the distribution of three variables: age at first kill, age at last kill, and career duration

summary(mysample_cleaned)
View(mysample_cleaned)

#---Moment based summaries---#

mean(mysample_cleaned$AgeFirstKill) # Output ~ 29.70902
mean(mysample_cleaned$AgeLastKill) # Output ~ 32.78821
mean(mysample_cleaned$CareerDuration) # Output ~ 3.07919

sd(mysample_cleaned$AgeFirstKill) # Output ~ 9.036828
sd(mysample_cleaned$AgeLastKill) # Output ~ 10.76524
sd(mysample_cleaned$CareerDuration) # Output ~ 6.15087

sd(mysample_cleaned$AgeFirstKill)^2 # Sample Variance ~ 81.66425
sd(mysample_cleaned$AgeLastKill)^2 # Sample Variance ~ 115.8905
sd(mysample_cleaned$CareerDuration)^2 # Sample Variance ~ 37.8332


#---Quantile based summaries---#

quantile(mysample_cleaned$AgeFirstKill, type = 1) #  13   23   28   35   75 
quantile(mysample_cleaned$AgeLastKill, type = 1) # 15   25   30   39   77 
quantile(mysample_cleaned$CareerDuration, type = 1) # 0    0    0    3   39 

IQR(mysample_cleaned$AgeFirstKill, type = 1) # 12
IQR(mysample_cleaned$AgeLastKill, type = 1) # 14
IQR(mysample_cleaned$CareerDuration, type = 1) # 3

#---Graphical Summaries---#

boxplot(mysample_cleaned$AgeFirstKill, mysample_cleaned$AgeLastKill, mysample_cleaned$CareerDuration)

par(mfrow = c(1,3))
hist(mysample_cleaned$AgeFirstKill, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.06))
hist(mysample_cleaned$AgeLastKill, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.06))
hist(mysample_cleaned$CareerDuration, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.2))


cor(mysample_cleaned$AgeFirstKill, mysample_cleaned$AgeLastKill) # 0.8209063

cor(mysample_cleaned$AgeFirstKill, mysample_cleaned$CareerDuration) # -0.03244588

cor(mysample_cleaned$AgeLastKill, mysample_cleaned$CareerDuration) #0.5441272


plot(mysample_cleaned$AgeFirstKill, mysample_cleaned$AgeLastKill)

plot(mysample_cleaned$AgeFirstKill, mysample_cleaned$CareerDuration)

nrow(mysample_cleaned)

############## Modelling #################

## Age of First Kill ####
hist(mysample_cleaned$AgeFirstKill, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.06)) # 29.70902

mean(mysample_cleaned$AgeFirstKill) # 29.70902
sd(mysample_cleaned$AgeFirstKill) # 9.036828

x <- seq(from = min(mysample_cleaned$AgeFirstKill), to = max(mysample_cleaned$AgeFirstKill), by = 0.1) 

lines(x, dnorm(x, mean = 29.70902, sd = 9.036828), lwd = 2, col = "blue")


## Age of last Kill #####

hist(mysample_cleaned$AgeLastKill, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.06))

mean(mysample_cleaned$AgeLastKill) #32.78821
sd(mysample_cleaned$AgeLastKill) #10.76524

x <- seq(from = min(mysample_cleaned$AgeLastKill), to = max(mysample_cleaned$AgeLastKill), by = 0.1) 

lines(x, dnorm(x, mean = 32.78821, sd = 10.76524), lwd = 2, col = "blue")

### Career Duration ####

hist(mysample_cleaned$CareerDuration, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.2))

x <- seq(from = min(mysample_cleaned$CareerDuration), to = max(mysample_cleaned$CareerDuration), by = 0.1) 

mean(mysample_cleaned$CareerDuration) #3.07919
sd(mysample_cleaned$CareerDuration) #6.15087

# estimating lambda(rate) by method of moments rate = 1/3.07919 = 0.3247

lines(x, dexp(x, rate = 0.3247), lwd = 2, col = "blue")

########## Estimation ###########

# Age of first kill


hist(mysample_cleaned$AgeFirstKill, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.06)) # 29.70902

mean(mysample_cleaned$AgeFirstKill) # 29.70902
sd(mysample_cleaned$AgeFirstKill) # 9.036828

sd(mysample_cleaned$AgeFirstKill)^2 # 81.66425

# Estimating the mu - mean

mu      <- mean(mysample_cleaned$AgeFirstKill)
sigma   <- sd(mysample_cleaned$AgeFirstKill)

muhat1  <- rep(NA, 1000)
muhat2  <- rep(NA, 1000)

for(i in 1:1000){
  
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type = 1)[3]   
  
}

par(mfrow = c(1, 1))

hist(muhat1, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat1), col = "blue", lty = 2, lwd = 3)

hist(muhat2, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat2), col = "blue", lty = 2, lwd = 3)

# Estimating the sigma 

sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)

for(i in 1:1000){
  
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (9/10)*sd(x)^2   
}


par(mfrow = c(1, 2))

hist(sigma2hat1, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat1), col = "blue", lty = 2, lwd = 3)

hist(sigma2hat2, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat2), col = "blue", lty = 2, lwd = 3)


# Age of last kill

hist(mysample_cleaned$AgeLastKill, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.06))

mean(mysample_cleaned$AgeLastKill) #32.78821
sd(mysample_cleaned$AgeLastKill) #10.76524
sd(mysample_cleaned$AgeLastKill)^2 #115.8905

# Estimating the mu - mean

mu      <- mean(mysample_cleaned$AgeLastKill)
sigma   <- sd(mysample_cleaned$AgeLastKill)

muhat1  <- rep(NA, 1000)
muhat2  <- rep(NA, 1000)

for(i in 1:1000){
  
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  muhat1[i] <- mean(x)
  muhat2[i] <- quantile(x, type = 1)[3]   
  
}

par(mfrow = c(1, 2))

hist(muhat1, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat1), col = "blue", lty = 2, lwd = 3)

hist(muhat2, xlim = range(c(muhat1, muhat2)))
abline(v = mu, col = "red3", lwd = 3)
abline(v = mean(muhat2), col = "blue", lty = 2, lwd = 3)

# Estimating the sigma 

sigma2hat1 <- rep(NA, 1000)
sigma2hat2 <- rep(NA, 1000)

for(i in 1:1000){
  
  x <- rnorm(n = 10, mean = mu, sd = sigma)
  
  sigma2hat1[i] <- sd(x)^2
  sigma2hat2[i] <- (9/10)*sd(x)^2   
}


par(mfrow = c(1, 2))

hist(sigma2hat1, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat1), col = "blue", lty = 2, lwd = 3)

hist(sigma2hat2, xlim = range(c(sigma2hat1, sigma2hat2)))
abline(v = sigma^2, col = "red3", lwd = 3)
abline(v = mean(sigma2hat2), col = "blue", lty = 2, lwd = 3)

# Career duration 

hist(mysample_cleaned$CareerDuration, freq = FALSE, xlim = c(0, 80), ylim = c(0, 0.2))

x <- seq(from = min(mysample_cleaned$CareerDuration), to = max(mysample_cleaned$CareerDuration), by = 0.1) 

mean(mysample_cleaned$CareerDuration) #3.07919
sd(mysample_cleaned$CareerDuration) #6.15087
1/mean(mysample_cleaned$CareerDuration)

# estimating lambda(rate) by method of moments rate = 1/3.07919 = 0.3247
n <- 10

lambda <- 1/mean(mysample_cleaned$CareerDuration)

estimates <- rep(NA, 1000)   # Empty vector to store estimates.

for(i in 1:1000){
  
  x <- rexp(n, lambda)       # Sample in the i-th experiment. 
  
  lambdahat <- 1/mean(x)     # Estimate in the i-th experiment.
  
  estimates[i] <- lambdahat  # Store the lambdahat in our vector.
}

hist(estimates, breaks = "FD", freq = FALSE)   

abline(v = mean(estimates), col = "blue")

abline(v = lambda, col = "red")

####### Hypothesis Testing ########
unique(mysample_cleaned$Motive)

Motives_AngelofDeath <- mysample_cleaned[mysample_cleaned$Motive == "Angel of Death", ]
Motives_Convenience <- mysample_cleaned[mysample_cleaned$Motive == "Convenience (didn't want children/spouse)", ]
Motives_Robbery <- mysample_cleaned[mysample_cleaned$Motive == "Robbery or financial gain", ]


nrow(Motives_AngelofDeath) # 23
nrow(Motives_Convenience) # 10
nrow(Motives_Robbery) # 510

summary(Motives_AngelofDeath$AgeFirstKill)
summary(Motives_Convenience$AgeFirstKill)
summary(Motives_Robbery$AgeFirstKill)

### Normality check using Chi-squared goodness of fit test

pearson.test(Motives_AngelofDeath$AgeFirstKill)
pearson.test(Motives_Convenience$AgeFirstKill)
pearson.test(Motives_Robbery$AgeFirstKill)

### Z test for hypothesis testing of the null hypothesis that the average age of first kill accross motives is 27

Z_AOD <- z.test(x = Motives_AngelofDeath$AgeFirstKill, mu = 27, sigma.x = 74^(1/2), conf.level = 0.95)
Z_CNV <- z.test(x = Motives_Convenience$AgeFirstKill, mu = 27, sigma.x = 74^(1/2), conf.level = 0.95)
Z_ROB <- z.test(x = Motives_Robbery$AgeFirstKill, mu = 27, sigma.x = 74^(1/2), conf.level = 0.95)


### Two sample hypothesis test

t.test(x = Motives_AngelofDeath$AgeFirstKill, y = Motives_Convenience$AgeFirstKill, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

t.test(x = Motives_AngelofDeath$AgeFirstKill, y = Motives_Robbery$AgeFirstKill, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

t.test(x = Motives_Convenience$AgeFirstKill, y = Motives_Robbery$AgeFirstKill, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)


sd(Motives_AngelofDeath$AgeFirstKill) / sd(Motives_Convenience$AgeFirstKill)

sd(Motives_AngelofDeath$AgeFirstKill) / sd(Motives_Robbery$AgeFirstKill)

sd(Motives_Convenience$AgeFirstKill) / sd(Motives_Robbery$AgeFirstKill)




