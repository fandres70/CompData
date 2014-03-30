outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

# plot 30-day mortality rates for heart attack
#--------------------------------------------#
outcome[, 11] <- as.numeric(outcome[, 11])  ## col for heart attack mort.
hist(outcome[, 11], main="Heart Attack 30-day Death Rate", 
     xlab="30-day Death Rate")


# plot the 30-day mortality rates for heart attack, 
# heart failure, and pneumonia
#-------------------------------------------------#
outcome[, 17] <- as.numeric(outcome[, 17])  ## col for heart failure mort.
outcome[, 23] <- as.numeric(outcome[, 23])  ## col for pneumonia mort.

x.range = range(outcome[, c(11, 17, 23)], na.rm=TRUE)

par(mfrow=c(3, 1))
hist(outcome[, 11], xlab="30-day Death Rate", xlim=x.range,
     main="", freq=FALSE)
abline(v=median(outcome[, 11], na.rm=TRUE), col="red", lwd=2)
lines(density(outcome[, 11], na.rm=TRUE), col="blue")
title(main=substitute("Heart Attack 30-day Death Rate (" * bar(X)==mean * ")", 
                      list(mean=mean(outcome[, 11], na.rm=TRUE))))

hist(outcome[, 17], xlab="30-day Death Rate", xlim=x.range,
     main="", freq=FALSE)
abline(v=median(outcome[, 17], na.rm=TRUE), col="red", lwd=2)
lines(density(outcome[, 17], na.rm=TRUE), col="blue")
title(main=substitute("Heart Failure 30-day Death Rate (" * bar(X)==mean * ")", 
                      list(mean=mean(outcome[, 17], na.rm=TRUE))))

hist(outcome[, 23], xlab="30-day Death Rate", xlim=x.range,
     main="", freq=FALSE)
abline(v=median(outcome[, 23], na.rm=TRUE), col="red", lwd=2)
lines(density(outcome[, 23], na.rm=TRUE), col="blue")
title(main=substitute("Pneumonia 30-day Death Rate (" * bar(X)==mean * ")", 
                      list(mean=mean(outcome[, 23], na.rm=TRUE))))
par(mfrow=c(1, 1))


# plot 30-day death rates by state
#---------------------------------#
t <- table(outcome$State)
states.20 <- names(t[t >= 20]) ## list of state names with 20+ hospitals
rm(t)
outcome2 <- outcome[outcome$State %in% states.20, ]

# compute median death rate for each state and sort them
death.medians <- tapply(outcome2[, 11], outcome2$State, median, na.rm=TRUE)
death.medians <- sort(death.medians)

death <- outcome2[, 11]  ## heart attack death rate for subset

# compute names of states in format: AL (<count of obs>)
t2 <- table(outcome2$State)  ## vector is indexable by state name
states.20 <- paste(names(death.medians), " (", 
                   t2[names(death.medians)], ")", sep="")

# set order of states for boxplot
state <- ordered(outcome2$State, levels=names(death.medians))
levels(state) <- states.20  ## change labels of state factor
boxplot(death ~ state, las=2, cex.axis=0.6)
title(main="Heart Attack 30-day Death Rate by State", 
      ylab="30-day Death Rate")

# plot 30-day death rates and numbers of patients
#-----------------------------------------------#
hospital <- read.csv("hospital-data.csv", colClasses="character")
outcome.hospital <- merge(outcome, hospital, by="Provider.Number")

death <- as.numeric(outcome.hospital[, 11])  ## heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)

library(lattice)
xyplot(death ~ npatient | owner, main="Heart Attack 30-day Death Rate",
       xlab="Number of Patients Seen", ylab="30-day Death Rate",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(npatient, death)
         })


# finding the best hospital in a state
#------------------------------------#
