#stackloss

#medii
print("Medii")
mean(stackloss$Air.Flow)
mean(stackloss$Water.Temp)
mean(stackloss$Acid.Conc.)
mean(stackloss$stack.loss)

#variatii
print("Variatii")
var(stackloss$Air.Flow)
var(stackloss$Water.Temp)
var(stackloss$Acid.Conc.)
var(stackloss$stack.loss)

#boxplots
print("Boxplots")
boxplot(stackloss$Air.Flow, main="Air Flow", sub=paste("Outlier rows: ", boxplot.stats(stackloss$Air.Flow)$out))  # box plot for 'speed'
boxplot(stackloss$Water.Temp, main="Water Temp", sub=paste("Outlier rows: ", boxplot.stats(stackloss$Water.Temp)$out)) #water temp
boxplot(stackloss$Acid.Conc., main="Acid Conc.", sub=paste("Outlier rows: ", boxplot.stats(stackloss$Acid.Conc.)$out)) #acid conc.
boxplot(stackloss$stack.loss, main="Stack loss", sub=paste("Outlier rows: ", boxplot.stats(stackloss$stack.lossp)$out)) #stack loss

#quartile
print("Quartile")
quantile(stackloss$Air.Flow)
quantile(stackloss$Water.Temp)
quantile(stackloss$Acid.Conc.)
quantile(stackloss$stack.loss)

par(mfrow=c(2, 2))  # divide graph area in 4 columns
plot(density(stackloss$Air.Flow), main="Density Plot: Air Flow", ylab="Frequency")  # density plot for 'Air Flow'
polygon(density(stackloss$Air.Flow), col="red")
plot(density(stackloss$Water.Temp), main="Density Plot: Water Temp", ylab="Frequency")  # density plot for 'Water Temp'
polygon(density(stackloss$Water.Temp), col="red")
plot(density(stackloss$Acid.Conc.), main="Density Plot: Acid Conc", ylab="Frequency")  # density plot for 'Acid Conc'
polygon(density(stackloss$Acid.Conc.), col="red")
plot(density(stackloss$stack.loss), main="Density Plot: Stack Loss", ylab="Frequency")  # density plot for 'Stack Loss'
polygon(density(stackloss$stack.loss), col="red")

cor(stackloss$Air.Flow, stackloss$stack.loss)  # corelatie Air.Flow, stack.loss

cor(stackloss$Water.Temp, stackloss$stack.loss)  # corelatie Water.Temp, stack.loss

cor(stackloss$Acid.Conc, stackloss$stack.loss)  # corelatie Acid.Conc, stack.loss

scatter.smooth(x=stackloss$Air.Flow, y=stackloss$stack.loss, main="Air Flow ~ Stack Loss")

scatter.smooth(x=stackloss$Water.Temp, y=stackloss$stack.loss, main="Water Temp ~ Stack Loss")

scatter.smooth(x=stackloss$Acid.Conc, y=stackloss$stack.loss, main="Acid Conc ~ Stack Loss")

linearMod <- lm(stack.loss ~ Air.Flow, data=stackloss)
print(linearMod)
summary(linearMod)

linearMod2 <- lm(stack.loss ~ Water.Temp + Air.Flow, data=stackloss)
print(linearMod2)
summary(linearMod2)

AIC(linearMod)
AIC(linearMod2)
BIC(linearMod)
BIC(linearMod2)

set.seed(12)
y <- rpois(length(stackloss$stack.loss), mean(stackloss$stack.loss))
plot(density(y), main="Density Plot: Poisson", ylab="Frequency")
polygon(density(y), col="red")

my.stackloss <- stackloss
my.stackloss['Poisson'] = y
my.stackloss

T <- rt(1000, 100)
hist(T, 30)

set.seed(1)
par(mfrow = c(2, 1))
t <- seq(0, 10, 0.25)
f <- dt(t, 10, 5)
plot(t, f, col = "darkblue")
F <- pt(t, 10, 5)
plot(t, F, col = "darkred")
