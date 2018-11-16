
heli <- read.csv("C:/Users/sonam/Desktop/heli.csv")
View(heli)


#plot the data using different colours for each tree
plot(Velocity~Load, pch = as.character(Tree), col = Tree+1, data = heli)
#There is a linear relationship between velocity and disk loading

#(b)
#We test for presence of interaction between disk loading and tree
heli$Tree <- factor(heli$Tree)
addmodel <- lm(Velocity ~ Load + Tree, data = heli)
fullmodel <- lm(Velocity ~ Load + Tree + Load * Tree, data = heli)
anova(addmodel, fullmodel)
#We do not reject the null hypothesis but the pvalue is on the border

#(c)
#Using backward elimination for feature selection in interaction model
drop1(fullmodel, scope=~., test = 'F')
#We do not drop the Tree variable until we drop the interaction effect of Load and Tree
#So we keep tree and remove the interaction term and continue
fullmodel2 <- lm(Velocity~Load + Tree, data = heli)
drop1(fullmodel2, scope=~., test = 'F')
fullmodel3 <- lm(Velocity~Load, data = heli)
drop1(fullmodel3, scope=~., test = 'F')
finalmodel <- lm(Velocity~Load, data = heli)
finalmodel

#(d)
plot(Velocity ~ Load, pch=as.character(Tree), col=Tree+1, data=heli)
abline(finalmodel)
for (i in 1:3){
  with(heli, lines(Load[Tree==i], fitted(fullmodel)[Tree==i], col=i+1))
}

#(e)
summary(fullmodel)
library(car)
linearHypothesis(fullmodel, c(1,0.2,1,0,0.2,0), 1)
#We reject the null hypothesis at 5% level of significance
