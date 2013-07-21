### 20130710_Homework01.R by dexlau@gmail.com (Dexter Lau) ###

# Load the data from train.csv
train <- read.csv('train.csv', header=T)

# Fit data to a linear model
# From the start, remove obviously irrelevant features: unique passanger and ticket identifiers and intercept
linear.fit <- lm(Survived ~ . -Name -PassengerId -Ticket +0, data=train)
print("Initial linear model $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# Take a look at train$Cabin and see how clean it is
as.matrix(train$Cabin)
sum(train$Cabin!="")
sum(train$Cabin=="")

# Cabin may cause the model to be too highly fitted, so let's remove it from the model. 
# Note adjusted R^2 decreases slightly.
linear.fit <- update(linear.fit, .~. -Cabin)
print("Remove Cabin $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# Let's try using the deck (first letter of the cabin), and see if that helps.
# Note adjusted R^2 increases slightly.
train$Deck <- as.factor(substr(train$Cabin,1,1))
linear.fit <- update(linear.fit, .~. +train$Deck)
print("Add Deck $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# The next feature set with a low t-value is Embarked
# Note adjusted R^2 increases slightly.
linear.fit <- update(linear.fit, .~. -Embarked)
print("Remove Embarked $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# The next feature set with a low t-value is Parch (Parent, Children)
# Note adjusted R^2 increases slightly.
linear.fit <- update(linear.fit, .~. -Parch)
print("Remove Parch $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# Pclass is currently a continuous value. It should be discreet values (either 1, 2, or 3)
# Let's remove it and replace it as.factor
linear.fit <- update(linear.fit, .~. -Pclass + as.factor(train$Pclass))
print("Pclass as.factor $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# The next lowest t-value is fare.
# Note adjusted R^2 decreases slightly.
linear.fit <- update(linear.fit, .~. -Fare)
print("Remove Fare $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# Add fare back in.
linear.fit <- update(linear.fit, .~. +Fare)
print("Add Fare $adj.r.squared:")
print(summary(linear.fit)$adj.r.squared)
readline("Press return to continue")

# That's about as best we can do without decreasing $adj.r.squared more

# Let's try lm.ridge now, with the same coefficients
library(MASS)
ridge <- lm.ridge(Survived ~ Sex + Age + SibSp + train$Deck + as.factor(train$Pclass) + Fare + 0, data=train)

# The coefficients for both lm and lm.ridge look the same for this feature set
print("coef(linear.fit):")
print(coef(linear.fit))
print("coef(ridge):")
print(coef(ridge))
