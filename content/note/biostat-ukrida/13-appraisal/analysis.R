## BRIEF INTRODUCTION

# Assigning a variable
x <- "This is a variable"

# Or you can use `=`, they work the same
y = "This is also a variable"

# Mathematical operation
3 + 4

# Assign a variable to contain your result
z <- 3 + 4
z

# Using a function
print(z)

# Concatenating objects
c(x, y)

# A range of integers
-3 : 3

# Range of real numbers
seq(from=-3, to=3, by=0.1)

# Implicit arguments when using a function
seq(-3, 3, 0.1)

# List registered objects
ls()

# Remove a registered object
rm("x")

# Remove ALL registered objects
rm(list=ls())


## LOAD PACKAGES

# Installing a package
install.packages("magrittr")

# Install multiple packages
install.packages(c("magrittr", "ggplot2", "rstatix", "epitools"))

# Here's one way to load your packages
library("magrittr")
library("ggplot2")

# But if you have a lot of package to load, it's easier to maintain this way
pkgs <- c("magrittr", "ggplot2")
pkgs.load <- sapply(pkgs, library, character.only=TRUE)


## PREPARATION

# Load the data frame
tbl <- read.csv("heart.csv")

# See the structure of our data frame
str(tbl)

# Set factors to a column
tbl$gender <- factor(tbl$sex)

# Set levels for each factor
levels(tbl$gender) <- c("Female", "Male")

# Same operation as above, but using a pipeline `%>%`
tbl$gender <- ifelse(tbl$sex==0, "Female", "Male") %>%
    factor(levels=c("Female", "Male"))

# Create a diagnosis column
tbl$diagnosis <- ifelse(tbl$target==0, "Healthy", "Heart Disease") %>%
    factor(levels=c("Healthy", "Heart Disease"))

# High glucose detected
tbl$high.glucose <- ifelse(tbl$fbs==0, "Low", "High") %>%
    factor(levels=c("Low", "High"))

# Angina detected
tbl$angina <- ifelse(tbl$exang==0, "No", "Yes") %>%
    factor(levels=c("No", "Yes"))


## STATISTICS

# Test of proportional difference
table(tbl$diagnosis, tbl$gender) %>% chisq.test(correct=TRUE)

# Same as above, but with more complete results
table(tbl$gender, tbl$diagnosis) %>% epitools::riskratio(correct=TRUE)

# Get all categoric IVs
iv.cat <- subset(tbl, select=c(gender, angina, high.glucose))

# Loop over categoric IV
res.prop <- lapply(iv.cat, function(iv) {
    table(iv, tbl$diagnosis) %>% epitools::riskratio(correct=TRUE) 
})

# Get all numeric IVs
iv.num <- subset(tbl, select=c(
    age, trestbps, thalach, oldpeak
))

# Check normality for all numeric IVs
res.norm <- lapply(iv.num, function(iv) {
    tapply(iv, tbl$diagnosis, shapiro.test)
})

# Check homogeneity of variance
res.homvar <- lapply(iv.num, function(iv) {
    car::leveneTest(iv, tbl$diagnosis)
})

# Perform a T-Test on trestbps
rstatix::t_test(trestbps ~ diagnosis, data=tbl, var.equal=TRUE)

# Perform non-parametric test
res.md <- lapply(iv.num, function(iv) {
    wilcox.test(iv ~ tbl$diagnosis, conf.int=TRUE)
})

# Measure effect size
effsize.md <-lapply(iv.num, function(iv) {
    tbl <- data.frame(list(iv=iv, group=tbl$diagnosis))
    rstatix::wilcox_effsize(iv ~ group, data=tbl)
})

# Make an ANOVA model
res.aov <- lapply(iv.num, function(iv) {
    aov(iv ~ tbl$diagnosis)
})

# Logistic regression
res.log <- glm(diagnosis ~ age + gender + trestbps + chol + thalach + angina,
    data=tbl, family=binomial(link="logit")
)
