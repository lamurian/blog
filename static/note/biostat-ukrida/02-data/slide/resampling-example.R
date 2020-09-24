## PACKGES

library("ggplot2")  # Plotting


## FUNCTIONS

dice <- function(n) { # Roll the dice n times
	sample(1:6, n, replace=TRUE, prob=rep(1/6, 6))
}

epsilon <- function(p.hat, n) { # Calculate error
	sqrt({p.hat * (1-p.hat)}/n)
}


## INITIATE

# Set number of rolls
roll <- c(10, 100, 1000, 10000, 100000, 1000000, 10000000)

# Roll the dice with n \in roll
prob <- sapply(roll, function(n) {
	set.seed(1); roll <- dice(n)
	sum(roll==4) / length(roll)
})

# Concatenate roll and probability as a data frame
df <- data.frame(list("roll"=roll, "prob"=prob))

# Calculate error
df$error <- mapply(function(p.hat, n) {
	epsilon(p.hat, n)
}, p.hat=df$prob, n=df$roll)


## PLOT

# Probability with different number of rolls
ggplot(df, aes(x=log(roll, 10), y=prob)) +
	geom_hline(yintercept=1/6, linetype=2, color="red") + geom_smooth() + geom_point(size=3) +
	theme_minimal() + ylim(0, 0.3) + geom_text(aes(label=prob), vjust=2) +
	annotate("text", label="Actual probability", x=2, y=0.16, color="red") +
	labs(title="Dice-rolling experiment", x="Log of trials", y="Probability")

# Error with different number of rolls
ggplot(df, aes(x=log(roll, 10), y=error)) + geom_smooth() + geom_point(size=3) +
	theme_minimal() + ylim(0, 0.1) + geom_text(aes(label=round(error, 4)), vjust=-2) +
	labs(title="Dice-rolling experiment", x="Log of trials", y="Error")
