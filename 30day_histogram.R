outcome <- read.csv("outcome-of-care-measures.csv")

#A brief overview of the data

head(outcome)

#calculates the no. of rows in the data

no.rows <- nrow(outcome)

#calculates the no. of columns in the data

no.cols <- ncol(outcome)

#converts character to numeric

outcome[, 11] <- as.numeric(outcome[, 11])

#makes a simple histogram

hist(outcome[, 11])