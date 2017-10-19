##
##
submission <- lapply(submission, function(x) as.numeric(x) - 1) 
prob   <- Reduce("+",submission) / length(submission)
d      <- 0.7
label  <- (1*(prob>d)) 

results <- read.csv("Titanic/data/submission.csv",stringsAsFactors = F)
results$Survived <- label
write.csv(results,"answer submission.csv", row.names = F)
