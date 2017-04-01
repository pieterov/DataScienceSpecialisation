set.seed(16091970)
mean_vector <- NULL
sd_vector <- NULL
sim_vector <- NULL

for (i in 1:1000) {
        sim <- rexp(40,0.2)
        mean_vector <- c(mean_vector, mean(sim))
        sd_vector <- c(sd_vector, sd(sim))
        sim_vector <- cbind(sim_vector, sim)
}

par(mfrow = c(1, 2))
hist(mean_vector,25, xlab="Sample Means", main="Distribution of 1,000 sample means")
abline(v = mean(mean_vector), col = "red", lwd = 3)

hist(sim_vector,25, xlab="Sample values", main="Distribution of 40,000 samples")
abline(v = mean(mean_vector), col = "red", lwd = 3)


#par(mfrow = c(1, 2))
#qqnorm(mean_vector, main="Normal Q-Q Plot of 1,000 sample means")
#qqline(mean_vector)

#qqnorm(sim_vector, main="Normal Q-Q Plot of 40,000 samples")
#qqline(sim_vector)


#par(mfrow = c(1, 2))
#hist(sd_vector,25, xlab="Sample Standard Deviations", main="Distribution of 1,000 sample sd's")
#abline(v = mean(sd_vector), col = "red", lwd = 3)

#qqnorm(sd_vector, main="Normal Q-Q Plot of 1,000 sample sd's")
#qqline(sd_vector)


