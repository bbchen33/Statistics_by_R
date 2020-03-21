# Generate a matrix with random housing prices in SF and LA and compare if there is a difference (p < 0.05) between the average housing prices.
housing_prices = matrix(, nrow = 150, ncol = 2)
housing_prices[,1] = sample(10:100,150,replace=T)
housing_prices[,2] = sample(10:100,150,replace=T)
colnames(housing_prices) = c("SF", "LA")

# determine the means
SF_mean = mean(housing_prices[,1])
LA_mean = mean(housing_prices[,2])
actual_mean_difference = SF_mean - LA_mean
combined = c(housing_prices[,1], housing_prices[,2])
shuffled = sample(combined, replace = F)
hist(combined)
hist(shuffled)
# The two histograms look exactly the same so we are confident that no data point is replaced
test_means = rep(NA,10000)
for (i in 1:10000) {
  new_shuffled = sample(combined, replace = F)
  test_means[i] = mean(new_shuffled[1:70]) - mean(new_shuffled[71:150])
}
# plot histogram to see how the observed mean difference is relative to the reshuffled 10000 tests
hist(test_means)
abline(v = actual_mean_difference, col = 'red', lty=2)
abline(v = -actual_mean_difference, col = 'red', lty=2)

# calculate p value
right_side = sum(test_means >= abs(actual_mean_difference))
left_side = sum(test_means <= -abs(actual_mean_difference))
p_val = (right_side + left_side)/10000

