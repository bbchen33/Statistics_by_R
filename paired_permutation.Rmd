# Generate a matrix with random housing prices in LA
housing_prices = matrix(, nrow = 150, ncol = 2)
housing_prices[,1] = sample(10:100,150,replace=T)
housing_prices[,2] = sample(30:300,150,replace=T) # Notice that I change the range from 10-100 to 30-300 because everything seems expensive to me nowadays
colnames(housing_prices) = c("2000", "2010")
# Use boxplot to see how the data points are distributed
boxplot(housing_prices, xlab = 'year', ylab = 'thousand dollars', main = 'Mean housing prices in LA in 2000 and 2010 (fake)')

# determine the observed mean from collected data
two_thousand_mean = mean(housing_prices[,1])
two_thousand_ten_mean = mean(housing_prices[,2])
actual_mean_difference = two_thousand_mean - two_thousand_ten_mean

# Run paired permutation test: only switch between 2000 and 2010, not within the group
test_means = rep(NA,10000)
for (i in 1:10000) {
  rows_to_flip = sample(1:150, 75, replace = F)
  housing_prices[rows_to_flip, 1:2] = housing_prices[rows_to_flip, 2:1]
  test_means[i] = mean(housing_prices[,1]) - mean(housing_prices[,2])
}


# plot histogram to see how the observed mean difference is relative to the reshuffled 10000 tests
hist(test_means, xlim= c(-150, 150), 30) # I made the x range wide because the observed mean difference is quite unlikely to be random (likely to be outside shuffled mean difference)
abline(v = actual_mean_difference, col= 'red', lty=2)
abline(v = -actual_mean_difference, col= 'red', lty=2)

# calculate p value
right_side = sum(test_means >= abs(actual_mean_difference))
left_side = sum(test_means <= -abs(actual_mean_difference))
p_val = (right_side + left_side)/10000
# As expected, the p value is very small or 0 because the difference is real
