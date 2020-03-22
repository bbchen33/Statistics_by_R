# One-sample boostrap to determine 95% confidence interval for the mean of housing prices in SF
# Generate a matrix with housing prices in SF
housing_prices = sample(10:100, 150, replace = T)

# What is the mean of housing prices in SF
shuffled = sample(housing_prices, replace = T)
hist(housing_prices, 20)
hist(shuffled, 20)
# The histograms have different distributions so we know the data have been randomly selected with replacement.
observed_mean = mean(housing_prices)
test_means = rep(NA,10000)
for (i in 1:10000) {
  test_means[i] = mean(sample(housing_prices, replace = T))
}

hist(test_means,30)
mean_of_all_means = mean(test_means) # It's pretty much the same as the observed mean because of 10,000 times of shuffling. The new mean is still pretty close to the observed mean.
sorted_test_means = sort(test_means)
# Confidence interval, 5% split evenly on from the lowest mean and highest mean
# 2.5% from the lowest mean and 2.5% from the highest mean
Left_CI = round(sorted_test_means[0.025*10000],2)
Right_CI = round(sorted_test_means[0.975*10000],2)
# Blue line as the mean and two red dashed lines to show where 95% confidence intervals are
abline(v = observed_mean, col = 'blue')
abline(v = Left_CI, col = 'red', lty=2)
abline(v = Right_CI, col = 'red', lty=2)
sprintf("The average housing prices in SF is %g, with 95%% CI: %g to %g", observed_mean, Left_CI, Right_CI)

## Now I want to do boostrap to determine how big the difference is between housing prcies in SF and LA
# 2-sample bootstrap
new_housing_prices = matrix(, nrow = 150, ncol = 2)
new_housing_prices[,1] = sample(10:100,150,replace=T)
new_housing_prices[,2] = sample(30:300,150,replace=T)
colnames(new_housing_prices) = c("2000", "2010")
mean_2000 = mean(new_housing_prices[,1])
mean_2010 = mean(new_housing_prices[,2])
actual_mean_difference = mean_2000 - mean_2010
shuffled_mean_difference = rep(NA, 10000)
for (i in 1:10000) {
  shuffled_mean_difference[i] = mean(sample(new_housing_prices[,1], replace = T)) - mean(sample(new_housing_prices[,2], replace = T))
}
sort_difference = sort(shuffled_mean_difference)
Left_CI_difference = round(sort_difference[0.025*10000],2)
Right_CI_difference = round(sort_difference[0.975*10000],2)
sprintf("The difference in the housing prices in 2000 and 2010 is %g, with 95%% CI: %g to %g", actual_mean_difference, Left_CI_difference, Right_CI_difference)
