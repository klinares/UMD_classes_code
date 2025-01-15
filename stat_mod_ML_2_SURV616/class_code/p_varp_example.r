#Implement matrix calculations for the multinomial expected value and variance.
#Slides 29=30, Lecture 1.

#the data, Y
varieties<-c(773,231,238,59)
expected <- c(731.815, 243.9375, 243.9375, 81.3125)

trials <- sum(varieties)
#calculate p_hat, Y/n
p_hat<-varieties/trials

#Variance of p-hat, this is a matrix with variances on the diagonal and the off-diagonal elements are covariances
var_p<-(diag(p_hat) - p_hat %*% t(p_hat))/sum(varieties)


# to get probabilities of two 
p_hat[2]+p_hat[4]

# calculate variance for two probability estimates
# 1st prob variance
var_p[2,2] + 
# 2nd prob variance
  var_p[4,4] +
  (2 * ( -(p_hat[2]*p_hat[4]) / trials) )


p2_4 <- p_hat[2]+p_hat[4]

var_p2_4 <- (1-p2_4) * (p2_4/trials)


# create table with observed, expected, chi square, G2
varieties 
expected
chi_square <- ((varieties - expected)^2) / expected
LRT <- (2*varieties)*log(varieties/ (expected))

## Hypothesis Test
# calculate expected probabilities
exp_p <- expected/sum(expected)

cbind(varieties, p_hat, expected, exp_p, chi_square, LRT, exp_p)


# Pearson chi-sq
test1 <- chisq.test(varieties, p=exp_p)
print(test1)


# Likelihood ratio test
sum <- 0
for (i in 1:4) {
  sum <-  sum + 2 * (
    varieties[i] * log(varieties[i]/(exp_p[i]*trials))
    )
}
print(sum)



