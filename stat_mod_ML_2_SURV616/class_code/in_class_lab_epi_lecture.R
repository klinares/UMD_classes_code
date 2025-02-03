
# replicate table
matrix_ct <- matrix(
  c(a = 171, b = 3264, c = 117, d = 4320),
  ncol = 2,
  byrow = TRUE
)

# add margins
matrix_ct_margins <- addmargins(matrix_ct)

# add names
dimnames(matrix_ct_margins) <- list(
  stroke = c("Yes", "No", "colsum"),
  smoker = c("yes", "no", "rowsum")
)

# print margin totals for contingency table
matrix_ct_margins 

# calculate incidence rate for groups
# (A / A+B)
ir_exp <- matrix_ct[1,1]  / matrix_ct_margins[1,3] ; ir_exp
# (C / C+D)
ir_non <- matrix_ct[2,1]  / matrix_ct_margins[2,3] ; ir_non
# about 5% of smokers had a stroke vs 2.5% of non smokers 

# attributed risk (exp - non) / exp
attri_risk <- (ir_exp - ir_non) / ir_exp ; attri_risk

# calculate relative risk
RR <- round(ir_exp / ir_non, 4)

# compute odds ratio
OR <- round(
  (matrix_ct[1,1] * matrix_ct[2,2] ) / 
  (matrix_ct[1,2] * matrix_ct[2,1] ),
  4)














# compute variance estimation for retrospective studies

# formula for variance for relative risk
#b/(c* a+b) + d / (a* c+d)
var_RR <- (matrix_ct[1, 2] / (matrix_ct[2,1] * (
  (matrix_ct[1,1] + (matrix_ct[1,2]))
)
)) + (matrix_ct[2, 2] / (matrix_ct[1,1] * (
  (matrix_ct[2,1] + (matrix_ct[2,2]))
))) ; var_RR


lower <- round(exp(log(RR) - 1.96 * sqrt(var_RR)) , 4)
upper <- round(exp(log(RR) + 1.96 * sqrt(var_RR)) , 4)

print(str_c("Printing 95% CI for relative risk . . . ", RR, 
            " [", lower, ", ",  upper, "]"))




# calculate variance for odds ratio
var_OR <- (1/matrix_ct[1, 1]) + (1/matrix_ct[1, 2]) + 
  (1/matrix_ct[2, 1]) + (1/matrix_ct[2, 2]) ; var_OR


lower <- round(exp(log(OR) - 1.96 * sqrt(var_OR)), 4)
upper <- round(exp(log(OR) + 1.96 * sqrt(var_OR)), 4)

print(str_c("Printing 95% CI for odds ratio. . . ", RR, 
            " [", lower, ", ", upper, "]"))





# prospective study

a <- 23
b <- 125
c <- 13 
d <- 150
t <- sum(a, b, c, d) 

attrition <- sum(a/t, b/t, c/t, d/t) ; attrition

# relative risk, compare incidence rates 
R_hat <- (a*(c+d)) / 
  (c*(a+b)) ; R_hat

OR <- (a*d)/(b*c) ;  OR

# attributed risk
A_exposed <-  (R_hat - 1) / R_hat ; A_exposed

# proportion of the population exposed to the risk factor
P_hat <- (a+b) / t ; P_hat

# reduction in incidence in the population that would
# occur in the absence of the risk factor
A_hat_pop <- (a*d - b*c) / ((a+c)* (c+d)) ; A_hat_pop

# check t * c
V <- (b + A_hat_pop* (a+d)) / (t*c) ;  V


# retrospective 
R_hat <- (a*d)/(b*c) ; R_hat

A_exposed <-  (R_hat - 1) / R_hat ; A_exposed

A_hat_pop <- (a*d - b*c) / ( d * (a+c)) ; A_hat_pop

V <- (a / (c * (a + c))) +
  (b / (d*(b+d)));  V

# compute CI
lower <- 1 - exp(
  (A_hat_pop) - 1.96*sqrt(V * (A_hat_pop)) 
) ; lower







