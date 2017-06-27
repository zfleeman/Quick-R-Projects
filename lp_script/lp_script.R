#Linear Programming Script
library(lpSolve)
library(lpSolveAPI)

#Example 1
obj_func <- c(400,500)
constraints <- matrix(c(10,20,40,30), ncol = 2, byrow = TRUE)
const_dir <- c("<=", "<=")
rhs_constraints <- c(400,1200)

lp_solution <- lp("max", obj_func, constraints, const_dir, rhs_constraints)

lp_solution$solution
lp_solution$objval



#Example 2
obj_func <- c(350,300)
constraints <- matrix(c(9,6,12,16,1,1), ncol = 2, byrow = TRUE)
const_dir <- c("<=", "<=", "<=")
rhs_constraints <- c(1566,2880,200)

lp_solution <- lp("max", obj_func, constraints, const_dir, rhs_constraints)

lp_solution$solution
lp_solution$objval


#Example 3: Portfolio Optimization
row_weights <- matrix(c(0.50, 0.25, 0.25), nrow = 1, ncol = 3)
exp_returns <- matrix(c(0.10, 0.20, 0.30), nrow = 3, ncol = 1)

portfolio_return <- row_weights %*% exp_returns #matrix multiplier


#Example 4: Portfolio Risk
covariance_matrix <- matrix(c(225, -60, 0, -60, 400, 30, 0, 30, 49), nrow = 3, ncol = 3)
column_weights <- matrix(c(0.50, 0.25, 0.25), nrow = 3, ncol = 1)
#or
column_weights <- t(row_weights)

portfolio_variance <- row_weights %*% covariance_matrix %*% column_weights
portfolio_sd <- sqrt(portfolio_variance)
