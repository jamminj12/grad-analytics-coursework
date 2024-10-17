library(lpSolve)

### part a
objective.in <- c(2,5)
const.matA <- matrix(c(1,2,
                       1,3), nrow = 2, byrow = TRUE)
const.dir <- c("<=", "<=")
const.rhs <- c(10,12)

optimum <- lp(direction="max", objective.in, const.matA, const.dir, const.rhs, compute.sens=TRUE)

# Optimal values for decision variables
optimum$solution
View(optimum$solution)

# Objective value at optimum (maximum)
optimum$objval

# Sensitivities to objective function coefficients
optimum$sens.coef.from
optimum$sens.coef.to

# Dual values for the constraints & variables 
optimum$duals

# Sensitivities to right-hand side values
optimum$duals.from
optimum$duals.to

# Remove scientific notation if 
options(scipen = 999)

# Return
options(scipen = 3)

View(optimum)

### part b
objective.in.b1 <- c(1,5)
optimum.b1 <- lp(direction="max", objective.in.b1, const.matA, const.dir, const.rhs, compute.sens=TRUE)
optimum.b1$solution
# solution does not stay optimal

objective.in.b3 <- c(3,5)
optimum.b3 <- lp(direction="max", objective.in.b3, const.matA, const.dir, const.rhs, compute.sens=TRUE)
optimum.b3$solution
# solution does not stay optimal

### part c
objective.in.c2.5 <- c(2,2.5)
optimum.c2.5 <- lp(direction="max", objective.in.c2.5, const.matA, const.dir, const.rhs, compute.sens=TRUE)
optimum.c2.5$solution
# solution does not stay optimal

objective.in.c7.5 <- c(2,7.5)
optimum.c7.5 <- lp(direction="max", objective.in.c7.5, const.matA, const.dir, const.rhs, compute.sens=TRUE)
optimum.c7.5$solution
# solution does not stay optimal

