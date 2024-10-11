library(readxl)
MATH_543_HW_Ch_07 <- read_excel("data/MATH 543 HW Ch 07.xlsx", 
                                          range = "AD2:CA62")
View(MATH_543_HW_Ch_07)

library(lpSolve)

data7.6 <- MATH_543_HW_Ch_07
head(data7.6)

objective.in <- c(rep(1, 12), rep(0, 36)) #first twelve are my profits, rest are other variables.
head(objective.in)

const.dir <- data7.6$Inequality
head(const.dir)
const.rhs <- data7.6$RHS
head(const.rhs)
const.matA <- data7.6[,c(1:48)]
View(const.matA)

optimum <- lp(direction="max", objective.in, const.matA, const.dir, const.rhs, compute.sens=TRUE)

View(optimum)

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
