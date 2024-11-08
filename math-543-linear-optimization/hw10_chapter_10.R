# MATH 543 HW Ch 10

# Problem 10-05
library(lpSolve)

objective.in <- c(1, 1.8, 1.6, 0.8, 1.4)
capital.required <- c(6, 12, 10, 4, 8)
capital.available <- 18

const.dir <- "<="
const.rhs <- capital.available
const.matA <- matrix(capital.required, nrow = 1, 
                     ncol = 5, byrow = TRUE)

optimum <- lp(direction = "max", 
              objective.in, 
              const.matA, 
              const.dir, 
              const.rhs, 
              all.bin = TRUE)

optimum$solution
optimum$objval

#################################################

# Problem 10-11
library(readxl)
MATH_543_HW_Ch_10_11 <- read_excel("data/MATH 543 HW Ch 10.xlsx", 
                                          sheet = "10-11sv4",
                                          range = "W3:BC35")

library(lpSolve)

data10.11 <- MATH_543_HW_Ch_10_11
head(data10.11)

objective.in <- c(rep(1, 1), rep(0, 30)) #objective is first number
head(objective.in)

const.dir <- data10.11$Inequality
head(const.dir)
const.rhs <- data10.11$RHS
head(const.rhs)
const.matA <- data10.11[,c(1:31)]

binary.vars <- 2:31

optimum <- lp(direction = "min",
              objective.in,
              const.matA,
              const.dir,
              const.rhs,
              all.bin = FALSE,
              binary.vec = binary.vars,
              compute.sens = TRUE)

optimum$solution
optimum$objval

############################################################

# Problem 10-16
library(readxl)
MATH_543_HW_Ch_10_16 <- read_excel("data/MATH 543 HW Ch 10.xlsx", 
                                             sheet = "10-16sv4",
                                             range = "U2:AG14")

library(lpSolve)

data10.16 <- MATH_543_HW_Ch_10_16
head(data10.16)

objective.in <- c(rep(1, 1), rep(0, 10)) #objective is first number
head(objective.in)

const.dir <- data10.16$Inequality
head(const.dir)
const.rhs <- data10.16$RHS
head(const.rhs)
const.matA <- data10.16[,c(1:11)]

binary.vars <- 2:6

optimum <- lp(direction = "min",
              objective.in,
              const.matA,
              const.dir,
              const.rhs,
              all.bin = FALSE,
              binary.vec = binary.vars,
              compute.sens = TRUE)

optimum$solution
optimum$objval
