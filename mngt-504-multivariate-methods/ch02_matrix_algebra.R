
# All functions from Appendix: Matrix Algebra in R

# Matrix 
x <- c(1, 2, 3, 4, 5, 6)
A <- matrix(x, nrow = 3, ncol = 2)
print("Matrix A (column-wise):")
print(A)

B <- matrix(x, nrow = 2, ncol = 3, byrow = TRUE)
print("Matrix B (row-wise):")
print(B)

#############################################################################

# Binding Vectors
vec1 <- c(1, 2, 3)
vec2 <- c(4, 5, 6)
vec3 <- c(7, 8, 9)

C_bind <- cbind(v1, v2, v3)
print("Matrix created by column binding vec1, vec2, vec3:")
print(C_bind)

#############################################################################

# Matrix Dimensions
print("Dimensions of A:")
print(dim(A))
print("Number of rows in A:")
print(nrow(A))
print("Number of columns in A:")
print(ncol(A))

#############################################################################

# Matrix Transpose
print("Transpose of A:")
print(t(A))

#############################################################################

# Zero Matrix n x m (3 x 2)
zero_matrix <- matrix(0, 3, 2)
print("3x2 Zero Matrix:")
print(zero_matrix)

#############################################################################

# Diagonal matrix with scalers
D <- diag(c(1, 2, 3))
print("Diagonal Matrix with 1, 2, 3:")
print(D)

#############################################################################

# Diagonal extraction of the square matrix
I <- diag(3)
print("3x3 Identity Matrix:")
print(I)

#############################################################################

# Trace of a square matrix
print("Diagonal elements of D:")
print(diag(D))
print("Trace of D (sum of diagonal elements):")
print(sum(diag(D)))

#############################################################################

# Matrix Addition and Subtraction
C <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2) # same as A
print("Matrix C:")
print(C)
print("A + C:")
print(A + C)
print("A - C:")
print(A - C)

#############################################################################

# Scalar Multiplication
k <- 2
print("Scalar multiplication: 2 * A:")
print(k * A)

#############################################################################

# Element-wise vs. Matrix Multiplication
print("Element-wise multiplication A * C:")
print(A * C)
print("Matrix multiplication A %*% t(C):")
print(A %*% t(C))  # Note: t(C) to make dimensions conformable

#############################################################################

# Determinant
square_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
print("Square Matrix for Determinant:")
print(square_matrix)
print("Determinant of square matrix:")
print(det(square_matrix))

#############################################################################

# Matrix Inverse
print("Inverse of square matrix:")
print(solve(square_matrix))

#############################################################################

# Solving a System of Linear Equations
b <- c(1, 2)
print("Vector b for system A %*% x = b:")
print(b)
print("Solution x for square_matrix %*% x = b:")
print(solve(square_matrix, b))

#############################################################################

# Spectral Decomposition (Eigenvalues and Eigenvectors)
print("Eigenvalues and Eigenvectors of square matrix:")
eigen_result <- eigen(square_matrix)
print("Eigenvalues:")
print(eigen_result$values)
print("Eigenvectors:")
print(eigen_result$vectors)

#############################################################################

# Column and Row Means
print("Column means of A:")
print(colMeans(A))
print("Row means of A:")
print(rowMeans(A))

#############################################################################

# Variance-Covariance and Correlation Matrices
data_matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 4, ncol = 3)
print("Data Matrix for Cov and Cor:")
print(data_matrix)
print("Variance-Covariance Matrix:")
print(cov(data_matrix))
print("Correlation Matrix:")
print(cor(data_matrix))
