# Первая задача
A1 <- matrix(c(1, 0, 0, 2, 0, 3, 1, 4, 2, 0, -3, 5), nrow = 3, byrow = TRUE)
A2 <- matrix(c(1, 3, 5, 1), nrow = 4, byrow = TRUE)

A1_transposed <- t(A1)
A2_transposed <- t(A2)

dim_A1 <- dim(A1)
dim_A1_transposed <- dim(A1_transposed)
dim_A2 <- dim(A2)
dim_A2_transposed <- dim(A2_transposed)
print("A1 матрица:")
print(A1)
print("A2 матрица:")
print(A2)
print("Первая задача: Транспонированная матрица A1:")
print(A1_transposed)
print("Размерности A1:")
print(dim_A1)
print("Размерности транспонированной A1:")
print(dim_A1_transposed)

print("Транспонированная матрица A2:")
print(A2_transposed)
print("Размерности A2:")
print(dim_A2)
print("Размерности транспонированной A2:")
print(dim_A2_transposed)

# Вторая задача
A <- matrix(c(2, -3, 4, 7, 6, -5, -1, 8, 9), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 3, -4, -7, -5, 5, 1, -8, -8), nrow = 3, byrow = TRUE)

result_AB <- A + B

print("Вторая задача: A + B:")
print(result_AB)

# Третья задача
A <- matrix(c(1, 0, -1, 3, -4, 2), nrow = 3, byrow = TRUE)
B <- matrix(c(1, -1, 2, 3, 1, -5), nrow = 3, byrow = TRUE)
C <- matrix(c(3, 4, 1, -3, 8, 6), nrow = 3, byrow = TRUE)

result <- 3 * A + 4 * B - 2 * C

print("Третья задача: 3A + 4B - 2C:")
print(result)

# Четвертая задача
print("Четвертая задача: Значения m и n:")
m1 <- 3
n1 <- 5
print(paste("Для A_{3x4} и B_{4x5}, размер матрицы C:", m1, "x", n1))

m2 <- 3
n2 <- 6
print(paste("Для A_{2x3} и B_{mxn}, размер матрицы C:", m2, "x", n2))

# Пятая задача
A <- matrix(c(1, 0, 2, -3, 2, 1, 0, 0, 1), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 3, 1, 0, 1, -1, 2, 1, 3), nrow = 3, byrow = TRUE)

AB <- A %*% B
BA <- B %*% A

print("Пятая задача: Произведение A * B:")
print(AB)
print("Произведение B * A:")
print(BA)

# Задача 6 (a): Возведение матрицы в квадрат
A <- matrix(c(1, 2, 3, -1), nrow = 2, byrow = TRUE)
A_squared <- A %*% A

print("Задача 6 (a): A^2:")
print(A_squared)

# Задача 6 (б): Возведение матрицы в куб
B <- matrix(c(2, 1, 1, 4), nrow = 2, byrow = TRUE)
B_cubed <- B %*% B %*% B

print("Задача 6 (б): B^3:")
print(B_cubed)

# Задача 7: Найти f(A), если f(x) = x^2 - 2x
A <- matrix(c(4, -3, 9, 1), nrow = 2, byrow = TRUE)
A_squared <- A %*% A
f_A <- A_squared - 2 * A

print("Задача 7: f(A) = A^2 - 2A:")
print(f_A)

