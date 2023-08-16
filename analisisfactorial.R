# Cargamos la librería psych para el análisis factorial
library(psych)

# Dataframe
data <- data.frame(
  CC = c(4, 3, 5, 2, 4, 5, 3, 2, 4, 5),
  AP = c(3, 4, 5, 2, 4, 4, 3, 2, 5, 5),
  AM = c(4, 2, 3, 4, 5, 5, 3, 2, 4, 4)
)

# Estadísticas básicas
summary(data)  # Resumen estadístico

# Matriz de correlaciones
cor_matrix <- cor(data)
print(cor_matrix)

# Matriz de covarianzas
cov_matrix <- cov(data)
print(cov_matrix)

# Eigenvalores
eigen_values <- eigen(cor_matrix)$values
print(eigen_values)

# Test de Bartlett para la esfericidad de las correlaciones
bartlett_test <- cortest.bartlett(cor_matrix, n = nrow(data))
print(bartlett_test)

# Kaiser-Meyer-Olkin (KMO)
kmo_result <- KMO(data)
print(kmo_result$MSA)

# Análisis factorial para 2 factores sin rotación
factor_analysis_2_none <- principal(data, nfactors = 2, rotate = "none")
print(factor_analysis_2_none)

# Análisis factorial para 3 factores sin rotación
factor_analysis_3_none <- principal(data, nfactors = 3, rotate = "none")
print(factor_analysis_3_none)

# Análisis factorial para 2 factores sin rotación
factor_analysis_2_varimax <- principal(data, nfactors = 2, rotate = "varimax")
print(factor_analysis_2_varimax)

# Análisis factorial para 3 factores con rotación varimax
factor_analysis_3_varimax <- principal(data, nfactors = 3, rotate = "varimax")
print(factor_analysis_3_varimax)

# Aplicar el modelo de regresión múltiple
modelo <- lm(CC ~ AP + AM, data = data)

# Resumen del modelo
summary(modelo)

