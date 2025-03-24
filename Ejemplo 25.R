# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Electroforesis de ADN  
# Objetivo: Analizar la migración de fragmentos de ADN en un gel de agarosa  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
fragmentos <- c("Fragmento A", "Fragmento B", "Fragmento C", "Fragmento D")  
tamanos <- c(500, 1000, 2000, 5000)  # en pares de bases (pb)  
distancias <- c(4.5, 3.5, 2.5, 1.5)  # en cm  

# Crear un data frame con los datos  
datos <- data.frame(  
  Fragmento = fragmentos,  
  Tamano = tamanos,  
  Distancia = distancias  
)  

# 1. Gráfica de calibración (log(Tamaño) vs. Distancia Migrada)  
ggplot(datos, aes(x = Distancia, y = log10(Tamano))) +  
  geom_point(size = 3, color = "red") +  
  labs(  
    title = "Gráfica de Calibración para Electroforesis de ADN",  
    subtitle = "Logaritmo del Tamaño del Fragmento vs. Distancia Migrada",  
    x = "Distancia Migrada (cm)",  
    y = "Log10(Tamaño en pb)"  
  ) +  
  theme_minimal()  

# 2. Ecuación de la recta de calibración (regresión lineal)  
modelo <- lm(log10(Tamano) ~ Distancia, data = datos)  
coeficientes <- coef(modelo)  
cat("Ecuación de la recta de calibración:\n")  
cat("log10(Tamaño) =", round(coeficientes[1], 2), "+", round(coeficientes[2], 2), "* Distancia\n\n")  

# 3. Predicción del tamaño de un fragmento desconocido  
distancia_desconocida <- 3.0  # en cm  
log_tamano_predicho <- coeficientes[1] + coeficientes[2] * distancia_desconocida  
tamano_predicho <- 10^log_tamano_predicho  
cat("Tamaño estimado para un fragmento que migra", distancia_desconocida, "cm:", round(tamano_predicho, 2), "pb\n\n")  

# 4. Análisis de resultados  
cat("Análisis de resultados:\n")  
cat("Los fragmentos de ADN migran distancias diferentes debido a su tamaño.\n")  
cat("Los fragmentos más pequeños (menos pares de bases) migran más lejos en el gel,\n")  
cat("mientras que los fragmentos más grandes (más pares de bases) migran menos.\n")  

