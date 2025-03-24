# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Electroforesis de Proteínas (SDS-PAGE)  
# Objetivo: Analizar la migración de proteínas en un gel de poliacrilamida  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
proteinas <- c("Albúmina", "Citocromo C", "Mioglobina")  
peso_molecular <- c(66, 12, 17)  # en kDa  
distancia_migrada <- c(2.5, 6.0, 5.0)  # en cm  

# Crear un data frame con los datos  
datos <- data.frame(  
  Proteina = proteinas,  
  Peso_Molecular = peso_molecular,  
  Distancia = distancia_migrada  
)  

# 1. Gráfica de calibración (log(Peso Molecular) vs. Distancia Migrada)  
ggplot(datos, aes(x = Distancia, y = log10(Peso_Molecular))) +  
  geom_point(size = 3, color = "blue") +  
  labs(  
    title = "Gráfica de Calibración para Electroforesis SDS-PAGE",  
    subtitle = "Logaritmo del Peso Molecular vs. Distancia Migrada",  
    x = "Distancia Migrada (cm)",  
    y = "Log10(Peso Molecular)"  
  ) +  
  theme_minimal()  

# 2. Ecuación de la recta de calibración (regresión lineal)  
modelo <- lm(log10(Peso_Molecular) ~ Distancia, data = datos)  
coeficientes <- coef(modelo)  
cat("Ecuación de la recta de calibración:\n")  
cat("log10(Peso Molecular) =", round(coeficientes[1], 2), "+", round(coeficientes[2], 2), "* Distancia\n\n")  

# 3. Predicción del peso molecular para una proteína desconocida  
distancia_desconocida <- 4.2  # en cm  
log_peso_predicho <- coeficientes[1] + coeficientes[2] * distancia_desconocida  
peso_predicho <- 10^log_peso_predicho  
cat("Peso molecular estimado para una proteína que migra", distancia_desconocida, "cm:", round(peso_predicho, 2), "kDa\n\n")  

# 4. Análisis de resultados  
cat("Análisis de resultados:\n")  
cat("Las proteínas migran distancias diferentes debido a su tamaño molecular.\n")  
cat("Las proteínas más pequeñas (menor peso molecular) migran más lejos en el gel,\n")  
cat("mientras que las proteínas más grandes (mayor peso molecular) migran menos.\n")  

