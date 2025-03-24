# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Sólido-Líquido (Lixiviación) en la Industria de Alimentos  
# Problema: Extracción de antioxidantes a partir de cáscaras de cítricos  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
masa_materia_prima <- 100  # Masa de cáscaras de cítricos (g)  
porcentaje_solidos <- 0.60  # Fracción de sólidos insolubles  
porcentaje_compuestos_solubles <- 0.40  # Fracción de compuestos solubles  
concentracion_antioxidantes <- 0.10  # Fracción de antioxidantes en los compuestos solubles  
eficiencia_extraccion <- 0.70  # Eficiencia de extracción (70%)  
tiempo_extraccion <- 60  # Tiempo de extracción (minutos)  
factor_concentracion <- 5  # Factor de concentración  

# Función para calcular la cantidad de antioxidantes extraídos  
calcular_extraccion <- function(masa_materia_prima, porcentaje_compuestos_solubles, concentracion_antioxidantes, eficiencia_extraccion) {  
  # Masa total de compuestos solubles  
  masa_compuestos_solubles <- masa_materia_prima * porcentaje_compuestos_solubles  
  
  # Masa total de antioxidantes en la materia prima  
  masa_antioxidantes <- masa_compuestos_solubles * concentracion_antioxidantes  
  
  # Cantidad de antioxidantes extraídos  
  antioxidantes_extraidos <- masa_antioxidantes * eficiencia_extraccion  
  
  return(antioxidantes_extraidos)  
}  

# Función para calcular la concentración de antioxidantes después de la concentración  
calcular_concentracion <- function(antioxidantes_extraidos, factor_concentracion) {  
  concentracion_final <- antioxidantes_extraidos * factor_concentracion  
  return(concentracion_final)  
}  

# Aplicar las funciones  
antioxidantes_extraidos <- calcular_extraccion(masa_materia_prima, porcentaje_compuestos_solubles, concentracion_antioxidantes, eficiencia_extraccion)  
concentracion_final <- calcular_concentracion(antioxidantes_extraidos, factor_concentracion)  

# Crear un data frame para graficar  
tiempo <- seq(0, 120, by = 10)  # Tiempo en minutos  
extraccion <- ifelse(tiempo <= tiempo_extraccion, antioxidantes_extraidos * (1 - exp(-0.05 * tiempo)), antioxidantes_extraidos)  # Simulación de la extracción  
concentracion <- ifelse(tiempo > tiempo_extraccion, concentracion_final * (1 - exp(-0.03 * (tiempo - tiempo_extraccion))), NA)  # Simulación de la concentración  

datos <- data.frame(  
  Tiempo = tiempo,  
  Extraccion = extraccion,  
  Concentracion = concentracion  
)  

# Gráfico de extracción y concentración  
ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Extraccion, color = "Extracción"), size = 1) +  
  geom_line(aes(y = Concentracion, color = "Concentración"), size = 1) +  
  labs(title = "Extracción y Concentración de Antioxidantes",  
       x = "Tiempo (minutos)",  
       y = "Cantidad de Antioxidantes (g)",  
       color = "Proceso") +  
  theme_minimal() +  
  theme(legend.position = "right")  

# Mostrar resultados en la consola  
cat("Cantidad de antioxidantes extraídos:", round(antioxidantes_extraidos, 2), "g\n")  
cat("Concentración final de antioxidantes:", round(concentracion_final, 2), "g\n")  

