# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Líquido-Líquido  
# Problema: Extracción de ácido acético de una solución acuosa usando éter dietílico  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
concentracion_inicial <- 5  # g/L de ácido acético en la fase acuosa  
volumen_acuoso <- 500       # mL de solución acuosa  
volumen_organico <- 100     # mL de éter dietílico  
K <- 2.5                    # Relación de distribución (C_orgánico / C_acuoso)  

# Función para calcular la concentración en ambas fases  
calcular_concentraciones <- function(C_inicial, V_acuoso, V_organico, K) {  
  # Masa total de ácido acético en la fase acuosa inicial  
  masa_total <- C_inicial * V_acuoso / 1000  # Convertir mL a L  
  
  # Concentración en la fase acuosa después de la extracción  
  C_acuoso_final <- masa_total / (V_acuoso / 1000 + V_organico / 1000 * K)  
  
  # Concentración en la fase orgánica después de la extracción  
  C_organico_final <- K * C_acuoso_final  
  
  return(list(C_acuoso = C_acuoso_final, C_organico = C_organico_final))  
}  

# Aplicar la función  
resultados <- calcular_concentraciones(concentracion_inicial, volumen_acuoso, volumen_organico, K)  
C_acuoso_final <- resultados$C_acuoso  
C_organico_final <- resultados$C_organico  

# Calcular la eficiencia de extracción  
eficiencia <- (C_organico_final * volumen_organico / 1000) / (concentracion_inicial * volumen_acuoso / 1000) * 100  

# Crear un data frame para graficar  
datos <- data.frame(  
  Fase = c("Acuosa", "Orgánica"),  
  Concentracion = c(C_acuoso_final, C_organico_final)  
)  

# Gráfico de barras  
ggplot(datos, aes(x = Fase, y = Concentracion, fill = Fase)) +  
  geom_bar(stat = "identity", color = "black") +  
  labs(title = "Concentración de Ácido Acético después de la Extracción",  
       x = "Fase",  
       y = "Concentración (g/L)",  
       fill = "Fase") +  
  theme_minimal()  

# Mostrar resultados en la consola  
cat("Concentración en la fase acuosa después de la extracción:", round(C_acuoso_final, 2), "g/L\n")  
cat("Concentración en la fase orgánica después de la extracción:", round(C_organico_final, 2), "g/L\n")  
cat("Eficiencia de extracción:", round(eficiencia, 2), "%\n")  
