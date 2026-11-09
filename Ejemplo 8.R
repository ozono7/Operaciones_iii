# ==========================================================================
# Extracción Líquido-Líquido
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de ácido acético de una solución acuosa usando éter dietílico
# ==========================================================================


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
p <- ggplot(datos, aes(x = Fase, y = Concentracion, fill = Fase)) +  
  geom_bar(stat = "identity", color = "black") +  
  labs(title = "Concentración de Ácido Acético después de la Extracción",  
       x = "Fase",  
       y = "Concentración (g/L)",  
       fill = "Fase") +  
  theme_minimal()

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_08_acido_acetico.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Concentración de Ácido Acético después de la Extracción
# --------------------------------------------------------------------------
# - Eje X: Fase. No es un eje numérico: son dos categorías, la fase acuosa
#   (agua) y la fase orgánica (éter dietílico). Cada barra es una fase.
# - Eje Y: Concentración de ácido acético, en gramos por litro (g/L). La altura
#   de cada barra es la concentración que queda en esa fase tras la extracción.
# - Lo que se aprecia: la barra de la fase orgánica es mucho más alta que la
#   de la fase acuosa. El ácido acético se concentró en el éter y quedó poco
#   en el agua.
# - Qué significa: el éter extrae bien el ácido acético del agua, porque la
#   relación de distribución K = 2,5 indica que, en equilibrio, la concentración
#   en la fase orgánica es 2,5 veces la de la fase acuosa. La eficiencia de
#   extracción (en %) que imprime el script resume qué parte del ácido total
#   terminó en el éter.

# Mostrar resultados en la consola  
cat("Concentración en la fase acuosa después de la extracción:", round(C_acuoso_final, 2), "g/L\n")  
cat("Concentración en la fase orgánica después de la extracción:", round(C_organico_final, 2), "g/L\n")  
cat("Eficiencia de extracción:", round(eficiencia, 2), "%\n")  

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En una extracción líquido-líquido el soluto se reparte entre dos fases que
#   no se mezclan, y la relación de distribución K dice cuántas veces más
#   concentrado queda en una fase que en la otra (K es adimensional).
# - Un gráfico de barras permite comparar de un vistazo la concentración final
#   en cada fase, en g/L.
# - La eficiencia de extracción (en %) indica qué fracción de la masa inicial
#   de soluto pasó a la fase orgánica.
