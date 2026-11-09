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
# Descripción: Extracción de yodo de una solución acuosa usando tetracloruro de carbono (CCl₄)
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
concentracion_inicial <- 0.8  # g/L de yodo en la fase acuosa  
volumen_acuoso <- 200         # mL de solución acuosa  
volumen_organico <- 50        # mL de tetracloruro de carbono (CCl₄)  
K <- 85                       # Relación de distribución (C_orgánico / C_acuoso)  

# Función para calcular la concentración en ambas fases  
calcular_concentraciones <- function(C_inicial, V_acuoso, V_organico, K) {  
  # Masa total de yodo en la fase acuosa inicial  
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
  labs(title = "Concentración de Yodo después de la Extracción",  
       x = "Fase",  
       y = "Concentración (g/L)",  
       fill = "Fase") +  
  theme_minimal()  


# --------------------------------------------------------------------------
# Interpretación: Concentración de Yodo después de la Extracción
# --------------------------------------------------------------------------
# - Eje X: Fase. No es un eje numérico: son dos categorías, la fase acuosa
#   (agua) y la fase orgánica (tetracloruro de carbono, CCl4). Cada barra es
#   una fase.
# - Eje Y: Concentración de yodo, en gramos por litro (g/L). La altura de cada
#   barra es la concentración que queda en esa fase tras la extracción.
# - Lo que se aprecia: la barra de la fase orgánica es muchísimo más alta que
#   la de la fase acuosa, que queda casi en cero. Casi todo el yodo se fue al
#   tetracloruro de carbono.
# - Qué significa: el tetracloruro de carbono extrae el yodo con mucha fuerza,
#   porque la relación de distribución K = 85 indica que, en equilibrio, la
#   concentración en la fase orgánica es 85 veces la de la fase acuosa. Con un
#   K tan grande, una sola extracción deja el agua prácticamente sin yodo, y la
#   eficiencia de extracción (en %) queda muy cerca del 100 %.

# Mostrar resultados en la consola  
cat("Concentración en la fase acuosa después de la extracción:", round(C_acuoso_final, 4), "g/L\n")  
cat("Concentración en la fase orgánica después de la extracción:", round(C_organico_final, 4), "g/L\n")  
cat("Eficiencia de extracción:", round(eficiencia, 2), "%\n")  

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En una extracción líquido-líquido el soluto se reparte entre dos fases que
#   no se mezclan, y la relación de distribución K dice cuántas veces más
#   concentrado queda en una fase que en la otra (K es adimensional).
# - Cuando K es muy grande (aquí K = 85), casi todo el soluto pasa a la fase
#   orgánica en una sola extracción y la fase acuosa queda casi vacía.
# - Un gráfico de barras permite comparar de un vistazo la concentración final
#   en cada fase, en g/L, y la eficiencia de extracción (en %) resume el
#   resultado en un solo número.

