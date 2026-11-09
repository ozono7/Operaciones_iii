# ==========================================================================
# Electroforesis de Proteínas (SDS-PAGE)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Analizar la migración de proteínas en un gel de poliacrilamida
# ==========================================================================


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
p <- ggplot(datos, aes(x = Distancia, y = log10(Peso_Molecular))) +  
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(  
    title = "Gráfica de Calibración para Electroforesis SDS-PAGE",  
    subtitle = "Logaritmo del Peso Molecular vs. Distancia Migrada",  
    x = "Distancia Migrada (cm)",  
    y = "Log10(Peso Molecular)"  
  ) +  
  theme_minimal()

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_24_sds_page.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Gráfica de Calibración para Electroforesis SDS-PAGE
# --------------------------------------------------------------------------
# - Eje X: Distancia migrada en el gel, en centímetros (cm). Se mide desde el
#   punto de siembra hasta donde llegó cada banda de proteína.
# - Eje Y: Log10 del peso molecular. Es el logaritmo en base 10 del peso
#   molecular (los datos están en kilodaltons, kDa), por lo que es adimensional:
#   un logaritmo no tiene unidades. Un valor de 1,8 equivale a unos 66 kDa y un
#   valor de 1,1 a unos 12 kDa.
# - Lo que se aprecia: tres puntos que bajan de izquierda a derecha: la
#   Albúmina (66 kDa) migró solo 2,5 cm, la Mioglobina (17 kDa) migró 5,0 cm y
#   el Citocromo C (12 kDa) llegó más lejos, 6,0 cm. La línea recta del gráfico
#   es la recta de regresión ajustada con estos tres puntos, y tiene pendiente
#   negativa.
# - Qué significa: en SDS-PAGE las proteínas pequeñas atraviesan mejor los poros
#   del gel y migran más lejos, mientras que las grandes se quedan atrás. Por
#   eso la recta permite estimar el peso molecular de una proteína desconocida
#   a partir de lo que migra: el script lo hace para una banda a 4,2 cm.

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


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En SDS-PAGE, la distancia migrada (cm) depende del tamaño: las proteínas
#   pequeñas migran más lejos que las grandes.
# - Graficar el logaritmo del peso molecular contra la distancia da una recta
#   de calibración: el eje Y es adimensional porque un logaritmo no tiene
#   unidades, y los pesos se expresan en kDa.
# - Con la ecuación de esa recta se estima el peso molecular (kDa) de una
#   proteína desconocida midiendo solo cuánto migró en el gel.
# - La calibración de este ejemplo usa solo tres proteínas, así que es una
#   recta aproximada para aprender el método, no una calibración de laboratorio.
