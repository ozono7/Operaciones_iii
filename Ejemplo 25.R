# ==========================================================================
# Electroforesis de ADN
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Analizar la migración de fragmentos de ADN en un gel de agarosa
# ==========================================================================


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
p <- ggplot(datos, aes(x = Distancia, y = log10(Tamano))) +  
  geom_point(size = 3, color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(  
    title = "Gráfica de Calibración para Electroforesis de ADN",  
    subtitle = "Logaritmo del Tamaño del Fragmento vs. Distancia Migrada",  
    x = "Distancia Migrada (cm)",  
    y = "Log10(Tamaño en pb)"  
  ) +  
  theme_minimal()

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_25_adn.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Gráfica de Calibración para Electroforesis de ADN
# --------------------------------------------------------------------------
# - Eje X: Distancia migrada en el gel de agarosa, en centímetros (cm). Se mide
#   desde el pozo de siembra hasta donde llegó cada banda de ADN.
# - Eje Y: Log10 del tamaño del fragmento. Es el logaritmo en base 10 del tamaño
#   en pares de bases (pb), por lo que es adimensional: un logaritmo no tiene
#   unidades. Un valor de 3,7 equivale a unos 5000 pb y un valor de 2,7 a unos
#   500 pb.
# - Lo que se aprecia: cuatro puntos que bajan de izquierda a derecha: el
#   Fragmento A (500 pb) migró 4,5 cm, el B (1000 pb) 3,5 cm, el C (2000 pb)
#   2,5 cm y el D (5000 pb) solo 1,5 cm. La línea recta del gráfico es la recta
#   de regresión ajustada con estos cuatro puntos, y tiene pendiente negativa.
# - Qué significa: en un gel de agarosa los fragmentos pequeños de ADN se mueven
#   más rápido y llegan más lejos, mientras que los grandes avanzan poco. Por
#   eso la recta permite estimar el tamaño en pares de bases de un fragmento
#   desconocido a partir de lo que migró: el script lo hace para una banda
#   a 3,0 cm.

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


# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En electroforesis de ADN, la distancia migrada (cm) depende del tamaño: los
#   fragmentos pequeños migran más lejos que los grandes.
# - Graficar el logaritmo del tamaño contra la distancia da una recta de
#   calibración: el eje Y es adimensional porque un logaritmo no tiene
#   unidades, y los tamaños se expresan en pares de bases (pb).
# - Con la ecuación de esa recta se estima el tamaño (pb) de un fragmento
#   desconocido midiendo solo cuánto migró en el gel.
# - La calibración de este ejemplo usa solo cuatro fragmentos, así que es una
#   recta aproximada para aprender el método, no una calibración de laboratorio.
