# ==========================================================================
# Extracción Sólido-Líquido (Lixiviación) en la Industria de Alimentos
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de antioxidantes a partir de cáscaras de cítricos
# ==========================================================================


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

# Función para calcular la cantidad de antioxidantes en el extracto concentrado
calcular_cantidad_concentrada <- function(antioxidantes_extraidos, factor_concentracion) {  
  # Es una masa en gramos, no una concentración en g/L: el archivo no indica
  # ningún volumen final del extracto, así que no se puede pasar a g/L.
  cantidad_concentrada <- antioxidantes_extraidos * factor_concentracion
  return(cantidad_concentrada)
}  

# Aplicar las funciones  
antioxidantes_extraidos <- calcular_extraccion(masa_materia_prima, porcentaje_compuestos_solubles, concentracion_antioxidantes, eficiencia_extraccion)  
cantidad_concentrada <- calcular_cantidad_concentrada(antioxidantes_extraidos, factor_concentracion)  

# Crear un data frame para graficar  
tiempo <- seq(0, 120, by = 10)  # Tiempo en minutos  
extraccion <- ifelse(tiempo <= tiempo_extraccion, antioxidantes_extraidos * (1 - exp(-0.05 * tiempo)), antioxidantes_extraidos)  # Simulación de la extracción  
cantidad_concentrada_curva <- ifelse(tiempo > tiempo_extraccion, cantidad_concentrada * (1 - exp(-0.03 * (tiempo - tiempo_extraccion))), NA)  # Simulación del extracto concentrado

datos <- data.frame(  
  Tiempo = tiempo,  
  Extraccion = extraccion,  
  Cantidad_Concentrada = cantidad_concentrada_curva  
)  

# Gráfico de extracción y concentración  
p <- ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Extraccion, color = "Extracción"), linewidth = 1) +  
  geom_line(aes(y = Cantidad_Concentrada, color = "Extracto concentrado"), linewidth = 1) +  
  labs(title = "Extracción y Concentración de Antioxidantes",  
       x = "Tiempo (minutos)",  
       y = "Cantidad de Antioxidantes (g)",  
       color = "Proceso") +  
  theme_minimal() +  
  theme(legend.position = "right")

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_14_antioxidantes.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Extracción y Concentración de Antioxidantes
# --------------------------------------------------------------------------
# - Eje X: Tiempo, en minutos (min). Va de 0 a 120 minutos; la extracción dura
#   los primeros 60 minutos y la concentración empieza después.
# - Eje Y: Cantidad de antioxidantes, en gramos (g). Las dos curvas son masas,
#   no concentraciones: la curva de extracción muestra los gramos extraídos de
#   las cáscaras y la segunda curva muestra esos mismos gramos multiplicados
#   por el factor de concentración (5). No es una concentración en g/L, porque
#   el archivo no indica ningún volumen final del extracto.
# - Lo que se aprecia: la curva de extracción sube rápido al principio y se
#   aplana al llegar a la cantidad máxima extraíble con una eficiencia del 70 %;
#   después, la segunda curva (extracto concentrado) sube desde cero hasta
#   acercarse a 14 g, que es la cantidad extraída (2,8 g) por el factor 5.
# - Qué significa: de 100 g de cáscaras solo una parte son antioxidantes
#   extraíbles, y el extracto se concentra después de la extracción. La segunda
#   curva es un modelo simplificado del archivo (cantidad extraída por un
#   factor, en gramos), no una medición de concentración en g/L: concentrar
#   no crea más antioxidante, solo lo reúne en menos volumen.

# Mostrar resultados en la consola  
cat("Cantidad de antioxidantes extraídos:", round(antioxidantes_extraidos, 2), "g\n")  
cat("Cantidad de antioxidantes en el extracto concentrado:", round(cantidad_concentrada, 2), "g\n")
# (No es una concentración en g/L: el archivo no indica el volumen final del extracto.)  

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La extracción sólido-líquido saca los compuestos solubles de un sólido con
#   un disolvente: la cantidad extraída crece rápido al inicio y se aplana al
#   agotarse lo extraíble.
# - La eficiencia de extracción (70 % en este ejemplo, adimensional y expresada
#   en %) limita cuántos antioxidantes se recuperan de los que había en la
#   materia prima.
# - Concentrar un extracto aumenta su contenido por unidad de volumen, pero no
#   crea más antioxidante: en este archivo el extracto concentrado se modela
#   de forma simplificada como la cantidad extraída por un factor (5), y se
#   grafica en gramos (g), no en g/L, porque no se indica el volumen final.

