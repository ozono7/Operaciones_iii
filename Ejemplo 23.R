# ==========================================================================
# Simulación de Cromatografía de Intercambio Iónico
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Separar y analizar dos proteínas (Quimasa y Lactoperoxidasa)
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Parámetros de la columna cromatográfica  
L <- 30  # Longitud de la columna (cm)  
d <- 1.2  # Diámetro de la columna (cm)  
V_s <- 15  # Volumen de la fase estacionaria (mL)  
V_m <- 50  # Volumen de la fase móvil (mL)  

# Tiempo muerto de la columna: lo que tarda en salir un compuesto no retenido.
# Es un dato del problema (1 min): no se puede deducir de V_m, porque para
# eso haría falta el caudal de fase móvil, que el archivo no indica.
t_M <- 1  # Tiempo muerto (min)

# Datos de las proteínas  
tR_Quimasa <- 8  # Tiempo de retención de la Quimasa (min)  
W_Quimasa <- 1.2  # Ancho de pico de la Quimasa (min)  
tR_Lactoperoxidasa <- 11  # Tiempo de retención de la Lactoperoxidasa (min)  
W_Lactoperoxidasa <- 1.8  # Ancho de pico de la Lactoperoxidasa (min)  

# Cálculos  
# 1. Factor de retención (k) para cada proteína  
# El factor de retención es k = (t_R - t_M) / t_M, con t_M el tiempo muerto.
k_Quimasa <- (tR_Quimasa - t_M) / t_M  
k_Lactoperoxidasa <- (tR_Lactoperoxidasa - t_M) / t_M  

# 2. Resolución (R) entre las dos proteínas  
R <- 2 * (tR_Lactoperoxidasa - tR_Quimasa) / (W_Quimasa + W_Lactoperoxidasa)  

# 3. Número de platos teóricos (N) para cada proteína  
N_Quimasa <- 16 * (tR_Quimasa / W_Quimasa)^2  
N_Lactoperoxidasa <- 16 * (tR_Lactoperoxidasa / W_Lactoperoxidasa)^2  

# 4. Altura equivalente a un plato teórico (HETP) para cada proteína  
HETP_Quimasa <- L / N_Quimasa  
HETP_Lactoperoxidasa <- L / N_Lactoperoxidasa  

# Mostrar resultados en la consola  
cat("----------------------------------------\n")  
cat("Resultados de la simulación cromatográfica\n")  
cat("----------------------------------------\n")  
cat("Factor de retención (k) para la Quimasa:", round(k_Quimasa, 2), "\n")  
cat("Factor de retención (k) para la Lactoperoxidasa:", round(k_Lactoperoxidasa, 2), "\n\n")  
cat("Resolución (R) entre las dos proteínas:", round(R, 2), "\n")  
cat("Nota: Una resolución > 1.5 indica una buena separación.\n\n")  
cat("Número de platos teóricos (N) para la Quimasa:", round(N_Quimasa, 2), "\n")  
cat("Número de platos teóricos (N) para la Lactoperoxidasa:", round(N_Lactoperoxidasa, 2), "\n\n")  
cat("Altura equivalente a un plato teórico (HETP) para la Quimasa:", round(HETP_Quimasa, 2), "cm\n")  
cat("Altura equivalente a un plato teórico (HETP) para la Lactoperoxidasa:", round(HETP_Lactoperoxidasa, 2), "cm\n\n")  

# Simulación de los picos cromatográficos  
tiempo <- seq(0, 20, by = 0.1)  # Tiempo en minutos  
intensidad_Quimasa <- dnorm(tiempo, mean = tR_Quimasa, sd = W_Quimasa / 4)  
intensidad_Lactoperoxidasa <- dnorm(tiempo, mean = tR_Lactoperoxidasa, sd = W_Lactoperoxidasa / 4)  

# Crear un data frame para graficar  
datos <- data.frame(  
  Tiempo = tiempo,  
  Quimasa = intensidad_Quimasa,  
  Lactoperoxidasa = intensidad_Lactoperoxidasa  
)  

# Graficar los picos cromatográficos  
p <- ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Quimasa, color = "Quimasa"), linewidth = 1) +  
  geom_line(aes(y = Lactoperoxidasa, color = "Lactoperoxidasa"), linewidth = 1) +  
  labs(  
    title = "Picos Cromatográficos de Quimasa y Lactoperoxidasa",  
    subtitle = "Separación por Cromatografía de Intercambio Iónico",  
    x = "Tiempo (min)",  
    y = "Intensidad",  
    color = "Proteína"  
  ) +  
  theme_minimal() +  
  theme(legend.position = "top")

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_23_quimasa_lactoperoxidasa.png", plot = p, width = 8, height = 6, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Picos Cromatográficos de Quimasa y Lactoperoxidasa
# --------------------------------------------------------------------------
# - Eje X: Tiempo, en minutos (min). La Quimasa sale a los 8 min y la
#   Lactoperoxidasa a los 11 min.
# - Eje Y: Intensidad. La unidad no está especificada en el archivo: es un
#   valor relativo de altura del pico, calculado con la función de densidad
#   normal (dnorm), que solo sirve para comparar los picos entre sí, no es una
#   señal medida en unidades físicas.
# - Lo que se aprecia: dos picos en forma de campana bien separados. El pico de
#   la Quimasa es más alto y estrecho (ancho de 1,2 min) y el de la
#   Lactoperoxidasa es más bajo y ancho (ancho de 1,8 min). Entre sus máximos
#   hay 3 minutos y las bases apenas se tocan.
# - Qué significa: la separación es buena. Con los datos del archivo, la
#   resolución es R = 2,0, por encima de 1,5, que es el valor que el propio
#   script indica como buena separación. La Lactoperoxidasa se retiene más
#   (sale después) que la Quimasa en esta columna de intercambio iónico.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En un cromatograma, cada pico es un compuesto: su posición en el eje del
#   tiempo (min) es su tiempo de retención y su ancho (min) dice qué tan
#   disperso sale de la columna.
# - La resolución R es adimensional y compara la distancia entre dos picos con
#   sus anchos: con R = 2,0 (mayor que 1,5) la separación de Quimasa y
#   Lactoperoxidasa es buena.
# - El número de platos teóricos N (adimensional) y la altura equivalente HETP
#   (en cm) miden la eficiencia de la columna: más platos y menor HETP
#   significan picos más estrechos y mejor separación.
# - La intensidad del eje Y es un valor relativo sin unidad especificada en el
#   archivo: sirve para comparar los picos, no es una concentración medida.
