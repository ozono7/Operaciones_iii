# ==========================================================================
# Cromatografía en la Industria Bioquímica
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Separación de dos proteínas mediante cromatografía de intercambio iónico
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
L <- 25  # Longitud de la columna (cm)  
d <- 1  # Diámetro de la columna (cm)  

# Volúmenes de las fases: el volumen geométrico de la columna es
# V_columna = pi * (d/2)^2 * L = 19,63 mL, y las dos fases tienen que caber
# en él: V_m + V_s = V_columna. Los valores del archivo (40 mL y 10 mL)
# sumaban 50 mL, más que la columna. Se conservan su relación de fases
# (V_m / V_s = 40 / 10 = 4) y se escalan a la columna: V_m = 15,71 mL y
# V_s = 3,93 mL. La porosidad total es V_m / V_columna = 0,80.
V_columna <- pi * (d / 2)^2 * L  # Volumen geométrico de la columna (mL)
V_m <- V_columna * 4 / 5  # Volumen de la fase móvil (mL)
V_s <- V_columna * 1 / 5  # Volumen de la fase estacionaria (mL)

# Tiempo muerto de la columna: lo que tarda en salir un compuesto no retenido.
# Es un dato del problema (1 min): no se puede deducir de V_m, porque para
# eso haría falta el caudal de fase móvil, que el archivo no indica. Por la
# relación estándar t_M = V_m / F, ese dato equivale a un caudal
# F = V_m / t_M = 15,71 mL/min.
t_M <- 1  # Tiempo muerto (min)
caudal <- V_m / t_M  # Caudal de fase móvil que corresponde a t_M (mL/min)

# Tiempos de retención y anchos de pico  
t_R_A <- 10  # Tiempo de retención de la proteína A (min)  
W_A <- 1.5  # Ancho de pico de la proteína A (min)  
t_R_B <- 12  # Tiempo de retención de la proteína B (min)  
W_B <- 2.0  # Ancho de pico de la proteína B (min)  

# Cálculos  
# 1. Factor de retención (k) para cada proteína  
# El factor de retención es k = (t_R - t_M) / t_M, con t_M el tiempo muerto.
k_A <- (t_R_A - t_M) / t_M  # Factor de retención de la proteína A
k_B <- (t_R_B - t_M) / t_M  # Factor de retención de la proteína B

# 2. Resolución (R) entre las dos proteínas  
R <- 2 * (t_R_B - t_R_A) / (W_A + W_B)  # Resolución  

# 3. Número de platos teóricos (N) para cada proteína  
N_A <- 16 * (t_R_A / W_A)^2  # Número de platos teóricos para la proteína A  
N_B <- 16 * (t_R_B / W_B)^2  # Número de platos teóricos para la proteína B  

# 4. Altura equivalente a un plato teórico (HETP) para cada proteína  
HETP_A <- L / N_A  # HETP para la proteína A  
HETP_B <- L / N_B  # HETP para la proteína B  

# Mostrar resultados en la consola  
cat("Volumen geométrico de la columna:", round(V_columna, 2), "mL\n")
cat("Volumen de la fase móvil (V_m):", round(V_m, 2), "mL\n")
cat("Volumen de la fase estacionaria (V_s):", round(V_s, 2), "mL\n")
cat("Caudal de fase móvil (V_m / t_M):", round(caudal, 2), "mL/min\n\n")
cat("Factor de retención (k) para la proteína A:", round(k_A, 2), "\n")  
cat("Factor de retención (k) para la proteína B:", round(k_B, 2), "\n\n")  

cat("Resolución (R) entre las dos proteínas:", round(R, 2), "\n\n")  

cat("Número de platos teóricos (N) para la proteína A:", round(N_A, 2), "\n")  
cat("Número de platos teóricos (N) para la proteína B:", round(N_B, 2), "\n\n")  

cat("Altura equivalente a un plato teórico (HETP) para la proteína A:", round(HETP_A, 2), "cm\n")  
cat("Altura equivalente a un plato teórico (HETP) para la proteína B:", round(HETP_B, 2), "cm\n\n")  

# Crear un data frame para graficar los picos cromatográficos  
tiempo <- seq(0, 20, by = 0.1)  # Tiempo en minutos  
intensidad_A <- dnorm(tiempo, mean = t_R_A, sd = W_A / 4)  # Intensidad del pico de la proteína A  
intensidad_B <- dnorm(tiempo, mean = t_R_B, sd = W_B / 4)  # Intensidad del pico de la proteína B  

datos <- data.frame(  
  Tiempo = tiempo,  
  Intensidad_A = intensidad_A,  
  Intensidad_B = intensidad_B  
)  

# Gráfico de los picos cromatográficos  
p <- ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Intensidad_A, color = "Proteína A"), linewidth = 1) +  
  geom_line(aes(y = Intensidad_B, color = "Proteína B"), linewidth = 1) +  
  labs(title = "Picos Cromatográficos de las Proteínas A y B",  
       x = "Tiempo (min)",  
       y = "Intensidad",  
       color = "Proteína") +  
  theme_minimal()

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_22_cromatografia.png", plot = p, width = 8, height = 6, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Picos Cromatográficos de las Proteínas A y B
# --------------------------------------------------------------------------
# - Eje X: Tiempo, en minutos (min). Es el tiempo que tarda cada proteína en
#   salir de la columna: la proteína A sale a los 10 min y la B a los 12 min.
# - Eje Y: Intensidad. La unidad no está especificada en el archivo: es un
#   valor relativo de altura del pico, calculado con la función de densidad
#   normal (dnorm), que solo sirve para comparar qué tan alto y ancho es cada
#   pico, no es una señal medida en unidades físicas.
# - Lo que se aprecia: dos picos en forma de campana, uno para cada proteína.
#   El pico de la proteína A es más alto y estrecho (ancho de 1,5 min) y el de
#   la B es más bajo y ancho (ancho de 2,0 min). Los picos están cerca (solo
#   2 minutos entre sus máximos) y se solapan parcialmente en la base.
# - Qué significa: las proteínas se separan porque salen a tiempos distintos,
#   pero como sus picos están cerca y se solapan un poco, la separación no es
#   completa. La resolución R (adimensional) que calcula el script mide justo
#   eso: qué tan separados están los picos en relación con su ancho.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En un cromatograma, cada pico es un compuesto: su posición en el eje del
#   tiempo (min) es su tiempo de retención y su ancho (min) dice qué tan
#   disperso sale de la columna.
# - La resolución R es adimensional y compara la distancia entre dos picos con
#   sus anchos: si los picos están cerca y son anchos, se solapan y la
#   separación es mala.
# - El número de platos teóricos N (adimensional) y la altura equivalente HETP
#   (en cm) miden la eficiencia de la columna: más platos y menor HETP
#   significan picos más estrechos y mejor separación.
# - Los volúmenes de las fases caben en la columna: V_m + V_s = V_columna
#   (volumen geométrico pi por (d/2)^2 por L). El tiempo muerto es
#   t_M = V_m / F: como el archivo no da el caudal, t_M = 1 min es un dato
#   del problema y el caudal que le corresponde es V_m / t_M = 15,71 mL/min.
# - La intensidad del eje Y es un valor relativo sin unidad especificada en el
#   archivo: sirve para comparar los picos, no es una concentración medida.

