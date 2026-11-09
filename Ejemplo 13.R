# ==========================================================================
# Extracción Sólido-Líquido (Lixiviación)
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Adsorción y desorción de plomo en carbón activado
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
C0 <- 100  # Concentración inicial de plomo en la solución (mg/L)  
qmax <- 50  # Capacidad máxima de adsorción (mg/g)  
K <- 0.1  # Constante de equilibrio (L/mg)  
m_adsorbente <- 1  # Masa de carbón activado (g)  
V_solucion <- 1  # Volumen de la solución (L)  
eficiencia_desorcion <- 0.8  # Eficiencia de desorción (80%)  

# Función para calcular la cantidad de plomo adsorbido  
calcular_adsorcion <- function(C0, qmax, K, m_adsorbente, V_solucion) {  
  # Concentración de equilibrio: el balance de masa V*(C0 - Ce) = m*q junto
  # con la isoterma de Langmuir q = qmax*K*Ce/(1 + K*Ce) lleva a la ecuación
  # cuadrática K*Ce^2 + (1 + m*qmax*K/V - C0*K)*Ce - C0 = 0, de la que se
  # toma la raíz positiva. Así el plomo que sale de la solución es
  # exactamente el que queda adsorbido en el carbón.
  coef_b <- 1 + m_adsorbente * qmax * K / V_solucion - C0 * K
  Ce <- (-coef_b + sqrt(coef_b^2 + 4 * K * C0)) / (2 * K)
  
  # Cantidad de plomo adsorbido  
  q <- qmax * K * Ce / (1 + K * Ce)  
  cantidad_adsorbida <- q * m_adsorbente  
  
  return(list(Ce = Ce, q = q, cantidad_adsorbida = cantidad_adsorbida))  
}  

# Función para calcular la cantidad de plomo desorbido  
calcular_desorcion <- function(cantidad_adsorbida, eficiencia_desorcion) {  
  cantidad_desorbida <- cantidad_adsorbida * eficiencia_desorcion  
  return(cantidad_desorbida)  
}  

# Aplicar las funciones  
resultados_adsorcion <- calcular_adsorcion(C0, qmax, K, m_adsorbente, V_solucion)  
Ce <- resultados_adsorcion$Ce  
q <- resultados_adsorcion$q  
cantidad_adsorbida <- resultados_adsorcion$cantidad_adsorbida  

cantidad_desorbida <- calcular_desorcion(cantidad_adsorbida, eficiencia_desorcion)  

# Crear un data frame para graficar  
tiempo <- seq(0, 120, by = 10)  # Tiempo en minutos  
adsorcion <- cantidad_adsorbida * (1 - exp(-0.05 * tiempo))  # Simulación de la adsorción  
desorcion <- cantidad_desorbida * (1 - exp(-0.03 * (tiempo - 60)))  # Simulación de la desorción  

datos <- data.frame(  
  Tiempo = tiempo,  
  Adsorcion = ifelse(tiempo <= 60, adsorcion, NA),  # Adsorción ocurre en los primeros 60 minutos  
  Desorcion = ifelse(tiempo > 60, desorcion, NA)  # Desorción ocurre después de 60 minutos  
)  

# Gráfico de adsorción y desorción  
p <- ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Adsorcion, color = "Adsorción"), linewidth = 1) +  
  geom_line(aes(y = Desorcion, color = "Desorción"), linewidth = 1) +  
  labs(title = "Adsorción y Desorción de Plomo en Carbón Activado",  
       x = "Tiempo (minutos)",  
       y = "Cantidad de Plomo (mg)",  
       color = "Proceso") +  
  theme_minimal() +  
  theme(legend.position = "right")

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_13_plomo.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Adsorción y Desorción de Plomo en Carbón Activado
# --------------------------------------------------------------------------
# - Eje X: Tiempo, en minutos (min). Va de 0 a 120 minutos.
# - Eje Y: Cantidad de plomo, en miligramos (mg). Es la masa de plomo retenida
#   en el carbón (adsorción) o liberada de él (desorción).
# - Lo que se aprecia: la curva de adsorción (primeros 60 minutos) sube rápido
#   al principio y luego se aplana al acercarse a un valor máximo: el carbón se
#   va saturando de plomo. A partir de los 60 minutos aparece la curva de
#   desorción, que también sube y se aplana: el plomo sale del carbón, pero no
#   todo, porque la eficiencia de desorción del archivo es del 80 %.
# - Qué significa: el carbón activado retiene el plomo del agua hasta llegar a
#   su capacidad de equilibrio (isoterma de Langmuir, con qmax = 50 mg/g y
#   K = 0,1 L/mg según el archivo), y después puede regenerarse parcialmente
#   liberando el plomo adsorbido. Las curvas son una simulación con forma
#   exponencial, no datos medidos minuto a minuto.
#   Con los datos del archivo, la concentración de equilibrio es Ce = 57,42 mg/L
#   y la cantidad adsorbida es 42,58 mg: el balance cierra, porque el plomo que
#   sale de la solución (1 L x (100 - 57,42) mg/L) es el adsorbido.

# Mostrar resultados en la consola  
cat("Concentración de equilibrio de plomo en la solución:", round(Ce, 2), "mg/L\n")  
cat("Cantidad de plomo adsorbido:", round(cantidad_adsorbida, 2), "mg\n")  
cat("Cantidad de plomo desorbido:", round(cantidad_desorbida, 2), "mg\n")  

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La adsorción sube rápido al inicio y se aplana al saturarse el adsorbente:
#   la curva se acerca a un valor máximo y ya no crece aunque pase más tiempo.
# - La isoterma de Langmuir describe ese máximo: qmax es la capacidad máxima
#   en mg de plomo por g de carbón (mg/g) y K es la constante de equilibrio
#   en L/mg.
# - La desorción no recupera todo el plomo: con una eficiencia del 80 %, una
#   parte queda retenida en el carbón.
# - Las curvas de este script son una simulación para visualizar el proceso,
#   no mediciones experimentales.
