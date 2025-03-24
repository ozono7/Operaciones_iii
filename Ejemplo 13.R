# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Sólido-Líquido (Lixiviación)  
# Problema: Adsorción y desorción de plomo en carbón activado  

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
  # Concentración de equilibrio (Langmuir)  
  Ce <- C0 / (1 + K * qmax * m_adsorbente / V_solucion)  
  
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
ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Adsorcion, color = "Adsorción"), size = 1) +  
  geom_line(aes(y = Desorcion, color = "Desorción"), size = 1) +  
  labs(title = "Adsorción y Desorción de Plomo en Carbón Activado",  
       x = "Tiempo (minutos)",  
       y = "Cantidad de Plomo (mg)",  
       color = "Proceso") +  
  theme_minimal() +  
  theme(legend.position = "right")  

# Mostrar resultados en la consola  
cat("Concentración de equilibrio de plomo en la solución:", round(Ce, 2), "mg/L\n")  
cat("Cantidad de plomo adsorbido:", round(cantidad_adsorbida, 2), "mg\n")  
cat("Cantidad de plomo desorbido:", round(cantidad_desorbida, 2), "mg\n")  
