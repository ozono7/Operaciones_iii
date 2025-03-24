
# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Simulación de Cromatografía de Intercambio Iónico  
# Objetivo: Separar y analizar dos proteínas (Quimasa y Lactoperoxidasa)  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Parámetros de la columna cromatográfica  
L <- 30  # Longitud de la columna (cm)  
d <- 1.2  # Diámetro de la columna (cm)  
V_s <- 15  # Volumen de la fase estacionaria (mL)  
V_m <- 50  # Volumen de la fase móvil (mL)  

# Datos de las proteínas  
tR_Quimasa <- 8  # Tiempo de retención de la Quimasa (min)  
W_Quimasa <- 1.2  # Ancho de pico de la Quimasa (min)  
tR_Lactoperoxidasa <- 11  # Tiempo de retención de la Lactoperoxidasa (min)  
W_Lactoperoxidasa <- 1.8  # Ancho de pico de la Lactoperoxidasa (min)  

# Cálculos  
# 1. Factor de retención (k) para cada proteína  
k_Quimasa <- (tR_Quimasa - (V_m / V_m)) / (V_m / V_m)  
k_Lactoperoxidasa <- (tR_Lactoperoxidasa - (V_m / V_m)) / (V_m / V_m)  

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
ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Quimasa, color = "Quimasa"), size = 1) +  
  geom_line(aes(y = Lactoperoxidasa, color = "Lactoperoxidasa"), size = 1) +  
  labs(  
    title = "Picos Cromatográficos de Quimasa y Lactoperoxidasa",  
    subtitle = "Separación por Cromatografía de Intercambio Iónico",  
    x = "Tiempo (min)",  
    y = "Intensidad",  
    color = "Proteína"  
  ) +  
  theme_minimal() +  
  theme(legend.position = "top")  
