# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cromatografía en la Industria Bioquímica  
# Problema: Separación de dos proteínas mediante cromatografía de intercambio iónico  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
L <- 25  # Longitud de la columna (cm)  
d <- 1  # Diámetro de la columna (cm)  
V_s <- 10  # Volumen de la fase estacionaria (mL)  
V_m <- 40  # Volumen de la fase móvil (mL)  

# Tiempos de retención y anchos de pico  
t_R_A <- 10  # Tiempo de retención de la proteína A (min)  
W_A <- 1.5  # Ancho de pico de la proteína A (min)  
t_R_B <- 12  # Tiempo de retención de la proteína B (min)  
W_B <- 2.0  # Ancho de pico de la proteína B (min)  

# Cálculos  
# 1. Factor de retención (k) para cada proteína  
k_A <- (t_R_A - (V_m / V_m)) / (V_m / V_m)  # Factor de retención de la proteína A  
k_B <- (t_R_B - (V_m / V_m)) / (V_m / V_m)  # Factor de retención de la proteína B  

# 2. Resolución (R) entre las dos proteínas  
R <- 2 * (t_R_B - t_R_A) / (W_A + W_B)  # Resolución  

# 3. Número de platos teóricos (N) para cada proteína  
N_A <- 16 * (t_R_A / W_A)^2  # Número de platos teóricos para la proteína A  
N_B <- 16 * (t_R_B / W_B)^2  # Número de platos teóricos para la proteína B  

# 4. Altura equivalente a un plato teórico (HETP) para cada proteína  
HETP_A <- L / N_A  # HETP para la proteína A  
HETP_B <- L / N_B  # HETP para la proteína B  

# Mostrar resultados en la consola  
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
ggplot(datos, aes(x = Tiempo)) +  
  geom_line(aes(y = Intensidad_A, color = "Proteína A"), size = 1) +  
  geom_line(aes(y = Intensidad_B, color = "Proteína B"), size = 1) +  
  labs(title = "Picos Cromatográficos de las Proteínas A y B",  
       x = "Tiempo (min)",  
       y = "Intensidad",  
       color = "Proteína") +  
  theme_minimal()  

