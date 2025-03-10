# -----------------------------------------------------------
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# -----------------------------------------------------------

# Cargar librerías
library(psychrolib)  # Para cálculos psicrométricos
library(ggplot2)     # Para crear gráficos

# Establecer el sistema de unidades
SetUnitSystem("SI")  # Usar unidades del Sistema Internacional (SI)

# ---------------------- Paso 1: Obtener las humedades absolutas inicial y final ----------------------

# Datos de entrada
Tdb1 <- 20  # Temperatura de bulbo seco inicial en °C
HR1 <- 0.30  # Humedad relativa inicial (30%)
Tdb2 <- 25  # Temperatura de bulbo seco final en °C
HR2 <- 0.60  # Humedad relativa final (60%)
P_atm <- 101325  # Presión atmosférica en Pa

# Obtener humedad absoluta inicial y final (kg vapor/kg aire seco)
W1 <- GetHumRatioFromRelHum(Tdb1, HR1, P_atm)  # Humedad absoluta inicial
W2 <- GetHumRatioFromRelHum(Tdb2, HR2, P_atm)  # Humedad absoluta final

# Convertir de kg/kg a g/kg
W1_g <- W1 * 1000  # Humedad absoluta inicial en g/kg
W2_g <- W2 * 1000  # Humedad absoluta final en g/kg

cat("Humedad absoluta inicial (W1):", W1_g, "g/kg\n")
cat("Humedad absoluta final (W2):", W2_g, "g/kg\n")

# ---------------------- Paso 2: Calcular la cantidad de agua agregada ----------------------

# Caudal volumétrico de aire
Q_aire <- 5000  # m³/h

# Densidad del aire a temperatura media (~1.2 kg/m³)
densidad_aire <- 1.2  # kg/m³

# Flujo másico de aire
m_aire <- Q_aire * densidad_aire  # kg/h

# Agua agregada (kg/h)
agua_agregada <- m_aire * (W2 - W1)  # Cantidad de agua agregada al aire

cat("Flujo de agua agregada:", agua_agregada, "kg/h\n")

# ---------------------- Paso 3: Calcular la energía térmica requerida ----------------------

# Calcular entalpía inicial y final (kJ/kg)
h1 <- 1.006 * Tdb1 + W1 * (2501 + 1.86 * Tdb1)  # Entalpía inicial
h2 <- 1.006 * Tdb2 + W2 * (2501 + 1.86 * Tdb2)  # Entalpía final

# Energía térmica requerida (kJ/h)
Q_total_kJ <- m_aire * (h2 - h1)  # Energía térmica en kJ/h

# Convertir a kW
Q_total_kW <- Q_total_kJ / 3600  # Energía térmica en kW

cat("Energía térmica requerida:", Q_total_kW, "kW\n")

# ---------------------- Paso 4: Graficar los resultados ----------------------

# Generar datos para el diagrama psicrométrico
Tdb_seq <- seq(-10, 50, by = 1)  # Rango de temperaturas de bulbo seco

# Curva de saturación (100% HR)
HR_values <- seq(0.1, 1, by = 0.1)  # Valores de humedad relativa
df_hr <- expand.grid(Tdb = Tdb_seq, HR = HR_values)  # Dataframe para almacenar los datos
df_hr$W <- mapply(function(T, HR) GetHumRatioFromRelHum(T, HR, P_atm), df_hr$Tdb, df_hr$HR) * 1000  # Humedad absoluta en g/kg
df_saturacion <- subset(df_hr, HR == 1)  # Curva de saturación (HR = 100%)

# Crear gráfico
p <- ggplot() +
  # Líneas de humedad relativa
  geom_line(data = df_hr, aes(x = Tdb, y = W, group = HR, color = factor(HR * 100)), size = 0.8) +
  
  # Curva de saturación
  geom_line(data = df_saturacion, aes(x = Tdb, y = W), color = "blue", size = 1.5) +
  
  # Proceso de humidificación
  geom_segment(aes(x = Tdb1, y = W1_g, xend = Tdb2, yend = W2_g), arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
  
  # Puntos inicial y final
  geom_point(aes(x = Tdb1, y = W1_g), color = "blue", size = 4) +
  geom_point(aes(x = Tdb2, y = W2_g), color = "red", size = 4) +
  
  # Etiquetas
  geom_text(aes(x = Tdb1, y = W1_g, label = "Inicio"), vjust = -1, color = "blue", size = 5) +
  geom_text(aes(x = Tdb2, y = W2_g, label = "Final"), vjust = -1, color = "red", size = 5) +
  
  # Estilos del gráfico
  labs(title = "Proceso de Humidificación en el Diagrama Psicrométrico",
       x = "Temperatura de Bulbo Seco (°C)",
       y = "Humedad Absoluta (g de vapor/kg aire seco)",
       color = "Humedad Relativa (%)") +
  theme_minimal() +
  theme(legend.position = "right")

# Mostrar gráfico
print(p)

