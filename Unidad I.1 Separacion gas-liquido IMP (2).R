# -----------------------------------------------------------
# Autor: José Juan Torres Martínez
# Doctorado en Ciencias en Bioprocesos
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática
# -----------------------------------------------------------
install.packages("ggplot2")
install.packages("psychrolib")

# Cargar librerías y asegurarse de que estén instaladas
if (!require(psychrolib)) install.packages("psychrolib", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)


library(psychrolib)
library(ggplot2)

# Configurar unidades en el sistema internacional (SI)
SetUnitSystem("SI")

# Definir el rango de temperaturas de bulbo seco (TBS)
temperaturas_tbs <- seq(-10, 50, by = 1)

# Función para calcular la humedad absoluta a partir de la humedad relativa
calcular_humedad_absoluta <- function(TBS, HR) {
  return(GetHumRatioFromRelHum(TBS, HR, 101325))  # Presión atmosférica: 101325 Pa
}

# 1 - Líneas de humedad relativa (HR)
humedad_relativa <- seq(0.1, 1, by = 0.1)
datos_hr <- data.frame(
  TBS = rep(temperaturas_tbs, times = length(humedad_relativa)),
  HR = rep(humedad_relativa, each = length(temperaturas_tbs))
)
datos_hr$Humedad_Absoluta <- mapply(calcular_humedad_absoluta, datos_hr$TBS, datos_hr$HR) * 1000  # g/kg

# 2 - Curva de saturación (100% HR)
datos_saturacion <- subset(datos_hr, HR == 1)

# 3 - Líneas de entalpía constante
entalpias <- seq(10, 120, by = 10)  # kJ/kg
datos_entalpia <- data.frame()
for (h in entalpias) {
  for (TBS in temperaturas_tbs) {
    humedad_absoluta <- (h - 1.006 * TBS) / (2501 + 1.86 * TBS) * 1000  # Convertir kg/kg a g/kg
    datos_entalpia <- rbind(datos_entalpia, data.frame(TBS = TBS, Humedad_Absoluta = humedad_absoluta, Entalpía = h))
  }
}
etiquetas_entalpia <- datos_entalpia[datos_entalpia$TBS == max(temperaturas_tbs), ]

# 4 - Líneas de temperatura de bulbo húmedo (TBH) constante
temperaturas_tbh <- seq(0, 40, by = 5)
datos_tbh <- data.frame()
for (TBH in temperaturas_tbh) {
  for (TBS in temperaturas_tbs) {
    if (TBH <= TBS) {  # TBH debe ser menor o igual a TBS
      humedad_absoluta <- GetHumRatioFromTWetBulb(TBS, TBH, 101325) * 1000  # g/kg
      datos_tbh <- rbind(datos_tbh, data.frame(TBS = TBS, Humedad_Absoluta = humedad_absoluta, TBH = TBH))
    }
  }
}
etiquetas_tbh <- datos_tbh[datos_tbh$TBS == min(temperaturas_tbs), ]

# 5 - Gráfica del diagrama psicrométrico
ggplot() +
  geom_line(data = datos_hr, aes(x = TBS, y = Humedad_Absoluta, group = HR, color = factor(HR * 100)), size = 0.8) +
  geom_line(data = datos_saturacion, aes(x = TBS, y = Humedad_Absoluta), color = "blue", size = 1.5) +
  geom_line(data = datos_entalpia, aes(x = TBS, y = Humedad_Absoluta, group = Entalpía), color = "red", linetype = "dashed", size = 0.7) +
  geom_text(data = etiquetas_entalpia, aes(x = TBS, y = Humedad_Absoluta, label = paste(Entalpía, "kJ/kg")), color = "red", hjust = 0, size = 3) +
  geom_line(data = datos_tbh, aes(x = TBS, y = Humedad_Absoluta, group = TBH), color = "green", linetype = "dotted", size = 0.7) +
  geom_text(data = etiquetas_tbh, aes(x = TBS, y = Humedad_Absoluta, label = paste(TBH, "°C")), color = "green", hjust = 1, size = 3) +
  labs(title = "Diagrama Psicrométrico", 
       x = "Temperatura de Bulbo Seco (°C)", 
       y = "Humedad Absoluta (g/kg de aire seco)", 
       color = "Humedad Relativa (%)") +
  theme_minimal() +
  theme(legend.position = "right")


