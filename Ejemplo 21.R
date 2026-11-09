# ==========================================================================
# Cristalización en la Industria de Alimentos
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Cristalización de sacarosa a partir de una solución
# ==========================================================================


# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
volumen_solucion <- 2000  # Volumen de la solución (L)  
# Densidad de una solución de sacarosa al 70 % en peso a 20 °C: 1,34956 g/mL
# (NBS Circular 440, tabla 109; aquí 1,35 kg/L).
densidad_solucion <- 1.35  # Densidad de la solución (kg/L)  
# Fracción en peso de sacarosa en la solución: 0,70, por encima de la
# fracción de saturación a 293 K (200 g por cada 100 g de agua = 200/300
# = 0,667). Una solución al 70 % se disuelve completa en caliente (a 50 °C
# la solubilidad es de unos 260 g por cada 100 g de agua, es decir, una
# fracción de saturación de 0,722) y al enfriarla a 293 K queda
# sobresaturada y cristaliza.
concentracion_sacarosa <- 0.70  # Fracción en peso de sacarosa en la solución  
# Solubilidad de sacarosa a 20 °C: 200 g por cada 100 g de agua (valor
# redondeado de manual; el valor de referencia es 203,9 g por cada 100 g
# de agua, CRC Handbook of Chemistry and Physics).
solubilidad_sacarosa <- 200  # Solubilidad de sacarosa (g/100 g de agua)  
temperatura_cristalizacion <- 293  # Temperatura de cristalización (K)  
porcentaje_evaporacion <- 0.08  # Porcentaje de agua evaporada (8%)  

# Cálculos preliminares  
masa_solucion <- volumen_solucion * densidad_solucion  # Masa de la solución (kg)  
masa_sacarosa_inicial <- masa_solucion * concentracion_sacarosa  # Masa de sacarosa en la solución (kg)  
masa_agua_inicial <- masa_solucion - masa_sacarosa_inicial  # Masa de agua en la solución (kg)

# Comprobación de saturación: con una solubilidad de 2 kg de sacarosa por kg
# de agua, disolver toda la sacarosa inicial exigiría 945 kg de agua y la
# solución solo trae 810 kg: está sobresaturada a 293 K y cristaliza al
# enfriarla.
masa_agua_saturacion <- masa_sacarosa_inicial / (solubilidad_sacarosa / 100)  # kg  

# a) Sin evaporación de agua  
# Masa de sacarosa que permanece disuelta después de la cristalización  
# La solubilidad es 200 g de sacarosa por 100 g de agua, es decir, 2 kg de sacarosa por kg de agua  
masa_sacarosa_disuelta <- (solubilidad_sacarosa / 100) * masa_agua_inicial  # kg  

# Masa de sacarosa cristalizada
# La masa disuelta no puede superar a la inicial: si la solución está por
# debajo de la saturación, no cristaliza nada (la masa no puede ser negativa).
masa_sacarosa_cristalizada <- max(0, masa_sacarosa_inicial - masa_sacarosa_disuelta)  # kg

# Rendimiento de cristalización sin evaporación  
rendimiento_a <- (masa_sacarosa_cristalizada / masa_sacarosa_inicial) * 100  # %  

# b) Con evaporación del 8% del agua  
# Masa de agua perdida por evaporación  
masa_agua_perdida <- masa_agua_inicial * porcentaje_evaporacion  # kg  

# Masa de agua después de la evaporación  
masa_agua_final <- masa_agua_inicial - masa_agua_perdida  # kg  

# Masa de sacarosa que permanece disuelta después de la cristalización con evaporación  
masa_sacarosa_disuelta_evaporacion <- (solubilidad_sacarosa / 100) * masa_agua_final  # kg  

# Masa de sacarosa cristalizada con evaporación
masa_sacarosa_cristalizada_evaporacion <- max(0, masa_sacarosa_inicial - masa_sacarosa_disuelta_evaporacion)  # kg

# Rendimiento de cristalización con evaporación  
rendimiento_b <- (masa_sacarosa_cristalizada_evaporacion / masa_sacarosa_inicial) * 100  # %  

# Mostrar resultados en la consola  
cat("a) Masa de sacarosa cristalizada sin evaporación de agua:", round(masa_sacarosa_cristalizada, 2), "kg\n")  
cat("   Rendimiento de cristalización sin evaporación:", round(rendimiento_a, 2), "%\n\n")  

cat("b) Masa de sacarosa cristalizada con evaporación del 8% del agua:", round(masa_sacarosa_cristalizada_evaporacion, 2), "kg\n")  
cat("   Rendimiento de cristalización con evaporación:", round(rendimiento_b, 2), "%\n\n")

# La solución ya está sobresaturada: para disolver toda la sacarosa harían
# falta 945 kg de agua y solo hay 810 kg, así que cristaliza al enfriarla
# sin necesidad de evaporar nada.
cat("La solución está sobresaturada a 293 K: para disolver toda la sacarosa harían falta",
    round(masa_agua_saturacion, 2), "kg de agua y solo hay",
    round(masa_agua_inicial, 2), "kg.\n\n")

# Crear un data frame para graficar  
porcentajes_evaporacion <- seq(0, 0.10, by = 0.01)  # Porcentajes de evaporación de 0% a 10%  
masas_cristalizadas <- numeric(length(porcentajes_evaporacion))  

for (i in 1:length(porcentajes_evaporacion)) {  
  masa_agua_perdida <- masa_agua_inicial * porcentajes_evaporacion[i]  
  masa_agua_final <- masa_agua_inicial - masa_agua_perdida  
  masa_sacarosa_disuelta <- (solubilidad_sacarosa / 100) * masa_agua_final  
  masas_cristalizadas[i] <- max(0, masa_sacarosa_inicial - masa_sacarosa_disuelta)  
}  

datos <- data.frame(  
  Evaporacion = porcentajes_evaporacion * 100,  # Convertir a porcentaje  
  Masa_Cristalizada = masas_cristalizadas  
)  

# Gráfico de la masa de sacarosa cristalizada en función de la evaporación de agua  
p <- ggplot(datos, aes(x = Evaporacion, y = Masa_Cristalizada)) +  
  geom_line(color = "blue", linewidth = 1) +  
  geom_point(color = "blue", size = 3) +  
  labs(title = "Masa de Sacarosa Cristalizada en Función de la Evaporación de Agua",  
       x = "Evaporación de Agua (%)",  
       y = "Masa de Sacarosa Cristalizada (kg)") +  
  theme_minimal()

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_21_sacarosa.png", plot = p, width = 8, height = 6, dpi = 300)

# --------------------------------------------------------------------------
# Interpretación: Masa de Sacarosa Cristalizada en Función de la Evaporación de Agua
# --------------------------------------------------------------------------
# - Eje X: Evaporación de agua, en porcentaje (%). Es adimensional: es la
#   fracción de agua inicial que se evapora, expresada de 0 % a 10 %.
# - Eje Y: Masa de sacarosa cristalizada, en kilogramos (kg). Es la masa de
#   cristales que se forma al enfriar la solución; si la solución no está
#   saturada, es cero.
# - Lo que se aprecia: la línea sube en todo el intervalo de 0 % a 10 % de
#   evaporación, de 270 kg a 432 kg de sacarosa cristalizada: a más
#   evaporación, más cristales.
# - Qué significa: la solución trae 810 kg de agua para 1890 kg de sacarosa,
#   es decir, unos 233 g de sacarosa por cada 100 g de agua, por encima de
#   la solubilidad (200 g por cada 100 g de agua). Al enfriarla a 293 K el
#   excedente sale como cristales: 270 kg sin evaporar (rendimiento
#   14,29 %) y 399,60 kg evaporando el 8 % del agua (rendimiento 21,14 %).
#   Evaporar agua deja menos agua para retener sacarosa disuelta, así que
#   cristaliza más.

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - La cristalización por enfriamiento y evaporación se rige por la solubilidad:
#   solo cabe disuelta una masa fija de soluto por cada masa de disolvente.
# - Antes de calcular un rendimiento hay que comprobar si la solución está
#   saturada: con 200 g por cada 100 g de agua, la saturación es una fracción
#   en peso de 200/300 = 0,667. La solución del archivo trae 0,70, así que
#   está sobresaturada a 293 K y cristaliza al enfriarla; con 0,30 no
#   cristalizaría nada.
# - Una solución concentrada se prepara en caliente (a 50 °C la solubilidad es
#   de unos 260 g por cada 100 g de agua) y cristaliza al enfriarla: el
#   enfriamiento baja la solubilidad y el excedente sale como cristales.
# - Evaporar agua deja menos agua para retener soluto disuelto: al pasar de
#   0 % a 8 % de evaporación, la masa cristalizada sube de 270 kg a 399,60 kg
#   y el rendimiento de 14,29 % a 21,14 %.
# - La solubilidad de 200 g por cada 100 g de agua equivale a 2 kg de sacarosa
#   por kg de agua: confundir g/100 g con g/kg divide el resultado entre 10.
# - La evaporación se expresa en % (adimensional) y la masa cristalizada en
#   kilogramos (kg); una masa cristalizada nunca puede ser negativa.

