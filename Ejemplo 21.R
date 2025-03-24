# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cristalización en la Industria de Alimentos  
# Problema: Cristalización de sacarosa a partir de una solución  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
volumen_solucion <- 2000  # Volumen de la solución (L)  
densidad_solucion <- 1.12  # Densidad de la solución (kg/L)  
concentracion_sacarosa <- 0.30  # Fracción en peso de sacarosa en la solución  
solubilidad_sacarosa <- 200  # Solubilidad de sacarosa (g/100 g de agua)  
temperatura_cristalizacion <- 293  # Temperatura de cristalización (K)  
porcentaje_evaporacion <- 0.08  # Porcentaje de agua evaporada (8%)  

# Cálculos preliminares  
masa_solucion <- volumen_solucion * densidad_solucion  # Masa de la solución (kg)  
masa_sacarosa_inicial <- masa_solucion * concentracion_sacarosa  # Masa de sacarosa en la solución (kg)  
masa_agua_inicial <- masa_solucion - masa_sacarosa_inicial  # Masa de agua en la solución (kg)  

# a) Sin evaporación de agua  
# Masa de sacarosa que permanece disuelta después de la cristalización  
# La solubilidad es 200 g de sacarosa por 100 g de agua, es decir, 2 kg de sacarosa por kg de agua  
masa_sacarosa_disuelta <- (solubilidad_sacarosa / 1000) * masa_agua_inicial  # kg  

# Masa de sacarosa cristalizada  
masa_sacarosa_cristalizada <- masa_sacarosa_inicial - masa_sacarosa_disuelta  # kg  

# Rendimiento de cristalización sin evaporación  
rendimiento_a <- (masa_sacarosa_cristalizada / masa_sacarosa_inicial) * 100  # %  

# b) Con evaporación del 8% del agua  
# Masa de agua perdida por evaporación  
masa_agua_perdida <- masa_agua_inicial * porcentaje_evaporacion  # kg  

# Masa de agua después de la evaporación  
masa_agua_final <- masa_agua_inicial - masa_agua_perdida  # kg  

# Masa de sacarosa que permanece disuelta después de la cristalización con evaporación  
masa_sacarosa_disuelta_evaporacion <- (solubilidad_sacarosa / 1000) * masa_agua_final  # kg  

# Masa de sacarosa cristalizada con evaporación  
masa_sacarosa_cristalizada_evaporacion <- masa_sacarosa_inicial - masa_sacarosa_disuelta_evaporacion  # kg  

# Rendimiento de cristalización con evaporación  
rendimiento_b <- (masa_sacarosa_cristalizada_evaporacion / masa_sacarosa_inicial) * 100  # %  

# Mostrar resultados en la consola  
cat("a) Masa de sacarosa cristalizada sin evaporación de agua:", round(masa_sacarosa_cristalizada, 2), "kg\n")  
cat("   Rendimiento de cristalización sin evaporación:", round(rendimiento_a, 2), "%\n\n")  

cat("b) Masa de sacarosa cristalizada con evaporación del 8% del agua:", round(masa_sacarosa_cristalizada_evaporacion, 2), "kg\n")  
cat("   Rendimiento de cristalización con evaporación:", round(rendimiento_b, 2), "%\n\n")  

# Crear un data frame para graficar  
porcentajes_evaporacion <- seq(0, 0.10, by = 0.01)  # Porcentajes de evaporación de 0% a 10%  
masas_cristalizadas <- numeric(length(porcentajes_evaporacion))  

for (i in 1:length(porcentajes_evaporacion)) {  
  masa_agua_perdida <- masa_agua_inicial * porcentajes_evaporacion[i]  
  masa_agua_final <- masa_agua_inicial - masa_agua_perdida  
  masa_sacarosa_disuelta <- (solubilidad_sacarosa / 1000) * masa_agua_final  
  masas_cristalizadas[i] <- masa_sacarosa_inicial - masa_sacarosa_disuelta  
}  

datos <- data.frame(  
  Evaporacion = porcentajes_evaporacion * 100,  # Convertir a porcentaje  
  Masa_Cristalizada = masas_cristalizadas  
)  

# Gráfico de la masa de sacarosa cristalizada en función de la evaporación de agua  
ggplot(datos, aes(x = Evaporacion, y = Masa_Cristalizada)) +  
  geom_line(color = "blue", size = 1) +  
  geom_point(color = "blue", size = 3) +  
  labs(title = "Masa de Sacarosa Cristalizada en Función de la Evaporación de Agua",  
       x = "Evaporación de Agua (%)",  
       y = "Masa de Sacarosa Cristalizada (kg)") +  
  theme_minimal()  

