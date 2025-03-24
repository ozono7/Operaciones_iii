# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cristalización en la Industria Bioquímica  
# Problema: Cristalización de una proteína a partir de una solución  

# Datos del problema  
volumen_solucion <- 1000  # Volumen de la solución (L)  
densidad_solucion <- 1.02  # Densidad de la solución (kg/L)  
concentracion_proteina <- 0.10  # Fracción en peso de proteína en la solución  
solubilidad_proteina <- 5  # Solubilidad de la proteína (g/L)  
temperatura_cristalizacion <- 277  # Temperatura de cristalización (K)  
porcentaje_evaporacion <- 0.05  # Porcentaje de agua evaporada (5%)  

# Cálculos preliminares  
masa_solucion <- volumen_solucion * densidad_solucion  # Masa de la solución (kg)  
masa_proteina_inicial <- masa_solucion * concentracion_proteina  # Masa de proteína en la solución (kg)  
masa_agua_inicial <- masa_solucion - masa_proteina_inicial  # Masa de agua en la solución (kg)  

# a) Sin evaporación de agua  
# Masa de proteína que permanece disuelta después de la cristalización  
masa_proteina_disuelta <- solubilidad_proteina * volumen_solucion / 1000  # kg  

# Masa de proteína cristalizada  
masa_proteina_cristalizada <- masa_proteina_inicial - masa_proteina_disuelta  # kg  

# Rendimiento de cristalización sin evaporación  
rendimiento_a <- (masa_proteina_cristalizada / masa_proteina_inicial) * 100  # %  

# b) Con evaporación del 5% del agua  
# Masa de agua perdida por evaporación  
masa_agua_perdida <- masa_agua_inicial * porcentaje_evaporacion  # kg  

# Masa de agua después de la evaporación  
masa_agua_final <- masa_agua_inicial - masa_agua_perdida  # kg  

# Volumen de solución después de la evaporación  
volumen_solucion_final <- (masa_agua_final + masa_proteina_inicial) / densidad_solucion  # L  

# Masa de proteína que permanece disuelta después de la cristalización con evaporación  
masa_proteina_disuelta_evaporacion <- solubilidad_proteina * volumen_solucion_final / 1000  # kg  

# Masa de proteína cristalizada con evaporación  
masa_proteina_cristalizada_evaporacion <- masa_proteina_inicial - masa_proteina_disuelta_evaporacion  # kg  

# Rendimiento de cristalización con evaporación  
rendimiento_b <- (masa_proteina_cristalizada_evaporacion / masa_proteina_inicial) * 100  # %  

# Mostrar resultados en la consola  
cat("a) Masa de proteína cristalizada sin evaporación de agua:", round(masa_proteina_cristalizada, 2), "kg\n")  
cat("   Rendimiento de cristalización sin evaporación:", round(rendimiento_a, 2), "%\n\n")  

cat("b) Masa de proteína cristalizada con evaporación del 5% del agua:", round(masa_proteina_cristalizada_evaporacion, 2), "kg\n")  
cat("   Rendimiento de cristalización con evaporación:", round(rendimiento_b, 2), "%\n")  

