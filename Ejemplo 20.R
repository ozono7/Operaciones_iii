# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cristalización en la Industria Química  
# Problema: Cristalización de sulfato de sodio (Na₂SO₄) a partir de una solución  

# Datos del problema  
volumen_solucion <- 5000  # Volumen de la solución (L)  
densidad_solucion <- 1.15  # Densidad de la solución (kg/L)  
concentracion_Na2SO4 <- 0.25  # Fracción en peso de Na₂SO₄ en la solución  
solubilidad_Na2SO4 <- 19.4  # Solubilidad de Na₂SO₄ (g/100 g de agua)  
temperatura_cristalizacion <- 293  # Temperatura de cristalización (K)  
porcentaje_evaporacion <- 0.10  # Porcentaje de agua evaporada (10%)  

# Cálculos preliminares  
masa_solucion <- volumen_solucion * densidad_solucion  # Masa de la solución (kg)  
masa_Na2SO4_inicial <- masa_solucion * concentracion_Na2SO4  # Masa de Na₂SO₄ en la solución (kg)  
masa_agua_inicial <- masa_solucion - masa_Na2SO4_inicial  # Masa de agua en la solución (kg)  

# a) Sin evaporación de agua  
# Masa de Na₂SO₄ que permanece disuelta después de la cristalización  
masa_Na2SO4_disuelta <- (solubilidad_Na2SO4 / 100) * masa_agua_inicial  # kg  

# Masa de Na₂SO₄ cristalizado  
masa_Na2SO4_cristalizado <- masa_Na2SO4_inicial - masa_Na2SO4_disuelta  # kg  

# Rendimiento de cristalización sin evaporación  
rendimiento_a <- (masa_Na2SO4_cristalizado / masa_Na2SO4_inicial) * 100  # %  

# b) Con evaporación del 10% del agua  
# Masa de agua perdida por evaporación  
masa_agua_perdida <- masa_agua_inicial * porcentaje_evaporacion  # kg  

# Masa de agua después de la evaporación  
masa_agua_final <- masa_agua_inicial - masa_agua_perdida  # kg  

# Masa de Na₂SO₄ que permanece disuelta después de la cristalización con evaporación  
masa_Na2SO4_disuelta_evaporacion <- (solubilidad_Na2SO4 / 100) * masa_agua_final  # kg  

# Masa de Na₂SO₄ cristalizado con evaporación  
masa_Na2SO4_cristalizado_evaporacion <- masa_Na2SO4_inicial - masa_Na2SO4_disuelta_evaporacion  # kg  

# Rendimiento de cristalización con evaporación  
rendimiento_b <- (masa_Na2SO4_cristalizado_evaporacion / masa_Na2SO4_inicial) * 100  # %  

# Mostrar resultados en la consola  
cat("a) Masa de Na₂SO₄ cristalizado sin evaporación de agua:", round(masa_Na2SO4_cristalizado, 2), "kg\n")  
cat("   Rendimiento de cristalización sin evaporación:", round(rendimiento_a, 2), "%\n\n")  

cat("b) Masa de Na₂SO₄ cristalizado con evaporación del 10% del agua:", round(masa_Na2SO4_cristalizado_evaporacion, 2), "kg\n")  
cat("   Rendimiento de cristalización con evaporación:", round(rendimiento_b, 2), "%\n")  

