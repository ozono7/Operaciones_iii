# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Cristalización de una solución salina  
# Problema: Cálculo del rendimiento de cristales de Na₂SO₄·10H₂O  

# Datos del problema  
masa_solucion <- 100000  # Masa de la solución (kg)  
concentracion_Na2SO4 <- 0.30  # Fracción en peso de Na₂SO₄ en la solución  
solubilidad_Na2SO4 <- 21.5  # Solubilidad de Na₂SO₄ anhidro (kg/1000 kg de agua)  
temperatura_final <- 228  # Temperatura final (K)  
masa_molar_Na2SO4 <- 142.04  # Masa molar de Na₂SO₄ (g/mol)  
masa_molar_H2O <- 18.015  # Masa molar de H₂O (g/mol)  
masa_molar_Na2SO4_10H2O <- masa_molar_Na2SO4 + 10 * masa_molar_H2O  # Masa molar de Na₂SO₄·10H₂O (g/mol)  

# a) Sin evaporación de agua  
# Masa de Na₂SO₄ en la solución inicial  
masa_Na2SO4_inicial <- masa_solucion * concentracion_Na2SO4  # kg  

# Masa de agua en la solución inicial  
masa_agua_inicial <- masa_solucion - masa_Na2SO4_inicial  # kg  

# Masa de Na₂SO₄ que permanece disuelta después de la cristalización  
masa_Na2SO4_disuelta <- solubilidad_Na2SO4 * (masa_agua_inicial / 1000)  # kg  

# Masa de Na₂SO₄ que cristaliza  
masa_Na2SO4_cristalizada <- masa_Na2SO4_inicial - masa_Na2SO4_disuelta  # kg  

# Masa de cristales de Na₂SO₄·10H₂O  
masa_cristales <- masa_Na2SO4_cristalizada * (masa_molar_Na2SO4_10H2O / masa_molar_Na2SO4)  # kg  

# Rendimiento de cristales  
rendimiento_a <- (masa_cristales / masa_solucion) * 100  # %  

# b) Con evaporación del 3% del peso total de la solución  
# Masa de agua perdida por evaporación  
masa_agua_perdida <- 0.03 * masa_solucion  # kg  

# Masa de agua después de la evaporación  
masa_agua_final <- masa_agua_inicial - masa_agua_perdida  # kg  

# Masa de Na₂SO₄ que permanece disuelta después de la cristalización con evaporación  
masa_Na2SO4_disuelta_evaporacion <- solubilidad_Na2SO4 * (masa_agua_final / 1000)  # kg  

# Masa de Na₂SO₄ que cristaliza con evaporación  
masa_Na2SO4_cristalizada_evaporacion <- masa_Na2SO4_inicial - masa_Na2SO4_disuelta_evaporacion  # kg  

# Masa de cristales de Na₂SO₄·10H₂O con evaporación  
masa_cristales_evaporacion <- masa_Na2SO4_cristalizada_evaporacion * (masa_molar_Na2SO4_10H2O / masa_molar_Na2SO4)  # kg  

# Rendimiento de cristales con evaporación  
rendimiento_b <- (masa_cristales_evaporacion / masa_solucion) * 100  # %  

# Mostrar resultados en la consola  
cat("a) Rendimiento de cristales sin evaporación de agua:", round(rendimiento_a, 2), "%\n")  
cat("b) Rendimiento de cristales con evaporación del 3% del peso total de la solución:", round(rendimiento_b, 2), "%\n")  

