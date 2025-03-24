# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Lixiviación en la Industria Minera  
# Problema: Extracción de sulfato de cobre (SO₄Cu) a partir de un mineral de cobre  

# Datos del problema  
masa_mineral <- 500  # Masa del mineral (kg)  
composicion_SO4Cu <- 0.12  # Fracción de SO₄Cu en el mineral  
composicion_H2O <- 0.03  # Fracción de H₂O en el mineral  
composicion_inertes <- 0.85  # Fracción de inertes en el mineral  
masa_agua <- 3000  # Masa de agua utilizada (kg)  
retencion <- 0.8  # Cantidad de disolución retenida por kg de inertes (kg/kg)  

# Cálculos preliminares  
masa_SO4Cu <- masa_mineral * composicion_SO4Cu  # Masa de SO₄Cu en el mineral (kg)  
masa_H2O_mineral <- masa_mineral * composicion_H2O  # Masa de H₂O en el mineral (kg)  
masa_inertes <- masa_mineral * composicion_inertes  # Masa de inertes en el mineral (kg)  

# Masa de disolución retenida por los sólidos  
masa_disolucion_retenida <- masa_inertes * retencion  # kg  

# Masa total de disolución (agua + SO₄Cu extraído)  
masa_disolucion_total <- masa_agua + masa_SO4Cu  # kg  

# Masa de extracto (flujo superior)  
masa_extracto <- masa_disolucion_total - masa_disolucion_retenida  # kg  

# Masa de refinado (flujo inferior)  
masa_refinado <- masa_mineral + masa_agua - masa_extracto  # kg  

# Composición del extracto  
concentracion_SO4Cu_extracto <- masa_SO4Cu / masa_disolucion_total  # Fracción de SO₄Cu en el extracto  
concentracion_H2O_extracto <- 1 - concentracion_SO4Cu_extracto  # Fracción de H₂O en el extracto  

# Composición del refinado  
masa_SO4Cu_refinado <- masa_SO4Cu - (masa_SO4Cu * (masa_extracto / masa_disolucion_total))  # kg  
masa_H2O_refinado <- masa_H2O_mineral + masa_agua - (masa_disolucion_total - masa_disolucion_retenida)  # kg  
masa_inertes_refinado <- masa_inertes  # kg  

concentracion_SO4Cu_refinado <- masa_SO4Cu_refinado / masa_refinado  # Fracción de SO₄Cu en el refinado  
concentracion_H2O_refinado <- masa_H2O_refinado / masa_refinado  # Fracción de H₂O en el refinado  
concentracion_inertes_refinado <- masa_inertes_refinado / masa_refinado  # Fracción de inertes en el refinado  

# Porcentaje de SO₄Cu extraído  
porcentaje_extraido <- (masa_SO4Cu - masa_SO4Cu_refinado) / masa_SO4Cu * 100  # %  

# Mostrar resultados en la consola  
cat("Composición del extracto:\n")  
cat("  SO₄Cu:", round(concentracion_SO4Cu_extracto * 100, 2), "%\n")  
cat("  H₂O:", round(concentracion_H2O_extracto * 100, 2), "%\n\n")  

cat("Composición del refinado:\n")  
cat("  SO₄Cu:", round(concentracion_SO4Cu_refinado * 100, 2), "%\n")  
cat("  H₂O:", round(concentracion_H2O_refinado * 100, 2), "%\n")  
cat("  Inertes:", round(concentracion_inertes_refinado * 100, 2), "%\n\n")  

cat("Cantidad de extracto:", round(masa_extracto, 2), "kg\n")  
cat("Cantidad de refinado:", round(masa_refinado, 2), "kg\n\n")  

cat("Porcentaje de SO₄Cu extraído:", round(porcentaje_extraido, 2), "%\n")  
