# -----------------------------------------------------------
# Autor: José Juan Torres Martínez  
# Doctorado en Ciencias en Bioprocesos  
# Laboratorio de Biotecnología Molecular de Células Vegetales y Bioinformática  
# -----------------------------------------------------------

# Tema: Extracción Sólido-Líquido (Lixiviación) en la Industria Bioquímica  
# Problema: Lixiviación de un antibiótico a partir de un residuo biológico  

# Instalar y cargar librerías necesarias  
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)  
library(ggplot2)  

# Datos del problema  
masa_residuo <- 1000  # Masa de residuo biológico (kg)  
concentracion_antibiotico <- 0.15  # Fracción de antibiótico en el residuo  
concentracion_agua_residuo <- 0.25  # Fracción de agua en el residuo  
concentracion_inertes <- 0.60  # Fracción de sólidos inertes en el residuo  
masa_agua <- 2000  # Masa de agua utilizada (kg)  
K <- 2.5  # Relación de distribución (C_agua / C_sólidos)  
retencion <- 0.6  # Cantidad de disolución retenida por kg de sólidos inertes (kg/kg)  

# Cálculos preliminares  
masa_antibiotico <- masa_residuo * concentracion_antibiotico  # Masa de antibiótico (kg)  
masa_agua_residuo <- masa_residuo * concentracion_agua_residuo  # Masa de agua en el residuo (kg)  
masa_inertes <- masa_residuo * concentracion_inertes  # Masa de sólidos inertes (kg)  

# Masa de disolución retenida  
masa_disolucion_retenida <- masa_inertes * retencion  # kg  

# Masa total de disolución (agua + antibiótico extraído)  
masa_disolucion_total <- masa_agua + masa_antibiotico  # kg  

# Masa de extracto (flujo superior)  
masa_extracto <- masa_disolucion_total - masa_disolucion_retenida  # kg  

# Masa de refinado (flujo inferior)  
masa_refinado <- masa_residuo + masa_agua - masa_extracto  # kg  

# Composición del refinado  
masa_antibiotico_refinado <- masa_antibiotico - (masa_antibiotico * (masa_extracto / masa_disolucion_total))  # kg  
masa_agua_refinado <- masa_agua_residuo + masa_agua - (masa_disolucion_total - masa_disolucion_retenida)  # kg  
masa_inertes_refinado <- masa_inertes  # kg  

concentracion_antibiotico_refinado <- masa_antibiotico_refinado / masa_refinado  # Fracción de antibiótico en el refinado  
concentracion_agua_refinado <- masa_agua_refinado / masa_refinado  # Fracción de agua en el refinado  
concentracion_inertes_refinado <- masa_inertes_refinado / masa_refinado  # Fracción de inertes en el refinado  

# Composición de la disolución retenida  
concentracion_antibiotico_retenido <- masa_antibiotico_refinado / masa_disolucion_retenida  # Fracción de antibiótico en la disolución retenida  
concentracion_agua_retenida <- masa_agua_refinado / masa_disolucion_retenida  # Fracción de agua en la disolución retenida  

# Coordenadas del punto de mezcla  
masa_mezcla <- masa_residuo + masa_agua  # Masa total de la mezcla (kg)  
concentracion_antibiotico_mezcla <- (masa_antibiotico + 0) / masa_mezcla  # Fracción de antibiótico en la mezcla  
concentracion_agua_mezcla <- (masa_agua_residuo + masa_agua) / masa_mezcla  # Fracción de agua en la mezcla  

# Composición del extracto  
concentracion_antibiotico_extracto <- (masa_antibiotico - masa_antibiotico_refinado) / masa_extracto  # Fracción de antibiótico en el extracto  
concentracion_agua_extracto <- 1 - concentracion_antibiotico_extracto  # Fracción de agua en el extracto  

# Coordenadas del polo  
polo_x <- concentracion_antibiotico_mezcla  
polo_y <- concentracion_agua_mezcla  

# Número de etapas ideales (simplificado)  
numero_etapas <- log((masa_antibiotico - masa_antibiotico_refinado) / masa_antibiotico_refinado) / log(K)  

# Mostrar resultados en la consola  
cat("Composición del refinado:\n")  
cat("  Antibiótico:", round(concentracion_antibiotico_refinado * 100, 2), "%\n")  
cat("  Agua:", round(concentracion_agua_refinado * 100, 2), "%\n")  
cat("  Sólidos inertes:", round(concentracion_inertes_refinado * 100, 2), "%\n\n")  

cat("Composición de la disolución retenida:\n")  
cat("  Antibiótico:", round(concentracion_antibiotico_retenido * 100, 2), "%\n")  
cat("  Agua:", round(concentracion_agua_retenida * 100, 2), "%\n\n")  

cat("Coordenadas del punto de mezcla:\n")  
cat("  Antibiótico:", round(concentracion_antibiotico_mezcla * 100, 2), "%\n")  
cat("  Agua:", round(concentracion_agua_mezcla * 100, 2), "%\n\n")  

cat("Composición del extracto:\n")  
cat("  Antibiótico:", round(concentracion_antibiotico_extracto * 100, 2), "%\n")  
cat("  Agua:", round(concentracion_agua_extracto * 100, 2), "%\n\n")  

cat("Cantidad de disolución retenida:", round(masa_disolucion_retenida, 2), "kg\n\n")  

cat("Coordenadas del polo:\n")  
cat("  Antibiótico:", round(polo_x * 100, 2), "%\n")  
cat("  Agua:", round(polo_y * 100, 2), "%\n\n")  

cat("Número de etapas ideales:", round(numero_etapas, 2), "\n")  
