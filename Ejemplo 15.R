# ==========================================================================
# Lixiviación en la Industria Minera
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Extracción de sulfato de cobre (SO₄Cu) a partir de un mineral de cobre
# ==========================================================================


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

# Masa total de disolución (agua añadida + agua del mineral + SO₄Cu extraído)
# El agua que trae el mineral se une a la disolución: si no se cuenta, el
# balance de agua del refinado no cierra.
masa_disolucion_total <- masa_agua + masa_H2O_mineral + masa_SO4Cu  # kg

# Masa de extracto (flujo superior)  
masa_extracto <- masa_disolucion_total - masa_disolucion_retenida  # kg  

# Masa de refinado (flujo inferior)  
masa_refinado <- masa_mineral + masa_agua - masa_extracto  # kg  

# Composición del extracto  
concentracion_SO4Cu_extracto <- masa_SO4Cu / masa_disolucion_total  # Fracción de SO₄Cu en el extracto  
concentracion_H2O_extracto <- 1 - concentracion_SO4Cu_extracto  # Fracción de H₂O en el extracto  

# Composición del refinado
# El extracto y la disolución retenida tienen la misma composición (disolución
# uniforme): cada uno se lleva SO₄Cu y agua en proporción a su masa.
masa_SO4Cu_extracto <- masa_SO4Cu * (masa_extracto / masa_disolucion_total)  # kg
masa_SO4Cu_refinado <- masa_SO4Cu - masa_SO4Cu_extracto  # kg
masa_H2O_extracto <- masa_extracto - masa_SO4Cu_extracto  # kg
masa_H2O_refinado <- masa_H2O_mineral + masa_agua - masa_H2O_extracto  # kg
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

# --------------------------------------------------------------------------
# Nota: este script no produce ningún gráfico
# --------------------------------------------------------------------------
# Este ejemplo solo calcula y muestra resultados en la consola (composiciones
# en %, masas en kg y porcentaje de SO₄Cu extraído).

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - En una lixiviación en una etapa se calculan los balances de masa del
#   extracto (flujo superior) y del refinado (flujo inferior), con todas las
#   composiciones en fracción másica (adimensional, aquí mostrada en %).
# - El agua que trae el mineral forma parte de la disolución: sin ella, las
#   masas de los componentes del refinado no suman su masa total.
# - Parte de la disolución queda retenida en los sólidos inertes (retención de
#   0,8 kg de disolución por kg de inertes, en kg/kg): esa disolución retenida
#   tiene la misma composición que el extracto y se lleva SO₄Cu consigo.
# - Las composiciones del refinado deben sumar 100 %: es la comprobación de
#   que el balance de masa cierra.
