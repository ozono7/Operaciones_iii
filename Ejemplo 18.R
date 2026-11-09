# ==========================================================================
# Cristalización de una solución salina
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Cálculo del rendimiento de cristales de Na₂SO₄·10H₂O
# ==========================================================================


# Datos del problema  
masa_solucion <- 100000  # Masa de la solución (kg)  
concentracion_Na2SO4 <- 0.30  # Fracción en peso de Na₂SO₄ en la solución  
solubilidad_Na2SO4 <- 21.5  # Solubilidad de Na₂SO₄ anhidro (kg/1000 kg de agua)  
# La temperatura final del archivo era 228 K (-45 °C), imposible para una
# cristalización acuosa: el agua se congela a 273 K. Se corrige a 293 K
# (20 °C), la temperatura de cristalización del mismo tipo de problema que
# el ejemplo 12.11-1 de Geankoplis (Transport Processes and Separation
# Process Principles: solución al 30 %, cristales decahidrato, solubilidad
# 21,5, evaporación del 3 %) y la misma que usan los Ejemplos 20 y 21 de
# este repositorio. El cálculo no usa la temperatura: la solubilidad es el
# dato del problema.
temperatura_final <- 293  # Temperatura final (K)  
masa_molar_Na2SO4 <- 142.04  # Masa molar de Na₂SO₄ (g/mol)  
masa_molar_H2O <- 18.015  # Masa molar de H₂O (g/mol)  
masa_molar_Na2SO4_10H2O <- masa_molar_Na2SO4 + 10 * masa_molar_H2O  # Masa molar de Na₂SO₄·10H₂O (g/mol)  

# Fracciones en masa dentro del cristal decahidrato: 142,04/322,19 de
# Na₂SO₄ anhidro y el resto agua de hidratación.
fraccion_anhidra_cristal <- masa_molar_Na2SO4 / masa_molar_Na2SO4_10H2O  # kg anhidro / kg cristal
fraccion_agua_cristal <- 1 - fraccion_anhidra_cristal  # kg agua / kg cristal

# a) Sin evaporación de agua  
# Masa de Na₂SO₄ en la solución inicial  
masa_Na2SO4_inicial <- masa_solucion * concentracion_Na2SO4  # kg  

# Masa de agua en la solución inicial  
masa_agua_inicial <- masa_solucion - masa_Na2SO4_inicial  # kg  

# Masa de cristales de Na₂SO₄·10H₂O. El balance tiene en cuenta el agua de
# hidratación: los cristales se llevan agua de la solución (Geankoplis,
# cap. 12: cuando los cristales son hidratados, parte del agua de la
# solución sale con los cristales). Si P es la masa de cristales, el agua
# que queda líquida es masa_agua_inicial - P * fraccion_agua_cristal, y el
# Na₂SO₄ disuelto en ella es la solubilidad por esa agua. El balance de
# Na₂SO₄ es:
#   inicial = disuelto + anhidro en los cristales
#   masa_Na2SO4_inicial = solubilidad * agua_líquida / 1000
#                         + P * fraccion_anhidra_cristal
# De ahí se despeja P.
masa_cristales <- (masa_Na2SO4_inicial - solubilidad_Na2SO4 * masa_agua_inicial / 1000) /
  (fraccion_anhidra_cristal - solubilidad_Na2SO4 / 1000 * fraccion_agua_cristal)  # kg  

# Masa de Na₂SO₄ anhidro que sale como cristales y masa que queda disuelta
masa_Na2SO4_cristalizada <- masa_cristales * fraccion_anhidra_cristal  # kg  
masa_Na2SO4_disuelta <- masa_Na2SO4_inicial - masa_Na2SO4_cristalizada  # kg  

# Rendimiento de cristales: soluto recuperado como cristales entre soluto
# inicial (la definición unificada de los Ejemplos 18, 19 y 20; véase
# "Qué aprendimos"). La masa de cristales no se usa como numerador porque
# el decahidrato incluye agua de hidratación y superaría a la masa de
# soluto inicial.
rendimiento_a <- (masa_Na2SO4_cristalizada / masa_Na2SO4_inicial) * 100  # %  

# b) Con evaporación del 3% del peso total de la solución  
# Masa de agua perdida por evaporación  
masa_agua_perdida <- 0.03 * masa_solucion  # kg  

# Masa de agua después de la evaporación  
masa_agua_final <- masa_agua_inicial - masa_agua_perdida  # kg  

# Masa de cristales de Na₂SO₄·10H₂O con evaporación (mismo balance que
# en el caso a, con el agua que queda después de evaporar)
masa_cristales_evaporacion <- (masa_Na2SO4_inicial - solubilidad_Na2SO4 * masa_agua_final / 1000) /
  (fraccion_anhidra_cristal - solubilidad_Na2SO4 / 1000 * fraccion_agua_cristal)  # kg  

# Masa de Na₂SO₄ anhidro que sale como cristales con evaporación
masa_Na2SO4_cristalizada_evaporacion <- masa_cristales_evaporacion * fraccion_anhidra_cristal  # kg  

# Rendimiento de cristales con evaporación
rendimiento_b <- (masa_Na2SO4_cristalizada_evaporacion / masa_Na2SO4_inicial) * 100  # %  

# Mostrar resultados en la consola  
cat("a) Masa de cristales de Na2SO4·10H2O sin evaporación de agua:", round(masa_cristales, 2), "kg\n")
cat("   Rendimiento de cristalización sin evaporación:", round(rendimiento_a, 2), "%\n\n")
cat("b) Masa de cristales de Na2SO4·10H2O con evaporación del 3% del peso total de la solución:", round(masa_cristales_evaporacion, 2), "kg\n")
cat("   Rendimiento de cristalización con evaporación:", round(rendimiento_b, 2), "%\n")

# --------------------------------------------------------------------------
# Nota: este script no produce ningún gráfico
# --------------------------------------------------------------------------
# Este ejemplo solo calcula y muestra resultados en la consola (masas de
# cristales en kg y rendimientos en %, con y sin evaporación de agua).

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - Al enfriar una solución, solo cabe disuelta la masa que permite la
#   solubilidad: el resto del soluto sale como cristales.
# - Los cristales de Na₂SO₄·10H₂O pesan más que el Na₂SO₄ anhidro que
#   contienen, porque cada unidad lleva 10 moléculas de agua: la masa de
#   cristales se obtiene con el cociente de masas molares (322,19/142,04).
# - Cuando los cristales son hidratados, parte del agua de la solución sale
#   con ellos: el agua que queda líquida para retener soluto disuelto es
#   menor, así que cristalizan más kilos que si se ignorara esa agua
#   (66 447 kg en vez de 64 635 kg sin evaporación).
# - Evaporar parte del agua antes de cristalizar deja menos agua para retener
#   soluto disuelto, así que cristaliza más y el rendimiento (en %) sube.
# - El rendimiento se expresa sobre la masa de soluto inicial: soluto
#   recuperado como cristales entre soluto inicial. Es la misma definición
#   que en los Ejemplos 19 y 20. La masa de cristales no sirve como
#   numerador aquí porque el decahidrato es 55,91 % agua y superaría a la
#   masa de soluto inicial.

