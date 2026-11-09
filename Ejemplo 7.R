# ==========================================================================
# Diseño de una torre de absorción y el cálculo del número de etapas usando el método de McCabe-Thiele
# ==========================================================================
# Autor: Dr. José Juan Torres Martínez
# Docente de Ciencias Químicas
# Universidad Autónoma de San Luis Potosí (UASLP)
# México
#
# Material académico para estudiantes
#
# Descripción: Diseño de una torre de absorción y el cálculo del número de etapas usando el método de McCabe-Thiele.
# ==========================================================================


library(ggplot2)  # Para crear gráficos

# ---------------------- Datos del problema ----------------------
flujo_gas <- 100  # Flujo de gas en kg/h
flujo_liquido <- 1.5 * flujo_gas  # Flujo de líquido en kg/h (1.5 veces el flujo de gas)
y_entrada <- 0.02  # Fracción molar de A en el gas de entrada
y_salida <- 0.002  # Fracción molar de A en el gas de salida
pendiente_equilibrio <- 1.5  # Pendiente de la línea de equilibrio (relación y/x en equilibrio)

# ---------------------- Balance de masa ----------------------
x_entrada <- 0  # Se asume que el líquido entra puro (sin soluto A)
x_salida <- (flujo_gas / flujo_liquido) * (y_entrada - y_salida)  # Fracción molar de A en el líquido de salida

# ---------------------- Definir las líneas de equilibrio y operación ----------------------

# Línea de equilibrio: y = m * x
# Representa la relación entre las fracciones molares de A en el gas y el líquido en equilibrio.
linea_equilibrio <- function(x) pendiente_equilibrio * x

# Línea de operación: y = y_salida + (L/G) * (x - x_entrada)
# Representa la relación entre las fracciones molares de A en el gas y el líquido durante la operación.
# Pasa por (x_entrada, y_salida) arriba de la torre y por (x_salida, y_entrada) abajo.
linea_operacion <- function(x) y_salida + (flujo_liquido / flujo_gas) * (x - x_entrada)

# ---------------------- Generar puntos para graficar ----------------------
# Se generan valores de x (fracción molar de A en el líquido) desde 0 hasta x_salida.
x_valores <- seq(0, x_salida, length.out = 100)

# Se calculan los valores de y correspondientes a la línea de equilibrio y la línea de operación.
y_equilibrio <- linea_equilibrio(x_valores)
y_operacion <- linea_operacion(x_valores)

# ---------------------- Cálculo del número de etapas ----------------------
# Se empieza arriba de la torre, en el punto (x_entrada, y_salida), y se
# construye la escalera etapa por etapa:
# 1. Se mueve en horizontal hasta la línea de equilibrio (misma y, nueva x).
# 2. Se mueve en vertical hasta la línea de operación (misma x, nueva y).
# Esto se repite hasta llegar a la fracción molar de A en el gas de entrada.
x_etapa <- x_entrada  # Fracción molar de A en el líquido que entra (arriba).
y_etapa <- y_salida  # Fracción molar de A en el gas que sale (arriba).

lista_etapas <- list()  # Lista temporal para almacenar las coordenadas de las etapas.
lista_etapas[[1]] <- c(x_etapa, y_etapa)  # Punto inicial en la línea de operación.
iteracion <- 2  # Contador de puntos guardados.
max_iteraciones <- 50  # Número máximo de etapas para evitar bucles infinitos.
num_etapas <- 0  # Contador de etapas teóricas.

while (y_etapa < y_entrada && num_etapas < max_iteraciones) {
  # Movimiento horizontal hasta la línea de equilibrio.
  x_etapa <- y_etapa / pendiente_equilibrio
  lista_etapas[[iteracion]] <- c(x_etapa, y_etapa)
  iteracion <- iteracion + 1

  # Movimiento vertical hasta la línea de operación.
  y_etapa <- linea_operacion(x_etapa)
  lista_etapas[[iteracion]] <- c(x_etapa, y_etapa)
  iteracion <- iteracion + 1

  num_etapas <- num_etapas + 1
}

# Convertir la lista de etapas a un data.frame para facilitar su uso en ggplot.
etapas <- as.data.frame(do.call(rbind, lista_etapas))
colnames(etapas) <- c("x", "y")

# ---------------------- Graficar el diagrama McCabe-Thiele ----------------------
p <- ggplot() +
  # Línea de equilibrio (azul, punteada).
  geom_line(aes(x = x_valores, y = y_equilibrio), color = "blue", linewidth = 1, linetype = "dashed") +
  
  # Línea de operación (roja, continua).
  geom_line(aes(x = x_valores, y = y_operacion), color = "red", linewidth = 1) +
  
  # Puntos de las etapas (negros).
  geom_point(data = etapas, aes(x = x, y = y), color = "black", size = 3) +
  
  # Línea que conecta las etapas (negra, punteada).
  geom_path(data = etapas, aes(x = x, y = y), color = "black", linetype = "dotted") +
  
  # Estilos del gráfico.
  labs(title = "Diagrama McCabe-Thiele para la Torre de Absorción",
       x = "Fracción molar de A en líquido",
       y = "Fracción molar de A en gas") +
  theme_minimal()

# Mostrar gráfico
print(p)

# Guardar el gráfico
ggsave("ejemplo_07_mccabe_thiele.png", plot = p, width = 8, height = 6, dpi = 300)


# --------------------------------------------------------------------------
# Interpretación: Diagrama McCabe-Thiele para la Torre de Absorción
# --------------------------------------------------------------------------
# - Eje X: Fracción molar de A en el líquido (x). Es adimensional: es el
#   cociente entre los moles de A y los moles totales del líquido; va de 0 a 1
#   (aquí solo se muestra el intervalo pequeño desde 0 hasta la fracción de salida).
# - Eje Y: Fracción molar de A en el gas (y). También es adimensional, con el
#   mismo significado pero para el gas.
# - Lo que se aprecia: dos líneas, la de equilibrio (azul, punteada) y la de
#   operación (roja, continua), y entre ellas una escalera de puntos negros
#   unidos por una línea punteada. Cada escalón baja en vertical hasta la línea
#   de equilibrio y luego avanza en horizontal hasta la línea de operación.
# - Qué significa: cada escalón completo de la escalera es una etapa teórica
#   de la torre. Contar los escalones da el número de etapas necesarias para
#   bajar la fracción de A en el gas desde 0,02 (entrada) hasta 0,002 (salida).
#   Con los datos del archivo, la escalera tiene 9 escalones: 9 etapas teóricas.
#   Cuanto más separadas estén las dos líneas, menos escalones hacen falta.

# ---------------------- Mostrar el número de etapas ----------------------
cat("Número de etapas requeridas:", num_etapas, "\n")

# --------------------------------------------------------------------------
# Qué aprendimos
# --------------------------------------------------------------------------
# - El método de McCabe-Thiele dibuja la línea de equilibrio (lo que la
#   termodinámica permite) y la línea de operación (lo que el balance de masa
#   impone) en un mismo gráfico de fracción molar en el gas contra fracción
#   molar en el líquido.
# - Las fracciones molares son adimensionales (moles de A entre moles totales)
#   y por eso los dos ejes no llevan unidades físicas.
# - El número de etapas teóricas se lee contando los escalones entre las dos
#   líneas: cada escalón es una etapa de contacto gas-líquido.
# - Cambiar la relación de flujo líquido/gas mueve la línea de operación y
#   cambia el número de etapas necesarias.
