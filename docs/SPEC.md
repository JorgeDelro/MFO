# MFO: fundamentos científicos y técnicos del paquete y la app

Nov 6, 2026 · @Jorge

## Resumen

Este documento es el manual científico-técnico de referencia del paquete de R **MFO** (v0.2.0 en GitHub; v0.1.0 en CRAN) y de la app que lo expondrá a científicos del deporte y la salud sin necesidad de programar. Explica, desde cero, qué es la oxidación máxima de grasas (MFO), la intensidad a la que ocurre (Fatmax) y la cinética de la oxidación de grasas; cómo se miden mediante calorimetría indirecta; qué ecuaciones estequiométricas convierten VO2 y VCO2 en gramos de grasa e hidratos oxidados; cómo se diseña y procesa un test incremental de MFO; qué métodos matemáticos (P3, P2, MRER, modelo SIN) localizan el máximo; cómo se expresan y normalizan los resultados; y cuál es el error de medida esperable. En la segunda mitad se describe, función por función, qué hace exactamente el código del paquete y con qué ecuación de la literatura se corresponde cada paso, señalando las decisiones de implementación y las limitaciones que motivan la hoja de ruta de la v0.3.0 (fiabilidad, intervalos de confianza, nuevos protocolos, importadores de analizadores de gases, app Shiny).

**Palabras clave:** oxidación de grasas, Fatmax, calorimetría indirecta, metabolismo de sustratos, modelo SIN, R, software científico.

**Cómo leer este documento.** Las secciones 1–8 son la base científica y se pueden leer sin saber R. La sección 9 es la correspondencia con el código y conviene leerla con el repositorio abierto. Las secciones 10–11 justifican las decisiones de la próxima versión y la interpretación práctica de los resultados. Todas las fórmulas van en bloques independientes y se nombran por su autor para que el usuario pueda elegirlas en la app con criterio.

## 1. Introducción

La capacidad de oxidar grasas durante el ejercicio es un marcador de flexibilidad metabólica con interés doble: en rendimiento, porque ahorra glucógeno y se asocia con el rendimiento en resistencia de larga duración; en salud, porque está reducida en obesidad, resistencia a la insulina y diabetes tipo 2, y se correlaciona con la sensibilidad a la insulina, la capacidad oxidativa muscular y la lipólisis del tejido adiposo. Dos magnitudes resumen esa capacidad: la **oxidación máxima de grasas (MFO)**, en g/min, y la **intensidad a la que ocurre (Fatmax)**, expresada como %VO2max, %HRmax, potencia o velocidad.

El concepto de Fatmax lo formalizaron Achten, Gleeson y Jeukendrup (2002) con un test incremental en cicloergómetro y, desde entonces, se ha usado tanto para caracterizar poblaciones como para prescribir ejercicio a la intensidad que maximiza el uso de grasa. Sin embargo, la literatura muestra una heterogeneidad metodológica importante: duración de escalones, incrementos de carga, promediado de los gases, ecuación estequiométrica, criterio de finalización y método de localización del máximo cambian de un estudio a otro, y cada decisión altera el valor obtenido. Las revisiones de 2023–2026 insisten en lo mismo: hace falta estandarizar y reportar con detalle el protocolo y el algoritmo de análisis.

Ahí encaja un software abierto y reproducible. El paquete MFO nació en 2022 para que el cálculo de MFO y Fatmax no dependiera de hojas de cálculo artesanales, implementando los métodos usados en Montes de Oca et al. (2021) y el modelo de cinética de Chenevière et al. (2009). La v0.2.0 amplía los métodos (P2, P3, MRER), las ecuaciones (Frayn, Jeukendrup-Wallis, Péronnet-Massicotte), el manejo de artefactos (RER cutoff, valores negativos) y la expresión de resultados (relativos a masa y masa libre de grasa, %HRmax, zona Fatmax, punto de cruce). El objetivo de este documento es que cualquier usuario del paquete o de la app entienda cada número que obtiene y pueda defenderlo en un artículo o en un informe clínico.

## 2. Fisiología: sustratos, intensidad y el concepto de Fatmax

El músculo obtiene ATP de dos grandes sustratos durante el ejercicio aeróbico: hidratos de carbono (glucógeno muscular y glucosa plasmática) y grasas (ácidos grasos plasmáticos procedentes del tejido adiposo y triglicéridos intramusculares). La mezcla depende, sobre todo, de la intensidad; en segundo lugar, de la duración, el estado nutricional, el entrenamiento, el sexo y la composición corporal.

**La curva de oxidación de grasas.** A intensidades bajas la grasa aporta la mayor parte de la energía. Al aumentar la intensidad, la oxidación de grasas en valor absoluto (g/min) sube, alcanza un máximo (MFO) y después cae hasta hacerse despreciable (Fatmin), mientras que la de hidratos crece de forma monótona. Fatmax es la intensidad en la que se da el MFO. En adultos sanos Fatmax suele situarse entre el 45 y el 65 % del VO2max; en personas con obesidad o sedentarias la curva se desplaza hacia la izquierda y hacia abajo (Fatmax y MFO menores); en deportistas de resistencia, hacia la derecha y hacia arriba.

**Por qué cae la oxidación de grasas a alta intensidad.** Varios mecanismos actún a la vez: la mayor liberación de catecolaminas y el lactato reducen la lipólisis y la disponibilidad de ácidos grasos; el aumento de la glucogenólisis eleva el acetil-CoA y el malonil-CoA, que inhiben el transporte de ácidos grasos a la mitocondria vía CPT-1; y el reclutamiento de fibras tipo II, menos oxidativas, aumenta. El resultado es el "crossover" descrito por Brooks y Mercier (1994): a partir de cierta intensidad los hidratos pasan a ser el sustrato dominante. El paquete calcula ese punto de cruce como la intensidad en la que CHO supera a FAT.

**Tres magnitudes, tres usos.** MFO (g/min) informa de la *capacidad* oxidativa; Fatmax (%VO2max, %HRmax, W o km/h) es el *parámetro de prescripción*; y la forma de la curva (cinética) describe *cómo* se distribuye esa capacidad a lo largo del rango de intensidades. Dos personas con el mismo MFO pueden tener curvas muy distintas (una estrecha y picuda, otra ancha y plana), y eso cambia el margen de error práctico al prescribir a Fatmax. De ahí el interés de la zona Fatmax (intensidades en las que la oxidación de grasas está dentro del 10 % del MFO; Achten et al., 2002) y del modelo SIN de Chenevière et al. (2009), que resume la curva en tres parámetros.

## 3. Calorimetría indirecta y ecuaciones estequiométricas

No se mide la grasa oxidada: se infiere del intercambio gaseoso. Oxidar un gramo de grasa consume más O2 y produce menos CO2 que oxidar un gramo de hidrato, de modo que el cociente VCO2/VO2 (cociente de intercambio respiratorio, RER) informa de la mezcla de sustratos: RER cercano a 0,70 indica oxidación casi exclusiva de grasa; 1,00, casi exclusiva de hidratos. Esta inferencia es válida solo si el RER refleja el cociente respiratorio tisular, lo que exige estado estable (sin acumulación de CO2 por tamponamiento de lactato ni hiperventilación) e ignora, por convención, la contribución proteica durante el ejercicio.

**Ecuaciones de Frayn (1983).** Son las más usadas y el defecto del paquete. Con VO2 y VCO2 en L/min y n = nitrógeno urinario (g/min), que en ejercicio se asume 0:

```latex
\text{CHO}\ (\mathrm{g/min}) = 4.55\,\dot{V}\mathrm{CO}_2 - 3.21\,\dot{V}\mathrm{O}_2 - 2.87\,n
```

```latex
\text{FAT}\ (\mathrm{g/min}) = 1.67\,\dot{V}\mathrm{O}_2 - 1.67\,\dot{V}\mathrm{CO}_2 - 1.92\,n
```

Nótese que, con n = 0, FAT = 1,67 · VO2 · (1 − RER): la oxidación de grasas es proporcional al VO2 y a la distancia del RER a 1. Cuando RER > 1 (tamponamiento de lactato a alta intensidad) la fórmula devuelve valores negativos, que son un artefacto y no fisiología.

**Ecuaciones de Péronnet y Massicotte (1991).** Revisan los coeficientes con valores más precisos de los equivalentes calóricos de glucosa y palmitato:

```latex
\text{CHO} = 4.58519\,\dot{V}\mathrm{CO}_2 - 3.22597\,\dot{V}\mathrm{O}_2 \qquad \text{FAT} = 1.69469\,\dot{V}\mathrm{O}_2 - 1.70130\,\dot{V}\mathrm{CO}_2
```

El artículo no imprime estas ecuaciones: se derivan de su tabla 1 (§19, hallazgo 6). Hasta la v0.2.0 el paquete usaba los valores redondeados 4,585 / 3,2255 / 1,6946 / 1,7012; desde la 0.2.1 los deriva en código.

**Ecuaciones de Jeukendrup y Wallis (2005).** Específicas para ejercicio: asumen que a intensidad moderada-alta el hidrato oxidado es mayoritariamente glucógeno (no glucosa), lo que cambia ligeramente la estequiometría. Proponen dos juegos de coeficientes para CHO según la intensidad, con FAT común:

```latex
40\text{--}50\%\ \dot{V}\mathrm{O}_{2\max}:\quad \text{CHO} = 4.344\,\dot{V}\mathrm{CO}_2 - 3.061\,\dot{V}\mathrm{O}_2
```

```latex
50\text{--}75\%\ \dot{V}\mathrm{O}_{2\max}:\quad \text{CHO} = 4.210\,\dot{V}\mathrm{CO}_2 - 2.962\,\dot{V}\mathrm{O}_2
```

```latex
\text{FAT} = 1.695\,\dot{V}\mathrm{O}_2 - 1.701\,\dot{V}\mathrm{CO}_2
```

Por eso el paquete exige VO2max cuando se elige "Jeukendrup": necesita saber en qué tramo de intensidad está cada escalón.

**Aproximación basada en RER ("MRER" en el paquete).** En lugar de dos coeficientes, usa el RER directamente:

```latex
\text{FAT}\ (\mathrm{mg/min}) = 1.7\,(1 - \mathrm{RER})\,\dot{V}\mathrm{O}_2\ (\mathrm{mL/min})
```

Es la ecuación de Péronnet y Massicotte (1,6946 VO2 − 1,7012 VCO2) simplificada a 1,7(1−RER)VO2; así lo describe Chenevière et al. (2009, p. 1617) citando a Pérez-Martín et al. (2001), de modo que la atribución del código es correcta. Lo que distingue al MRER de Chenevière no es la ecuación sino el **procedimiento**: (1 − RER) y VO2 se modelan como funciones lineales de la intensidad, su producto es una parábola, y Fatmax es el punto en que la derivada de esa parábola se anula. Su precisión depende de que RER sea lineal con la intensidad hasta RER = 1,0 (r = 0,72 en su muestra) y resultó inferior a SIN y P3. **La v0.2.0 no hace esto**: toma el escalón con mayor 1,7(1−RER)VO2 observado, que es un método de valores medidos (MV) con otra ecuación. Corregido en la 0.2.1: MRER real y MV como método explícito (§6.1, §19).

**Energía.** Hasta la v0.2.0 el gasto se estimaba siempre como 4 kcal/g de CHO y 9 kcal/g de FAT. Desde la 0.2.1, `energy_equivalents = "classic"` (defecto en 0.2.x, con mensaje único por sesión: el defecto pasará a `"source"` en la 0.3.0) mantiene 4/9, y `"source"` usa los de la fuente: Péronnet y Massicotte 3,8683/9,7460 kcal/g (también para Frayn, que no da ninguno: decisión del paquete), Jeukendrup y Wallis 3,95 (< 50 % VO2max) o 4,07 (≥ 50 %) y 9,75 kcal/g; en reposo con Jeukendrup, los de Péronnet (par de glucosa pura). En el ejemplo (Frayn) `Kcal` cambia entre −1,9 % y +5,3 % por escalón; MFO y Fatmax no cambian (§19, hallazgo 1). Para comparar estudios conviene reportar siempre la ecuación usada: las diferencias entre Frayn y Péronnet son del orden del 1–2 % en FAT, pero crecen en CHO y en el punto de cruce.

**Qué diferencia hay entre las tres y cuál elegir.** Frayn es el estándar histórico y el recomendado por las revisiones recientes para MFO; Jeukendrup-Wallis es teóricamente más correcta en ejercicio y es la referencia en fisiología del rendimiento; Péronnet se usa mucho en la escuela francesa (LIPOXmax). En la app se ofrecerán las tres con Frayn por defecto y una nota que recuerde que el artículo debe citar la elegida.

## 4. El test de MFO: condiciones, protocolo y criterios de finalización

El paquete espera tres registros por participante, que son las tres fases del día de evaluación: (1) metabolismo en reposo, (2) test incremental de MFO y (3) test incremental máximo (o un VO2max ya conocido). Cada decisión de protocolo afecta al resultado, por lo que debe reportarse.

**Condiciones previas.** Ayuno de 8–12 h (la ingesta de hidratos eleva el RER y deprime la oxidación de grasas), sin ejercicio intenso las 24–48 h previas, sin cafeína ni alcohol, y a la misma hora del día en las repeticiones. En mujeres premenopáusicas conviene registrar la fase del ciclo. El analizador de gases debe calibrarse (volumen y gases de referencia) antes de cada sesión; el error del analizador es, según la revisión de Chávez-Guevara et al. (2025), una fuente mayor de variabilidad día a día de lo que se suele reconocer.

**Fase 1: reposo.** 15–30 min sentado o tumbado con registro continuo. De ese registro se extrae la ventana más estable (habitualmente 5 min) para obtener VO2, RER y oxidación de sustratos basales. Sirve de punto de partida de la curva (intensidad ≈ 0) y para comprobar que el participante llegó en condiciones (RER basal 0,75–0,85 es compatible con ayuno; > 0,90 sugiere ingesta reciente o hiperventilación).

**Fase 2: test incremental de MFO.** Escalones de carga de 3–6 min (3 min es lo más frecuente; en obesidad y poblaciones con MFO bajo se recomiendan escalones más largos y/o incrementos menores) hasta que el RER alcanza el criterio de parada. Modalidades: cicloergómetro (incrementos de 15–35 W) o cinta (incrementos de velocidad o pendiente). El protocolo de Achten et al. (2002) usaba 35 W / 3 min; los protocolos con cargas relativas a la masa corporal (p. ej., 0,15 W/kg cada 4 min desde 0,45 W/kg) se han propuesto en 2026 para homogeneizar el estímulo en poblaciones con MFO bajo.

**Criterio de finalización.** El clásico es RER = 1,00, porque a partir de ahí la ecuación devuelve FAT ≤ 0. Amaro-Gahete et al. (2019) mostraron que el MFO ya se ha alcanzado mucho antes: proponen terminar en RER = 0,93 en sedentarios y 0,90 en entrenados, lo que permite escalones más largos o incrementos más finos sin alargar el test. Por eso el paquete incorpora `RER_cutoff`.

**Fase 3: VO2max.** Tras un descanso (o en otro día), un test incremental máximo del que se toma el VO2max para expresar Fatmax como %VO2max. Si se dispone de un VO2max previo puede pasarse directamente. Un detalle importante: el VO2max debe ser el promedio de los últimos 15–30 s (o la mayor media móvil de 30 s), no el valor instantáneo más alto; en §9 se señala cómo lo trata el código actual.

**Protocolos alternativos.** Los tests en rampa (incremento continuo) acortan la evaluación y, en mujeres posmenopáusicas, han dado MFO y Fatmax equivalentes a los escalonados; a cambio no hay estado estable y el promediado se hace por ventanas temporales. Los tests de un solo escalón prolongado a Fatmax sirven para validar que la intensidad calculada se sostiene, ya que el test incremental tiende a sobreestimar la oxidación de grasas respecto a un esfuerzo continuo a esa misma intensidad.

| Decisión de protocolo | Práctica recomendada (2025–2026) | Efecto si se incumple |
| --- | --- | --- |
| Ayuno | 8–12 h, misma hora del día | RER basal alto, MFO infraestimado |
| Duración de escalón | 3 min (sanos), 4–6 min (obesidad, baja forma) | Sin estado estable, RER inestable |
| Promediado | Últimos 60–120 s de cada escalón | Ruido respiratorio desplaza el máximo |
| Ecuación | Frayn por defecto; reportar la elegida | Diferencias del 1–3 % en MFO |
| Parada | RER 1,00 (clásico) o 0,90–0,93 (Amaro-Gahete 2019) | Escalones con FAT negativa |
| VO2max | Media de 30 s en test máximo | Fatmax en %VO2max sesgado |

## 5. Procesamiento de la señal

Entre los gases crudos y la curva de oxidación de grasas hay cuatro pasos de procesamiento, y cada uno es una fuente de variabilidad entre laboratorios.

**5.1 Resolución temporal.** Los analizadores exportan respiración a respiración (breath-by-breath) o promediados a intervalos fijos (10, 15, 20, 30 s). Los datos breath-by-breath tienen un ruido elevado y deben promediarse antes de cualquier cálculo; el paquete los agrupa en intervalos fijos (`preprocess_bxb`, por defecto 20 s) y después trabaja siempre con el parámetro `step_time` = segundos entre registros.

**5.2 Ventana basal estable.** Del registro de reposo se descartan los primeros 5 min (estabilización) y, en lo que queda, se busca la ventana de 5 min con menor coeficiente de variación (CV = desviación típica / media) de la variable elegida (`cv_var`: VO2, VCO2 o RER). Esa ventana define el estado basal. Elegir RER como criterio favorece ventanas con mezcla de sustratos estable; elegir VO2 favorece ventanas con ventilación estable. Ambas son defendibles; lo importante es reportarlo.

**5.3 Promediado por escalones (estado estable).** De cada escalón de `step_duration` segundos se promedian solo los últimos `steady_state_duration` segundos (por defecto, el último minuto de escalones de 3 min). La razón fisiológica es que el VO2 tarda 1–2 min en alcanzar una meseta tras cada incremento de carga; promediar el escalón entero mezclaría la transición con la meseta. La recomendación más reciente para obesidad es promediar los últimos 2 min de escalones de ≥ 4 min. El paquete asume que el registro del test empieza exactamente al inicio del primer escalón y que todos los escalones tienen la misma duración; si no es así, el usuario debe recortar el archivo (`remove_rows`).

**5.4 Artefactos de alta intensidad.** Cuando RER > 1, las ecuaciones devuelven FAT < 0. Hay tres políticas (`negative_FAT`): *truncar* a 0 (la más habitual: la grasa no puede oxidarse negativamente), *excluir* el escalón (evita que puntos artefactuales deformen el polinomio) o *mantener* (solo con fines de inspección). Además, `RER_cutoff` excluye los escalones con RER por encima del umbral antes de ajustar nada, lo que implementa el criterio de Amaro-Gahete (2019) a posteriori cuando el test se prolongó hasta RER = 1,00.

**5.5 Eje de intensidad.** La curva se construye en %VO2max (VO2 del escalón / VO2max × 100). Es la escala que permite comparar entre personas y modalidades, pero obliga a disponer de un VO2max fiable. Para prescribir, el usuario necesita además la equivalencia en frecuencia cardiaca (interpolada en el paquete) y en carga externa (W o km/h), que el paquete aún no devuelve y que se añadirá en la v0.3.0.

Flujo resumido:

1. Importar reposo, test MFO y test máximo; estandarizar nombres de columnas (VO2, VCO2, RER, HR).
2. Si es breath-by-breath, promediar a intervalos fijos.
3. Ventana basal más estable → valores de reposo.
4. Por escalón: media del tramo estable → VO2, VCO2, RER, HR → CHO y FAT con la ecuación elegida.
5. Aplicar `RER_cutoff` y la política de negativos.
6. Expresar cada escalón en %VO2max. El punto basal se añade como escalón 0 solo si `include_basal = TRUE` (Montes de Oca et al., 2021); por defecto no entra en el ajuste (§9.5).
7. Ajustar la curva y localizar MFO y Fatmax (§6).

## 6. Determinación de MFO y Fatmax: P3, P2, MRER y modelo SIN

Una vez se tiene un punto (FAT, %VO2max) por escalón, hay que decidir dónde está el máximo. La elección no es trivial: con 6–10 escalones, el escalón con el valor más alto puede estar a 5–10 puntos porcentuales del verdadero máximo, y el ruido de un solo escalón puede desplazarlo. Los métodos se agrupan en dos familias: los que toman el valor observado (MV, "measured values") y los que ajustan una curva y localizan su máximo analítico.

**6.1 Valor observado (MV) y método RER (MRER).** MV: Fatmax es el %VO2max del escalón con mayor FAT. Es transparente y no asume forma de curva, pero su resolución es la del protocolo (un escalón) y es sensible al ruido. MRER (Chenevière 2009, siguiendo a Pérez-Martín 2001): se ajustan dos rectas, (1 − RER) frente a intensidad y VO2 frente a intensidad; FAT = 1,7(1−RER)VO2 es entonces una parábola cuyo vértice (derivada nula) da Fatmax y MFO. Es el menos preciso de los cuatro en la comparación de Chenevière. **En la v0.2.0 el método llamado `MRER` es en realidad MV sobre 1,7(1−RER)VO2**, con un P3 solo para dibujar. **Corregido en la 0.2.1:** `method = "MV"` toma el escalón con mayor FAT observado (ecuación de `author`), sin R² ni RMSE y con la zona Fatmax por interpolación lineal entre escalones (`FatMax_zone_method`); `method = "MRER"` ajusta las dos rectas y toma el vértice x = −(a₁b₂ + a₂b₁)/(2b₁b₂) de 1,7(1−RER)VO2, sea cual sea `author`, y cae a MV con aviso si el vértice no es un máximo dentro del rango medido. En el ejemplo (Frayn): MV 41,0 % VO2max / 0,327 g/min; MRER 35,8 % VO2max / 0,327 g/min (la v0.2.0 daba 41,0 / 0,339 con `"MRER"`).

**6.2 Polinomio de tercer grado (P3).** Es el método por defecto y el más usado en la literatura reciente. Se ajusta por mínimos cuadrados:

```latex
\text{FAT}(x) = a + b\,x + c\,x^2 + d\,x^3, \qquad x = \%\dot{V}\mathrm{O}_{2\max}
```

y el máximo se obtiene anulando la derivada, que es una ecuación de segundo grado:

```latex
b + 2c\,x + 3d\,x^2 = 0 \;\Rightarrow\; x = \frac{-2c \pm \sqrt{4c^2 - 12\,d\,b}}{6d}
```

De las dos raíces, una es un máximo y otra un mínimo; el paquete evalúa FAT en ambas, descarta las que caen fuera del rango de intensidades medido y se queda con la de mayor FAT. Si el discriminante es negativo (no hay puntos críticos reales), los coeficientes son NA (pocos escalones) o el máximo predicho es negativo, cae a P2 o al valor observado con un aviso. El P3 permite curvas asimétricas (subida lenta, caída rápida), que es lo que se observa fisiológicamente, y por eso se prefiere al P2.

**6.3 Polinomio de segundo grado (P2).** Parábola simétrica cuyo vértice está en:

```latex
\text{FAT}(x) = a + b\,x + c\,x^2, \qquad x_{\text{Fatmax}} = -\frac{b}{2c}, \qquad \text{MFO} = a + b\,x_{\text{Fatmax}} + c\,x_{\text{Fatmax}}^2
```

Más estable con pocos escalones (4–5), pero fuerza simetría y tiende a desplazar Fatmax hacia el centro del rango. Útil como respaldo.

**Bondad de ajuste.** Para P2 y P3 el paquete devuelve R² (1 − SSres/SStot) y RMSE (raíz del error cuadrático medio de los residuos). Un R² < 0,80 o un RMSE del orden del 10–15 % del MFO indican que la curva no describe bien los datos y que Fatmax es poco fiable; la app deberá avisar de ello.

**6.4 Zona Fatmax y punto de cruce.** Sobre la curva ajustada se calcula la zona Fatmax como el intervalo de intensidades en que FAT ≥ 0,90 · MFO (Achten et al., 2002): su anchura informa de cuánto margen hay al prescribir. El punto de cruce (crossover) es la intensidad a la que CHO supera a FAT; el paquete lo obtiene por interpolación lineal entre los dos escalones en que cambia el signo de FAT − CHO.

**6.5 Modelo SIN (Chenevière et al., 2009).** Describe toda la curva, normalizada a %MFO, con una función seno de tres parámetros:

```latex
\%\text{MFO}(x) = \sin\!\left\{\left[\frac{\pi^{1/s}}{\pi + 2d}\left(K\,x + d + t\right)\right]^{s}\right\}, \qquad K = \frac{\pi}{100}, \quad x = \%\dot{V}\mathrm{O}_{2\max}
```

Ecuación \[14\] de Chenevière et al. (2009), p. 1618, verificada sobre el texto completo el 9 de noviembre de 2026. Atención al denominador: es π^(1/s) dividido por (π + 2d), no (π^(1/s)/π) + 2d. El código de la v0.2.0 implementa la segunda forma (véase §9.5 y §19, hallazgo 5). Puntos característicos derivados de \[14\] (no impresos en el artículo): primer cero en x₁ = −(d + t)/K; Fatmax en x = \[(π + 2d)·2^(−1/s) − d − t\]/K; Fatmin (segundo cero) en x₂ = (π + d − t)/K. Con d = t = 0 y s = 1 dan 0, 50 y 100 % VO2max.

Con d = 0, s = 1, t = 0 la función es un semiciclo de seno "básico" que vale 0 en x = 0, 1 en x = 50 y 0 en x = 100. Cada parámetro deforma esa referencia:

| Parámetro | Nombre | Qué hace sobre la curva | Interpretación fisiológica |
| --- | --- | --- | --- |
| d | Dilatación | Ensancha (d > 0) o estrecha (d < 0) la curva | Amplitud del rango de intensidades con alta oxidación de grasa |
| s | Simetría | s > 1 desplaza el pico a la derecha y hace la caída brusca; s < 1 al revés | Forma de la caída: cuánto se mantiene la grasa por encima de Fatmax |
| t | Traslación | Desplaza la curva entera a la izquierda (t > 0) o a la derecha (t < 0) | Dónde está Fatmax en el continuo de intensidad |

El ajuste se hace por mínimos cuadrados no lineales (Levenberg-Marquardt, `minpack.lm::nlsLM`). **Cómo lo hace Chenevière (2009):** promedia el último minuto de cada escalón con RER ≤ 1,0, expresa la oxidación de grasa como energía derivada de lípidos (E\_lipid) y determina d, s y t minimizando la suma de cuadrados entre el modelo y los valores medidos, **sin suavizado previo**; el P3 aparece solo como método comparador. Fatmin es el segundo cero de la curva (fat = 0 % MFO), sin umbral. El artículo no dice explícitamente si el 100 % MFO es el máximo observado o el del ajuste; por construcción del modelo (pico = 1) y por su figura 2, los valores medidos se normalizaron al máximo observado. **Cómo lo hace la v0.2.0:** normaliza al máximo observado, suaviza con un P3, genera una malla fina de 0 al último escalón, renormaliza y ajusta el SIN a esa malla; Fatmin se toma como la primera intensidad con %MFO < 5 % por encima de Fatmax; y la fórmula tiene el paréntesis del denominador mal colocado. Las cuatro desviaciones están **corregidas en la 0.2.1** (§19): ajuste directo a los escalones observados normalizados al máximo observado (el ajuste en g/min con MFO como parámetro de escala queda para §13.2), sin malla sintética, Fatmax y Fatmin por fórmula cerrada, Fatmin en el segundo cero (con `fatmin_threshold` opcional como fracción del MFO) y fórmula \[14\] exacta. `MFO_kinetics()` ajusta todas las filas que recibe: `RER_cutoff = 1` reproduce la selección de Chenevière (escalones con RER ≤ 1,0) e `include_basal = TRUE` se aparta de ella también en el SIN. En el ejemplo el SIN depende del último escalón (RER 1,03): con `negative_FAT = "truncate"` d = −0,385, s = 0,910, t = 0,355, Fatmax 36,2, Fatmin 76,4 % VO2max; con `"exclude"` o `RER_cutoff = 1`, d = −0,440, s = 0,539, t = 0,023, Fatmax 33,2, Fatmin 85,3 % VO2max. Si los parámetros no se mueven de los valores iniciales o R² < 0, el ajuste se declara no convergente; ocurre con menos de 6 escalones o cuando la oxidación de grasas solo decrece. Con menos de 4 escalones (3 parámetros) no se ajusta (`convergence = FALSE`, aviso); con 4–5 se ajusta pero se avisa de que el ajuste es poco fiable y se recomiendan ≥ 6 escalones.

**6.6 Comparación entre métodos.** Amaro-Gahete et al. (2019) y la meta-regresión de Chávez-Guevara et al. (2023) coinciden en que MV, P3 y SIN dan MFO similares a nivel de grupo, pero Fatmax puede diferir varios puntos porcentuales en un individuo; el SIN es el que mejor describe la forma, el P3 el mejor compromiso entre robustez y resolución. La recomendación para la app: P3 como método principal, SIN como descripción de la cinética, y mostrar siempre el valor observado al lado para que el usuario vea cuánto aporta el ajuste.

## 7. Expresión y normalización de resultados

Un mismo test produce varias cifras y cada una responde a una pregunta distinta. Reportar solo "MFO = 0,45 g/min" es insuficiente.

| Magnitud | Unidad | Cómo la calcula el paquete | Para qué sirve |
| --- | --- | --- | --- |
| MFO absoluto | g/min | Máximo de la curva ajustada (o observado) | Capacidad oxidativa total; comparar con la literatura |
| MFO relativo | mg/kg/min | MFO × 1000 / masa corporal | Comparar personas de distinto tamaño |
| MFO por masa libre de grasa | mg/kgFFM/min | MFO × 1000 / FFM | Capacidad oxidativa del tejido metabólicamente activo; preferible en obesidad |
| Fatmax | %VO2max | Intensidad del máximo de la curva | Referencia fisiológica estándar |
| HR a Fatmax | lpm | Interpolación lineal HR vs %VO2max en Fatmax | Prescripción con pulsometro |
| Fatmax | %HRmax | HR a Fatmax / HRmax × 100 | Prescripción sin analizador; referencia recomendada en obesidad |
| Zona Fatmax | %VO2max (inferior, superior) | Intensidades con FAT ≥ 90 % del MFO | Margen de prescripción |
| Punto de cruce | %VO2max | Donde CHO supera a FAT | Marcador de dependencia de hidratos |
| Fatmin | %VO2max | Primera intensidad por encima de Fatmax con %MFO < 5 % (SIN) | Límite superior de uso relevante de grasa |
| d, s, t | adimensional | Parámetros del SIN | Forma de la curva; comparación pre-post |
| Basal: VO2, RER, CHO, FAT, kcal | mL/min, –, g/min, kcal/min | Ventana estable de reposo (Frayn) | Control de calidad del ayuno; punto 0 de la curva |

**Valores de referencia orientativos.** En adultos sanos activos MFO suele estar entre 0,3 y 0,6 g/min (hasta > 1 g/min en deportistas de resistencia de élite), con Fatmax entre el 45 y el 65 % del VO2max. En obesidad el MFO absoluto puede ser similar o mayor (más masa), pero el relativo a FFM es menor y Fatmax se desplaza a la izquierda. La meta-regresión de Chávez-Guevara et al. (2023) recomienda expresar Fatmax en %HRpeak en pacientes con obesidad: 61–66 % HRpeak con > 35 % de grasa corporal y 57–64 % con < 35 %. Las referencias por sexo y edad en población general proceden de cohortes como FIT-AGEING y las muestras de Maunder et al. (2018) para deportistas; en la v0.3.0 se incorporarán como percentiles consultables, siempre con la advertencia de que dependen del protocolo con el que se obtuvieron.

**Qué reportar en un artículo o informe.** MFO absoluto y relativo a FFM; Fatmax en %VO2max y en %HRmax (o W/km/h); método de localización y ecuación; R² del ajuste; número de escalones usados y criterio de parada; y, si procede, los parámetros del SIN. La app generará un párrafo de métodos con todo ello.

## 8. Fiabilidad, error de medida e incertidumbre

MFO y Fatmax tienen una variabilidad día a día considerable, y confundir esa variabilidad con un cambio real es el error más frecuente al usarlos para evaluar intervenciones. Esta sección fija los conceptos que la v0.3.0 implementará.

**Lo que dice la evidencia (2025).** La revisión de Chávez-Guevara et al. (Int J Obes, 2025) sintetiza 11 estudios de reproducibilidad: el coeficiente de variación test-retest de MFO se sitúa típicamente entre el 8 y el 20 % y el de Fatmax entre el 6 y el 16 %, con ICC moderados-altos (0,6–0,9) en sanos y algo peores en obesidad. Buena parte de esa variación es atribuible al error del analizador de gases al calcular sustratos cerca de Fatmax, donde la diferencia VO2 − VCO2 es pequeña y cualquier error en VCO2 se amplifica. Conclusión práctica: un cambio individual de MFO menor del \~20 % no puede distinguirse del ruido con un solo test antes y otro después.

**Tres estadísticos que hay que conocer.** Con dos tests (T1, T2) en n personas:

```latex
\text{ICC}_{(2,1)} = \frac{\sigma^2_{\text{entre}}}{\sigma^2_{\text{entre}} + \sigma^2_{\text{error}}}, \qquad \text{SEM} = \text{SD}_{\text{pooled}}\,\sqrt{1 - \text{ICC}} = \frac{\text{SD}_{\text{diff}}}{\sqrt{2}}
```

```latex
\text{MDC}_{95} = 1.96\,\sqrt{2}\,\text{SEM}, \qquad \text{CV}\,(\%) = \frac{\text{SEM}}{\bar{x}} \times 100
```

El ICC indica cuánto de la variabilidad total es diferencia real entre personas (fiabilidad relativa, útil para clasificar). El SEM es el error típico de una medida en sus unidades (fiabilidad absoluta). El MDC95 es el cambio mínimo que, con un 95 % de confianza, excede el error: es el umbral que debe superar un cambio pre-post individual para considerarse real. Un laboratorio debería calcular sus propios SEM y MDC con su analizador y su protocolo, porque los publicados no son trasladables.

**Incertidumbre del propio ajuste.** Independientemente del error biológico y del analizador, el P3 y el SIN se ajustan a 6–10 puntos ruidosos, y la posición del máximo tiene incertidumbre estadística propia. Hay dos formas de cuantificarla: (a) bootstrap de residuos (reajustar la curva cientos de veces con residuos remuestreados y tomar los percentiles 2,5 y 97,5 de MFO y Fatmax); (b) un ajuste bayesiano del SIN o del P3 (p. ej., con `brms`), que devuelve distribuciones posteriores de MFO, Fatmax, d, s y t y permite incorporar priors informativos sobre la forma de la curva, muy útil cuando hay pocos escalones y el ajuste clásico no converge. Un intervalo creíble ancho para Fatmax es, en sí mismo, un resultado: dice que la curva es plana y que la prescripción tiene margen.

**Lo que hará el paquete.** (1) `MFO_reliability()`: a partir de dos o más tests por persona, ICC(2,1) con IC 95 %, SEM, MDC95, CV y gráfico de Bland-Altman. (2) `MFO(..., ci = "bootstrap")`: IC 95 % para MFO, Fatmax y la zona Fatmax. (3) `MFO_bayes()`: versión bayesiana del P3/SIN con intervalos creíbles. (4) En la app, el informe individual mostrará siempre el intervalo junto al valor puntual y, si hay un test previo, comparará el cambio con el MDC del laboratorio.

## 9. El paquete MFO (v0.2.0): arquitectura, flujo y correspondencia función-ecuación

El paquete tiene tres funciones exportadas y seis auxiliares internas, en ocho ficheros de R (\~1.900 líneas con tests). Depende de `dplyr`, `ggplot2`, `tibble`, `minpack.lm`, `openxlsx`, `readxl` y `tidyr`, y exige R ≥ 4.1. La arquitectura es lineal: importar → procesar → ajustar → expresar → graficar, y `MFOs()` repite ese flujo sobre una carpeta de participantes.

**9.1 Funciones exportadas**

| Función | Entrada | Salida | Sección de este documento |
| --- | --- | --- | --- |
| `MFO()` | Tres data frames (basal, test MFO, test máximo) o VO2max; parámetros de protocolo | Lista con MFO\_db (tabla por escalón), gráfico, MFO, Fatmax, HR, zona, cruce, R², RMSE, basales | §3–§7 |
| `MFO_kinetics()` | `MFO_db` devuelto por `MFO()` | d, s, t con sus errores típicos, Fatmax\_SIN, Fatmin, R², RMSE, convergencia, gráfico SIN básico vs ajustado | §6.5 |
| `MFOs()` | Carpeta con un xlsx por participante (o un xlsx con tres hojas) | Tabla resumen (una fila por participante) en xlsx/csv/rds y carpeta de gráficos | todo |

**9.2 Auxiliares internas y la ecuación que implementan**

| Auxiliar | Qué hace | Ecuación / regla |
| --- | --- | --- |
| `read_MFO_databases()` | Lee xlsx, renombra columnas a VO2, VCO2, RER, HR, convierte a numérico, elimina filas con NA | §5.1 |
| `preprocess_bxb()` | Agrupa breath-by-breath en intervalos fijos (media por bin) | §5.1 |
| `met_basal()` | Descarta 5 min, busca la ventana de 5 min con menor CV de `cv_var`, calcula basales con la ecuación de `author` (Frayn hasta la v0.2.0) | §5.2, §3 |
| `substrate_oxidation()` | CHO, FAT y kcal a partir de VO2 y VCO2 según `author` | Frayn / Jeukendrup-Wallis / Péronnet (§3) |
| `fat_oxidation_MRER()` | FAT = 1,7(1−RER)VO2/1000 | §3 (MRER) |
| `calculate_vars()` | Por escalón: media del tramo estable → sustratos; aplica `RER_cutoff` | §5.3–5.4 |
| `.fit_P3()`, `.fit_P2()`, `.fit_MRER()` | Ajuste y localización del máximo; R² y RMSE; funciones de predicción FAT y CHO | §6.1–6.3 |
| `.calculate_fatmax_zone()` | Malla de 1000 puntos; intervalo donde FAT ≥ 0,9 MFO | §6.4 |
| `.calculate_crossover()` | Interpolación lineal en el primer cambio de signo de FAT − CHO | §6.4 |
| `.create_MFO_plot()` | ggplot con curvas FAT/CHO, puntos, zona sombreada, líneas de Fatmax y cruce | §7 |

**9.3 Flujo interno de `MFO()`, paso a paso**

1. Valida `method` (P3/P2/MRER) y `negative_FAT` (truncate/exclude/keep).
2. VO2max: si se pasa `VO2max`, se usa tal cual (y `db_graded` se ignora con aviso). Si no, la mayor media móvil de `vo2max_window` s (30 por defecto) en `db_graded`, redondeando hacia arriba al número de registros (30 s con registros de 20 s = 2 registros = 40 s). `MFO()` devuelve `VO2max`, `VO2max_window` (efectiva) y `VO2max_window_requested`. Hasta la v0.2.0 tomaba `max(VO2)` (corregido en 0.2.1, §9.5).
3. `met_basal()` → valores de reposo.
4. `calculate_vars()` → una fila por escalón con VO2, VCO2, HR, RER, CHO, FAT, kcal.
5. Une la fila basal (escalón 0, con `HR = NA`) con los escalones y calcula `porc_VO2` = VO2/VO2max × 100. Desde la 0.2.1 la fila basal se conserva o se descarta de forma explícita según `include_basal` (por defecto `FALSE`, idéntico a la v0.2.0); hasta la v0.2.0 la eliminaba `na.omit()` porque su HR es NA (§9.5).
6. Aplica la política de FAT negativa.
7. Ajusta según `method`; obtiene MFO, Fatmax, R², RMSE y las funciones de predicción.
8. Zona Fatmax, punto de cruce, HR a Fatmax (interpolación `approx` de HR vs porc\_VO2) y %HRmax si se dio `HRmax`.
9. MFO relativo a masa y a FFM si se dieron.
10. Gráfico y lista de resultados.

**9.4 Flujo interno de `MFO_kinetics()`** (0.2.1; la v0.2.0 suavizaba con un P3 y ajustaba una malla, §6.5)

1. Normaliza FAT a %MFO usando el máximo observado en `MFO_db`.
2. Ajusta el SIN (ec. \[14\], helper interno `.sin_model()`) con `nlsLM` directamente a los escalones observados, inicio d = 0, s = 1, t = 0, 200 iteraciones; exige al menos 4 filas; captura errores.
3. Comprueba convergencia trivial (parámetros en los iniciales o R² < 0).
4. Fatmax\_SIN y Fatmin por fórmula cerrada (`.sin_fatmax()`, `.sin_fatmin()`); Fatmin = segundo cero o, con `fatmin_threshold`, la intensidad sobre Fatmax a esa fracción del MFO.
5. Gráfico con los escalones observados, el SIN básico (gris) y el ajustado entre sus dos ceros (naranja), con etiquetas d, s, t.

**9.5 Observaciones de revisión del código (a corregir o documentar en v0.3.0)**

- **VO2max = valor instantáneo máximo (corregido en 0.2.1).** `max(db_graded$VO2)` tomaba el registro más alto, no una media de 30 s. Con datos a 20 s sobreestima el VO2max y desplaza Fatmax hacia la izquierda. Corregido con `vo2max_window` (30 s por defecto; si no es múltiplo de `step_time` se redondea hacia arriba el número de registros y se informa de la ventana efectiva). Si se pasa `VO2max`, manda sobre `db_graded` (decisiones de Jorge, 10 nov 2026). En los datos de ejemplo: VO2max 3442 → 3315 mL/min, Fatmax 34,8 → 36,2 % VO2max, MFO sin cambios.
- **Los basales siempre se calculaban con Frayn (corregido en 0.2.1)**, aunque el usuario eligiera otra ecuación para el test. Ahora `met_basal()` recibe `author` y `VO2max` de `MFO()`. Jeukendrup y Wallis (2005, tablas 4 y 5) no proponen ecuación de reposo; con `author = "Jeukendrup"` el basal usa el par 100 % glucosa derivado de las constantes de su tabla 4 (CHO = 4,585 VCO2 − 3,226 VO2; FAT = 1,695 VO2 − 1,701 VCO2). Es una decisión del paquete, no de la fuente (Jorge, 10 nov 2026; sustituye a la extrapolación del tramo 40–50 % del primer borrador). El aviso del punto 6 del backlog no debe saltar para el basal. No cambia MFO ni Fatmax mientras el basal no entre en el ajuste.
- **Punto basal como escalón 0 (corregido 10 nov 2026).** Versiones anteriores de este documento decían que la fila de reposo entra en el ajuste polinómico. **No es así**: `MFO()` construye la fila basal con `HR = NA` y `na.omit(MFO_db)` la elimina antes de la política de negativos y del ajuste, así que la v0.1.0/v0.2.0 equivale de hecho a `include_basal = FALSE`, y la fila basal tampoco aparece en el `MFO_db` devuelto (los basales solo se ven en `x_CHO`, `x_FAT`, `x_Kcal`). Incluir el reposo como escalón 0 es la práctica de Montes de Oca et al. (2021), pero no todos los autores la siguen. Decisión (Jorge, 10 nov 2026): nuevo argumento `include_basal = FALSE` por defecto (conserva los resultados publicados con la v0.2.0); `TRUE` documentado como opción, porque cambia el P3. Si se incluye, la exclusión del basal no debe depender de que falte HR. **Implementado en 0.2.1** (`MFO()`, `MFOs()`); en el ejemplo (Frayn, P3) `TRUE` mueve MFO 0,3167 → 0,3225 g/min y Fatmax 36,17 → 36,11 % VO2max. Con `TRUE`, `MFO_kinetics()` también ajusta el basal (decisión de Jorge, 10 nov 2026), lo que se aparta de Chenevière.
- **Columna `Load` sintética.** La columna `Load` de `MFO_db` no es la carga medida: se fabrica como `seq_len(n_steps) * step_duration/60 * 5` (5, 10, 15… para escalones de 1 min; 15, 30, 45… para 3 min) y vale `"basal"` en la fila de reposo, lo que además convierte la columna en texto. No debe interpretarse como W ni km/h. Hasta que exista la columna `Load` real (§14) se documentará como índice de escalón o se renombrará; no se usa en ningún cálculo.
- **MRER: atribución y naturaleza.** Es el método "valor observado" con un coeficiente redondeado. Renombrarlo a `MV` (measured values) y corregir la cita, o eliminarlo.
- **Jeukendrup: umbral del 50 %** aplicado al VO2 del escalón; las ecuaciones originales cubren 40–75 % VO2max. Fuera de ese rango el paquete extrapola. **Corregido en 0.2.1:** un aviso único lista los escalones de ejercicio fuera de rango (tras `RER_cutoff`); el basal nunca avisa.
- **Cutoff de RER en `calculate_vars` y política de negativos en `MFO()`** actúan sobre la misma situación en dos sitios; la lógica se entiende, pero conviene centralizarla y describirla en una vignette.
- **HR ausente.** Si la tabla no trae HR, `mean(NULL)` devuelve NA sin error; `MFO()` lo gestiona, pero `MFOs()` genera columnas vacías sin explicación. Añadir mensaje.
- **Escalones incompletos.** `calculate_vars` descarta silenciosamente las filas sobrantes del último escalón incompleto (`n_steps = floor(nrow/records_per_step)`). Debe avisar y ofrecer usarlo si tiene al menos el tramo estable.
- **SIN: la normalización usa el máximo observado, no el del P3.** Si el máximo observado está en un escalón ruidoso, toda la curva se escala con él. Documentar; valorar normalizar con el MFO del ajuste.
- **Fatmin con umbral fijo del 5 %** y sin IC. **Corregido en 0.2.1:** segundo cero por defecto; `fatmin_threshold` (fracción del MFO, `NULL` por defecto) como opción. El IC queda para §13.
- **Sin salida en unidades de carga externa** (W, km/h): el usuario no puede prescribir directamente. Añadir una columna opcional `Load` en el test y devolver Fatmax en esa unidad por interpolación.
- **Infraestructura.** Un solo commit en el historial, badge de Travis obsoleto, README de la 0.1.0, NEWS vacío, sin vignettes ni pkgdown ni GitHub Actions. Son requisitos prácticos para CRAN y explícitos para JOSS.

**Añadidos tras la verificación con textos completos (9 nov 2026; detalle en §19):**

- **Error en la fórmula del SIN**: el código calcula (π^(1/s)/π) + 2d donde Chenevière escribe π^(1/s)/(π + 2d). Corregir y añadir test de regresión con los parámetros de su figura 5. *(Corregido en 0.2.1.)*
- **Suavizado y malla sintética antes del SIN**: no están en la fuente e inflan n. Ajustar a los escalones observados. *(Corregido en 0.2.1.)*
- **MRER es en realidad MV**: implementar el procedimiento de las dos rectas y añadir `method = "MV"`.
- **Fatmin**: segundo cero del SIN, no umbral del 5 %. *(Corregido en 0.2.1.)*
- **Equivalentes energéticos**: 4/9 kcal/g no proceden de ninguna de las tres fuentes; hacerlos dependientes de `author`.
- **Coeficientes de Péronnet (corregido en 0.2.1)**: derivados en código desde su tabla 1 (4,58519/3,22597; 1,69469/1,70130) en lugar de constantes redondeadas.

**9.6 Formato de datos de entrada (contrato para la app).** Cada tabla debe tener, como mínimo, las columnas VO2 (mL/min), VCO2 (mL/min), RER y HR (lpm), a intervalos regulares de `step_time` segundos y sin cabeceras intermedias. Los nombres se pueden mapear con `col_name_*`. La app leerá directamente exportaciones de los analizadores más comunes (Cosmed, Cortex, Vyntus, Parvo) y detectará unidades (mL/min vs L/min) antes de llamar a `MFO()`.

## 10. Hoja de ruta científica v0.3.0 y app Shiny

Cada punto de la hoja de ruta responde a una limitación identificada en §4–§9 o a una recomendación de la literatura 2023–2026. El orden es el de implementación.

| Fase | Qué se añade | Por qué (origen científico) |
| --- | --- | --- |
| 1. Saneamiento | Correcciones de §9.5; GitHub Actions (R CMD check en Windows/macOS/Linux); pkgdown; README y NEWS; vignette "De los gases al Fatmax"; envío a CRAN | Requisitos CRAN y JOSS; reproducibilidad |
| 2. VO2max robusto | Media móvil configurable; VO2peak vs VO2max; aviso si el test máximo no cumple criterios | §4 fase 3 |
| 3. Protocolos | Escalones de duración variable; rampa con ventanas temporales; cargas relativas (W/kg); columna `Load` y Fatmax en W/km/h | Scoping review 2025; Physiologia 2025; test relativo 2026 |
| 4. Fiabilidad | `MFO_reliability()`: ICC, SEM, MDC95, CV, Bland-Altman | Chávez-Guevara 2025 |
| 5. Incertidumbre | IC por bootstrap en `MFO()`; `MFO_bayes()` con `brms` para P3/SIN | §8; diferenciador del paquete |
| 6. Referencias | Zonas %HRpeak en obesidad; percentiles por sexo/edad con cita del protocolo de origen | Chávez-Guevara 2023; cohortes publicadas |
| 7. Importadores | Lectores para Cosmed, Cortex, Vyntus, Parvo; detección de unidades y de breath-by-breath | §9.6 |
| 8. App Shiny | `MFO::run_app()` dentro del paquete; despliegue en shinyapps.io/Posit Connect | JOSS acepta apps que exponen una librería núcleo |
| 9. Publicación | `paper.md`, bibliografía, declaración de uso de IA, presubmission inquiry a JOSS | Criterios JOSS 2026 |

**Decisiones de arquitectura ya tomadas** (y su porqué en una línea):

- La app vive dentro del paquete, no en un repo aparte: un solo objeto de publicación y la app es evidencia de diseño para JOSS.
- Las ecuaciones y los métodos se implementan como funciones puras y pequeñas, cada una con su test contra valores calculados a mano de los artículos originales: así un revisor puede verificar que Frayn devuelve lo que dice Frayn.
- La capa bayesiana es opcional (`Suggests`), para no obligar a instalar Stan a quien solo quiera P3.
- Los importadores de analizadores se basan en ficheros de ejemplo anonimizados incluidos en el paquete (`inst/extdata`), que sirven a la vez de tests y de tutorial.

**Qué verá el usuario de la app** (decisiones de producto pendientes de confirmar):

1. Subir uno o varios ficheros (o una carpeta) y elegir el analizador de origen.
2. Configurar protocolo: duración de escalón, tramo estable, ecuación, método, cutoff RER, política de negativos, VO2max.
3. Ver la curva interactiva con MFO, Fatmax, zona, cruce y su IC; la cinética SIN; y las alertas de calidad (R² bajo, RER basal alto, escalones insuficientes).
4. Descargar informe individual (PDF/HTML) con párrafo de métodos listo para un artículo, y tabla de grupo (xlsx/csv).
5. Módulo de fiabilidad: subir dos sesiones por persona y obtener ICC/SEM/MDC del laboratorio.

## 11. Interpretación práctica: deporte, salud y errores frecuentes

**En rendimiento.** Un MFO alto y un Fatmax desplazado a la derecha indican alta capacidad oxidativa y ahorro de glucógeno; se asocian con el rendimiento en pruebas de más de 2–3 h (Ironman, ultra) y responden al entrenamiento de volumen en zona 2, al entrenamiento en ayunas o con baja disponibilidad de hidratos, y a la mejora del VO2max. Para prescribir, Fatmax en W o km/h es más útil que %VO2max, y la zona Fatmax da el margen. Un cambio pre-post de MFO solo es interpretable si supera el MDC del laboratorio (§8).

**En salud.** En obesidad, síndrome metabólico y diabetes tipo 2 el entrenamiento a Fatmax mejora la sensibilidad a la insulina, los lípidos plasmáticos y la composición corporal con intensidades bien toleradas. Conviene expresar Fatmax en %HRpeak (61–66 % con > 35 % de grasa), relativizar MFO a la masa libre de grasa y usar protocolos con escalones más largos e incrementos menores. El MFO absoluto puede engañar: una persona con obesidad puede tener un MFO absoluto normal por su mayor masa y, sin embargo, una capacidad oxidativa por kg de FFM reducida.

**Errores frecuentes y cómo los evita el software.**

| Error | Consecuencia | Salvaguarda en el paquete/app |
| --- | --- | --- |
| Test sin ayuno o tras ejercicio | RER alto, MFO bajo | Alerta si RER basal > 0,90 |
| Promediar escalones enteros | Mezcla transición y meseta | `steady_state_duration` por defecto = último minuto |
| Incluir escalones con RER > 1 en el polinomio | Curva deformada, Fatmax desplazado | `RER_cutoff`, `negative_FAT` |
| Usar el VO2 instantáneo más alto como VO2max | Fatmax (%VO2max) sesgado a la baja | Media móvil de 30 s (v0.3.0) |
| Ajustar P3 con 4 escalones | Raíces fuera de rango, NA | Fallback a P2/observado con aviso; recomendación de ≥ 6 escalones |
| Interpretar un cambio del 10 % como mejora | Falso positivo | Comparación con MDC e IC |
| No reportar ecuación ni método | Resultados no comparables | Párrafo de métodos automático |
| Comparar con referencias de otro protocolo | Clasificación errónea | Percentiles etiquetados con su protocolo |

**Un ejemplo de lectura.** Un corredor de 70 kg obtiene MFO = 0,62 g/min (8,9 mg/kg/min), Fatmax = 58 % VO2max (HR 142 lpm, 76 % HRmax), zona Fatmax 49–66 %, cruce al 71 %, R² = 0,96, SIN con d = 0,12, s = 1,25, t = −0,04. Lectura: capacidad oxidativa alta, curva algo ensanchada y asimétrica a la derecha (mantiene la oxidación de grasa por encima de Fatmax), buena calidad de ajuste. Prescripción: sesiones largas a 135–150 lpm. Si tras 12 semanas MFO = 0,70 g/min, el cambio (+13 %) solo cuenta como mejora si el MDC del laboratorio es menor que 0,08 g/min.

## PARTE II. Especificación científica de la v0.3.0

La v0.3.0 convierte el paquete de una calculadora de MFO/Fatmax en una herramienta de medición con control de calidad: cada resultado saldrá acompañado de su incertidumbre, de su comparación con el error del laboratorio y de un párrafo de métodos reproducible. Esta parte fija qué se implementa, con qué fórmulas y por qué, y es la referencia que seguirá el desarrollo en Claude Code.

**Principios de diseño científico.**

1. Ninguna estimación puntual sin intervalo: MFO, Fatmax, zona Fatmax y parámetros SIN se devuelven con IC 95 % (bootstrap) o intervalo creíble (bayesiano).
2. Ninguna recomendación sin su protocolo: los valores de referencia se etiquetan con modalidad, protocolo, ecuación y método de la cohorte de origen; el paquete avisa si el protocolo del usuario difiere.
3. Toda ecuación es una función pura con un test unitario contra valores calculados a mano del artículo original.
4. Lo opcional es opcional: Stan/brms y los importadores van en `Suggests`, el núcleo sigue instalándose en segundos.
5. Compatibilidad: `MFO()` y `MFO_kinetics()` mantienen sus argumentos; lo nuevo entra por argumentos con valores por defecto y por funciones nuevas (`MFO_reliability()`, `MFO_bayes()`, `read_cart()`, `MFO_report()`).

| Función nueva o ampliada | Entrada | Salida principal | Sección |
| --- | --- | --- | --- |
| `MFO(..., ci = c("none","bootstrap"), n_boot = 2000)` | Como v0.2.0 + columna `Load` opcional | MFO y Fatmax con IC 95 %; Fatmax en W/km/h | 13, 14, 15 |
| `MFO_kinetics(..., ci = "bootstrap")` | `MFO_db` | d, s, t, Fatmax\_SIN, Fatmin con IC | 13 |
| `MFO_bayes(MFO_db, model = c("P3","SIN"), priors)` | `MFO_db` | Posteriores de MFO, Fatmax, d, s, t; intervalos creíbles; diagnósticos | 13 |
| `MFO_reliability(results, id, session)` | Tabla de `MFOs()` con ≥ 2 sesiones por persona | ICC(2,1) + IC, SEM, MDC95, CV, sesgo, Bland-Altman | 12 |
| `MFO_protocol(type, step_duration, increments, ...)` | Descripción del protocolo | Objeto de protocolo que guía el promediado (escalón, rampa, relativo) | 14 |
| `VO2max_from_test(db_graded, window = 30)` | Test máximo | VO2max/VO2peak por media móvil con criterios de maximalidad | 14 |
| `MFO_zones(result, HRmax, body_fat)` | Resultado de `MFO()` | Zona de FC de prescripción y percentil frente a referencias | 15 |
| `read_cart(path, device)` | Export del analizador | Lista basal/MFO/graded estandarizada, con unidades detectadas | 16 |
| `MFO_report(result, format)` | Resultado individual | Informe HTML/PDF con párrafo de métodos | 17 |

## 12. Módulo de fiabilidad

**Evidencia que lo motiva.** Croci et al. (2014) hallaron CV intraindividuales de Fatmax del 16–21 % según el método (SIN 16,4 %, P3 18,6 %, MV 20,8 %) y límites de acuerdo de ±27–32 % VO2max en hombres entrenados. Chrzanowski-Smith et al. (2020), en 99 adultos con dos tests separados 7–28 días, reportaron CV del 21 % para MFO y 26 % para Fatmax, errores típicos de ±0,06 g/min y ×/÷1,26 %VO2peak, límites de acuerdo de ±0,17 g/min, ICC de 0,75 (MFO) y 0,45 (Fatmax), sin sesgo sistemático y con resultados consistentes entre subgrupos; su conclusión es que hacen falta mediciones repetidas. Chávez-Guevara y Amaro-Gahete (2025) añaden que parte de esa variación es error del analizador y recomiendan sesión de familiarización en sedentarios y reportar varias métricas de fiabilidad (ICC, concordancia, CV).

**Diseño que el módulo asume.** k ≥ 2 sesiones idénticas por participante (mismo protocolo, hora, ayuno, analizador), separadas 2–28 días, sin intervención entre ellas. El usuario pasa la tabla de `MFOs()` con columnas `id` y `session`. Si hay 3 sesiones, se calcula además si la primera difiere sistemáticamente (efecto de familiarización) y se recomienda descartarla.

**Estadísticos y fórmulas.** Siguiendo Weir (2005) y Koo y Li (2016):

- ICC(2,1): modelo de dos factores con efectos aleatorios, acuerdo absoluto, medida única, calculado por ANOVA de medidas repetidas; IC 95 % por F. Interpretación de Koo y Li: < 0,50 pobre, 0,50–0,75 moderada, 0,75–0,90 buena, > 0,90 excelente, juzgada sobre el límite inferior del IC.
- SEM = √MSE del ANOVA (independiente del ICC elegido, preferible a SD√(1−ICC)).
- MDC95 = 1,96 · √2 · SEM; también MDC90.
- CV (%) = SEM / media × 100; para Fatmax y variables con error proporcional, CV sobre logaritmos (error típico como factor ×/÷), como en Hopkins (2000).
- Sesgo sistemático: media de las diferencias con IC 95 %; test t pareado.
- Bland-Altman (1986): límites de acuerdo ±1,96 SD de las diferencias, con gráfico y comprobación de heterocedasticidad (correlación diferencia-media).

```latex
\text{SEM} = \sqrt{\text{MS}_{\text{error}}}, \qquad \text{MDC}_{95} = 1.96\sqrt{2}\,\text{SEM}, \qquad \text{LoA} = \bar{d} \pm 1.96\,\text{SD}_d
```

**Salida.** Tabla con una fila por variable (MFO, MFO/FFM, Fatmax %VO2max, Fatmax %HRmax, Fatmax W, zona inferior/superior, d, s, t) y columnas ICC \[IC\], SEM, MDC95, CV, sesgo \[IC\], LoA; gráficos de Bland-Altman; y un objeto `reliability` que `MFO_report()` usa para marcar si un cambio pre-post individual supera el MDC del laboratorio.

**Uso con datos simulados.** Mientras no haya test-retest reales, el paquete incluirá `simulate_MFO_sessions()` que genera pares de curvas con error biológico (CV 10–20 %) y error del analizador (±1–2 % en VO2 y VCO2, correlacionado) a partir de los parámetros publicados; sirve para los tests unitarios y para que la app muestre el módulo con datos de ejemplo.

## 13. Incertidumbre del ajuste: bootstrap y modelo bayesiano

La fiabilidad (§12) cuantifica el error entre días; esta sección cuantifica el error *dentro* de un test: con 6–10 escalones ruidosos, la posición del máximo de un P3 o de un SIN es incierta aunque el test se repitiera idéntico. Ningún software de MFO publicado reporta esa incertidumbre; es el principal aporte metodológico de la v0.3.0.

**13.1 Bootstrap de residuos (por defecto, `ci = "bootstrap"`).** Se ajusta el modelo, se guardan los residuos, se generan B = 2000 réplicas sumando a los valores ajustados residuos remuestreados con reemplazo, se reajusta cada réplica y se recalcula MFO, Fatmax, zona y parámetros SIN. El IC 95 % es el intervalo percentil (2,5–97,5) o, si el usuario lo pide, BCa (Efron y Tibshirani, 1993). Con pocos escalones (n ≤ 6) el bootstrap de residuos es preferible al de casos porque no deja fuera escalones enteros. Limitaciones que el paquete declara: asume residuos intercambiables (homocedasticidad) y no captura el error del analizador ni el biológico; por eso el informe presenta este IC como "incertidumbre del ajuste" y el MDC como "incertidumbre de la medida".

**13.2 Modelo bayesiano (`MFO_bayes()`, opcional).** Se formula el P3 o el SIN como modelo no lineal con `brms` (Bürkner, 2017) sobre Stan (Carpenter et al., 2017):

```latex
\text{FAT}_i \sim \mathcal{N}\!\left(f(x_i;\theta),\ \sigma\right), \qquad f_{\text{SIN}}(x;\theta) = \text{MFO}\cdot\sin\!\left\{\left[\frac{\pi^{1/s}}{\pi+2d}\left(\tfrac{\pi}{100}x+d+t\right)\right]^{s}\right\}
```

A diferencia de la implementación clásica, el SIN bayesiano se ajusta en g/min directamente (MFO como parámetro de escala), sin la normalización previa por el máximo observado ni el suavizado con P3, lo que elimina dos pasos que introducen sesgo (§9.5). Priors por defecto, débilmente informativos y justificados por la literatura: MFO \~ Normal(0,45, 0,25) truncada en 0 (rango de Maunder 2018 y Amaro-Gahete 2018); d \~ Normal(0, 0,3); s \~ LogNormal(0, 0,3) (s > 0, centrado en 1); t \~ Normal(0, 0,2); σ \~ Exponential(10). El usuario puede sustituirlos (p. ej., priors de deportistas de resistencia). Salida: posteriores y intervalos creíbles del 95 % para MFO, Fatmax (derivado numéricamente de cada muestra posterior), zona Fatmax, Fatmin, d, s, t; diagnósticos (R-hat, ESS, divergencias) y gráfico de curvas posteriores. Ventajas concretas: converge donde `nlsLM` cae en la solución trivial (pocos escalones, curvas planas), permite comparar pre-post como distribución de la diferencia, y escala a un modelo jerárquico multi-participante (priors de grupo) en una versión posterior.

**13.3 Cómo se presenta al usuario.** Siempre tres números por magnitud: estimación, IC/intervalo creíble del ajuste y, si existe `reliability`, MDC del laboratorio. Regla de interpretación en el informe: un cambio pre-post se califica de "probable" si los intervalos del ajuste no se solapan y de "detectable" si además supera el MDC95.

## 14. Protocolos nuevos y VO2max robusto

La v0.2.0 asume escalones de igual duración desde el segundo 0. La v0.3.0 introduce un objeto de protocolo (`MFO_protocol()`) que describe cómo se recogió el test y guía el promediado; `MFO()` lo acepta en lugar de `step_duration`/`steady_state_duration`.

| Tipo de protocolo | Descripción | Promediado | Referencia |
| --- | --- | --- | --- |
| `step` (defecto) | Escalones de duración fija, incrementos fijos | Últimos 60–120 s de cada escalón | Achten 2002; scoping review 2025 |
| `step_variable` | Escalones de duración distinta (calentamiento largo, escalones finales cortos) | Vector de duraciones; tramo estable configurable por escalón | Amaro-Gahete 2019 (cinta 3,5 km/h + 1 km/h/3 min) |
| `relative` | Incrementos en W/kg (p. ej., 0,15 W/kg/4 min desde 0,45 W/kg) | Como `step`; registra la carga relativa | Test de potencia relativa 2026 |
| `ramp` | Incremento continuo (p. ej., 10–15 W/min) | Ventanas móviles de 30–60 s; curva FAT vs %VO2max sin escalones | Physiologia 2025 |
| `single_day` | MFO seguido, tras 5 min, de test máximo desde la última carga | Ambos tests en un mismo fichero; corte automático por marca de tiempo | SMHS 2026 (ICC 0,957 para VO2max) |

**Carga externa.** Si el test incluye una columna `Load` (W o km/h) por registro, `MFO()` devuelve Fatmax también en esa unidad por interpolación sobre la curva ajustada, y la zona Fatmax en W/km/h. Es lo que el entrenador necesita para prescribir.

**VO2max robusto (`VO2max_from_test()`).** Se sustituye `max(VO2)` por la mayor media móvil de `window` segundos (30 s por defecto; opciones 15, 20, 60). La media móvil ya está en la 0.2.1 como argumento `vo2max_window` de `MFO()` (helper interno `.vo2max_rolling()`, §9.3); la 0.3.0 la expone como función con criterios de maximalidad. Se reportan criterios de maximalidad (meseta de VO2 < 150 mL/min entre cargas, RER > 1,10, HR ≥ 90 % del teórico) y se etiqueta el valor como VO2max o VO2peak según se cumplan. El usuario puede pasar el VO2max calculado por el laboratorio y el paquete no lo recalcula.

**Criterio de parada y escalones válidos.** `RER_cutoff` pasa a admitir los valores recomendados por nombre (`"classic"` = 1,00; `"sedentary"` = 0,93; `"trained"` = 0,90; Amaro-Gahete 2019). Se añade `min_steps` (6 por defecto) con aviso si la curva se ajusta con menos escalones, y aviso si Fatmax cae en el primer o último escalón (máximo en el borde, probable protocolo inadecuado).

**Modalidad.** Se registra `mode = c("cycling", "running", "walking")` porque la cinética difiere: en carrera la curva es más dilatada y asimétrica a la derecha y Fatmax es mayor que en ciclismo con MFO similar (Chenevière et al., 2010). La modalidad se usa para elegir referencias (§15) y se imprime en el párrafo de métodos.

## 15. Zonas de frecuencia cardiaca y valores de referencia

**15.1 Zona de prescripción en FC (`MFO_zones()`).** Para cada resultado se devuelve: HR a Fatmax (interpolada), HR en los límites de la zona Fatmax, y su expresión en %HRmax (medida en el test máximo, o teórica si el usuario la pide, indicando la fórmula). Cuando el usuario declara obesidad (porcentaje de grasa), el informe añade la recomendación de la meta-regresión de Chávez-Guevara et al. (2023): 61–66 % HRpeak con > 35 % de grasa corporal y 57–64 % HRpeak con < 35 %, y señala si la zona individual medida cae dentro o fuera de ese rango. La razón para preferir %HRpeak a %VO2max en obesidad es que la relación HR-VO2 es más estable que el VO2max medido, a menudo submáximo en estos pacientes.

**15.2 Valores de referencia (`reference_MFO`).** El paquete incluirá una tabla interna con percentiles publicados, cada fila etiquetada con población, modalidad, protocolo, ecuación y método:

| Cohorte | Población | Modalidad / protocolo | MFO (g/min), media y P25–P75 | Fatmax (%VO2max) |
| --- | --- | --- | --- | --- |
| Amaro-Gahete et al. 2018 (FIT-AGEING / ACTIBATE) | 167 adultos sedentarios (107 mujeres), 18–65 años | Cinta, 3,5 km/h + 1 km/h/3 min; último minuto; Frayn | 0,34 ± 0,10; P25 0,30, P50 0,35, P75 0,42 | 44,2 ± 12,4; P25 39,6, P50 44,6, P75 54,1 |
| — hombres sedentarios | 60 | id. | 0,37 ± 0,11 | 40,8 ± 11,0 |
| — mujeres sedentarias | 107 | id. | 0,32 ± 0,10 | 46,1 ± 12,8 |
| — jóvenes (22 años) vs mediana edad (52 años) | 125 / 42 | id. | 0,36 ± 0,11 / 0,29 ± 0,08 | 44,0 / 44,7 |
| Maunder et al. 2018 | Síntesis de estudios en cicloergómetro, activos y deportistas | Cicloergómetro; varios | Percentiles derivados de media ± SD (no empíricos) | id. |
| Randell et al. 2017 | 1.121 deportistas | Cinta | Gran variabilidad interindividual | — |

La tabla de Amaro-Gahete et al. (2018) también desglosa por estado ponderal (normopeso 0,34; sobrepeso 0,33; obesidad 0,36 g/min, sin diferencias) y advierte que los percentiles de Maunder proceden de media y SD, no de distribuciones empíricas, y de cicloergómetro, por lo que no son trasladables a cinta. El paquete aplicará exactamente esa lógica: solo compara con una referencia cuya modalidad coincida y, si el protocolo difiere, lo dice en el informe.

**15.3 Posición relativa.** `MFO_zones()` devuelve el percentil aproximado del participante (interpolación en los percentiles publicados) para MFO absoluto, MFO/FFM cuando la referencia lo tenga, y Fatmax, con la etiqueta de la cohorte usada. Nunca se devuelve un percentil sin su cohorte.

## 16. Importadores de analizadores de gases y contrato de datos

El objetivo es que el usuario de la app suba el fichero tal y como sale del analizador. `read_cart(path, device)` devuelve la estructura que `MFO()` espera, y es la única capa que conoce los formatos propietarios.

**Contrato de datos interno (lo que `MFO()` recibe siempre):** un data frame por fase con columnas `Time` (s desde el inicio), `VO2` (mL/min), `VCO2` (mL/min), `RER`, `HR` (lpm) y opcionalmente `Load` (W o km/h), `VE` (L/min) y `Phase` (basal/MFO/graded), a intervalo regular o breath-by-breath con `Time` real. Unidades verificadas: si VO2 viene en L/min se multiplica por 1000; si viene en mL/kg/min se exige masa corporal. RER se recalcula como VCO2/VO2 y se compara con la columna exportada (aviso si difieren > 0,02).

**Dispositivos previstos (prioridad según lo que use el laboratorio de la UCA, pendiente de confirmar):**

| Dispositivo / software | Formato de export | Particularidades a resolver |
| --- | --- | --- |
| Cosmed (Omnia, Quark CPET, K5) | xlsx/csv con cabecera de metadatos y fila de unidades | Columnas en varios idiomas; tiempo en hh:mm:ss; marcadores de fase |
| Cortex (MetaSoft Studio) | xlsx/csv | Separador decimal por configuración regional; promediado ya aplicado |
| Vyaire Vyntus CPX (SentrySuite) | csv/xlsx | Columnas duplicadas (BTPS/STPD); carga en W |
| Parvo Medics TrueOne | txt/xlsx | Intervalos fijos; cabecera larga |
| Genérico | csv/xlsx con mapeo manual de columnas (como en v0.2.0) | Interfaz de mapeo en la app |

Cada importador se desarrollará a partir de un fichero real anonimizado incluido en `inst/extdata`, que sirve de test (el importador debe reproducir exactamente los valores de escalón calculados a mano) y de ejemplo en la documentación. La app detectará el dispositivo por la cabecera y pedirá confirmación.

**Separación de fases.** Si el fichero contiene las tres fases en una sola serie temporal (frecuente en `single_day`), `read_cart()` las separa por marcadores del analizador o por tiempos que el usuario introduce; si hay dudas, la app muestra la serie y deja marcar los cortes.

## 17. Plan de validación, tests y declaración de uso de IA

**Validación numérica.** (1) Ecuaciones: tests unitarios con los ejemplos numéricos de Frayn (1983), Péronnet y Massicotte (1991) y Jeukendrup y Wallis (2005). (2) Métodos: reproducir con los datos de ejemplo del paquete los valores de Montes de Oca et al. (2021) y comprobar que P3, MV y SIN dan MFO equivalentes a nivel de grupo, como reportan Chenevière et al. (2009) y Amaro-Gahete et al. (2019). (3) Bootstrap y bayesiano: estudio de simulación con curvas sintéticas de parámetros conocidos para verificar cobertura de los intervalos (≈ 95 %) con 6, 8 y 10 escalones y CV de ruido del 5–20 %. (4) Fiabilidad: comparar `MFO_reliability()` con `psych::ICC` e `irr` en datos simulados y con los estadísticos publicados por Chrzanowski-Smith et al. (2020) sobre su dataset abierto. (5) Importadores: igualdad exacta con cálculos manuales sobre los ficheros de `inst/extdata`.

**Infraestructura.** GitHub Actions (R CMD check en tres sistemas, cobertura ≥ 85 % con `covr`), pkgdown con vignettes "De los gases al Fatmax", "Fiabilidad de tu laboratorio" y "Incertidumbre bayesiana", NEWS por versión, issues públicas para cada punto de esta Parte II, y releases etiquetadas. Esto construye el historial público que JOSS exige.

**Uso de IA generativa y JOSS.** JOSS no prohíbe que el código se escriba con asistencia de IA; exige (a) que el autor que envía sea contribuidor principal del software, (b) una declaración de uso de IA que indique herramientas y versiones, dónde se usaron, qué tipo de ayuda prestaron y la confirmación de que personas revisaron y validaron el resultado, y (c) que la IA no se use en la comunicación con editores y revisores (salvo traducción). En la práctica, para este proyecto: el diseño científico, las decisiones de producto, la validación contra la literatura y la redacción final del `paper.md` son de Jorge; Claude Code genera código, tests y documentación bajo esa dirección; cada commit queda firmado con coautoría de Claude para que el historial sea transparente; y el `paper.md` incluirá la declaración con el texto aproximado: "Parts of the code, unit tests and documentation of MFO v0.3.0 were drafted with Claude (Anthropic; Claude Code, model and version), under the design and supervision of the authors, who reviewed, tested and validated all outputs against the primary literature". Los revisores de JOSS evalúan el software y el artículo; una base de código bien testeada y documentada con esa declaración no es un obstáculo. Lo que sí sería un problema es un historial de un solo commit o un paquete sin evidencia de uso: por eso la fase 1 (sanear y publicar en CRAN) y la acumulación de 6 meses de actividad pública van antes del envío.

**Criterios de "hecho" para la v0.3.0.** Todos los tests en verde en CI; vignettes renderizadas; `R CMD check` sin notas en CRAN; app Shiny desplegada con los datos de ejemplo; `paper.md` en el repo; al menos un laboratorio externo (además de la UCA) usando el paquete o la app.

## 18. Herramientas existentes: análisis competitivo

Búsqueda realizada el 9 de noviembre de 2026 en CRAN, GitHub, JOSS, PyPI, bioRxiv/PMC y webs de fabricantes. **Conclusión: no existe ninguna herramienta de código abierto dedicada a MFO/Fatmax aparte de MFO.** Las alternativas son, o bien paquetes generales de ergoespirometría sin cálculo de Fatmax, o bien software comercial cerrado sin método documentado ni incertidumbre.

| Herramienta | Tipo | Qué hace | Qué no hace (hueco para MFO) | Lección para la v0.3.0 |
| --- | --- | --- | --- | --- |
| [spiro](https://docs.ropensci.org/spiro/) (R, CRAN, rOpenSci, JOSS 2023) | Paquete abierto | Importa Cortex, Cosmed, Vyntus y ZAN; resúmenes por escalón; VO2max con medias móviles, filtros Butterworth; 9 paneles de Wasserman; protocolo autodetectado | No calcula sustratos ni Fatmax | Reutilizar su importación en vez de reescribirla (ver decisión abajo); su paso por rOpenSci y JOSS es el modelo a seguir |
| [whippr](https://fmmattioni.github.io/whippr/) (R, CRAN) | Paquete abierto | Lectura Cosmed/Cortex, cinética de VO2, detección de outliers, normalización de tests incrementales | Sin oxidación de sustratos | Su detección de outliers breath-by-breath es aplicable antes de promediar |
| Shiny-Calorie (R/Shiny, 2025) y CalR | App abierta | Calorimetría indirecta en modelos animales, integración de metadatos, estadística | Orientadas a roedores y 24 h, no a tests incrementales humanos | Referente de app Shiny con despliegue web + escritorio + Docker |
| wrictools (R, CRAN 2026) | Paquete abierto | Calorímetros de habitación (Maastricht Instruments) | Otro dominio | Ejemplo reciente de paquete con importador específico de fabricante aceptado en CRAN |
| Cosmed OMNIA ≥ 2.2 (2022) | Software del analizador | Parámetro y "calculadora" de Fat Max, zonas de entrenamiento | Método no documentado públicamente; sin IC, sin SIN, sin fiabilidad; solo Cosmed | El usuario de Cosmed ya tiene un número: MFO debe explicar por qué el suyo es mejor (método explícito, IC, comparabilidad entre laboratorios) |
| INSCYD | Software comercial | Estima FatMax y curva de combustión a partir de VO2max y VLamax, sin analizador | Modelo, no medida; cerrado | Distinto mercado; MFO es la referencia medida contra la que validar modelos así |
| Calculadora Endure IQ; estimador Python de Fatmax por lactato (SportRxiv) | Calculadoras | Estiman Fatmax desde tests de campo o lactato | Sin calorimetría | Igual que INSCYD: posibles usuarios de MFO para validación |
| Cortex MetaSoft Studio; Vyaire SentrySuite | Software del analizador | Exportan VO2/VCO2/RER por intervalo | No se ha podido verificar que calculen Fatmax (consultar manuales) | Son fuentes de datos, no competidores |

**Lo que ninguna ofrece y la v0.3.0 sí:** elección explícita y citada de ecuación y método; intervalos de confianza y versión bayesiana; modelo SIN con d, s, t; módulo de fiabilidad del propio laboratorio; valores de referencia etiquetados por protocolo; párrafo de métodos reproducible; y todo ello libre y auditable.

**Decisión de arquitectura derivada (una línea):** los importadores de la sección 16 se construirán sobre `spiro::spiro()` (en `Suggests`) para Cortex, Cosmed, Vyntus y ZAN, y `read_cart()` solo añadirá la separación de fases y la validación de unidades; así no duplicamos código ya revisado por rOpenSci, y la interoperabilidad con un paquete de JOSS es un argumento a favor en la revisión. Para analizadores que spiro no soporte (Parvo) se mantiene el lector genérico con mapeo de columnas.

## 19. Verificación de fórmulas y fuentes para la v0.3.0

Cada coeficiente y regla que usará el paquete se ha contrastado con la fuente primaria o con, al menos, dos documentos independientes que la transcriben (protocolos registrados en ClinicalTrials.gov, artículos en PMC). Estado: **verificado** (coincide), **matiz** (correcto pero incompleto en el código o en este documento) o **pendiente** (requiere el texto completo del artículo). Cuatro hallazgos cambian el código o la documentación y se han añadido al backlog.

| Elemento | Valor en el paquete / en este documento | Fuente contrastada | Estado | Acción |
| --- | --- | --- | --- | --- |
| Frayn (1983), ec. (5) y (6): CHO = 4,55 VCO2 − 3,21 VO2 − 2,87 n; FAT = 1,67 VO2 − 1,67 VCO2 − 1,92 n | Igual (n = 0) | Texto completo, p. 628; tabla 1 (glucosa 0,746/0,746; grasa 2,03/1,43 L/g) | Verificado | Frayn **imprime** las ecuaciones: se usan tal cual (4,55/3,21; 1,67/1,67). La derivación desde su tabla 1 (sistema 2×2) queda solo como test cruzado (regla de abajo) |
| Péronnet y Massicotte (1991): CHO = 4,585 VCO2 − 3,2255 VO2; FAT = 1,6946 VO2 − 1,7012 VCO2 | v0.2.0: 4,585 / 3,2255 / 1,6946 / 1,7012. 0.2.1: derivados de la tabla 1 (corregido) | Texto completo: el artículo **no imprime estas ecuaciones**; publica la tabla 1 (glucosa 0,7455 L O2 y 0,7426 L CO2 por g; ácido graso 2,0092 y 1,4136) de la que se derivan: CHO = 4,58519 VCO2 − 3,22597 VO2; FAT = 1,69469 VO2 − 1,70130 VCO2 | **Matiz (hallazgo 6)** | Como el artículo no imprime ecuación, se usan los coeficientes derivados (4,58519/3,22597; 1,69469/1,70130), resolviendo el sistema en código con las constantes de la tabla 1 y explicando la derivación en roxygen (diferencias en el 4.º decimal frente a los valores actuales). Hecho en la 0.2.1 (`.peronnet_coefficients()`); en el ejemplo Fatmax 35,288 → 35,285 % VO2max |
| Jeukendrup y Wallis (2005): FAT = 1,695 VO2 − 1,701 VCO2; CHO 40–50 % = 4,344 VCO2 − 3,061 VO2; CHO 50–75 % = 4,210 VCO2 − 2,962 VO2 | Igual, coeficientes impresos en la tabla 5 y citados por tabla; umbral en 50 % | Texto completo (Int J Sports Med 26 S1, tablas 4 y 5); protocolos NCT03574987 y NCT03851744; PMC5117036 confirma el rango 50–75 % | Verificado | Test cruzado: derivando desde las constantes de la tabla 4 (glucosa 0,7455/0,7426; glucógeno 0,8283/0,8251; ácido graso 2,0092/1,4136 L/g) con mezcla glucosa/glucógeno 50/50 → 4,3438/3,0561 (impreso 4,344/3,061) y 20/80 → 4,2107/2,9625 (impreso 4,210/2,962); tolerancia absoluta 0,006. Añadir aviso fuera de 40–75 % (ya en backlog) |
| Jeukendrup y Wallis (2005) en reposo | Basal con el par 100/0 (glucosa pura) derivado de la tabla 4: CHO = 4,58519 VCO2 − 3,22597 VO2; FAT = 1,69469 VO2 − 1,70130 VCO2 (derivado porque no hay ecuación impresa) | Tablas 4 y 5: **no proponen ecuación de reposo** | Decisión del paquete | Documentado como tal en roxygen (`substrate_oxidation(phase = "rest")`); no citar como ecuación de Jeukendrup y Wallis |
| **Equivalentes energéticos** | El paquete usa 4 kcal/g (CHO) y 9 kcal/g (FAT) para las tres ecuaciones | Frayn 1983 **no da ninguno** (lo dice explícitamente, p. 632). Péronnet y Massicotte 1991: glucosa 3,8683 kcal/g, grasa 9,7460 kcal/g, 1 kcal = 4,182 kJ; no dan glucógeno. Jeukendrup y Wallis 2005, tabla 5: CHO 3,95 kcal/g (mezcla 40–50 % VO2max), 4,07 kcal/g (50–75 %), grasa 9,75 kcal/g | **Matiz (hallazgo 1)** | Argumento `energy_equivalents` con defecto por ecuación: Péronnet 3,8683/9,7460; Jeukendrup 3,95 (40–50 %) / 4,07 (50–75 %) / 9,75 según el tramo de cada escalón (corregido 10 nov 2026; antes 4,15/9,75); Frayn: Péronnet (decisión del paquete). Hecho en la 0.2.1: `energy_equivalents = c("classic", "source")`, defecto `"classic"` (4/9) con mensaje único por sesión, `"source"` será el defecto en la 0.3.0; Jeukendrup en reposo usa Péronnet. `Kcal` cambia hasta un 8 % |
| Criterio de parada por RER | 1,00 clásico; 0,93 sedentarios; 0,90 entrenados | Amaro-Gahete 2019; Chenevière 2009 promedia solo escalones con RER ≤ 1,0 | **Matiz (hallazgo 2)** | Amaro-Gahete recomienda **0,95** como umbral seguro; 0,93/0,90 son RER medios en MFO. Añadir `"safe" = 0,95` y explicarlo |
| Zona Fatmax = FAT ≥ 90 % del MFO | Igual | Achten 2002 ("within 10% of the peak rate"); zona media 55–72 % VO2max | Verificado | Ninguna |
| Fatmin | v0.2.0: primera intensidad con %MFO < 5 % por encima de Fatmax. 0.2.1: segundo cero (corregido) | Chenevière 2009, texto completo: Fatmin es el **segundo cero** del SIN (fat = 0 % MFO), sin umbral; en su muestra 90,1 ± 14,1 % VO2max. Achten 2002: 89 ± 3 % | **Matiz (hallazgo 3)** | Fatmin = x₂ = (π + d − t)/K por defecto; `fatmin_threshold` opcional para quien quiera un umbral |
| Protocolo clásico | 3 min / 35 W | Achten 2002; Chenevière 2009 usó 3 min / 20 W (10 W en sedentarios) | Verificado | Ninguna |
| **Modelo SIN: fórmula** | v0.2.0: `(pi^(1/s) / pi + 2*d)`, es decir (π^(1/s)/π) + 2d. 0.2.1: `pi^(1/s) / (pi + 2*d)` (corregido) | Ec. \[14\], p. 1618 y Apéndice p. 1625: π^(1/s) / (π + 2d) | **Error (hallazgo 5)** | Corregir a `pi^(1/s) / (pi + 2*d)`. Con d = 0 ambas coinciden (por eso la curva básica y los tests pasaban), pero con d ≠ 0 el parámetro d del paquete **no es la dilatación de Chenevière** y no es comparable con la literatura. Test de regresión: con d = 0,18, s = 1,52, t = 0,21 (entrenado, fig. 5) la curva debe reproducir Fatmax y Fatmin de la fórmula cerrada |
| Modelo SIN: procedimiento de ajuste | v0.2.0: normaliza al máximo observado, suaviza con P3, malla sintética, renormaliza, ajusta. 0.2.1: ajuste directo a los escalones (corregido) | Chenevière: ajuste directo a los valores medidos (como E\_lipid) minimizando suma de cuadrados; **sin suavizado ni malla** | **Matiz (hallazgo 7)** | Eliminar el suavizado y la malla: inflan artificialmente n y reducen los errores típicos de d, s, t. Ajustar a los escalones observados; si no converge, avisar en lugar de suavizar |
| MRER | FAT = 1,7(1−RER)VO2, máximo observado; atribuido a Pérez-Martín | Chenevière 2009, p. 1617: la ecuación y la cita a Pérez-Martín son correctas; el método ajusta (1 − RER) y VO2 como rectas de la intensidad y toma el vértice de la parábola producto | **Matiz (hallazgo 4, resuelto)** | Hecho en la 0.2.1: MRER real (vértice de las dos rectas) y `method = "MV"` (máximo observado con la ecuación de `author`) |
| Fiabilidad: Chrzanowski-Smith 2020 comparó MV, P2 y SIN (no P3) | — | Resumen del artículo | Matiz | Citarlo con precisión en §12 |
| Fiabilidad: CV 21 % MFO, 26 % Fatmax; TE ±0,06 g/min; LoA ±0,17 g/min; ICC 0,75 / 0,45 | Igual | Chrzanowski-Smith 2020 (n = 99) | Verificado | Ninguna |
| Fiabilidad: CV Fatmax SIN 16,4 %, P3 18,6 %, MV 20,8 % | Igual | Croci 2014 (n = 15) | Verificado | Ninguna |
| Fiabilidad histórica: CV Fatmax 9,6 %, Fatmin 9,4 % | Añadido | Achten y Jeukendrup 2003 (n = 10) | Verificado | Incluir en §12 |
| Zonas %HRpeak en obesidad: 61–66 % (> 35 % grasa), 57–64 % (< 35 %) | Igual | Chávez-Guevara 2023 | Verificado | Ninguna |
| Valores normativos sedentarios (0,34 ± 0,10 g/min; 44,2 ± 12,4 % VO2max; percentiles) | Igual | Amaro-Gahete, Sanchez-Delgado y Ruiz 2018, tabla 1 | Verificado | Ninguna |
| ICC(2,1), SEM = √MSE, MDC95 = 1,96√2·SEM | Igual | Weir 2005; Koo y Li 2016 | Verificado | Ninguna |
| Fatmax típico 45–65 % VO2max; Achten 2002: 64 % VO2max, 74 % HRmax | Igual | Achten 2002; Maunder 2018 | Verificado | Ninguna |
| Interpretación de s | s > 1 asimetría a la derecha | Chenevière 2009, Discusión: 0 < s < 1 izquierda, s > 1 derecha | Verificado | Ninguna |

**Estado tras la lectura de los textos completos (9 de noviembre de 2026).** Los tres pendientes quedan cerrados. Balance: 7 hallazgos, de los cuales uno es un **error de implementación** (paréntesis del SIN, hallazgo 5), dos son desviaciones metodológicas respecto a la fuente (suavizado del SIN, hallazgo 7; MRER como MV, hallazgo 4) y cuatro son matices de documentación o parámetros (equivalentes energéticos, RER 0,95, Fatmin en cero, coeficientes de Péronnet derivados). Todos pasan al backlog de la 0.2.1 (los que corrigen código) o de la 0.3.0 (los que añaden opciones). Consecuencia práctica: los valores de d obtenidos con la v0.1.0/v0.2.0 no deben compararse con los publicados por el grupo de Chenevière; MFO y Fatmax por P3 no se ven afectados.

**Regla de procedencia (Jorge, 10 nov 2026): coeficiente impreso en la fuente > coeficiente derivado; derivado solo donde la fuente no imprime ecuación.** Si la fuente imprime la ecuación, se usa tal cual y se cita por tabla o ecuación; la derivación desde sus constantes (sistema 2×2) queda como test cruzado con tolerancia absoluta acorde al redondeo (0,006 en Jeukendrup y Wallis). Si no la imprime (Péronnet y Massicotte; reposo con Jeukendrup y Wallis), se deriva en código y la derivación se explica en roxygen.

**Regla para el desarrollo:** ningún coeficiente entra en el código sin (a) la cita en el bloque roxygen, (b) un test unitario con un ejemplo numérico de la fuente y (c) una fila en esta tabla. Esta tabla se exporta al repo como `docs/VERIFICATION.md` y se actualiza en cada PR que toque una fórmula.

## Referencias

Fundamentos y ecuaciones

- Frayn KN (1983). Calculation of substrate oxidation rates in vivo from gaseous exchange. *J Appl Physiol* 55:628–634.
- Péronnet F, Massicotte D (1991). Table of nonprotein respiratory quotient: an update. *Can J Sport Sci* 16:23–29.
- Jeukendrup AE, Wallis GA (2005). Measurement of substrate oxidation during exercise by means of gas exchange measurements. *Int J Sports Med* 26 Suppl 1:S28–S37.
- Brooks GA, Mercier J (1994). Balance of carbohydrate and lipid utilization during exercise: the "crossover" concept. *J Appl Physiol* 76:2253–2261.

Fatmax, protocolos y métodos de análisis

- Achten J, Gleeson M, Jeukendrup AE (2002). Determination of the exercise intensity that elicits maximal fat oxidation. *Med Sci Sports Exerc* 34:92–97.
- Chenevière X, Malatesta D, Peters EM, Borrani F (2009). A mathematical model to describe fat oxidation kinetics during graded exercise. *Med Sci Sports Exerc* 41:1615–1625. [doi](https://doi.org/10.1249/MSS.0b013e31819e2f91)
- Pérez-Martín A, Dumortier M, Raynaud E, et al. (2001). Balance of substrate oxidation during submaximal exercise in lean and obese people. *Diabetes Metab* 27:466–474.
- Amaro-Gahete FJ, Sanchez-Delgado G, Alcantara JMA, et al. (2019). Optimizing maximal fat oxidation assessment by a treadmill-based graded exercise protocol: when should the test end? *Front Physiol* 10:909. [doi](https://doi.org/10.3389/fphys.2019.00909)
- Montes de Oca-García A, González-Montesinos JL, et al. (2021). *Eur J Sport Sci*. [doi](https://doi.org/10.1080/17461391.2020.1788650)
- Maunder E, Plews DJ, Kilding AE (2018). Contextualising maximal fat oxidation during exercise: determinants and normative values. *Front Physiol* 9:599.

Revisiones y recomendaciones recientes (2023–2026)

- Chávez-Guevara IA, et al. (2023). Toward exercise guidelines for optimizing fat oxidation during exercise in obesity: a systematic review and meta-regression. *Sports Med*. [doi](https://doi.org/10.1007/s40279-023-01897-y)
- Chávez-Guevara IA, et al. (2025). Methodological issues related to maximal fat oxidation and FATmax reproducibility: a narrative review. *Int J Obes* 49:1984–1994. [enlace](https://www.nature.com/articles/s41366-025-01861-y)
- Scoping review (2025). Maximal fat oxidation during cycle ergometer protocols in obese adults. *Diseases* 14(1):4. [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC12840565/)
- Indexes of fat oxidation from ramp vs. graded incremental protocols in postmenopausal women (2025). *Physiologia* 5(1):3. [doi](https://doi.org/10.3390/physiologia5010003)
- Maximal fat oxidation assessment in active postmenopausal females: a novel relative power FATmax test (2026). [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC13315815/)
- Meta-análisis sobre ejercicio a FATmax y metabolismo glucídico-lipídico en sobrepeso/obesidad (2026). [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC13343287/)

Software

- Fernandez-Santos JR, et al. (2022). MFO: Maximal Fat Oxidation and Kinetics Calculation. R package v0.1.0, CRAN. [enlace](https://cran.r-project.org/web/packages/MFO/index.html)
- Repositorio v0.2.0: [github.com/JorgeDelro/MFO](https://github.com/JorgeDelro/MFO)
- Journal of Open Source Software, criterios de envío. [enlace](https://joss.theoj.org/about)

Valores de referencia, protocolos adicionales y cinética (Parte II)

- Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary: Contextualising maximal fat oxidation during exercise: determinants and normative values. *Front Physiol* 9:1460. [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC6201563/)
- Amaro-Gahete FJ, Sanchez-Delgado G, Jurado-Fasoli L, et al. (2019). Assessment of maximal fat oxidation during exercise: a systematic review. *Scand J Med Sci Sports* 29:910–921.
- Randell RK, Rollo I, Roberts TJ, et al. (2017). Maximal fat oxidation rates in an athletic population. *Med Sci Sports Exerc* 49:133–140.
- Chenevière X, Malatesta D, Gojanovic B, Borrani F (2010). Differences in whole-body fat oxidation kinetics between cycling and running. *Eur J Appl Physiol* 109:1037–1045. [enlace](https://doc.rero.ch/record/310501?ln=en)
- Validity and reliability of VO2max determination immediately after a maximal fat oxidation assessment in pre-diabetes/type 2 diabetes (2026). *Sports Med Health Sci* 8(3):327–334. [doi](https://doi.org/10.1016/j.smhs.2025.02.004)

Fiabilidad, error de medida y estadística (Parte II)

- Croci I, Borrani F, Byrne NM, et al. (2014). Reproducibility of Fatmax and fat oxidation rates during exercise in recreationally trained males. *PLoS ONE* 9:e97930. [enlace](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4041727/)
- Chrzanowski-Smith OJ, Edinburgh RM, Thomas MP, et al. (2020). The day-to-day reliability of peak fat oxidation and FATMAX. *Eur J Appl Physiol* 120:1745–1759. [enlace](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7340634/)
- De Souza Silveira R, Carlsohn A, Langen G, et al. (2016). Reliability and day-to-day variability of peak fat oxidation during treadmill ergometry. *J Int Soc Sports Nutr* 13:4.
- Weir JP (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM. *J Strength Cond Res* 19:231–240.
- Koo TK, Li MY (2016). A guideline of selecting and reporting intraclass correlation coefficients for reliability research. *J Chiropr Med* 15:155–163.
- Hopkins WG (2000). Measures of reliability in sports medicine and science. *Sports Med* 30:1–15.
- Bland JM, Altman DG (1986). Statistical methods for assessing agreement between two methods of clinical measurement. *Lancet* 1:307–310.
- Efron B, Tibshirani RJ (1993). *An Introduction to the Bootstrap*. Chapman & Hall.
- Bürkner PC (2017). brms: an R package for Bayesian multilevel models using Stan. *J Stat Softw* 80(1):1–28.
- Carpenter B, Gelman A, Hoffman MD, et al. (2017). Stan: a probabilistic programming language. *J Stat Softw* 76(1).

Herramientas relacionadas y verificación (secciones 18–19)

- Nolte S (2023). spiro: An R package for analyzing data from cardiopulmonary exercise testing. *J Open Source Softw* 8(81):5089. [enlace](https://docs.ropensci.org/spiro/)
- Mattioni Maturana F (2025). whippr: Tools for manipulating gas exchange data. R package v0.1.4, CRAN. [enlace](https://fmmattioni.github.io/whippr/)
- Grein S, et al. (2025). Shiny-Calorie: a context-aware application for indirect calorimetry data analysis and visualization using R. [enlace](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12867577/)
- COSMED (2022). OMNIA 2.2 software release (Fat Max parameter and calculator). [enlace](https://www.cosmed.com/en/news/company/1561-omnia-2-2-software-release)
- Achten J, Jeukendrup AE (2003). Maximal fat oxidation during exercise in trained men. *Int J Sports Med* 24:603–608. [enlace](https://www.thieme-connect.com/products/ejournals/html/10.1055/s-2003-43265)
- Achten J, Gleeson M, Jeukendrup AE (2002), resumen con definición de la zona Fatmax. [enlace](https://research.birmingham.ac.uk/en/publications/determination-of-the-exercise-intensity-that-elicits-maximal-fat-)
- Brun JF, Varlet-Marie E, Romain AJ, Mercier J (2012). Measurement and physiological relevance of the maximal lipid oxidation rate during exercise (LIPOXmax). IntechOpen. [enlace](https://core.ac.uk/download/322406893.pdf)
- Protocolos registrados que transcriben las ecuaciones de Jeukendrup-Wallis y Péronnet-Massicotte: NCT03574987, NCT03851744, NCT06164665, NCT05078138 (ClinicalTrials.gov).

*Nota: las referencias de 2025–2026 se han verificado en línea el 6 de noviembre de 2026; las de 2014–2020 sobre fiabilidad y los valores normativos de 2018 se han verificado en PMC el mismo día; las clásicas (1983–2017) y las de estadística se citan de memoria bibliográfica y deben cotejarse al preparar el `paper.md`.*
