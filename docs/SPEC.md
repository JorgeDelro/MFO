# MFO: fundamentos científicos y técnicos del paquete y la app

Nov 6, 2026 · @Jorge

## Resumen

Este documento es el manual científico-técnico de referencia del paquete de R **MFO** (v0.2.0 en GitHub; v0.1.0 en CRAN) y de la app que lo expondrá a científicos del deporte y la salud sin necesidad de programar. Explica, desde cero, qué es la oxidación máxima de grasas (MFO), la intensidad a la que ocurre (Fatmax) y la cinética de la oxidación de grasas; cómo se miden mediante calorimetría indirecta; qué ecuaciones estequiométricas convierten VO₂ y VCO₂ en gramos de grasa e hidratos oxidados; cómo se diseña y procesa un test incremental de MFO; qué métodos matemáticos (P3, P2, MRER, modelo SIN) localizan el máximo; cómo se expresan y normalizan los resultados; y cuál es el error de medida esperable. En la segunda mitad se describe, función por función, qué hace exactamente el código del paquete y con qué ecuación de la literatura se corresponde cada paso, señalando las decisiones de implementación y las limitaciones que motivan la hoja de ruta de la v0.3.0 (fiabilidad, intervalos de confianza, nuevos protocolos, importadores de analizadores de gases, app Shiny).

**Palabras clave:** oxidación de grasas, Fatmax, calorimetría indirecta, metabolismo de sustratos, modelo SIN, R, software científico.

**Cómo leer este documento.** Las secciones 1–8 son la base científica y se pueden leer sin saber R. La sección 9 es la correspondencia con el código y conviene leerla con el repositorio abierto. Las secciones 10–11 justifican las decisiones de la próxima versión y la interpretación práctica de los resultados. Todas las fórmulas van en bloques independientes y se nombran por su autor para que el usuario pueda elegirlas en la app con criterio.

## 1. Introducción

La capacidad de oxidar grasas durante el ejercicio es un marcador de flexibilidad metabólica con interés doble: en rendimiento, porque ahorra glucógeno y se asocia con el rendimiento en resistencia de larga duración; en salud, porque está reducida en obesidad, resistencia a la insulina y diabetes tipo 2, y se correlaciona con la sensibilidad a la insulina, la capacidad oxidativa muscular y la lipólisis del tejido adiposo. Dos magnitudes resumen esa capacidad: la **oxidación máxima de grasas (MFO)**, en g/min, y la **intensidad a la que ocurre (Fatmax)**, expresada como %VO₂max, %HRmax, potencia o velocidad.

El concepto de Fatmax lo formalizaron Achten, Gleeson y Jeukendrup (2002) con un test incremental en cicloergómetro y, desde entonces, se ha usado tanto para caracterizar poblaciones como para prescribir ejercicio a la intensidad que maximiza el uso de grasa. Sin embargo, la literatura muestra una heterogeneidad metodológica importante: duración de escalones, incrementos de carga, promediado de los gases, ecuación estequiométrica, criterio de finalización y método de localización del máximo cambian de un estudio a otro, y cada decisión altera el valor obtenido. Las revisiones de 2023–2026 insisten en lo mismo: hace falta estandarizar y reportar con detalle el protocolo y el algoritmo de análisis.

Ahí encaja un software abierto y reproducible. El paquete MFO nació en 2022 para que el cálculo de MFO y Fatmax no dependiera de hojas de cálculo artesanales, implementando los métodos usados en Montes de Oca et al. (2021) y el modelo de cinética de Chenevière et al. (2009). La v0.2.0 amplía los métodos (P2, P3, MRER), las ecuaciones (Frayn, Jeukendrup-Wallis, Péronnet-Massicotte), el manejo de artefactos (RER cutoff, valores negativos) y la expresión de resultados (relativos a masa y masa libre de grasa, %HRmax, zona Fatmax, punto de cruce). El objetivo de este documento es que cualquier usuario del paquete o de la app entienda cada número que obtiene y pueda defenderlo en un artículo o en un informe clínico.

## 2. Fisiología: sustratos, intensidad y el concepto de Fatmax

El músculo obtiene ATP de dos grandes sustratos durante el ejercicio aeróbico: hidratos de carbono (glucógeno muscular y glucosa plasmática) y grasas (ácidos grasos plasmáticos procedentes del tejido adiposo y triglicéridos intramusculares). La mezcla depende, sobre todo, de la intensidad; en segundo lugar, de la duración, el estado nutricional, el entrenamiento, el sexo y la composición corporal.

**La curva de oxidación de grasas.** A intensidades bajas la grasa aporta la mayor parte de la energía. Al aumentar la intensidad, la oxidación de grasas en valor absoluto (g/min) sube, alcanza un máximo (MFO) y después cae hasta hacerse despreciable (Fatmin), mientras que la de hidratos crece de forma monótona. Fatmax es la intensidad en la que se da el MFO. En adultos sanos Fatmax suele situarse entre el 45 y el 65 % del VO₂max; en personas con obesidad o sedentarias la curva se desplaza hacia la izquierda y hacia abajo (Fatmax y MFO menores); en deportistas de resistencia, hacia la derecha y hacia arriba.

**Por qué cae la oxidación de grasas a alta intensidad.** Varios mecanismos actún a la vez: la mayor liberación de catecolaminas y el lactato reducen la lipólisis y la disponibilidad de ácidos grasos; el aumento de la glucogenólisis eleva el acetil-CoA y el malonil-CoA, que inhiben el transporte de ácidos grasos a la mitocondria vía CPT-1; y el reclutamiento de fibras tipo II, menos oxidativas, aumenta. El resultado es el "crossover" descrito por Brooks y Mercier (1994): a partir de cierta intensidad los hidratos pasan a ser el sustrato dominante. El paquete calcula ese punto de cruce como la intensidad en la que CHO supera a FAT.

**Tres magnitudes, tres usos.** MFO (g/min) informa de la *capacidad* oxidativa; Fatmax (%VO₂max, %HRmax, W o km/h) es el *parámetro de prescripción*; y la forma de la curva (cinética) describe *cómo* se distribuye esa capacidad a lo largo del rango de intensidades. Dos personas con el mismo MFO pueden tener curvas muy distintas (una estrecha y picuda, otra ancha y plana), y eso cambia el margen de error práctico al prescribir a Fatmax. De ahí el interés de la zona Fatmax (intensidades en las que la oxidación de grasas está dentro del 10 % del MFO; Achten et al., 2002) y del modelo SIN de Chenevière et al. (2009), que resume la curva en tres parámetros.

## 3. Calorimetría indirecta y ecuaciones estequiométricas

No se mide la grasa oxidada: se infiere del intercambio gaseoso. Oxidar un gramo de grasa consume más O2 y produce menos CO2 que oxidar un gramo de hidrato, de modo que el cociente VCO₂/VO2 (cociente de intercambio respiratorio, RER) informa de la mezcla de sustratos: RER cercano a 0,70 indica oxidación casi exclusiva de grasa; 1,00, casi exclusiva de hidratos. Esta inferencia es válida solo si el RER refleja el cociente respiratorio tisular, lo que exige estado estable (sin acumulación de CO2 por tamponamiento de lactato ni hiperventilación) e ignora, por convención, la contribución proteica durante el ejercicio.

**Ecuaciones de Frayn (1983).** Son las más usadas y el defecto del paquete. Con VO₂ y VCO₂ en L/min y n = nitrógeno urinario (g/min), que en ejercicio se asume 0:

```latex
\text{CHO}\ (\mathrm{g/min}) = 4.55\,\dot{V}\mathrm{CO}_2 - 3.21\,\dot{V}\mathrm{O}_2 - 2.87\,n
```

```latex
\text{FAT}\ (\mathrm{g/min}) = 1.67\,\dot{V}\mathrm{O}_2 - 1.67\,\dot{V}\mathrm{CO}_2 - 1.92\,n
```

Nótese que, con n = 0, FAT = 1,67 · VO₂ · (1 − RER): la oxidación de grasas es proporcional al VO₂ y a la distancia del RER a 1. Cuando RER > 1 (tamponamiento de lactato a alta intensidad) la fórmula devuelve valores negativos, que son un artefacto y no fisiología.

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

Por eso el paquete exige VO₂max cuando se elige "Jeukendrup": necesita saber en qué tramo de intensidad está cada escalón.

**Aproximación basada en RER ("MRER" en el paquete).** En lugar de dos coeficientes, usa el RER directamente:

```latex
\text{FAT}\ (\mathrm{mg/min}) = 1.7\,(1 - \mathrm{RER})\,\dot{V}\mathrm{O}_2\ (\mathrm{mL/min})
```

Es la ecuación de Péronnet y Massicotte (1,6946 VO₂ − 1,7012 VCO₂) simplificada a 1,7(1−RER)VO₂; así lo describe Chenevière et al. (2009, p. 1617) citando a Pérez-Martín et al. (2001), de modo que la atribución del código es correcta. Lo que distingue al MRER de Chenevière no es la ecuación sino el **procedimiento**: (1 − RER) y VO₂ se modelan como funciones lineales de la intensidad, su producto es una parábola, y Fatmax es el punto en que la derivada de esa parábola se anula. Su precisión depende de que RER sea lineal con la intensidad hasta RER = 1,0 (r = 0,72 en su muestra) y resultó inferior a SIN y P3. **La v0.2.0 no hace esto**: toma el escalón con mayor 1,7(1−RER)VO₂ observado, que es un método de valores medidos (MV) con otra ecuación. Corregido en la 0.2.1: MRER real y MV como método explícito (§6.1, §19).

**Energía.** Hasta la v0.2.0 el gasto se estimaba siempre como 4 kcal/g de CHO y 9 kcal/g de FAT. Desde la 0.2.1, `energy_equivalents = "classic"` (defecto en 0.2.x, con mensaje único por sesión: el defecto pasará a `"source"` en la 0.3.0) mantiene 4/9, y `"source"` usa los de la fuente: Péronnet y Massicotte 3,8683/9,7460 kcal/g (también para Frayn, que no da ninguno: decisión del paquete), Jeukendrup y Wallis 3,95 (< 50 % VO₂max) o 4,07 (≥ 50 %) y 9,75 kcal/g; en reposo con Jeukendrup, los de Péronnet (par de glucosa pura). En el ejemplo (Frayn) `Kcal` cambia entre −1,9 % y +5,3 % por escalón; MFO y Fatmax no cambian (§19, hallazgo 1). Para comparar estudios conviene reportar siempre la ecuación usada: las diferencias entre Frayn y Péronnet son del orden del 1–2 % en FAT, pero crecen en CHO y en el punto de cruce.

**Qué diferencia hay entre las tres y cuál elegir.** Frayn es el estándar histórico y el recomendado por las revisiones recientes para MFO; Jeukendrup-Wallis es teóricamente más correcta en ejercicio y es la referencia en fisiología del rendimiento; Péronnet se usa mucho en la escuela francesa (LIPOXmax). En la app se ofrecerán las tres con Frayn por defecto y una nota que recuerde que el artículo debe citar la elegida.

## 4. El test de MFO: condiciones, protocolo y criterios de finalización

El paquete espera tres registros por participante, que son las tres fases del día de evaluación: (1) metabolismo en reposo, (2) test incremental de MFO y (3) test incremental máximo (o un VO₂max ya conocido). Cada decisión de protocolo afecta al resultado, por lo que debe reportarse.

**Condiciones previas.** Ayuno de 8–12 h (la ingesta de hidratos eleva el RER y deprime la oxidación de grasas), sin ejercicio intenso las 24–48 h previas, sin cafeína ni alcohol, y a la misma hora del día en las repeticiones. En mujeres premenopáusicas conviene registrar la fase del ciclo. El analizador de gases debe calibrarse (volumen y gases de referencia) antes de cada sesión; el error del analizador es, según la revisión de Chávez-Guevara et al. (2025), una fuente mayor de variabilidad día a día de lo que se suele reconocer.

**Fase 1: reposo.** 15–30 min sentado o tumbado con registro continuo. De ese registro se extrae la ventana más estable (habitualmente 5 min) para obtener VO₂, RER y oxidación de sustratos basales. Sirve de punto de partida de la curva (intensidad ≈ 0) y para comprobar que el participante llegó en condiciones (RER basal 0,75–0,85 es compatible con ayuno; > 0,90 sugiere ingesta reciente o hiperventilación).

**Fase 2: test incremental de MFO.** Escalones de carga de 3–6 min (3 min es lo más frecuente; en obesidad y poblaciones con MFO bajo se recomiendan escalones más largos y/o incrementos menores) hasta que el RER alcanza el criterio de parada. Modalidades: cicloergómetro (incrementos de 15–35 W) o cinta (incrementos de velocidad o pendiente). El protocolo de Achten et al. (2002) usaba 35 W / 3 min; los protocolos con cargas relativas a la masa corporal (p. ej., 0,15 W/kg cada 4 min desde 0,45 W/kg) se han propuesto en 2026 para homogeneizar el estímulo en poblaciones con MFO bajo.

**Criterio de finalización.** El clásico es RER = 1,00, porque a partir de ahí la ecuación devuelve FAT ≤ 0. Amaro-Gahete et al. (2019) mostraron que el MFO ya se ha alcanzado mucho antes: proponen terminar en RER = 0,93 en sedentarios y 0,90 en entrenados, lo que permite escalones más largos o incrementos más finos sin alargar el test. Por eso el paquete incorpora `RER_cutoff`.

**Fase 3: VO₂max.** Tras un descanso (o en otro día), un test incremental máximo del que se toma el VO₂max para expresar Fatmax como %VO₂max. Si se dispone de un VO₂max previo puede pasarse directamente. Un detalle importante: el VO₂max debe ser el promedio de los últimos 15–30 s (o la mayor media móvil de 30 s), no el valor instantáneo más alto; en §9 se señala cómo lo trata el código actual.

**Protocolos alternativos.** Los tests en rampa (incremento continuo) acortan la evaluación y, en mujeres posmenopáusicas, han dado MFO y Fatmax equivalentes a los escalonados; a cambio no hay estado estable y el promediado se hace por ventanas temporales. Los tests de un solo escalón prolongado a Fatmax sirven para validar que la intensidad calculada se sostiene, ya que el test incremental tiende a sobreestimar la oxidación de grasas respecto a un esfuerzo continuo a esa misma intensidad.

| Decisión de protocolo | Práctica recomendada (2025–2026) | Efecto si se incumple |
| --- | --- | --- |
| Ayuno | 8–12 h, misma hora del día | RER basal alto, MFO infraestimado |
| Duración de escalón | 3 min (sanos), 4–6 min (obesidad, baja forma) | Sin estado estable, RER inestable |
| Promediado | Últimos 60–120 s de cada escalón | Ruido respiratorio desplaza el máximo |
| Ecuación | Frayn por defecto; reportar la elegida | Diferencias del 1–3 % en MFO |
| Parada | RER 1,00 (clásico) o 0,90–0,93 (Amaro-Gahete 2019) | Escalones con FAT negativa |
| VO₂max | Media de 30 s en test máximo | Fatmax en %VO₂max sesgado |

## 5. Procesamiento de la señal

Entre los gases crudos y la curva de oxidación de grasas hay cuatro pasos de procesamiento, y cada uno es una fuente de variabilidad entre laboratorios.

**5.1 Resolución temporal.** Los analizadores exportan respiración a respiración (breath-by-breath) o promediados a intervalos fijos (10, 15, 20, 30 s). Los datos breath-by-breath tienen un ruido elevado y deben promediarse antes de cualquier cálculo; el paquete los agrupa en intervalos fijos (`preprocess_bxb`, por defecto 20 s) y después trabaja siempre con el parámetro `step_time` = segundos entre registros.

**5.2 Ventana basal estable.** Del registro de reposo se descartan los primeros 5 min (estabilización) y, en lo que queda, se busca la ventana de 5 min con menor coeficiente de variación (CV = desviación típica / media) de la variable elegida (`cv_var`: VO₂, VCO₂ o RER). Esa ventana define el estado basal. Elegir RER como criterio favorece ventanas con mezcla de sustratos estable; elegir VO₂ favorece ventanas con ventilación estable. Ambas son defendibles; lo importante es reportarlo.

**5.3 Promediado por escalones (estado estable).** De cada escalón de `step_duration` segundos se promedian solo los últimos `steady_state_duration` segundos (por defecto, el último minuto de escalones de 3 min). La razón fisiológica es que el VO₂ tarda 1–2 min en alcanzar una meseta tras cada incremento de carga; promediar el escalón entero mezclaría la transición con la meseta. La recomendación más reciente para obesidad es promediar los últimos 2 min de escalones de ≥ 4 min. El paquete asume que el registro del test empieza exactamente al inicio del primer escalón y que todos los escalones tienen la misma duración; si no es así, el usuario debe recortar el archivo (`remove_rows`).

**5.4 Artefactos de alta intensidad.** Cuando RER > 1, las ecuaciones devuelven FAT < 0. Hay tres políticas (`negative_FAT`): *truncar* a 0 (la más habitual: la grasa no puede oxidarse negativamente), *excluir* el escalón (evita que puntos artefactuales deformen el polinomio) o *mantener* (solo con fines de inspección). Además, `RER_cutoff` excluye los escalones con RER por encima del umbral antes de ajustar nada, lo que implementa el criterio de Amaro-Gahete (2019) a posteriori cuando el test se prolongó hasta RER = 1,00.

**5.5 Eje de intensidad.** La curva se construye en %VO₂max (VO₂ del escalón / VO₂max × 100). Es la escala que permite comparar entre personas y modalidades, pero obliga a disponer de un VO₂max fiable. Para prescribir, el usuario necesita además la equivalencia en frecuencia cardiaca (interpolada en el paquete) y en carga externa (W o km/h), que el paquete aún no devuelve y que se añadirá en la v0.3.0.

Flujo resumido:

1. Importar reposo, test MFO y test máximo; estandarizar nombres de columnas (`VO2`, `VCO2`, `RER`, `HR`).
2. Si es breath-by-breath, promediar a intervalos fijos.
3. Ventana basal más estable → valores de reposo.
4. Por escalón: media del tramo estable → VO₂, VCO₂, RER, HR → CHO y FAT con la ecuación elegida.
5. Aplicar `RER_cutoff` y la política de negativos.
6. Expresar cada escalón en %VO₂max. El punto basal se añade como escalón 0 solo si `include_basal = TRUE` (Montes de Oca et al., 2021); por defecto no entra en el ajuste (§9.5).
7. Ajustar la curva y localizar MFO y Fatmax (§6).

## 6. Determinación de MFO y Fatmax: P3, P2, MRER y modelo SIN

Una vez se tiene un punto (FAT, %VO₂max) por escalón, hay que decidir dónde está el máximo. La elección no es trivial: con 6–10 escalones, el escalón con el valor más alto puede estar a 5–10 puntos porcentuales del verdadero máximo, y el ruido de un solo escalón puede desplazarlo. Los métodos se agrupan en dos familias: los que toman el valor observado (MV, "measured values") y los que ajustan una curva y localizan su máximo analítico.

**6.1 Valor observado (MV) y método RER (MRER).** MV: Fatmax es el %VO₂max del escalón con mayor FAT. Es transparente y no asume forma de curva, pero su resolución es la del protocolo (un escalón) y es sensible al ruido. MRER (Chenevière 2009, siguiendo a Pérez-Martín 2001): se ajustan dos rectas, (1 − RER) frente a intensidad y VO₂ frente a intensidad; FAT = 1,7(1−RER)VO₂ es entonces una parábola cuyo vértice (derivada nula) da Fatmax y MFO. Es el menos preciso de los cuatro en la comparación de Chenevière. **En la v0.2.0 el método llamado `MRER` es en realidad MV sobre 1,7(1−RER)VO₂**, con un P3 solo para dibujar. **Corregido en la 0.2.1:** `method = "MV"` toma el escalón con mayor FAT observado (ecuación de `author`), sin R² ni RMSE y con la zona Fatmax por interpolación lineal entre escalones (`FatMax_zone_method`); `method = "MRER"` ajusta las dos rectas y toma el vértice x = −(a₁b₂ + a₂b₁)/(2b₁b₂) de 1,7(1−RER)VO₂, sea cual sea `author`, y cae a MV con aviso si el vértice no es un máximo dentro del rango medido. En el ejemplo (Frayn): MV 41,0 % VO₂max / 0,327 g/min; MRER 35,8 % VO₂max / 0,327 g/min (la v0.2.0 daba 41,0 / 0,339 con `"MRER"`).

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

**Bondad de ajuste.** Para P2 y P3 el paquete devuelve R² (1 − SSres/SStot) y RMSE (raíz del error cuadrático medio de los residuos). Como orientación, un R² < 0,80 o un RMSE del orden del 10–15 % del MFO sugieren que la curva no describe bien los datos y que Fatmax es poco fiable. **Estos umbrales no tienen fuente verificada (§19): son orientación, no regla del paquete**; ni `MFO()` ni la app avisan por ellos hasta que haya fuente (decisión de Jorge, 20 nov 2026; #49, 0.3.x).

**6.4 Zona Fatmax y punto de cruce.** Sobre la curva ajustada se calcula la zona Fatmax como el intervalo de intensidades en que FAT ≥ 0,90 · MFO (Achten et al., 2002): su anchura informa de cuánto margen hay al prescribir. El punto de cruce (crossover) es la intensidad a la que CHO supera a FAT; el paquete lo obtiene por interpolación lineal entre los dos escalones en que cambia el signo de FAT − CHO.

**6.5 Modelo SIN (Chenevière et al., 2009).** Describe toda la curva, normalizada a %MFO, con una función seno de tres parámetros:

```latex
\%\text{MFO}(x) = \sin\!\left\{\left[\frac{\pi^{1/s}}{\pi + 2d}\left(K\,x + d + t\right)\right]^{s}\right\}, \qquad K = \frac{\pi}{100}, \quad x = \%\dot{V}\mathrm{O}_{2\max}
```

Ecuación \[14\] de Chenevière et al. (2009), p. 1618, verificada sobre el texto completo el 9 de noviembre de 2026. Atención al denominador: es π^(1/s) dividido por (π + 2d), no (π^(1/s)/π) + 2d. El código de la v0.2.0 implementa la segunda forma (véase §9.5 y §19, hallazgo 5). Puntos característicos derivados de \[14\] (no impresos en el artículo): primer cero en x₁ = −(d + t)/K; Fatmax en x = \[(π + 2d)·2^(−1/s) − d − t\]/K; Fatmin (segundo cero) en x₂ = (π + d − t)/K. Con d = t = 0 y s = 1 dan 0, 50 y 100 % VO₂max.

Con d = 0, s = 1, t = 0 la función es un semiciclo de seno "básico" que vale 0 en x = 0, 1 en x = 50 y 0 en x = 100. Cada parámetro deforma esa referencia:

| Parámetro | Nombre | Qué hace sobre la curva | Interpretación fisiológica |
| --- | --- | --- | --- |
| d | Dilatación | Ensancha (d > 0) o estrecha (d < 0) la curva | Amplitud del rango de intensidades con alta oxidación de grasa |
| s | Simetría | s > 1 desplaza el pico a la derecha y hace la caída brusca; s < 1 al revés | Forma de la caída: cuánto se mantiene la grasa por encima de Fatmax |
| t | Traslación | Desplaza la curva entera a la izquierda (t > 0) o a la derecha (t < 0) | Dónde está Fatmax en el continuo de intensidad |

El ajuste se hace por mínimos cuadrados no lineales (Levenberg-Marquardt, `minpack.lm::nlsLM`). **Cómo lo hace Chenevière (2009):** promedia el último minuto de cada escalón con RER ≤ 1,0, expresa la oxidación de grasa como energía derivada de lípidos (E\_lipid) y determina d, s y t minimizando la suma de cuadrados entre el modelo y los valores medidos, **sin suavizado previo**; el P3 aparece solo como método comparador. Fatmin es el segundo cero de la curva (fat = 0 % MFO), sin umbral. El artículo no dice explícitamente si el 100 % MFO es el máximo observado o el del ajuste; por construcción del modelo (pico = 1) y por su figura 2, los valores medidos se normalizaron al máximo observado. **Cómo lo hace la v0.2.0:** normaliza al máximo observado, suaviza con un P3, genera una malla fina de 0 al último escalón, renormaliza y ajusta el SIN a esa malla; Fatmin se toma como la primera intensidad con %MFO < 5 % por encima de Fatmax; y la fórmula tiene el paréntesis del denominador mal colocado. Las cuatro desviaciones están **corregidas en la 0.2.1** (§19): ajuste directo a los escalones observados normalizados al máximo observado (el ajuste en g/min con MFO como parámetro de escala queda para §13.2), sin malla sintética, Fatmax y Fatmin por fórmula cerrada, Fatmin en el segundo cero (con `fatmin_threshold` opcional como fracción del MFO) y fórmula \[14\] exacta. `MFO_kinetics()` ajusta todas las filas que recibe: `RER_cutoff = 1` reproduce la selección de Chenevière (escalones con RER ≤ 1,0) e `include_basal = TRUE` se aparta de ella también en el SIN. En el ejemplo el SIN depende del último escalón (RER 1,03): con `negative_FAT = "truncate"` d = −0,385, s = 0,910, t = 0,355, Fatmax 36,2, Fatmin 76,4 % VO₂max; con `"exclude"` o `RER_cutoff = 1`, d = −0,440, s = 0,539, t = 0,023, Fatmax 33,2, Fatmin 85,3 % VO₂max. Si los parámetros no se mueven de los valores iniciales o R² < 0, el ajuste se declara no convergente; ocurre con menos de 6 escalones o cuando la oxidación de grasas solo decrece. Con menos de 4 escalones (3 parámetros) no se ajusta (`convergence = FALSE`, aviso); con 4–5 se ajusta pero se avisa de que el ajuste es poco fiable y se recomiendan ≥ 6 escalones.

**6.6 Comparación entre métodos.** Amaro-Gahete et al. (2019) y la meta-regresión de Chávez-Guevara et al. (2023) coinciden en que MV, P3 y SIN dan MFO similares a nivel de grupo, pero Fatmax puede diferir varios puntos porcentuales en un individuo; el SIN es el que mejor describe la forma, el P3 el mejor compromiso entre robustez y resolución. La recomendación para la app: P3 como método principal, SIN como descripción de la cinética, y mostrar siempre el valor observado al lado para que el usuario vea cuánto aporta el ajuste.

## 7. Expresión y normalización de resultados

Un mismo test produce varias cifras y cada una responde a una pregunta distinta. Reportar solo "MFO = 0,45 g/min" es insuficiente.

| Magnitud | Unidad | Cómo la calcula el paquete | Para qué sirve |
| --- | --- | --- | --- |
| MFO absoluto | g/min | Máximo de la curva ajustada (o observado) | Capacidad oxidativa total; comparar con la literatura |
| MFO relativo | mg/kg/min | MFO × 1000 / masa corporal | Comparar personas de distinto tamaño |
| MFO por masa libre de grasa | mg/kgFFM/min | MFO × 1000 / FFM | Capacidad oxidativa del tejido metabólicamente activo; preferible en obesidad |
| Fatmax | %VO₂max | Intensidad del máximo de la curva | Referencia fisiológica estándar |
| HR a Fatmax | lpm | Interpolación lineal HR vs %VO₂max en Fatmax | Prescripción con pulsometro |
| Fatmax | %HRmax | HR a Fatmax / HRmax × 100 | Prescripción sin analizador; referencia recomendada en obesidad |
| Zona Fatmax | %VO₂max (inferior, superior) | Intensidades con FAT ≥ 90 % del MFO | Margen de prescripción |
| Punto de cruce | %VO₂max | Donde CHO supera a FAT | Marcador de dependencia de hidratos |
| Fatmin | %VO₂max | Segundo cero del SIN, (π + d − t)/K (desde la 0.2.1); `fatmin_threshold` opcional para un umbral en fracción del MFO | Límite superior de uso relevante de grasa |
| d, s, t | adimensional | Parámetros del SIN | Forma de la curva; comparación pre-post |
| Basal: VO₂, RER, CHO, FAT, kcal | mL/min, –, g/min, kcal/min | Ventana estable de reposo (Frayn) | Control de calidad del ayuno; punto 0 de la curva |

**Valores de referencia orientativos.** En adultos sanos activos MFO suele estar entre 0,3 y 0,6 g/min (hasta > 1 g/min en deportistas de resistencia de élite), con Fatmax entre el 45 y el 65 % del VO₂max. En obesidad el MFO absoluto puede ser similar o mayor (más masa), pero el relativo a FFM es menor y Fatmax se desplaza a la izquierda. La meta-regresión de Chávez-Guevara et al. (2023) recomienda expresar Fatmax en %HRpeak en pacientes con obesidad: 61–66 % HRpeak con > 35 % de grasa corporal y 57–64 % con < 35 %. Las referencias por sexo y edad en población general proceden de cohortes como FIT-AGEING y las muestras de Maunder et al. (2018) para deportistas; en la v0.3.0 se incorporarán como percentiles consultables, siempre con la advertencia de que dependen del protocolo con el que se obtuvieron.

**Qué reportar en un artículo o informe.** MFO absoluto y relativo a FFM; Fatmax en %VO₂max y en %HRmax (o W/km/h); método de localización y ecuación; R² del ajuste; número de escalones usados y criterio de parada; y, si procede, los parámetros del SIN. La app generará un párrafo de métodos con todo ello.

## 8. Fiabilidad, error de medida e incertidumbre

MFO y Fatmax tienen una variabilidad día a día considerable, y confundir esa variabilidad con un cambio real es el error más frecuente al usarlos para evaluar intervenciones. Esta sección fija los conceptos que la v0.3.0 implementará.

**Lo que dice la evidencia (2025).** La revisión de Chávez-Guevara et al. (Int J Obes, 2025) sintetiza 11 estudios de reproducibilidad: el coeficiente de variación test-retest de MFO se sitúa típicamente entre el 8 y el 20 % y el de Fatmax entre el 6 y el 16 %, con ICC moderados-altos (0,6–0,9) en sanos y algo peores en obesidad. Buena parte de esa variación es atribuible al error del analizador de gases al calcular sustratos cerca de Fatmax, donde la diferencia VO₂ − VCO₂ es pequeña y cualquier error en VCO₂ se amplifica. Conclusión práctica: un cambio individual de MFO menor del \~20 % no puede distinguirse del ruido con un solo test antes y otro después.

**Tres estadísticos que hay que conocer.** Con dos tests (T1, T2) en n personas:

```latex
\text{ICC}_{(2,1)} = \frac{\sigma^2_{\text{entre}}}{\sigma^2_{\text{entre}} + \sigma^2_{\text{error}}}, \qquad \text{SEM} = \text{SD}_{\text{pooled}}\,\sqrt{1 - \text{ICC}} = \frac{\text{SD}_{\text{diff}}}{\sqrt{2}}
```

```latex
\text{MDC}_{95} = 1.96\,\sqrt{2}\,\text{SEM}, \qquad \text{CV}\,(\%) = \frac{\text{SEM}}{\bar{x}} \times 100
```

El ICC indica cuánto de la variabilidad total es diferencia real entre personas (fiabilidad relativa, útil para clasificar). El SEM es el error típico de una medida en sus unidades (fiabilidad absoluta). El MDC95 es el cambio mínimo que, con un 95 % de confianza, excede el error: es el umbral que debe superar un cambio pre-post individual para considerarse real. Un laboratorio debería calcular sus propios SEM y MDC con su analizador y su protocolo, porque los publicados no son trasladables.

**Incertidumbre del propio ajuste.** Independientemente del error biológico y del analizador, el P3 y el SIN se ajustan a 6–10 puntos ruidosos, y la posición del máximo tiene incertidumbre estadística propia. Hay dos formas de cuantificarla: (a) bootstrap de residuos (reajustar la curva cientos de veces con residuos remuestreados y tomar los percentiles 2,5 y 97,5 de MFO y Fatmax); (b) un ajuste bayesiano del SIN o del P3 (p. ej., con `brms`), que devuelve distribuciones posteriores de MFO, Fatmax, d, s y t y permite incorporar priors informativos sobre la forma de la curva, muy útil cuando hay pocos escalones y el ajuste clásico no converge. Un intervalo creíble ancho para Fatmax es, en sí mismo, un resultado: dice que la curva es plana y que la prescripción tiene margen.

**Lo que hará el paquete.** (1) `MFO_reliability()`: a partir de dos o más tests por persona, ICC(2,1) con IC 95 %, SEM, MDC95, CV y gráfico de Bland-Altman. (2) `MFO(..., ci = "parametric")` (recomendado; también `"bootstrap"`, por defecto `"none"`): IC 95 % para MFO, Fatmax y la zona Fatmax. (3) `MFO_bayes()`: versión bayesiana del P3/SIN con intervalos creíbles. (4) En la app, el informe individual mostrará siempre el intervalo junto al valor puntual y, si hay un test previo, comparará el cambio con el MDC del laboratorio.

## 9. El paquete MFO (v0.2.0): arquitectura, flujo y correspondencia función-ecuación

El paquete tiene tres funciones exportadas y seis auxiliares internas, en ocho ficheros de R (\~1.900 líneas con tests). Depende de `dplyr`, `ggplot2`, `tibble`, `minpack.lm`, `openxlsx`, `readxl` y `tidyr`, y exige R ≥ 4.1. La arquitectura es lineal: importar → procesar → ajustar → expresar → graficar, y `MFOs()` repite ese flujo sobre una carpeta de participantes.

**9.1 Funciones exportadas**

| Función | Entrada | Salida | Sección de este documento |
| --- | --- | --- | --- |
| `MFO()` | Tres data frames (basal, test MFO, test máximo) o VO₂max; parámetros de protocolo | Lista con MFO\_db (tabla por escalón), gráfico, MFO, Fatmax, HR, zona, cruce, R², RMSE, basales | §3–§7 |
| `MFO_kinetics()` | `MFO_db` devuelto por `MFO()` | d, s, t con sus errores típicos, Fatmax\_SIN, Fatmin, R², RMSE, convergencia, gráfico SIN básico vs ajustado | §6.5 |
| `MFOs()` | Carpeta con una subcarpeta de tres xlsx por participante (`"folder"`) o un xlsx de tres hojas por participante (`"files"`) | Tabla resumen (una fila por participante; desde la 0.2.1 incluye `VO2max`, `VO2max_window`, `FatMax_zone_method` y `energy_equivalents`) devuelta en R; el xlsx/csv/rds y la carpeta de gráficos solo se escriben en `output_dir` si el usuario lo da (desde la 0.3.0; hasta la 0.2.1 se escribían en el directorio de trabajo por defecto, contra la política de CRAN; sin `output_dir`, un mensaje único por sesión lo explica); cada aviso lleva delante el ID del participante | todo |

**9.2 Auxiliares internas y la ecuación que implementan**

| Auxiliar | Qué hace | Ecuación / regla |
| --- | --- | --- |
| `read_MFO_databases()` | Lee xlsx, renombra columnas a `VO2`, `VCO2`, `RER`, `HR`, convierte a numérico, elimina filas con NA | §5.1 |
| `preprocess_bxb()` | Agrupa breath-by-breath en intervalos fijos (media por bin) | §5.1 |
| `met_basal()` | Descarta 5 min, busca la ventana de 5 min con menor CV de `cv_var`, calcula basales con la ecuación de `author` (Frayn hasta la v0.2.0) | §5.2, §3 |
| `substrate_oxidation()` | CHO, FAT y kcal a partir de VO₂ y VCO₂ según `author` | Frayn / Jeukendrup-Wallis / Péronnet (§3) |
| `fat_oxidation_MRER()` | FAT = 1,7(1−RER)VO₂/1000 | §3 (MRER) |
| `calculate_vars()` | Por escalón: media del tramo estable → sustratos; aplica `RER_cutoff` | §5.3–5.4 |
| `.fit_P3()`, `.fit_P2()`, `.fit_MRER()` | Ajuste y localización del máximo; R² y RMSE; funciones de predicción FAT y CHO | §6.1–6.3 |
| `.calculate_fatmax_zone()` | Malla de 1000 puntos; intervalo donde FAT ≥ 0,9 MFO | §6.4 |
| `.calculate_crossover()` | Interpolación lineal en el primer cambio de signo de FAT − CHO | §6.4 |
| `.create_MFO_plot()` | ggplot con curvas FAT/CHO, puntos, zona sombreada, líneas de Fatmax y cruce | §7 |

**9.3 Flujo interno de `MFO()`, paso a paso**

1. Valida `method` (P3/P2/MRER) y `negative_FAT` (truncate/exclude/keep).
2. VO₂max: si se pasa `VO2max`, se usa tal cual (y `db_graded` se ignora con aviso). Si no, la mayor media móvil de `vo2max_window` s (30 por defecto) en `db_graded`, redondeando hacia arriba al número de registros (30 s con registros de 20 s = 2 registros = 40 s). `MFO()` devuelve `VO2max`, `VO2max_window` (efectiva) y `VO2max_window_requested`. Hasta la v0.2.0 tomaba `max(VO2)` (corregido en 0.2.1, §9.5).
3. `met_basal()` → valores de reposo.
4. `calculate_vars()` → una fila por escalón con VO₂, VCO₂, HR, RER, CHO, FAT, kcal.
5. Une la fila basal (escalón 0, con `HR = NA`) con los escalones y calcula `porc_VO2` = VO₂/VO2max × 100. Desde la 0.2.1 la fila basal se conserva o se descarta de forma explícita según `include_basal` (por defecto `FALSE`, idéntico a la v0.2.0); hasta la v0.2.0 la eliminaba `na.omit()` porque su HR es NA (§9.5).
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

- **VO₂max = valor instantáneo máximo (corregido en 0.2.1).** `max(db_graded$VO2)` tomaba el registro más alto, no una media de 30 s. Con datos a 20 s sobreestima el VO₂max y desplaza Fatmax hacia la izquierda. Corregido con `vo2max_window` (30 s por defecto; si no es múltiplo de `step_time` se redondea hacia arriba el número de registros y se informa de la ventana efectiva). Si se pasa `VO2max`, manda sobre `db_graded` (decisiones de Jorge, 10 nov 2026). En los datos de ejemplo: VO₂max 3442 → 3315 mL/min, Fatmax 34,8 → 36,2 % VO₂max, MFO sin cambios.
- **Los basales siempre se calculaban con Frayn (corregido en 0.2.1)**, aunque el usuario eligiera otra ecuación para el test. Ahora `met_basal()` recibe `author` y `VO2max` de `MFO()`. Jeukendrup y Wallis (2005, tablas 4 y 5) no proponen ecuación de reposo; con `author = "Jeukendrup"` el basal usa el par 100 % glucosa derivado de las constantes de su tabla 4 (CHO = 4,585 VCO₂ − 3,226 VO₂; FAT = 1,695 VO₂ − 1,701 VCO₂). Es una decisión del paquete, no de la fuente (Jorge, 10 nov 2026; sustituye a la extrapolación del tramo 40–50 % del primer borrador). El aviso del punto 6 del backlog no debe saltar para el basal. No cambia MFO ni Fatmax mientras el basal no entre en el ajuste.
- **Punto basal como escalón 0 (corregido 10 nov 2026).** Versiones anteriores de este documento decían que la fila de reposo entra en el ajuste polinómico. **No es así**: `MFO()` construye la fila basal con `HR = NA` y `na.omit(MFO_db)` la elimina antes de la política de negativos y del ajuste, así que la v0.1.0/v0.2.0 equivale de hecho a `include_basal = FALSE`, y la fila basal tampoco aparece en el `MFO_db` devuelto (los basales solo se ven en `x_CHO`, `x_FAT`, `x_Kcal`). Incluir el reposo como escalón 0 es la práctica de Montes de Oca et al. (2021), pero no todos los autores la siguen. Decisión (Jorge, 10 nov 2026): nuevo argumento `include_basal = FALSE` por defecto (conserva los resultados publicados con la v0.2.0); `TRUE` documentado como opción, porque cambia el P3. Si se incluye, la exclusión del basal no debe depender de que falte HR. **Implementado en 0.2.1** (`MFO()`, `MFOs()`); en el ejemplo (Frayn, P3) `TRUE` mueve MFO 0,3167 → 0,3225 g/min y Fatmax 36,17 → 36,11 % VO₂max. Con `TRUE`, `MFO_kinetics()` también ajusta el basal (decisión de Jorge, 10 nov 2026), lo que se aparta de Chenevière.
- **Columna `Load` sintética.** La columna `Load` de `MFO_db` no es la carga medida: se fabrica como `seq_len(n_steps) * step_duration/60 * 5` (5, 10, 15… para escalones de 1 min; 15, 30, 45… para 3 min) y vale `"basal"` en la fila de reposo, lo que además convierte la columna en texto. No debe interpretarse como W ni km/h. Hasta que exista la columna `Load` real (§14) se documentará como índice de escalón o se renombrará; no se usa en ningún cálculo.
- **MRER: atribución y naturaleza.** Es el método "valor observado" con un coeficiente redondeado. Renombrarlo a `MV` (measured values) y corregir la cita, o eliminarlo. **Corregido en 0.2.1:** `"MV"` es el máximo observado y `"MRER"` el vértice de las dos rectas.
- **Jeukendrup: umbral del 50 %** aplicado al VO₂ del escalón; las ecuaciones originales cubren 40–75 % VO₂max. Fuera de ese rango el paquete extrapola. **Corregido en 0.2.1:** un aviso único lista los escalones de ejercicio fuera de rango (tras `RER_cutoff`); el basal nunca avisa.
- **Cutoff de RER en `calculate_vars` y política de negativos en `MFO()`** actúan sobre la misma situación en dos sitios; la lógica se entiende, pero conviene centralizarla y describirla en una vignette.
- **HR ausente (corregido en 0.2.1).** Hasta la v0.2.0, sin columna HR `mean(NULL)` devolvía NA y `na.omit()` borraba todos los escalones, así que `MFO()` fallaba (`0 (non-NA) cases`) y `MFOs()` dejaba una fila vacía sin explicación; un escalón sin HR salía del ajuste en silencio, y `read_MFO_databases()` borraba cada fila con HR vacía, desplazando los escalones siguientes. Regla (Jorge, 10 nov 2026): **la HR nunca decide qué filas o escalones se descartan**; solo VO₂, VCO₂ y RER. Si falta HR, `warning()` (no `message()`, para que se vea también en lote; `MFOs()` antepone el ID del participante a cada aviso): sin HR, `FAT_MAX_HR` y `FAT_MAX_porcHRmax` son NA; con escalones sin HR, se listan por %VO₂max y la HR en Fatmax se interpola con los demás. MFO y Fatmax no cambian.
- **Escalones incompletos (corregido en 0.2.1).** `calculate_vars` descartaba silenciosamente las filas sobrantes del último escalón incompleto (`n_steps = floor(nrow/records_per_step)`). Ahora avisa siempre con la duración ("X s de Y s"). Nuevo argumento `use_incomplete_step = FALSE` (por defecto, resultados de la v0.2.0); con `TRUE` el escalón se usa solo si dura al menos `steady_state_duration`, y se promedian sus últimos `steady_state_duration` s (Jorge, 10 nov 2026). En el ejemplo (72 registros = 8 escalones completos) no salta.
- **SIN: la normalización usa el máximo observado, no el del P3.** Si el máximo observado está en un escalón ruidoso, toda la curva se escala con él. Documentar; valorar normalizar con el MFO del ajuste.
- **Fatmin con umbral fijo del 5 %** y sin IC. **Corregido en 0.2.1:** segundo cero por defecto; `fatmin_threshold` (fracción del MFO, `NULL` por defecto) como opción. El IC queda para §13.
- **Sin salida en unidades de carga externa** (W, km/h): el usuario no puede prescribir directamente. Añadir una columna opcional `Load` en el test y devolver Fatmax en esa unidad por interpolación.
- **Infraestructura.** Un solo commit en el historial, badge de Travis obsoleto, README de la 0.1.0, NEWS vacío, sin vignettes ni pkgdown ni GitHub Actions. Son requisitos prácticos para CRAN y explícitos para JOSS.

**Añadidos tras la verificación con textos completos (9 nov 2026; detalle en §19):**

- **Error en la fórmula del SIN**: el código calcula (π^(1/s)/π) + 2d donde Chenevière escribe π^(1/s)/(π + 2d). Corregir y añadir test de regresión con los parámetros de su figura 5. *(Corregido en 0.2.1.)*
- **Suavizado y malla sintética antes del SIN**: no están en la fuente e inflan n. Ajustar a los escalones observados. *(Corregido en 0.2.1.)*
- **MRER es en realidad MV**: implementar el procedimiento de las dos rectas y añadir `method = "MV"`. *(Corregido en 0.2.1.)*
- **Fatmin**: segundo cero del SIN, no umbral del 5 %. *(Corregido en 0.2.1.)*
- **Equivalentes energéticos**: 4/9 kcal/g no proceden de ninguna de las tres fuentes; hacerlos dependientes de `author`. *(Corregido en 0.2.1: `energy_equivalents`; `"source"` será el defecto en la 0.3.0.)*
- **Coeficientes de Péronnet (corregido en 0.2.1)**: derivados en código desde su tabla 1 (4,58519/3,22597; 1,69469/1,70130) en lugar de constantes redondeadas.

**9.6 Formato de datos de entrada (contrato para la app).** Cada tabla debe tener, como mínimo, las columnas `VO2` (mL/min), `VCO2` (mL/min), `RER` y `HR` (lpm), a intervalos regulares de `step_time` segundos y sin cabeceras intermedias. Los nombres se pueden mapear con `col_name_*`. La app leerá directamente exportaciones de los analizadores más comunes (Cosmed, Cortex, Vyntus, Parvo) y detectará unidades (mL/min vs L/min) antes de llamar a `MFO()`.

## 10. Hoja de ruta científica v0.3.0 y app Shiny

Cada punto de la hoja de ruta responde a una limitación identificada en §4–§9 o a una recomendación de la literatura 2023–2026. El orden es el de implementación.

| Fase | Qué se añade | Por qué (origen científico) |
| --- | --- | --- |
| 1. Saneamiento | Correcciones de §9.5; GitHub Actions (R CMD check en Windows/macOS/Linux); pkgdown; README y NEWS; vignette "De los gases al Fatmax"; envío a CRAN | Requisitos CRAN y JOSS; reproducibilidad |
| 2. VO₂max robusto | Media móvil configurable; VO₂peak vs VO₂max; aviso si el test máximo no cumple criterios | §4 fase 3 |
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

**Qué verá el usuario de la app** (decisiones de Jorge, 19 nov 2026; #24):

1. **Datos.** Subir varios ficheros o un zip (el navegador no permite subir carpetas) y elegir la fuente: csv/xlsx genérico con mapeo de columnas (incluye el formato xlsx de `MFOs()`) o export de un analizador leído por `spiro`; también, datos de ejemplo del paquete. Las fases se asignan con un fichero u hoja por fase, o con cortes sobre una única serie: se marcan arrastrando sobre la gráfica de VO₂ frente al tiempo y se ajustan con campos numéricos. Los cortes usados se guardan en la descarga.
2. **Protocolo y análisis.** Duración de escalón, tramo estable, ecuación, método, cutoff RER, política de negativos, VO₂max.
3. **Resultados.** Curva con MFO, Fatmax, zona, cruce y su IC (bootstrap bajo demanda, con barra de progreso); cinética SIN; alertas de calidad (los avisos y mensajes de `MFO()` se muestran en pantalla). El modelo bayesiano queda fuera de la app.
4. **Informe y tabla de grupo.** Un participante cada vez; la tabla de grupo se acumula durante la sesión y se descarga en xlsx/csv. Informe individual en HTML (PDF desde el navegador) con: curva y tabla de resultados, IC si se calcularon, cinética SIN, zonas de FC (`MFO_zones()`), percentil con su cohorte y `reference_differences`, alertas de calidad, párrafo de métodos, versión del paquete y commit, fecha del análisis y cortes de fase usados. La plantilla del párrafo de métodos la aprobó Jorge el 20 nov 2026 (`R/app_methods.R`): solo en inglés; abre con un corchete que completa el autor (horas de ayuno y ergómetro); cita solo fuentes de §19 (Davison y Hinkley y Gelman a nivel de libro; Howley solo para la etiqueta VO₂max/VO2peak, sin umbrales); la cita del paquete sale de `inst/CITATION` con su versión; el commit y la semilla van al pie del informe, no al párrafo; percentiles y obesidad, al final.
5. **Fiabilidad.** `MFO_reliability()` sobre la tabla de grupo con una columna de sesión (como `sessions.csv`), sin volver a subir los ficheros.

**Idioma.** Todas las cadenas de la interfaz están en un único fichero (`inst/app/i18n.csv`, clave → texto, una columna por idioma). Decisión de Jorge del 25 nov 2026: **la app es solo en inglés**; se quitan el selector y la columna `es`, y se revocan el español como segunda lengua (19 nov) y el cambio de idioma sin recarga (24 nov). Se conservan la búsqueda clave → texto, de modo que otro idioma sería otra columna, y las condiciones con clase: los avisos y mensajes de `MFO()`, `read_cart()`, `MFO_zones()`, `MFO_kinetics()`, `MFO_protocol()` y `MFO_reliability()` son `mfo_warning_<id>`/`mfo_message_<id>` con sus números como campos (p. ej. `mfo_warning_negative_fat` con `n`), y la app los redacta con las plantillas `cond.<id>`. Los errores salen con su texto de R.

**Resultados y nombres de columnas** (decisiones de Jorge, 25 nov 2026). La gráfica de Resultados no es interactiva y debajo aparece siempre la tabla de escalones (`MFO_db`), que se descarga en csv/xlsx y entra en el informe. En la app, el informe y las descargas, `porc_VO2` se muestra como «%VO₂max» (y `VO2`, `VCO2` como VO₂, VCO₂ con unidades); la columna del paquete no cambia por compatibilidad. **Candidato a renombrado en la 1.0:** `porc_VO2` → un nombre que diga % VO₂max (cambio incompatible, solo con versión mayor).

**Figuras** (Jorge, 25 nov 2026). Cada gráfica de la app (registro de VO₂, curva de oxidación, SIN, Bland-Altman) se descarga en PNG a 300 ppp, PDF o SVG, con 180 mm de ancho por defecto (figura a ancho completo de revista) y ancho y alto configurables. Solo `grDevices`: `cairo_pdf` y `svg` imprimen el subíndice de VO₂, que `pdf()` no puede; sin cairo, el PDF usa `pdf()` y el SVG no está disponible.

**PDF del informe** (Jorge, 25 nov 2026). Botón «Save as PDF» en la pestaña Informe (el informe se escribe en un iframe oculto y se abre el diálogo de impresión del navegador, donde «Guardar como PDF» es una impresora) y dentro del propio HTML (`window.print()`, oculto al imprimir); hoja de estilos de impresión A4 sin cortar tablas ni gráficas. Sin pandoc ni LaTeX. DOCX con `officer`: pendiente de la estimación de coste.

**Preparar los datos y Fiabilidad** (Jorge, 25 nov 2026). La zona derecha de Datos tiene dos pestañas: «Prepare your data» (por defecto) con las tres vías —test individual (formatos, fases, cortes), tabla de grupo construida en la app con «Add to the group table» (código y sesión) y tabla de resultados subida en Fiabilidad— y dos plantillas (`inst/app/templates`, generadas por `data-raw/app_templates.R`): xlsx de test individual con una hoja por fase y las columnas del contrato, y csv de resultados de 3 participantes × 2 sesiones simulados (ejecutable tal cual en Fiabilidad como demostración; Jorge, 25 nov 2026) con `simulate_MFO_sessions()`, escritos con `write_MFO_sessions()` y analizados con `MFOs()`; y «Loaded data» (gráfica de VO₂ a todo el ancho y tablas por fase en pestañas), que se selecciona al leer los datos. En Fiabilidad, una tabla de resultados subida necesita solo `id`, `session`, `MFO` (g/min) y `Fatmax` (% VO₂max); las demás columnas de `MFOs()` son opcionales. La app traduce esos nombres a los de `MFO_reliability()`, que no cambia, y sigue aceptando la tabla de grupo descargada.

**Despliegue.** La beta.1 es solo local, con `MFO::run_app()`. Una demo pública, si se hace, solo con datos simulados o de ejemplo y sin subida de ficheros (los datos de participantes no se suben a servidores de terceros). Escritorio y Docker, en la 0.3.x.

**Arquitectura de la app** (decisiones de Claude, una línea cada una):

- Shiny puro con módulos en `R/` (`app_*.R`): golem convierte el paquete en una app con dependencia en ejecución y rhino es para apps independientes; los módulos se prueban con `shiny::testServer()` y entran en `covr`.
- La app no contiene ciencia: solo llama a las funciones del paquete, así que no puede dar un resultado distinto del paquete.
- `shiny` y `bslib` en `Suggests` con `requireNamespace()` en `run_app()`; gráficos con ggplot y la interacción de Shiny (hover, brush), sin plotly.
- Tema (decisión de Jorge, 24 nov 2026): `bslib::bs_theme(version = 5, bootswatch = "minty")` con primary #6C63FF, secondary #A8A4CE, success #57CC99, info #80B3FF, warning #FFD166, danger #EF476F y la barra en primary; fuente Inter con `font_collection(font_google("Inter", local = TRUE), "system-ui", "sans-serif")`, que se descarga una vez a la caché de sass y después se sirve en local; si la primera vez no hay red, la app arranca con la fuente del sistema y lo dice. El tema se construye solo dentro de `run_app()`. Las gráficas de la app (y del informe) usan la misma paleta: FAT primary, CHO info, banda del IC warning, zona Fatmax primary, línea de Fatmax danger; la gráfica guardada en el resultado de `MFO()` no se modifica.
- El informe HTML se construye con `htmltools` (dependencia de `shiny`) y las gráficas van incrustadas como imágenes: un único fichero que no necesita pandoc.

## 11. Interpretación práctica: deporte, salud y errores frecuentes

**En rendimiento.** Un MFO alto y un Fatmax desplazado a la derecha indican alta capacidad oxidativa y ahorro de glucógeno; se asocian con el rendimiento en pruebas de más de 2–3 h (Ironman, ultra) y responden al entrenamiento de volumen en zona 2, al entrenamiento en ayunas o con baja disponibilidad de hidratos, y a la mejora del VO₂max. Para prescribir, Fatmax en W o km/h es más útil que %VO₂max, y la zona Fatmax da el margen. Un cambio pre-post de MFO solo es interpretable si supera el MDC del laboratorio (§8).

**En salud.** En obesidad, síndrome metabólico y diabetes tipo 2 el entrenamiento a Fatmax mejora la sensibilidad a la insulina, los lípidos plasmáticos y la composición corporal con intensidades bien toleradas. Conviene expresar Fatmax en %HRpeak (61–66 % con > 35 % de grasa), relativizar MFO a la masa libre de grasa y usar protocolos con escalones más largos e incrementos menores. El MFO absoluto puede engañar: una persona con obesidad puede tener un MFO absoluto normal por su mayor masa y, sin embargo, una capacidad oxidativa por kg de FFM reducida.

**Errores frecuentes y cómo los evita el software.**

| Error | Consecuencia | Salvaguarda en el paquete/app |
| --- | --- | --- |
| Test sin ayuno o tras ejercicio | RER alto, MFO bajo | Revisar el RER basal; un RER basal > 0,90 es orientación sin fuente verificada, no regla del paquete (sin alerta automática; #49, 0.3.x) |
| Promediar escalones enteros | Mezcla transición y meseta | `steady_state_duration` por defecto = último minuto |
| Incluir escalones con RER > 1 en el polinomio | Curva deformada, Fatmax desplazado | `RER_cutoff`, `negative_FAT` |
| Usar el VO₂ instantáneo más alto como VO₂max | Fatmax (%VO₂max) sesgado a la baja | Media móvil de 30 s (v0.3.0) |
| Ajustar P3 con 4 escalones | Raíces fuera de rango, NA | Fallback a P2/observado con aviso; recomendación de ≥ 6 escalones |
| Interpretar un cambio del 10 % como mejora | Falso positivo | Comparación con MDC e IC |
| No reportar ecuación ni método | Resultados no comparables | Párrafo de métodos automático |
| Comparar con referencias de otro protocolo | Clasificación errónea | Percentiles etiquetados con su protocolo |

**Un ejemplo de lectura.** Un corredor de 70 kg obtiene MFO = 0,62 g/min (8,9 mg/kg/min), Fatmax = 58 % VO₂max (HR 142 lpm, 76 % HRmax), zona Fatmax 49–66 %, cruce al 71 %, R² = 0,96, SIN con d = 0,12, s = 1,25, t = −0,04. Lectura: capacidad oxidativa alta, curva algo ensanchada y asimétrica a la derecha (mantiene la oxidación de grasa por encima de Fatmax), buena calidad de ajuste. Prescripción: sesiones largas a 135–150 lpm. Si tras 12 semanas MFO = 0,70 g/min, el cambio (+13 %) solo cuenta como mejora si el MDC del laboratorio es menor que 0,08 g/min.

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
| `MFO(..., ci = c("none","bootstrap","parametric"), B = 2000)`; `"parametric"` es el recomendado (§13.1, §17) | Como v0.2.0 + columna `Load` opcional | MFO y Fatmax con IC 95 %; Fatmax en W/km/h | 13, 14, 15 |
| `MFO_kinetics(..., ci = c("none","bootstrap","parametric"), B = 2000)`; `"parametric"` es el recomendado (§13.1, §17) | `MFO_db` | d, s, t, Fatmax\_SIN, Fatmin con IC | 13 |
| `MFO_bayes(MFO_db, model = c("P3","SIN"), priors)` | `MFO_db` | Posteriores de MFO, Fatmax, d, s, t; intervalos creíbles; diagnósticos | 13 |
| `MFO_reliability(results, id, session)` | Tabla de `MFOs()` con ≥ 2 sesiones por persona | ICC(2,1) + IC, SEM, MDC95, CV, sesgo, Bland-Altman | 12 |
| `MFO_protocol(type, step_duration, increments, ...)` | Descripción del protocolo | Objeto de protocolo que guía el promediado (escalón, rampa, relativo) | 14 |
| `VO2max_from_test(db_graded, window = 30)` | Test máximo | VO₂max/VO2peak por media móvil con criterios de maximalidad | 14 |
| `MFO_zones(x, HRmax, predicted_HRmax, age, population, body_fat, sex, age_group, weight_status)` | Resultado de `MFO()` o tabla de `MFOs()` | Zona de FC de prescripción y percentil frente a referencias | 15 |
| `read_cart(path, device)` | Export del analizador | Lista basal/MFO/graded estandarizada, con unidades detectadas | 16 |
| `MFO_report(result, format)` | Resultado individual | Informe HTML/PDF con párrafo de métodos | 17 |

## 12. Módulo de fiabilidad

**Evidencia que lo motiva.** Croci et al. (2014) hallaron CV intraindividuales de Fatmax del 16–21 % según el método (SIN 16,4 %, P3 18,6 %, MV 20,8 %) y límites de acuerdo de ±27–32 % VO₂max en hombres entrenados. Chrzanowski-Smith et al. (2020), en 99 adultos con dos tests separados 7–28 días, reportaron CV del 21 % para MFO y 26 % para Fatmax, errores típicos de ±0,06 g/min y ×/÷1,26 %VO₂peak, límites de acuerdo de ±0,17 g/min, ICC de 0,75 (MFO) y 0,45 (Fatmax), sin sesgo sistemático y con resultados consistentes entre subgrupos; su conclusión es que hacen falta mediciones repetidas. Chávez-Guevara y Amaro-Gahete (2025) añaden que parte de esa variación es error del analizador y recomiendan sesión de familiarización en sedentarios y reportar varias métricas de fiabilidad (ICC, concordancia, CV).

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

**Salida.** Tabla con una fila por variable (MFO, MFO/FFM, Fatmax %VO₂max, Fatmax %HRmax, Fatmax W, zona inferior/superior, d, s, t) y columnas ICC \[IC\], SEM, MDC95, CV, sesgo \[IC\], LoA; gráficos de Bland-Altman; y un objeto `reliability` que `MFO_report()` usa para marcar si un cambio pre-post individual supera el MDC del laboratorio.

**Uso con datos simulados.** Mientras no haya test-retest reales, el paquete incluirá `simulate_MFO_sessions()` que genera pares de curvas con error biológico (CV 10–20 %) y error de medida dentro de la sesión definido como en §17: ruido en la FAT de cada escalón respecto a la curva verdadera (`fat_noise_cv`); el ruido de los gases por registro (`noise_cv`, `noise_cor`) queda como opción, con `noise_cor = 0` por defecto y sin valor real asignado; sirve para los tests unitarios y para que la app muestre el módulo con datos de ejemplo.

## 13. Incertidumbre del ajuste: bootstrap y modelo bayesiano

La fiabilidad (§12) cuantifica el error entre días; esta sección cuantifica el error *dentro* de un test: con 6–10 escalones ruidosos, la posición del máximo de un P3 o de un SIN es incierta aunque el test se repitiera idéntico. Ningún software de MFO publicado reporta esa incertidumbre; es el principal aporte metodológico de la v0.3.0.

**13.1 Bootstrap de residuos (`ci = "bootstrap"`; por defecto `ci = "none"`, decisión de Jorge, 17 nov 2026, para no cambiar resultados ni tiempos sin pedirlo).** Se ajusta el modelo, se guardan los residuos **modificados** r/√(1 − h), con h el leverage de cada escalón (diagonal de J(JᵀJ)⁻¹Jᵀ; en P3/P2/MRER, la matriz de diseño), y centrados (Davison y Hinkley, 1997, cap. 6), se generan B = 2000 réplicas sumando a los valores ajustados esos residuos remuestreados con reemplazo, se reajusta cada réplica y se recalcula MFO, Fatmax, zona y parámetros SIN. El IC 95 % es el intervalo percentil (2,5–97,5); BCa no se ofrece en la 0.3.0 (decisión de Jorge). MV no admite bootstrap (no ajusta modelo); en MRER se remuestrean por separado los residuos de sus dos rectas. **Implementado en la 0.3.0 para `MFO()`** (P3, P2, MRER); el SIN, con el ajuste en g/min. Sin *wild bootstrap* (Wu, 1986): con 6–10 escalones y pesos de dos puntos solo hay 2^n muestras distintas; la heterocedasticidad queda como limitación documentada. Con pocos escalones (n ≤ 6) el bootstrap de residuos es preferible al de casos porque no deja fuera escalones enteros. Limitaciones que el paquete declara: asume residuos intercambiables (homocedasticidad) y no captura el error del analizador ni el biológico; por eso el informe presenta este IC como "incertidumbre del ajuste" y el MDC como "incertidumbre de la medida".

**13.2 Modelo bayesiano (`MFO_bayes()`, opcional).** Se formula el P3 o el SIN como modelo no lineal con `brms` (Bürkner, 2017) sobre Stan (Carpenter et al., 2017):

```latex
\text{FAT}_i \sim \mathcal{N}\!\left(f(x_i;\theta),\ \sigma\right), \qquad f_{\text{SIN}}(x;\theta) = \text{MFO}\cdot\sin\!\left\{\left[\frac{\pi^{1/s}}{\pi+2d}\left(\tfrac{\pi}{100}x+d+t\right)\right]^{s}\right\}
```

**Ajuste SIN en g/min también en la vía clásica (0.3.0).** `MFO_kinetics(fit = "absolute")` ajusta FAT = M·SIN por mínimos cuadrados con M como cuarto parámetro (arranque desde el ajuste normalizado). Sin ruido recupera d, s, t, Fatmax, Fatmin y MFO a 1e-6 aunque ningún escalón caiga en Fatmax; con ruido elimina buena parte del sesgo pero no la dispersión de d, Fatmax y Fatmin, y mejora MFO en sesgo y precisión; d queda mal identificado con datos ruidosos en ambos ajustes, así que d, s y t no se interpretan sueltos (lo interpretable es Fatmax, Fatmin y MFO). Defecto `"normalised"` hasta la beta.1; **en la beta.1 el defecto de `MFO_kinetics()` pasará a `"absolute"`** (decisión de Jorge, 18 nov 2026), porque sus intervalos cubren 0,92–0,97 frente a 0,74–0,93 del normalizado (§17).

Con ruido (`inst/validation/sin_noise.R`; 200 tests por CV, 8 escalones, forma entrenada, MFO 0,4 g/min, Fatmax 45 ± 8 % VO₂max; sesgo = mediana del error, error típico = mediana del error absoluto; en el normalizado el MFO es el escalón más alto):

| CV | Magnitud | Normalizado: sesgo | Normalizado: error típico | Absoluto: sesgo | Absoluto: error típico |
| --- | --- | --- | --- | --- | --- |
| 5 % | d | −0,120 | 0,399 | −0,022 | 0,371 |
| 5 % | Fatmax (% VO₂max) | −0,05 | 1,56 | −0,01 | 1,55 |
| 5 % | Fatmin (% VO₂max) | −0,39 | 2,72 | +0,01 | 2,87 |
| 5 % | MFO (g/min) | +0,010 | 0,013 | +0,002 | 0,008 |
| 10 % | d | −0,135 | 0,527 | +0,000 | 0,506 |
| 10 % | Fatmax (% VO₂max) | +0,36 | 2,82 | +0,44 | 2,61 |
| 10 % | Fatmin (% VO₂max) | −1,18 | 4,09 | −0,19 | 3,27 |
| 10 % | MFO (g/min) | +0,026 | 0,027 | +0,007 | 0,016 |

Tests con |error de d| > 1: 37 y 46 (CV 5 %), 50 y 67 (CV 10 %), normalizado y absoluto.

**Soporte del SIN en Stan (decisión de Jorge, 17 nov 2026).** Fuera de sus dos ceros la base u = π^(1/s)/(π + 2d)·(Kx + d + t) es negativa y u^s da NaN con s no entero: el modelo usa FAT = MFO·sin(min(max(u, 0)^s, π)), que vale 0 antes del primer cero y después de Fatmin, igual que `simulate_MFO_sessions()`. Los recortes se escriben con `abs()` y π como número, (v + |v|)/2 y (v + π − |v − π|)/2, para que la fórmula sea válida también en R (`loo()` la evalúa allí). Los recortes tienen un vértice en los ceros; se sospecha que causan parte de las divergencias (issue #44).

**Parametrización (decisión de Jorge, 18 nov 2026).** A diferencia de la implementación clásica, el SIN bayesiano se ajusta en g/min directamente (MFO como parámetro de escala), sin la normalización previa por el máximo observado ni el suavizado con P3, lo que elimina dos pasos que introducen sesgo (§9.5). Los parámetros muestreados son **(MFO, Fatmax, Fatmin, s)**, las magnitudes que los datos informan; d y t se obtienen en cada muestra invirtiendo las formas cerradas de §6.5, con K = π/100 y a = 2^(−1/s):

```latex
t = \pi + d - K\,\text{Fatmin}, \qquad d = \frac{\pi(1-a) + K(\text{Fatmax} - \text{Fatmin})}{2(a-1)}
```

(álgebra comprobada con un test frente a las formas cerradas, diferencias < 1e-12). Fatmin se muestrea como Fatmax + (100 − Fatmax)·logit⁻¹(g), de modo que siempre queda entre Fatmax y 100 % VO₂max; su prior normal truncado entra como término propio del modelo con el jacobiano de esa transformación. La zona Fatmax (FAT ≥ 0,90·MFO) también sale de formas cerradas en cada muestra, con asin(0,9).

Por qué no (d, s, t): con la parametrización original la posterior tiene una cresta (correlación de s y t ≈ 0,96) cuya posición fijan los priors. Con los priors propuestos al principio en esta sección (d \~ Normal(0, 0,3); s \~ LogNormal(0, 0,3); t \~ Normal(0, 0,2), centrados en el SIN básico) los intervalos del 95 % de s cubrían el valor verdadero en solo el 7–10 % de los tests simulados en que t está lejos de 0, y la mediana a priori de Fatmin era 99,9 % VO₂max. Se conservan en la viñeta `bayes` como motivación.

**Priors por defecto** (elección débilmente informativa de los autores, no estimaciones de la literatura; el usuario puede sustituirlos con `priors` y revisarlos con `MFO_prior_check()`):

| Parámetro | Prior | Justificación |
| --- | --- | --- |
| MFO (g/min) | Normal(0,45, 0,25) truncada en 0 | Por encima de la media normativa de adultos sedentarios (0,34 ± 0,10; Amaro-Gahete, Sánchez-Delgado y Ruiz, 2018, Tabla 1), con DE inflada para cubrir entrenados |
| Fatmax (% VO₂max) | Normal(50, 15) truncada en (10, 90) | Entre el valor normativo de sedentarios (44,2 ± 12,4; misma tabla) y el de hombres entrenados (64 %; Achten, Gleeson y Jeukendrup, 2002), con DE inflada |
| Fatmin (% VO₂max) | Normal(90, 14) truncada en (Fatmax, 100); argumento `fatmin_prior` | 90,1 ± 14,1 en Chenevière et al. (2009) |
| s | LogNormal(0, 0,3) | Elección débil centrada en la curva simétrica (s = 1); no hay fuente poblacional de s |
| σ | Exponential(10) (`sigma = "constant"`, defecto) | Opción `"proportional"`: σᵢ = σ₁₀₀·xᵢ/100 con log σ₁₀₀ \~ Normal(log 0,1, 1); la comparación con `loo()` de la viñeta es ilustrativa |

Muestreo: 4 cadenas × 2000 iteraciones, `adapt_delta = 0,99`; se recomienda `iter = 4000` para Fatmax alto (R-hat > 1,01 en 5 → 0 de 100 tests de la celda entrenada). Salida: mediana e intervalo creíble del 95 % de MFO, Fatmax, Fatmin, zona Fatmax, d, s y t; contracción 1 − Var(posterior)/Var(prior) con la etiqueta "prior-dominated" por debajo de 0,5; diagnósticos (R-hat, ESS, divergencias, |r| máxima entre d, s y t, con la nota fija de que d, s y t están mal identificados entre sí y lo interpretable es MFO, Fatmax y Fatmin). Validación en §17 y `inst/validation/bayes_*.R`.

**Limitación: sesgo residual de Fatmax.** Con los priors por defecto, Fatmax se infraestima: mediana del error −1,15 % VO₂max (sedentarios, CV 5 %) y −1,58 (entrenados por Fatmax y Fatmin, CV 10 %); cobertura 0,88 en ambas celdas, con los fallos casi todos por infraestimación. Parte se debe al prior de s, centrado en 1 cuando la verdad simulada es s = 1,52: con LogNormal(0, 0,5) el sesgo baja a −0,70 y −0,87, pero con 4000 iteraciones el 9 % y el 14 % de los ajustes tienen transiciones divergentes, por encima del 0–2 % exigido, así que el defecto sigue siendo LogNormal(0, 0,3) (decisión de Jorge, 18 nov 2026; issue #44). Escala: el error típico de Fatmax de los ajustes clásicos en los mismos tests es 1,6 (CV 5 %) y 2,6 % VO₂max (CV 10 %) con el SIN absoluto (mediana del error absoluto) y 2,5–3,1 y 4,1–5,0 con P3 (RMSE): el sesgo es menor que el error de un test, pero sistemático. Documentado en `?MFO_bayes` y en la viñeta `bayes`.

Ventajas concretas: converge donde `nlsLM` cae en la solución trivial (pocos escalones, curvas planas), permite comparar pre-post como distribución de la diferencia (issue #34) y escala a un modelo jerárquico multi-participante (priors de grupo) en una versión posterior.

**13.3 Cómo se presenta al usuario.** Siempre tres números por magnitud: estimación, IC/intervalo creíble del ajuste y, si existe `reliability`, MDC del laboratorio. Regla de interpretación en el informe: un cambio pre-post se califica de "probable" si los intervalos del ajuste no se solapan y de "detectable" si además supera el MDC95.

## 14. Protocolos nuevos y VO₂max robusto

La v0.2.0 asume escalones de igual duración desde el segundo 0. La v0.3.0 introduce un objeto de protocolo (`MFO_protocol()`) que describe cómo se recogió el test y guía el promediado; `MFO()` lo acepta en lugar de `step_duration`/`steady_state_duration`.

| Tipo de protocolo | Descripción | Promediado | Referencia |
| --- | --- | --- | --- |
| `step` (defecto) | Escalones de duración fija, incrementos fijos | Últimos 60–120 s de cada escalón | Achten 2002; scoping review 2025 |
| `step_variable` | Escalones de duración distinta (calentamiento largo, escalones finales cortos) | Vector de duraciones; tramo estable configurable por escalón | Amaro-Gahete 2019 (cinta 3,5 km/h + 1 km/h/3 min) |
| `relative` | Incrementos en W/kg (p. ej., 0,15 W/kg/4 min desde 0,45 W/kg) | Como `step`; registra la carga relativa | Test de potencia relativa 2026 |
| `ramp` | Incremento continuo (p. ej., 10–15 W/min) | Ventanas móviles de 30–60 s; curva FAT vs %VO₂max sin escalones | Physiologia 2025 |
| `single_day` | MFO seguido, tras 5 min, de test máximo desde la última carga | Ambos tests en un mismo fichero; corte automático por marca de tiempo | SMHS 2026 (ICC 0,957 para VO₂max) |

**Carga externa.** Si el test incluye una columna `Load` (W o km/h) por registro, `MFO()` devuelve Fatmax también en esa unidad por interpolación sobre la curva ajustada, y la zona Fatmax en W/km/h. Es lo que el entrenador necesita para prescribir.

**VO₂max robusto (`VO2max_from_test()`).** Se sustituye `max(VO2)` por la mayor media móvil de `window` segundos (30 s por defecto; opciones 15, 20, 60). La media móvil ya está en la 0.2.1 como argumento `vo2max_window` de `MFO()` (helper interno `.vo2max_rolling()`, §9.3); la 0.3.0 la expone como función con criterios de maximalidad. Se reportan criterios de maximalidad (meseta de VO₂ < 150 mL/min entre cargas, RER > 1,10, HR ≥ 90 % del teórico) y se etiqueta el valor como VO₂max o VO₂peak según se cumplan. El usuario puede pasar el VO₂max calculado por el laboratorio y el paquete no lo recalcula.

**Criterio de parada y escalones válidos.** `RER_cutoff` admite (hecho en la 0.2.1) los valores recomendados por nombre (`"classic"` = 1,00; `"safe"` = 0,95; `"sedentary"` = 0,93; `"trained"` = 0,90; Amaro-Gahete 2019). En el ejemplo dejan 7, 6, 5 y 4 de 8 escalones. Se añade `min_steps` (6 por defecto) con aviso si la curva se ajusta con menos escalones, y aviso si Fatmax cae en el primer o último escalón (máximo en el borde, probable protocolo inadecuado). **Hecho en la 0.2.1:** `min_steps` cuenta los escalones de ejercicio que entran en el ajuste, tras `RER_cutoff` y `negative_FAT = "exclude"`; el basal de `include_basal = TRUE` no cuenta. "En el borde" = el escalón de ejercicio más cercano a Fatmax es el primero o el último, basal excluido (Jorge, 10 nov 2026). En el ejemplo no salta ninguno; con `"sedentary"` (5 escalones) salta `min_steps`, y con `"trained"` (4 escalones, último a 41,0 %) MV, MRER y P2 sitúan Fatmax en el último escalón (41,0; 38,3; 39,0 %) y P3 no (29,4 %).

**Modalidad.** Se registra `mode = c("cycling", "running", "walking")` porque la cinética difiere: en carrera la curva es más dilatada y asimétrica a la derecha y Fatmax es mayor que en ciclismo con MFO similar (Chenevière et al., 2010). La modalidad se usa para elegir referencias (§15) y se imprime en el párrafo de métodos.

## 15. Zonas de frecuencia cardiaca y valores de referencia

**15.1 Zona de prescripción en FC (`MFO_zones()`).** `MFO_zones()` acepta un resultado de `MFO()` o la tabla de `MFOs()` y devuelve: HR a Fatmax y en los límites de la zona Fatmax (interpoladas linealmente entre escalones, como `FAT_MAX_HR`; `MFO()` y `MFOs()` las devuelven como `FatMax_zone_lower_HR` y `FatMax_zone_upper_HR`), y su expresión en %HRmax. La FCmáx se elige en este orden, y el informe dice la fuente (`HRmax_source`): (1) `HRmax` dado (en `MFO_zones()`, o en `MFO()`/`MFOs()`); (2) FC pico medida en el test máximo, `HR_peak`, la mayor media móvil de HR con la misma ventana que el VO₂max (§9.5, 14); (3) teórica, solo si se pide (`predicted_HRmax = TRUE` con `age`): Tanaka por defecto, Fox como opción (§19). Si se pide la teórica pero hay una FCmáx dada o medida, manda esta y se avisa con un mensaje.

Recomendación en obesidad: solo con `population = "obesity"` explícito, que exige `body_fat` (%). `body_fat` elige el rango de la meta-regresión de Chávez-Guevara et al. (2023): 61–66 % HRpeak con > 35 % de grasa corporal y 57–64 % HRpeak con < 35 %. Con 35,0 % exacto, que la fuente no cubre, no hay rango: la recomendación es `NA` con un mensaje que lo explica. La zona individual en %HRmax se etiqueta `"inside"` (dentro: toda la zona cae en el rango), `"partly overlaps"` (solapa en parte) o `"outside"` (fuera), siempre con los números en %HRpeak. El rango se refiere a la FC pico medida: con FCmáx teórica se muestra igualmente, con un aviso. La razón para preferir %HRpeak a %VO₂max en obesidad es que la relación HR-VO2 es más estable que el VO₂max medido, a menudo submáximo en estos pacientes.

**15.2 Valores de referencia (`reference_MFO`).** El paquete incluye el dataset exportado `reference_MFO`, generado por `data-raw/reference_MFO.R` a partir de la tabla 1 de Amaro-Gahete et al. (2018) (texto en `data-raw/Amaro_Gahete_2018.md`, verificado por Jorge el 19 nov 2026). Son percentiles empíricos P20/P40/P60/P80, media ± DE y N de 167 adultos sedentarios sanos (107 mujeres), en 12 filas: todos; por sexo; por grupo de edad; por sexo y grupo de edad; por estado ponderal. Cada fila lleva las condiciones del test, tal como las imprime la fuente:

| Campo | Valor en Amaro-Gahete et al. (2018) |
| --- | --- |
| Modalidad | Cinta, marcha (`mode = "walking"`) |
| Protocolo | 3 min a 3,5 km/h (0 %), +1 km/h cada 3 min hasta la velocidad máxima de marcha; después +2 % de pendiente cada 3 min hasta RER ≥ 1,0 |
| Promedio | Últimos 60 s de cada escalón |
| Ecuación | Frayn (1983), sin proteína |
| Método | Polinomio de tercer grado con intersección en (0;0) (Croci et al. 2014); distinto del `"P3"` libre del paquete |
| Ayuno | 5–6 h (no nocturno) |

| Fila | N | MFO (g/min), media ± DE; P20/P40/P60/P80 | Fatmax (% VO₂max), media ± DE; P20/P40/P60/P80 |
| --- | --- | --- | --- |
| Todos | 167 | 0,34 ± 0,10; 0,24/0,30/0,35/0,42 | 44,2 ± 12,4; 33,2/39,6/44,6/54,1 |
| Hombres | 60 | 0,37 ± 0,11; 0,29/0,34/0,38/0,44 | 40,8 ± 11,0; 32,2/37,2/41,1/48,8 |
| Mujeres | 107 | 0,32 ± 0,10; 0,24/0,28/0,33/0,40 | 46,1 ± 12,8; 34,8/42,1/47,8/55,9 |
| Jóvenes | 125 | 0,36 ± 0,11; 0,28/0,32/0,36/0,44 | 44,0 ± 13,3; 32,5/39,0/43,2/54,6 |
| Mediana edad | 42 | 0,29 ± 0,08; 0,22/0,24/0,28/0,38 | 44,7 ± 9,5; 35,8/41,4/46,0/53,4 |
| Hombres jóvenes | 41 | 0,38 ± 0,12; 0,28/0,35/0,38/0,48 | 39,8 ± 11,7; 29,4/36,7/40,3/48,6 |
| Mujeres jóvenes | 84 | 0,35 ± 0,09; 0,28/0,31/0,35/0,42 | 46,1 ± 13,6; 34,4/41,6/46,6/60,4 |
| Hombres de mediana edad | 19 | 0,35 ± 0,07; 0,29/0,33/0,38/0,42 | 43,0 ± 9,3; 35,5/39,5/44,4/53,1 |
| Mujeres de mediana edad | 23 | 0,24 ± 0,03; 0,22/0,23/0,24/0,27 | 46,1 ± 9,6; 39,0/42,7/50,1/54,4 |
| Normopeso | 88 | 0,34 ± 0,11; 0,25/0,30/0,34/0,42 | 45,3 ± 12,7; 35,1/41,1/46,5/55,6 |
| Sobrepeso | 50 | 0,33 ± 0,09; 0,24/0,30/0,35/0,41 | 42,7 ± 11,2; 32,7/39,1/42,7/53,1 |
| Obesidad | 29 | 0,36 ± 0,12; 0,26/0,30/0,39/0,45 | 43,3 ± 13,5; 32,3/39,1/42,0/54,8 |

Subgrupos: la fuente no imprime los cortes de edad ni de IMC. Los grupos de edad son las dos cohortes, jóvenes (22,1 ± 2,2 años, n = 125) y de mediana edad (52,1 ± 4,6 años, n = 42); el estado ponderal (normopeso, sobrepeso, obesidad) no tiene umbrales publicados. Por eso el paquete no deduce el subgrupo de la edad ni del IMC numéricos: el usuario lo elige con argumentos explícitos `age_group` (`"young"`, `"middle-aged"`) y `weight_status` (`"normal"`, `"overweight"`, `"obese"`), documentados así. Donde el texto y la tabla 1 discrepan (Fatmax por estado ponderal: 45,9/42,6 en el texto frente a 45,3/42,7 en la tabla; DE del MFO en mediana edad: 0,78 en el texto frente a 0,08 en la tabla), se usa la tabla.

Fuera de esta versión: MFO/FFM (la fuente da solo media ± DE por sexo, sin percentiles); Maunder et al. (2018), cuyos percentiles derivan de media y DE y proceden de cicloergómetro; y Randell et al. (2017), sin tabla de percentiles verificada. No hay referencias para deportistas.

Amaro-Gahete et al. (2018) advierten que la modalidad (cicloergómetro, carrera, marcha) cambia MFO y Fatmax, y que su ayuno de 5–6 h difiere del nocturno de otras cohortes. El paquete aplicará esa lógica: solo compara con una referencia cuya modalidad coincida y declara en el informe cada diferencia de protocolo, ecuación, método o ayuno; en particular, el `"P3"` libre del paquete frente al P3 por el origen de la referencia, y el ayuno de 5–6 h.

**15.3 Posición relativa.** `MFO_zones()` devuelve el percentil aproximado del participante para MFO absoluto y Fatmax (MFO/FFM queda fuera: la referencia no tiene percentiles), por interpolación lineal entre P20, P40, P60 y P80 de la fila de `reference_MFO` elegida. Fuera de ese intervalo devuelve la etiqueta `"< P20"` o `"> P80"` y percentil `NA`, sin extrapolar. Nunca se devuelve un percentil sin su cohorte: cada fila lleva `reference` (cohorte y grupo) y `reference_n`.

Grupo: solo las combinaciones que imprime la tabla 1 (todos; `sex`; `age_group`; `sex` × `age_group`; `weight_status`). `weight_status` junto con `sex` o `age_group` da error ("la tabla 1 no cruza sexo ni grupo de edad con estado ponderal; elige uno"), sin prevalencia silenciosa.

Modalidad: `mode = "walking"` y `"running"` reciben percentil (la referencia es marcha en cinta; en carrera se declara la diferencia); `"cycling"` da percentil `NA` con un mensaje que explica que la modalidad cambia MFO y Fatmax. `reference_differences` declara, en cada resultado, la modalidad, la ecuación si no es Frayn, el protocolo si no son escalones de 3 min promediados en los últimos 60 s, el método (siempre: la referencia usa P3 por (0;0)) y el ayuno (siempre: 5–6 h, no nocturno).

## 16. Importadores de analizadores de gases y contrato de datos

El objetivo es que el usuario de la app suba el fichero tal y como sale del analizador. `read_cart(path, device)` devuelve la estructura que `MFO()` espera, y es la única capa que conoce los formatos propietarios.

**Contrato de datos interno (lo que `MFO()` recibe siempre):** un data frame por fase con columnas `Time` (s desde el inicio), `VO2` (mL/min), `VCO2` (mL/min), `RER`, `HR` (lpm) y opcionalmente `Load` (W o km/h), `VE` (L/min) y `Phase` (basal/MFO/graded), a intervalo regular o breath-by-breath con `Time` real. Unidades verificadas: si VO₂ viene en L/min se multiplica por 1000; si viene en mL/kg/min se exige masa corporal. RER se recalcula como VCO₂/VO2 y se compara con la columna exportada (aviso si difieren > 0,02).

**Dispositivos previstos (prioridad según lo que use el laboratorio de la UCA, pendiente de confirmar):**

| Dispositivo / software | Formato de export | Particularidades a resolver |
| --- | --- | --- |
| Cosmed (Omnia, Quark CPET, K5) | xlsx/csv con cabecera de metadatos y fila de unidades | Columnas en varios idiomas; tiempo en hh:mm:ss; marcadores de fase |
| Cortex (MetaSoft Studio) | xlsx/csv | Separador decimal por configuración regional; promediado ya aplicado |
| Vyaire Vyntus CPX (SentrySuite) | csv/xlsx | Columnas duplicadas (BTPS/STPD); carga en W |
| ZAN (ZAN 600 y otros, vía spiro) | Export de ZAN leído por `spiro` | Soportado desde la 0.3.0 porque `spiro` lo lee (decisión de Jorge, 17 nov 2026). **Sin fichero real de validación**: solo probado con los ficheros de ejemplo de `spiro` (`zan_gxt`, `zan_ramp`); unidad de la carga sin confirmar |
| Parvo Medics TrueOne | txt/xlsx | Intervalos fijos; cabecera larga |
| Genérico (`device = "generic"`, #22) | csv/xlsx con mapeo manual de columnas (como en v0.2.0) | Hecho en la 0.3.0, sin `spiro`; ver «Lector genérico». **Sin fichero real de validación** (pedido al laboratorio el 19 nov 2026): probado con los datos de ejemplo escritos como csv/xlsx. Interfaz de mapeo en la app (#24) |

Cada importador se desarrollará a partir de un fichero real anonimizado incluido en `inst/extdata`, que sirve de test (el importador debe reproducir exactamente los valores de escalón calculados a mano) y de ejemplo en la documentación. La app detectará el dispositivo por la cabecera y pedirá confirmación.

**Lector genérico (`read_cart(device = "generic")`, #22; decisiones de Jorge, 19 nov 2026).** Lee csv (o txt/tsv) y xlsx y pasa por el mismo procesamiento que los demás importadores (unidades, promedio, RER y fases).

- *Mapeo de columnas:* `columns = c(Time = , VO2 = , VCO2 = , RER = , HR = , Load = , VE = )`, con los nombres del contrato por defecto, como los `col_name_*` de la v0.2.0. Nombre exacto: si falta una columna, el error lista las del fichero. Solo se conservan las columnas mapeadas; la cabecera de metadatos (donde suele estar el nombre) no sale de la lectura.
- *Cabecera:* `skip` líneas antes de la cabecera; con `units_row = TRUE`, la línea siguiente lleva las unidades. Una unidad de VO₂ reconocida ("mL/min", "L/min", "mL/kg/min") es una declaración del fichero y se usa con un mensaje; `vo2_unit` explícito tiene prioridad. Sin unidad reconocida rige la regla automática de #16.
- *Números:* `decimal = "."` por defecto; si los valores no se leen y llevan coma decimal, el error propone `decimal = ","`. Con `decimal = ","` un punto no se interpreta como separador de miles: el valor da error. El separador de campos del csv (tabulador, ";" o ",") se detecta en la línea de cabecera.
- *Tiempo:* detección automática de segundos, mm:ss, hh:mm:ss y horas de Excel (fracciones de día, redondeadas al milisegundo); si no se reconoce, error con un valor de ejemplo. `time_origin = "elapsed"` (defecto): tiempo transcurrido desde el inicio del registro; si el primer valor supera 1 h, error que sugiere `"clock"`. `time_origin = "clock"` (hora del día): se resta el primer valor, de modo que `Time` cuenta desde el primer registro, y el valor restado se guarda en `info$time_offset` (decisión de Jorge, 19 nov 2026).
- *Ficheros ya promediados:* `interval = "none"` conserva los registros tal como se exportan; `step_time` es el paso de `Time`, que debe ser regular (error si no). Un `interval` numérico es solo para breath-by-breath.
- *Fases:* solo por `cuts` (no se lee una columna de fase), o por hojas de un xlsx, `sheets = c(basal = , MFO = , graded = )`, alternativa a `cuts`; las hojas deben tener el mismo paso. Una única hoja sin nombre de fase es el registro: la fase de `phase`, o se divide con `cuts` (necesario para la app, #24).
- `MFOs()` con ficheros de analizador queda para un issue aparte (#47).

**Separación de fases.** Si el fichero contiene las tres fases en una sola serie temporal (frecuente en `single_day`), `read_cart()` las separa por marcadores del analizador o por tiempos que el usuario introduce; si hay dudas, la app muestra la serie y deja marcar los cortes.

## 17. Plan de validación, tests y declaración de uso de IA

**Validación numérica.** (1) Ecuaciones: tests unitarios con los ejemplos numéricos de Frayn (1983), Péronnet y Massicotte (1991) y Jeukendrup y Wallis (2005). (2) Métodos: reproducir con los datos de ejemplo del paquete los valores de Montes de Oca et al. (2021) y comprobar que P3, MV y SIN dan MFO equivalentes a nivel de grupo, como reportan Chenevière et al. (2009) y Amaro-Gahete et al. (2019). (3) Bootstrap y bayesiano: estudio de simulación con curvas sintéticas de parámetros conocidos para verificar cobertura de los intervalos (≈ 95 %) con 6, 8 y 10 escalones y CV de ruido del 5–20 %. **Definición (Jorge, 17 nov 2026):** el CV de ruido es el de la FAT de cada escalón respecto a la curva verdadera, DE residual / MFO (`fat_noise_cv` de `simulate_MFO_sessions()`); el ruido de los gases (`noise_cv`, `noise_cor`) queda como opción, con `noise_cor = 0` por defecto y sin valor "real" asignado a la correlación, porque no hay fuente. Script: `inst/validation/bootstrap_coverage.R`.

**Resultados del estudio de cobertura (17 nov 2026; detalle en `vignette("validation")`).** Verdad = curva SIN; 200 tests por celda; IC del 95 %; cobertura de Fatmax (mín.–máx. sobre 6/8/10 escalones):

| Método | Intervalo | CV 5 % | CV 10 % | CV 20 % | Sesgo de Fatmax (% VO₂max) |
| --- | --- | --- | --- | --- | --- |
| P3 | paramétrico (σ sorteada) | 0,91–0,95 | 0,92–0,94 | 0,88–0,96 | −0,4 a +1,6 |
| P3 | bootstrap de residuos | 0,77–0,91 | 0,84–0,88 | 0,75–0,87 | −0,2 a +1,9 |
| P2 | paramétrico (σ sorteada) | 0,91–0,95 | 0,94 | 0,95–0,98 | −7,0 a +0,9 (inestable con CV 20 %) |
| P2 | bootstrap de residuos | 0,82–0,91 | 0,84–0,90 | 0,83–0,94 | −6,2 a +1,5 |
| MRER | bootstrap de residuos | 0,48–0,61 | 0,61–0,67 | 0,68–0,74 | +2,8 a +4,2 |

**SIN (`MFO_kinetics(ci = "parametric")`, `inst/validation/sin_coverage.R`):** 8 escalones, B = 1000, 200 tests por celda, mismos participantes simulados que la tabla de 13.2; cobertura del IC 95 % (anchura mediana):

| Ajuste | CV | Fatmax | Fatmin | MFO_SIN |
| --- | --- | --- | --- | --- |
| absoluto | 5 % | 0,96 (10,6) | 0,93 (14,0) | 0,94 (0,061) |
| absoluto | 10 % | 0,94 (19,6) | 0,94 (28,8) | 0,97 (0,121) |
| absoluto | 20 % | 0,93 (35,9) | 0,94 (58,5) | 0,92 (0,266) |
| normalizado | 5 % | 0,93 (9,0) | 0,87 (13,1) | — |
| normalizado | 10 % | 0,87 (17,5) | 0,86 (27,4) | — |
| normalizado | 20 % | 0,74 (26,5) | 0,88 (62,9) | — |

Anchuras de Fatmax y Fatmin en % VO₂max y de MFO_SIN en g/min. Con CV 20 %, 13 de 200 tests sin ajuste en cada caso. El absoluto cubre cerca del 95 %; el normalizado se queda corto y empeora con el ruido (su sesgo de normalización).

Caso ideal (P2 sobre una parábola, 8 escalones, errores normales; 500 tests): cobertura de Fatmax 0,944 con el paramétrico de σ sorteada frente a 0,882 con el bootstrap de residuos (≈ P(|t₅| < 1,96) = 0,893). Decisiones (Jorge, 17 nov 2026): `ci = "parametric"` es el intervalo recomendado (`ci = TRUE`); MRER se mantiene por comparabilidad, no recomendado como principal; P2 avisa cuando el vértice cae fuera del rango medido. La verdad SIN favorece a los modelos más parecidos a un SIN (P3). Filas de P2 repetidas tras el arreglo de #38 (parábola sin máximo → MV): tests con c ≥ 0, 0/200 con CV 5 %, 0–1 con 10 %, 5–12 con 20 %; no explican las anchuras enormes de P2, que vienen de réplicas con curvatura negativa casi nula (vértice extrapolado lejos del rango medido). (4) Fiabilidad: comparar `MFO_reliability()` con `psych::ICC` en datos simulados (`simulate_MFO_sessions()`); no disponemos del dataset de Chrzanowski-Smith et al. (2020). (5) Importadores: igualdad exacta con cálculos manuales sobre los ficheros de `inst/extdata`.

**Infraestructura.** GitHub Actions (R CMD check en tres sistemas, cobertura ≥ 85 % con `covr`), pkgdown con vignettes "De los gases al Fatmax", "Fiabilidad de tu laboratorio" y "Incertidumbre bayesiana", NEWS por versión, issues públicas para cada punto de esta Parte II, y releases etiquetadas. Esto construye el historial público que JOSS exige.

**Uso de IA generativa y JOSS.** JOSS no prohíbe que el código se escriba con asistencia de IA; exige (a) que el autor que envía sea contribuidor principal del software, (b) una declaración de uso de IA que indique herramientas y versiones, dónde se usaron, qué tipo de ayuda prestaron y la confirmación de que personas revisaron y validaron el resultado, y (c) que la IA no se use en la comunicación con editores y revisores (salvo traducción). En la práctica, para este proyecto: el diseño científico, las decisiones de producto, la validación contra la literatura y la redacción final del `paper.md` son de Jorge; Claude Code genera código, tests y documentación bajo esa dirección; cada commit queda firmado con coautoría de Claude para que el historial sea transparente; y el `paper.md` incluirá la declaración con el texto aproximado: "Parts of the code, unit tests and documentation of MFO v0.3.0 were drafted with Claude (Anthropic; Claude Code, model and version), under the design and supervision of the authors, who reviewed, tested and validated all outputs against the primary literature". Los revisores de JOSS evalúan el software y el artículo; una base de código bien testeada y documentada con esa declaración no es un obstáculo. Lo que sí sería un problema es un historial de un solo commit o un paquete sin evidencia de uso: por eso la fase 1 (sanear y publicar en CRAN) y la acumulación de 6 meses de actividad pública van antes del envío.

**Criterios de "hecho" para la v0.3.0.** Todos los tests en verde en CI; vignettes renderizadas; `R CMD check` sin notas en CRAN; app Shiny desplegada con los datos de ejemplo; `paper.md` en el repo; al menos un laboratorio externo (además de la UCA) usando el paquete o la app.

## 18. Herramientas existentes: análisis competitivo

Búsqueda realizada el 9 de noviembre de 2026 en CRAN, GitHub, JOSS, PyPI, bioRxiv/PMC y webs de fabricantes. **Conclusión: no existe ninguna herramienta de código abierto dedicada a MFO/Fatmax aparte de MFO.** Las alternativas son, o bien paquetes generales de ergoespirometría sin cálculo de Fatmax, o bien software comercial cerrado sin método documentado ni incertidumbre.

| Herramienta | Tipo | Qué hace | Qué no hace (hueco para MFO) | Lección para la v0.3.0 |
| --- | --- | --- | --- | --- |
| [spiro](https://docs.ropensci.org/spiro/) (R, CRAN, rOpenSci, JOSS 2023) | Paquete abierto | Importa Cortex, Cosmed, Vyntus y ZAN; resúmenes por escalón; VO₂max con medias móviles, filtros Butterworth; 9 paneles de Wasserman; protocolo autodetectado | No calcula sustratos ni Fatmax | Reutilizar su importación en vez de reescribirla (ver decisión abajo); su paso por rOpenSci y JOSS es el modelo a seguir |
| [whippr](https://fmmattioni.github.io/whippr/) (R, CRAN) | Paquete abierto | Lectura Cosmed/Cortex, cinética de VO₂, detección de outliers, normalización de tests incrementales | Sin oxidación de sustratos | Su detección de outliers breath-by-breath es aplicable antes de promediar |
| Shiny-Calorie (R/Shiny, 2025) y CalR | App abierta | Calorimetría indirecta en modelos animales, integración de metadatos, estadística | Orientadas a roedores y 24 h, no a tests incrementales humanos | Referente de app Shiny con despliegue web + escritorio + Docker |
| wrictools (R, CRAN 2026) | Paquete abierto | Calorímetros de habitación (Maastricht Instruments) | Otro dominio | Ejemplo reciente de paquete con importador específico de fabricante aceptado en CRAN |
| Cosmed OMNIA ≥ 2.2 (2022) | Software del analizador | Parámetro y "calculadora" de Fat Max, zonas de entrenamiento | Método no documentado públicamente; sin IC, sin SIN, sin fiabilidad; solo Cosmed | El usuario de Cosmed ya tiene un número: MFO debe explicar por qué el suyo es mejor (método explícito, IC, comparabilidad entre laboratorios) |
| INSCYD | Software comercial | Estima Fatmax y curva de combustión a partir de VO₂max y VLamax, sin analizador | Modelo, no medida; cerrado | Distinto mercado; MFO es la referencia medida contra la que validar modelos así |
| Calculadora Endure IQ; estimador Python de Fatmax por lactato (SportRxiv) | Calculadoras | Estiman Fatmax desde tests de campo o lactato | Sin calorimetría | Igual que INSCYD: posibles usuarios de MFO para validación |
| Cortex MetaSoft Studio; Vyaire SentrySuite | Software del analizador | Exportan VO₂/VCO2/RER por intervalo | No se ha podido verificar que calculen Fatmax (consultar manuales) | Son fuentes de datos, no competidores |

**Lo que ninguna ofrece y la v0.3.0 sí:** elección explícita y citada de ecuación y método; intervalos de confianza y versión bayesiana; modelo SIN con d, s, t; módulo de fiabilidad del propio laboratorio; valores de referencia etiquetados por protocolo; párrafo de métodos reproducible; y todo ello libre y auditable.

**Decisión de arquitectura derivada (una línea):** los importadores de la sección 16 se construirán sobre `spiro::spiro()` (en `Suggests`) para Cortex, Cosmed, Vyntus y ZAN, y `read_cart()` solo añadirá la separación de fases y la validación de unidades; así no duplicamos código ya revisado por rOpenSci, y la interoperabilidad con un paquete de JOSS es un argumento a favor en la revisión. Para analizadores que spiro no soporte (Parvo) se mantiene el lector genérico con mapeo de columnas.

## 19. Verificación de fórmulas y fuentes para la v0.3.0

Cada coeficiente y regla que usará el paquete se ha contrastado con la fuente primaria o con, al menos, dos documentos independientes que la transcriben (protocolos registrados en ClinicalTrials.gov, artículos en PMC). Estado: **verificado** (coincide), **matiz** (correcto pero incompleto en el código o en este documento) o **pendiente** (requiere el texto completo del artículo). Cuatro hallazgos cambian el código o la documentación y se han añadido al backlog.

| Elemento | Valor en el paquete / en este documento | Fuente contrastada | Estado | Acción |
| --- | --- | --- | --- | --- |
| Frayn (1983), ec. (5) y (6): CHO = 4,55 VCO₂ − 3,21 VO₂ − 2,87 n; FAT = 1,67 VO₂ − 1,67 VCO₂ − 1,92 n | Igual (n = 0) | Texto completo, p. 628; tabla 1 (glucosa 0,746/0,746; grasa 2,03/1,43 L/g) | Verificado | Frayn **imprime** las ecuaciones: se usan tal cual (4,55/3,21; 1,67/1,67). La derivación desde su tabla 1 (sistema 2×2) queda solo como test cruzado (regla de abajo) |
| Péronnet y Massicotte (1991): CHO = 4,585 VCO₂ − 3,2255 VO₂; FAT = 1,6946 VO₂ − 1,7012 VCO₂ | v0.2.0: 4,585 / 3,2255 / 1,6946 / 1,7012. 0.2.1: derivados de la tabla 1 (corregido) | Texto completo: el artículo **no imprime estas ecuaciones**; publica la tabla 1 (glucosa 0,7455 L O2 y 0,7426 L CO2 por g; ácido graso 2,0092 y 1,4136) de la que se derivan: CHO = 4,58519 VCO₂ − 3,22597 VO₂; FAT = 1,69469 VO₂ − 1,70130 VCO₂ | **Matiz (hallazgo 6)** | Como el artículo no imprime ecuación, se usan los coeficientes derivados (4,58519/3,22597; 1,69469/1,70130), resolviendo el sistema en código con las constantes de la tabla 1 y explicando la derivación en roxygen (diferencias en el 4.º decimal frente a los valores actuales). Hecho en la 0.2.1 (`.peronnet_coefficients()`); en el ejemplo Fatmax 35,288 → 35,285 % VO₂max |
| Jeukendrup y Wallis (2005): FAT = 1,695 VO₂ − 1,701 VCO₂; CHO 40–50 % = 4,344 VCO₂ − 3,061 VO₂; CHO 50–75 % = 4,210 VCO₂ − 2,962 VO₂ | Igual, coeficientes impresos en la tabla 5 y citados por tabla; umbral en 50 % | Texto completo (Int J Sports Med 26 S1, tablas 4 y 5); protocolos NCT03574987 y NCT03851744; PMC5117036 confirma el rango 50–75 % | Verificado | Test cruzado: derivando desde las constantes de la tabla 4 (glucosa 0,7455/0,7426; glucógeno 0,8283/0,8251; ácido graso 2,0092/1,4136 L/g) con mezcla glucosa/glucógeno 50/50 → 4,3438/3,0561 (impreso 4,344/3,061) y 20/80 → 4,2107/2,9625 (impreso 4,210/2,962); tolerancia absoluta 0,006. Añadir aviso fuera de 40–75 % (ya en backlog) |
| Jeukendrup y Wallis (2005) en reposo | Basal con el par 100/0 (glucosa pura) derivado de la tabla 4: CHO = 4,58519 VCO₂ − 3,22597 VO₂; FAT = 1,69469 VO₂ − 1,70130 VCO₂ (derivado porque no hay ecuación impresa) | Tablas 4 y 5: **no proponen ecuación de reposo** | Decisión del paquete | Documentado como tal en roxygen (`substrate_oxidation(phase = "rest")`); no citar como ecuación de Jeukendrup y Wallis |
| **Equivalentes energéticos** | El paquete usa 4 kcal/g (CHO) y 9 kcal/g (FAT) para las tres ecuaciones | Frayn 1983 **no da ninguno** (lo dice explícitamente, p. 632). Péronnet y Massicotte 1991: glucosa 3,8683 kcal/g, grasa 9,7460 kcal/g, 1 kcal = 4,182 kJ; no dan glucógeno. Jeukendrup y Wallis 2005, tabla 5: CHO 3,95 kcal/g (mezcla 40–50 % VO₂max), 4,07 kcal/g (50–75 %), grasa 9,75 kcal/g | **Matiz (hallazgo 1)** | Argumento `energy_equivalents` con defecto por ecuación: Péronnet 3,8683/9,7460; Jeukendrup 3,95 (40–50 %) / 4,07 (50–75 %) / 9,75 según el tramo de cada escalón (corregido 10 nov 2026; antes 4,15/9,75); Frayn: Péronnet (decisión del paquete). Hecho en la 0.2.1: `energy_equivalents = c("classic", "source")`, defecto `"classic"` (4/9) con mensaje único por sesión, `"source"` será el defecto en la 0.3.0; Jeukendrup en reposo usa Péronnet. `Kcal` cambia hasta un 8 % |
| Criterio de parada por RER | 1,00 clásico; 0,93 sedentarios; 0,90 entrenados | Amaro-Gahete 2019; Chenevière 2009 promedia solo escalones con RER ≤ 1,0 | **Matiz (hallazgo 2)** | Amaro-Gahete recomienda **0,95** como umbral seguro; 0,93/0,90 son RER medios en MFO. Hecho en la 0.2.1: `RER_cutoff` admite `"classic"` 1,00, `"safe"` 0,95, `"sedentary"` 0,93 y `"trained"` 0,90, explicados en roxygen |
| Punto de cruce (crossover): intensidad a la que CHO supera a FAT | Igual (interpolación lineal entre los dos escalones en que cambia el signo de FAT − CHO) | Brooks y Mercier (1994), *J Appl Physiol* 76(6):2253–2261, doi:10.1152/jappl.1994.76.6.2253 | Verificado (Jorge, 11 nov 2026) | Citado en la vignette |
| Zona Fatmax = FAT ≥ 90 % del MFO | Igual | Achten 2002 ("within 10% of the peak rate"); zona media 55–72 % VO₂max | Verificado | Ninguna |
| Fatmin | v0.2.0: primera intensidad con %MFO < 5 % por encima de Fatmax. 0.2.1: segundo cero (corregido) | Chenevière 2009, texto completo: Fatmin es el **segundo cero** del SIN (fat = 0 % MFO), sin umbral; en su muestra 90,1 ± 14,1 % VO₂max. Achten 2002: 89 ± 3 % | **Matiz (hallazgo 3)** | Fatmin = x₂ = (π + d − t)/K por defecto; `fatmin_threshold` opcional para quien quiera un umbral |
| Protocolo clásico | 3 min / 35 W | Achten 2002; Chenevière 2009 usó 3 min / 20 W (10 W en sedentarios) | Verificado | Ninguna |
| Protocolo en rampa (`ramp`) | Ventanas de 30 s (defecto) sobre una rampa continua | Teso M, Ferrari L, Colosio AL, Pogliaghi S (2025). Indexes of fat oxidation from ramp vs. graded incremental protocols in postmenopausal women. *Physiologia* 5(1):3, doi:10.3390/physiologia5010003 | Verificado (Jorge, 12 y 17 nov 2026) | Citado en `MFO_protocol()` y en la vignette |
| Protocolo de potencia relativa (`relative`) | Escalones de duración fija con carga en W/kg | Maximal fat oxidation assessment in active postmenopausal females: a novel relative power Fatmax test (2026), PMC13315815 | Verificado (Jorge, 12 nov 2026) | Citado en `MFO_protocol()` (#14) |
| Protocolo de un solo día (`single_day`) | Test de MFO seguido del test máximo en el mismo fichero | Marín-Galindo A, Opazo-Díaz E, Ramírez-Jiménez M, Montes-de-Oca-García A, Corral-Pérez J, Pérez-Bey A, Casals C, Ponce-González JG (2026). Validity and reliability of VO₂max determination immediately after a maximal fat oxidation assessment in pre-diabetes/type 2 diabetes. *Sports Med Health Sci* 8(3):327–334, doi:10.1016/j.smhs.2025.02.004 | Verificado (Jorge, 12 y 17 nov 2026) | Tipo y `cut_time` en `MFO_protocol()`; separación automática de fases en #16 |
| FCmáx teórica de Tanaka | 208 − 0,7·edad (defecto de `VO2max_from_test()`) | Tanaka H, Monahan KD, Seals DR (2001). *J Am Coll Cardiol* 37(1):153–156, doi:10.1016/s0735-1097(00)01054-8 | Verificado (Jorge, 17 nov 2026) | Citado en `VO2max_from_test()` y en la vignette |
| FCmáx teórica de Fox | 220 − edad (opción `"fox"` de `VO2max_from_test()`) | Citada a través de Tanaka, Monahan y Seals (2001), que la describen como la ecuación de uso común | Verificado (Jorge, 17 nov 2026) | Citado en `VO2max_from_test()` y en la vignette |
| Criterios de maximalidad y meseta de VO₂ < 150 mL/min | Meseta entre las dos últimas cargas; "VO2max" con meseta, o sin ella si se cumplen RER y FC | Howley ET, Bassett DR, Welch HG (1995). *Med Sci Sports Exerc* 27(9):1292–1301, doi:10.1249/00005768-199509000-00009 (el umbral de 150 mL/min procede de Taylor et al. 1955, según Howley) | Verificado (Jorge, 17 nov 2026) | Citado en `VO2max_from_test()` y en la vignette |
| Umbrales de RER (1,10) y FC (90 % de la teórica) | Defectos de `VO2max_from_test()` | Howley et al. (1995), pendiente del texto completo | Pendiente | `TODO: source` en el código y la vignette |
| **Modelo SIN: fórmula** | v0.2.0: `(pi^(1/s) / pi + 2*d)`, es decir (π^(1/s)/π) + 2d. 0.2.1: `pi^(1/s) / (pi + 2*d)` (corregido) | Ec. \[14\], p. 1618 y Apéndice p. 1625: π^(1/s) / (π + 2d) | **Error (hallazgo 5)** | Corregir a `pi^(1/s) / (pi + 2*d)`. Con d = 0 ambas coinciden (por eso la curva básica y los tests pasaban), pero con d ≠ 0 el parámetro d del paquete **no es la dilatación de Chenevière** y no es comparable con la literatura. Test de regresión: con d = 0,18, s = 1,52, t = 0,21 (entrenado, fig. 5) la curva debe reproducir Fatmax y Fatmin de la fórmula cerrada |
| Modelo SIN: procedimiento de ajuste | v0.2.0: normaliza al máximo observado, suaviza con P3, malla sintética, renormaliza, ajusta. 0.2.1: ajuste directo a los escalones (corregido) | Chenevière: ajuste directo a los valores medidos (como E\_lipid) minimizando suma de cuadrados; **sin suavizado ni malla** | **Matiz (hallazgo 7)** | Eliminar el suavizado y la malla: inflan artificialmente n y reducen los errores típicos de d, s, t. Ajustar a los escalones observados; si no converge, avisar en lugar de suavizar |
| MRER | FAT = 1,7(1−RER)VO₂, máximo observado; atribuido a Pérez-Martín | Chenevière 2009, p. 1617: la ecuación y la cita a Pérez-Martín son correctas; el método ajusta (1 − RER) y VO₂ como rectas de la intensidad y toma el vértice de la parábola producto | **Matiz (hallazgo 4, resuelto)** | Hecho en la 0.2.1: MRER real (vértice de las dos rectas) y `method = "MV"` (máximo observado con la ecuación de `author`) |
| Fiabilidad: Chrzanowski-Smith 2020 comparó MV, P2 y SIN (no P3) | — | Resumen del artículo | Matiz | Citarlo con precisión en §12 |
| Fiabilidad: CV 21 % MFO, 26 % Fatmax; TE ±0,06 g/min; LoA ±0,17 g/min; ICC 0,75 / 0,45 | Igual | Chrzanowski-Smith 2020 (n = 99) | Verificado | Ninguna |
| Fiabilidad: CV Fatmax SIN 16,4 %, P3 18,6 %, MV 20,8 % | Igual | Croci 2014 (n = 15) | Verificado | Ninguna |
| Fiabilidad histórica: CV Fatmax 9,6 %, Fatmin 9,4 % | Añadido | Achten y Jeukendrup 2003 (n = 10) | Verificado | Incluir en §12 |
| Zonas %HRpeak en obesidad: 61–66 % (> 35 % grasa), 57–64 % (< 35 %) | Igual | Chávez-Guevara IA, Amaro-Gahete FJ, Ramos-Jiménez A, Brun JF (2023). *Sports Med* 53(12):2399–2416, doi:10.1007/s40279-023-01897-y | Verificado | Usado en `MFO_zones(population = "obesity")` (§15.1). La fuente define > 35 % y < 35 %: con 35,0 % exacto no hay rango (recomendación `NA` con mensaje) |
| Valores normativos sedentarios (0,34 ± 0,10 g/min; 44,2 ± 12,4 % VO₂max; percentiles) | Igual | Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). *Front Physiol* 9:1460, doi:10.3389/fphys.2018.01460, tabla 1: 12 filas (todos; sexo; grupo de edad; sexo × grupo de edad; estado ponderal) con N, media ± DE y P20/P40/P60/P80 de MFO y Fatmax; todos, n = 167: MFO 0,34 ± 0,10 g/min (0,24/0,30/0,35/0,42), Fatmax 44,2 ± 12,4 % VO₂max (33,2/39,6/44,6/54,1). Condiciones: cinta, marcha, 3 min a 3,5 km/h, +1 km/h/3 min hasta velocidad máxima de marcha y después +2 %/3 min hasta RER ≥ 1,0; últimos 60 s; Frayn sin proteína; P3 por (0;0) (Croci 2014, citado a través de Amaro-Gahete 2018, sin cotejar); ayuno 5–6 h. Cortes de edad e IMC no publicados (jóvenes 22,1 ± 2,2 años, n = 125; mediana edad 52,1 ± 4,6, n = 42). MFO/FFM solo media ± DE | Verificado sobre la tabla original (Jorge, 17 nov 2026) y sobre el texto completo, `data-raw/Amaro_Gahete_2018.md` (Jorge, 19 nov 2026) | Usos: (1) prior de MFO de `MFO_bayes()`, N(0,45; 0,25) truncada en 0, documentado como centrado por encima de la media normativa de sedentarios con DE inflada para cubrir entrenados; (2) dataset `reference_MFO` de #15 (§15.2), generado desde la tabla 1 por `data-raw/reference_MFO.R`, con el N del subgrupo en cada fila y `age_group`/`weight_status` explícitos; el P3 por el origen y el ayuno de 5–6 h se declaran como diferencias; MFO/FFM fuera; si texto y tabla discrepan, manda la tabla; (3) defectos "sedentario" de `simulate_MFO_sessions()` (MFO 0,34, Fatmax 44,2) |
| ICC(2,1), SEM = √MSE, MDC95 = 1,96√2·SEM | Igual | Weir 2005; Koo y Li 2016 | Verificado | Ninguna |
| ICC(2,1) = ICC(A,1), fuente original | Dos factores aleatorios, acuerdo absoluto, medida única | Shrout PE, Fleiss JL (1979). *Psychol Bull* 86(2):420–428 | Verificado (Jorge, 17 nov 2026) | Citado en `MFO_reliability()` |
| IC 95 % del ICC(A,1) por F | Aproximación de Satterthwaite (v) con cuantiles F(1 − α/2) | McGraw KO, Wong SP (1996). *Psychol Methods* 1(1):30–46, doi:10.1037/1082-989X.1.1.30, con la errata de la p. 390 | Fuente verificada (Jorge, 17 nov 2026); IC verificado **numéricamente** contra `psych::ICC` 2.6.9 (ICC2), que aplica la errata: coinciden hasta 1e-10 con 2 y 3 sesiones. **El texto de la errata (p. 390) no se ha consultado** | Implementado en `MFO_reliability()` |
| CV como error típico en logaritmos: 100 (e^SEM_log − 1) | Para todas las variables, junto a CV = SEM/media | Hopkins WG (2000). *Sports Med* 30(1):1–15, doi:10.2165/00007256-200030010-00001 | Verificado (Jorge, 17 nov 2026) | `CV_log` en `MFO_reliability()` |
| Límites de acuerdo: sesgo ± 1,96 DE de las diferencias | Por pares de sesiones consecutivas | Bland JM, Altman DG (1986). *Lancet* 1(8476):307–310 | Verificado (Jorge, 17 nov 2026) | `pairs` y `plot()` de `MFO_reliability()` |
| Tamaño de muestra: al menos 30 participantes heterogéneos y, si es posible, 3 evaluadores | Aviso con n < 30 | Koo y Li (2016), *J Chiropr Med* 15:155–163, doi:10.1016/j.jcm.2016.02.012: "researchers should try to obtain at least 30 heterogeneous samples and involve at least 3 raters whenever possible" | Verificado (Claude en el texto de PMC4913118, a petición de Jorge, 17 nov 2026) | Aviso en `MFO_reliability()` |
| Residuos modificados r/√(1 − h), centrados, para el bootstrap de residuos; regresión no lineal | `MFO(ci = "bootstrap")` | Davison AC, Hinkley DV (1997). *Bootstrap Methods and their Application*. Cambridge University Press, cap. 6 y §7.4 | Verificado por el índice (Jorge, 17 nov 2026); **ecuación pendiente** de comprobar en el texto | Citado en `?MFO` (*Fit uncertainty*) |
| *Wild bootstrap* (no ofrecido; limitación) | Con n escalones y pesos de dos puntos, 2^n muestras distintas | Wu CFJ (1986). *Ann Stat* 14(4):1261–1295, doi:10.1214/aos/1176350142 | Verificado (Jorge, 17 nov 2026) | Citado en `?MFO` |
| Bootstrap paramétrico con σ incierta: σ² ~ Inv-χ²(n − p, s²) y después residuos normales | `MFO(ci = "parametric")`, defecto con `ci = TRUE` | Gelman A, et al. (2013). *Bayesian Data Analysis*, 3.ª ed. Chapman & Hall/CRC, cap. 14, §14.2 (regresión normal con prior plano) | Verificada por Jorge (17 nov 2026); **apartado pendiente de confirmar** | Citado en `?MFO` (*Fit uncertainty*) |
| brms | Modelos no lineales bayesianos sobre Stan | Bürkner PC (2017). *J Stat Softw* 80(1):1–28, doi:10.18637/jss.v080.i01 | Verificado (Jorge, 17 nov 2026) | `MFO_bayes()` (fase 3 de #13) |
| Stan | Lenguaje de programación probabilística | Carpenter B, et al. (2017). *J Stat Softw* 76(1):1–32, doi:10.18637/jss.v076.i01 | Verificado (Jorge, 17 nov 2026) | `MFO_bayes()` (fase 3 de #13) |
| Fatmax típico 45–65 % VO₂max; Achten 2002: 64 % VO₂max, 74 % HRmax | Igual | Achten 2002; Maunder 2018 | Verificado | Ninguna |
| Interpretación de s | s > 1 asimetría a la derecha | Chenevière 2009, Discusión: 0 < s < 1 izquierda, s > 1 derecha | Verificado | Ninguna |

**Estado tras la lectura de los textos completos (9 de noviembre de 2026).** Los tres pendientes quedan cerrados. Balance: 7 hallazgos, de los cuales uno es un **error de implementación** (paréntesis del SIN, hallazgo 5), dos son desviaciones metodológicas respecto a la fuente (suavizado del SIN, hallazgo 7; MRER como MV, hallazgo 4) y cuatro son matices de documentación o parámetros (equivalentes energéticos, RER 0,95, Fatmin en cero, coeficientes de Péronnet derivados). Todos pasan al backlog de la 0.2.1 (los que corrigen código) o de la 0.3.0 (los que añaden opciones). Consecuencia práctica: los valores de d obtenidos con la v0.1.0/v0.2.0 no deben compararse con los publicados por el grupo de Chenevière; MFO y Fatmax por P3 no se ven afectados.

**Regla de procedencia (Jorge, 10 nov 2026): coeficiente impreso en la fuente > coeficiente derivado; derivado solo donde la fuente no imprime ecuación.** Si la fuente imprime la ecuación, se usa tal cual y se cita por tabla o ecuación; la derivación desde sus constantes (sistema 2×2) queda como test cruzado con tolerancia absoluta acorde al redondeo (0,006 en Jeukendrup y Wallis). Si no la imprime (Péronnet y Massicotte; reposo con Jeukendrup y Wallis), se deriva en código y la derivación se explica en roxygen.

**Regla para el desarrollo:** ningún coeficiente entra en el código sin (a) la cita en el bloque roxygen, (b) un test unitario con un ejemplo numérico de la fuente y (c) una fila en esta tabla. Esta tabla vive en esta sección de `docs/SPEC.md` y se actualiza en cada PR que toque una fórmula.

## Referencias

Fundamentos y ecuaciones

- Frayn KN (1983). Calculation of substrate oxidation rates in vivo from gaseous exchange. *J Appl Physiol* 55:628–634. [doi](https://doi.org/10.1152/jappl.1983.55.2.628)
- Péronnet F, Massicotte D (1991). Table of nonprotein respiratory quotient: an update. *Can J Sport Sci* 16(1):23–29. Sin DOI.
- Jeukendrup AE, Wallis GA (2005). Measurement of substrate oxidation during exercise by means of gas exchange measurements. *Int J Sports Med* 26 Suppl 1:S28–S37. [doi](https://doi.org/10.1055/s-2004-830512)
- Brooks GA, Mercier J (1994). Balance of carbohydrate and lipid utilization during exercise: the "crossover" concept. *J Appl Physiol* 76(6):2253–2261. [doi](https://doi.org/10.1152/jappl.1994.76.6.2253)

Fatmax, protocolos y métodos de análisis

- Achten J, Gleeson M, Jeukendrup AE (2002). Determination of the exercise intensity that elicits maximal fat oxidation. *Med Sci Sports Exerc* 34:92–97.
- Chenevière X, Malatesta D, Peters EM, Borrani F (2009). A mathematical model to describe fat oxidation kinetics during graded exercise. *Med Sci Sports Exerc* 41:1615–1625. [doi](https://doi.org/10.1249/MSS.0b013e31819e2f91)
- Pérez-Martín A, Dumortier M, Raynaud E, et al. (2001). Balance of substrate oxidation during submaximal exercise in lean and obese people. *Diabetes Metab* 27:466–474.
- Amaro-Gahete FJ, Sanchez-Delgado G, Alcantara JMA, et al. (2019). Optimizing maximal fat oxidation assessment by a treadmill-based graded exercise protocol: when should the test end? *Front Physiol* 10:909. [doi](https://doi.org/10.3389/fphys.2019.00909)
- Montes de Oca-García A, González-Montesinos JL, et al. (2021). *Eur J Sport Sci*. [doi](https://doi.org/10.1080/17461391.2020.1788650)
- Maunder E, Plews DJ, Kilding AE (2018). Contextualising maximal fat oxidation during exercise: determinants and normative values. *Front Physiol* 9:599.

Revisiones y recomendaciones recientes (2023–2026)

- Chávez-Guevara IA, Amaro-Gahete FJ, Ramos-Jiménez A, Brun JF (2023). Toward exercise guidelines for optimizing fat oxidation during exercise in obesity: a systematic review and meta-regression. *Sports Med* 53(12):2399–2416. [doi](https://doi.org/10.1007/s40279-023-01897-y)
- Chávez-Guevara IA, et al. (2025). Methodological issues related to maximal fat oxidation and Fatmax reproducibility: a narrative review. *Int J Obes* 49:1984–1994. [enlace](https://www.nature.com/articles/s41366-025-01861-y)
- Scoping review (2025). Maximal fat oxidation during cycle ergometer protocols in obese adults. *Diseases* 14(1):4. [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC12840565/)
- Teso M, Ferrari L, Colosio AL, Pogliaghi S (2025). Indexes of fat oxidation from ramp vs. graded incremental protocols in postmenopausal women. *Physiologia* 5(1):3. [doi](https://doi.org/10.3390/physiologia5010003)
- Maximal fat oxidation assessment in active postmenopausal females: a novel relative power Fatmax test (2026). [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC13315815/)
- Meta-análisis sobre ejercicio a Fatmax y metabolismo glucídico-lipídico en sobrepeso/obesidad (2026). [enlace](https://pmc.ncbi.nlm.nih.gov/articles/PMC13343287/)

Software

- Fernandez-Santos JR, et al. (2022). MFO: Maximal Fat Oxidation and Kinetics Calculation. R package v0.1.0, CRAN. [enlace](https://cran.r-project.org/web/packages/MFO/index.html)
- Repositorio v0.2.0: [github.com/JorgeDelro/MFO](https://github.com/JorgeDelro/MFO)
- Journal of Open Source Software, criterios de envío. [enlace](https://joss.theoj.org/about)

Valores de referencia, protocolos adicionales y cinética (Parte II)

- Amaro-Gahete FJ, Sanchez-Delgado G, Ruiz JR (2018). Commentary: Contextualising maximal fat oxidation during exercise: determinants and normative values. *Front Physiol* 9:1460. [doi](https://doi.org/10.3389/fphys.2018.01460)
- Amaro-Gahete FJ, Sanchez-Delgado G, Jurado-Fasoli L, et al. (2019). Assessment of maximal fat oxidation during exercise: a systematic review. *Scand J Med Sci Sports* 29:910–921.
- Randell RK, Rollo I, Roberts TJ, et al. (2017). Maximal fat oxidation rates in an athletic population. *Med Sci Sports Exerc* 49:133–140.
- Chenevière X, Malatesta D, Gojanovic B, Borrani F (2010). Differences in whole-body fat oxidation kinetics between cycling and running. *Eur J Appl Physiol* 109:1037–1045. [enlace](https://doc.rero.ch/record/310501?ln=en)
- Marín-Galindo A, Opazo-Díaz E, Ramírez-Jiménez M, Montes-de-Oca-García A, Corral-Pérez J, Pérez-Bey A, Casals C, Ponce-González JG (2026). Validity and reliability of VO₂max determination immediately after a maximal fat oxidation assessment in pre-diabetes/type 2 diabetes. *Sports Med Health Sci* 8(3):327–334. [doi](https://doi.org/10.1016/j.smhs.2025.02.004)
- Tanaka H, Monahan KD, Seals DR (2001). *J Am Coll Cardiol* 37(1):153–156. [doi](https://doi.org/10.1016/s0735-1097(00)01054-8)
- Howley ET, Bassett DR, Welch HG (1995). *Med Sci Sports Exerc* 27(9):1292–1301. [doi](https://doi.org/10.1249/00005768-199509000-00009)

Fiabilidad, error de medida y estadística (Parte II)

- Croci I, Borrani F, Byrne NM, et al. (2014). Reproducibility of Fatmax and fat oxidation rates during exercise in recreationally trained males. *PLoS ONE* 9:e97930. [doi](https://doi.org/10.1371/journal.pone.0097930)
- Chrzanowski-Smith OJ, Edinburgh RM, Thomas MP, et al. (2020). The day-to-day reliability of peak fat oxidation and Fatmax. *Eur J Appl Physiol* 120:1745–1759. [doi](https://doi.org/10.1007/s00421-020-04397-3)
- De Souza Silveira R, Carlsohn A, Langen G, et al. (2016). Reliability and day-to-day variability of peak fat oxidation during treadmill ergometry. *J Int Soc Sports Nutr* 13:4.
- Weir JP (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM. *J Strength Cond Res* 19:231–240.
- Koo TK, Li MY (2016). A guideline of selecting and reporting intraclass correlation coefficients for reliability research. *J Chiropr Med* 15:155–163. [doi](https://doi.org/10.1016/j.jcm.2016.02.012)
- Hopkins WG (2000). Measures of reliability in sports medicine and science. *Sports Med* 30(1):1–15. [doi](https://doi.org/10.2165/00007256-200030010-00001)
- Bland JM, Altman DG (1986). Statistical methods for assessing agreement between two methods of clinical measurement. *Lancet* 1(8476):307–310.
- McGraw KO, Wong SP (1996). Forming inferences about some intraclass correlation coefficients. *Psychol Methods* 1(1):30–46 (errata p. 390). [doi](https://doi.org/10.1037/1082-989X.1.1.30)
- Shrout PE, Fleiss JL (1979). Intraclass correlations: uses in assessing rater reliability. *Psychol Bull* 86(2):420–428.
- Efron B, Tibshirani RJ (1993). *An Introduction to the Bootstrap*. Chapman & Hall.
- Bürkner PC (2017). brms: an R package for Bayesian multilevel models using Stan. *J Stat Softw* 80(1):1–28. [doi](https://doi.org/10.18637/jss.v080.i01)
- Carpenter B, Gelman A, Hoffman MD, et al. (2017). Stan: a probabilistic programming language. *J Stat Softw* 76(1):1–32. [doi](https://doi.org/10.18637/jss.v076.i01)
- Davison AC, Hinkley DV (1997). *Bootstrap Methods and their Application*. Cambridge University Press.
- Gelman A, et al. (2013). *Bayesian Data Analysis*, 3.ª ed. Chapman & Hall/CRC.
- Wu CFJ (1986). Jackknife, bootstrap and other resampling methods in regression analysis. *Ann Stat* 14(4):1261–1295. [doi](https://doi.org/10.1214/aos/1176350142)

Herramientas relacionadas y verificación (secciones 18–19)

- Nolte S (2023). spiro: An R package for analyzing data from cardiopulmonary exercise testing. *J Open Source Softw* 8(81):5089. [enlace](https://docs.ropensci.org/spiro/)
- Mattioni Maturana F (2025). whippr: Tools for manipulating gas exchange data. R package v0.1.4, CRAN. [enlace](https://fmmattioni.github.io/whippr/)
- Grein S, et al. (2025). Shiny-Calorie: a context-aware application for indirect calorimetry data analysis and visualization using R. [enlace](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC12867577/)
- COSMED (2022). OMNIA 2.2 software release (Fat Max parameter and calculator). [enlace](https://www.cosmed.com/en/news/company/1561-omnia-2-2-software-release)
- Achten J, Jeukendrup AE (2003). Maximal fat oxidation during exercise in trained men. *Int J Sports Med* 24:603–608. [enlace](https://www.thieme-connect.com/products/ejournals/html/10.1055/s-2003-43265)
- Achten J, Gleeson M, Jeukendrup AE (2002), resumen con definición de la zona Fatmax. [enlace](https://research.birmingham.ac.uk/en/publications/determination-of-the-exercise-intensity-that-elicits-maximal-fat-)
- Brun JF, Varlet-Marie E, Romain AJ, Mercier J (2012). Measurement and physiological relevance of the maximal lipid oxidation rate during exercise (LIPOXmax). IntechOpen. [enlace](https://core.ac.uk/download/322406893.pdf)
- Protocolos registrados que transcriben las ecuaciones de Jeukendrup-Wallis y Péronnet-Massicotte: NCT03574987, NCT03851744, NCT06164665, NCT05078138 (ClinicalTrials.gov).

*Nota: las referencias de 2025–2026 se han verificado en línea el 6 de noviembre de 2026; las de 2014–2020 sobre fiabilidad y los valores normativos de 2018 se han verificado en PMC el mismo día; las demás se han cotejado en los puntos que recoge §19. **Siguen sin cotejar con la fuente** y deben revisarse al preparar el `paper.md`: Efron y Tibshirani (1993); Weir (2005); De Souza Silveira et al. (2016); Randell et al. (2017); Pérez-Martín et al. (2001); Achten et al. (2002), texto completo (solo se ha leído el resumen). Cotejadas en parte (§19): Davison y Hinkley (1997), ecuación pendiente; Gelman et al. (2013), apartado pendiente; Howley et al. (1995), umbrales de RER y FC pendientes del texto completo. Montes de Oca et al. (2021), Chenevière et al. (2010) y Brun et al. (2012) no tienen fila en §19.*
