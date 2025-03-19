##### Escuela Interdisciplinaria de Altos Estudios Sociales

##### Universidad Nacional de San Martín

<img src="img/logo-factor-data-solo.jpg" width="200" />

**Equipo Docente:**  
-[Valentín
Álvarez](https://www.linkedin.com/in/valentin-alvarez-850583119)  
-[Guido Weksler](https://ar.linkedin.com/in/guido-weksler-962677bb)  
-[Magalí Rodrígues
Pires](https://ar.linkedin.com/in/magali-rodrigues-pires)  
-[Nayla Sol Garcilazo](www.linkedin.com/in/nayla-sol-garcilazo)

## **Introducción al modelado de datos**

Los modelos estadísticos y de aprendizaje automático son herramientas
fundamentales para analizar datos y extraer patrones significativos. En
este módulo, se abordarán los modelos de regresión lineal y logística,
sus fundamentos teóricos y los métodos de estimación más utilizados,
como mínimos cuadrados y máxima verosimilitud. Se explorarán estrategias
para la interpretación de los resultados y la validación de los modelos
mediante medidas de ajuste, pruebas de hipótesis y diagnósticos.

Además, se introducirán conceptos clave del aprendizaje automático,
enfocándose en los problemas de clasificación y regresión dentro del
aprendizaje supervisado. Se analizarán aspectos como la diferencia entre
error de entrenamiento y error de testing, así como el problema del
sobreajuste y su impacto en la generalización de los modelos.

Para mejorar el desempeño de los modelos y evitar sobreajustes, se
estudiarán técnicas de regularización como LASSO y Ridge. Finalmente, se
presentará el ecosistema tidymodels, un conjunto de herramientas en R
diseñado para facilitar la construcción, validación y comparación de
modelos de manera estructurada y reproducible.

Este módulo combina teoría y práctica, brindando a los participantes las
herramientas necesarias para desarrollar modelos predictivos aplicados a
datos reales.

### Librerías a utilizar

En el archivo [0_packages](clase1/0_packages.R) <u> pueden encontrar el
código para descargar los paquetes que vamos usar en el módulo </u>,
también los dejamos anotados en el siguiente bloque:

``` r
install.packages("tidyverse")
install.packages("viridis")
install.packages("corrplot")
install.packages("ggridges")
install.packages("tidymodels")
install.packages("GGally")
install.packages("latex2exp")
install.packages("car")
install.packages("gtsummary")
install.packages("modelsummary")
install.packages("gt")
install.packages("discrim")
```

### Contenidos

<u> *Los materiales están organizados por clase: Pueden descargarse la
totalidad de los materiales de cada una para trabajar en un único
archivo .zip* </u>

**Clase 1 - Introducción: tipos de modelos, trade-offs:**

-   Temas de clase:
    -   ¿Por qué construir modelos?
    -   Predicción vs inferencia
    -   Regresión vs clasificación
    -   Paramétricos vs no paramétricos
    -   Supervisado vs no supervisado
    -   Trade-off precisión-interpretabilidad
    -   Trade-off sesgo-varianza
    -   Evaluación. Nociones de train/test, validation, matriz de
        confusión
    -   Flujo de trabajo: explorar, transformar, modelizar, evaluar
    -   Introducción a Tidymodels

[![](img/Download.png)](clase1.rar)

<br>

**Clase 2 - Explorando y transformando variables. Intro a regresión
lineal simple:**

-   Temas de clase:
    -   ¿Qué tipo de modelo es la regresión lineal?
    -   ¿Qué preguntas responde la regresión lineal?
    -   Tipos de relaciones entre variables
    -   Regresión lineal simple: fórmula, coeficientes y mínimos
        cuadrados
    -   Tests de hipótesis e intervalo de confianza. P-valor
    -   Predicciones
    -   Supuestos del modelo. Residuos y su distribución
    -   Métricas de evaluación de un modelo: R2
    -   Tidymodels 1

[![](img/Download.png)](clase2.rar)

<br>

**Clase 3 - Regresión lineal simple y múltiple:**

-   Temas de clase:
    -   Regresión múltiple: ¿por qué no una simple por cada variable?
    -   Fórmula, coeficientes e interpretación
    -   Variables independientes cualitativas y términos de interacción
    -   Tests de hipótesis 2: significatividad global
    -   Multicolinealidad. El trade-off sesgo-varianza
    -   Complejizamos la métrica de evaluación del modelo: R2 ajustado
    -   Tidymodels 2

[![](img/Download.png)](clase3.rar)

<br>

**Clase 4 - Clasificación 1: Regresión logística:**

-   Temas de clase:
    -   Modelos de clasificación: para qué sirven y cuáles existen
    -   Los limites de la regresión lineal.
    -   ¿Qué tipo de modelo es la regresión Logística?
    -   Coeficientes y lectura de outputs
    -   Train-Test split
    -   Medidas de evaluación del modelo

[![](img/Download.png)](clase4.rar)

<br>

**Clase 5 - Clasificación 2: Intro a KNN y LDA:**

-   Temas de clase:
    -   Limites de la regresión logística.
    -   Clasificador de Bayes.
    -   K-Nearest Neighbors supervisado
    -   LDA

[![](img/Download.png)](clase5.rar)

<br>

**Clase 6 - Cross-Validation y repaso general:**

-   Temas de clase:
    -   Limitantes del enfoque Train-Test
    -   Cross-Validation.
    -   Repaso de clases 1 a 5

[![](img/Download.png)](clase6.rar)

<br>

## Bibliografía complementaria

-   [An Introduction to Statistical Learning with applications in R
    (James, Witten, Hastie y Tibshirani) – 1st and 2nd
    version](https://emilhvitfeldt.github.io/ISLR-tidymodels-labs//)

-   [Tidy Modeling with R (Kuhn y Silge)](https://www.tmwr.org/)

-   [Introduction to Modern Statistics (Çetinkaya-Rundel y
    Hardin)](https://openintro-ims.netlify.app/)
