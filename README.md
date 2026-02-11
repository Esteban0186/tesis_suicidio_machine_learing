# Del silencio a la impulsividad: Perfiles de riesgo suicida masculino mediante Machine Learning

Este repositorio contiene el código fuente, los notebooks de análisis y los datos anonimizados utilizados en el artículo científico: **"Del silencio a la impulsividad: Espectros Internalizante y Externalizante de la conducta suicida masculina obtenidos mediante un enfoque de Machine Learning para la estratificación del riesgo"**.

## 📄 Descripción del Proyecto

El objetivo de este estudio fue validar un instrumento de evaluación psicosocial y construir una tipología de perfiles de vulnerabilidad suicida en hombres atendidos en el Instituto WEM de Costa Rica. Se analizaron datos de 226 participantes utilizando técnicas de aprendizaje no supervisado y supervisado.

### 🔍 Metodología

El flujo de trabajo analítico se dividió en dos fases principales:

1.  **Identificación de Perfiles (R):**
    * Análisis de Componentes Principales para datos mixtos (**PCAmix**).
    * Clustering Jerárquico Aglomerativo (Método de **Ward**).
    * Validación inferencial con pruebas de Kruskal-Wallis.

2.  **Modelo Predictivo (Python):**
    * Selección de características (*Feature Selection*) mediante triangulación de algoritmos: **Boruta**, **LASSO** y **Random Forest**.
    * Clasificación automatizada de perfiles mediante Máquinas de Vectores de Soporte (**SVM**) con validación cruzada anidada (*Nested Cross-Validation*).

## 📂 Estructura del Repositorio

* `/data`: Contiene el dataset procesado y anonimizado (sin información de identificación personal PII).
* `/R_scripts`: Scripts para el análisis descriptivo, PCAmix y Clustering Jerárquico.
* `/Python_notebooks`: Jupyter Notebooks para la selección de características y entrenamiento del modelo SVM.
* `/results`: Gráficos generados (dendrogramas, planos factoriales, matrices de confusión) y tablas de resultados.

## 🛠️ Requisitos y Dependencias

Para reproducir los análisis, se requiere el siguiente software:

* **R** (v4.5.2)
    * Librerías principales: `PCAmixdata`, `cluster`, `dendextend`.
* **Python** (v3.12.12)
    * Librerías principales: `scikit-learn`, `boruta`, `pandas`, `numpy`, `matplotlib`.

## 🚀 Cómo reproducir los resultados

1.  Clonar este repositorio:
    ```bash
    git clone [https://github.com/TU_USUARIO/NOMBRE_DEL_REPO.git](https://github.com/TU_USUARIO/NOMBRE_DEL_REPO.git)
    ```
2.  Ejecutar los scripts de R en la carpeta `/R_scripts` para generar los clústeres.
3.  Ejecutar los notebooks de Python en `/Python_notebooks` para entrenar el modelo predictivo.

## 👤 Autor y Contacto

**M.Sc. Esteban Navarro-Díaz**
* Instituto WEM / Universidad de Costa Rica
* Costa Rica
* Email: estebanalfonso.navarro@ucr.ac.cr

---
*Este proyecto cumple con los estándares de reproducibilidad exigidos para la publicación científica y respeta los protocolos éticos de manejo de datos clínicos.*
