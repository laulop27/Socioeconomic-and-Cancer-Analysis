# 🧬 Unsupervised Machine Learning: Socioeconomic and Cancer Analysis

## 🎯 Project Objective
Reduce dimensionality and find hidden patterns in a sociodemographic dataset (economic, health indicators, and cancer mortality rates) to segment regions based on homogeneous characteristics.

## 🛠️ Technical Stack
* **Tool:** SAS
* **Reduction Techniques:** Principal Component Analysis (PCA), Factor Analysis (KMO Index, Rotation).
* **Clustering Techniques:** Hierarchical and Non-Hierarchical Cluster Analysis.
* **Classification:** Discriminant Analysis.

## ⚙️ Methodology
1. **Cleaning and Preprocessing:** Detection and treatment of univariate and multivariate outliers (identifying 33 multivariate outliers).
2. **Dimensionality Reduction:** Application of PCA and Factor Analysis to reduce the 13 original variables to a smaller set of uncorrelated components.
3. **Segmentation:** Grouping of observations into robust and distinct clusters.
4. **Validation:** Use of discriminant functions to validate the groupings.

## 📊 Results and Conclusions
* Successfully grouped regions into well-differentiated clusters based on their socioeconomic similarities.
* The discriminant analysis confirmed the robustness of the created groups, correctly classifying 81.4% of the individuals with a very low error rate (18.6%) on the test set.
