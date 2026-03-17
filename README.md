# 📊 Register-Based Population Estimation Using Activity Signals (Lebenszeichenansatz)

A reproducible R-based workflow for integrating administrative registers and estimating population stocks using activity-based signals.

![made-with-R](https://img.shields.io/badge/Made%20with-R-276DC2.svg)
![license](https://img.shields.io/badge/license-MIT-green.svg)

---

## 🚀 Project Overview

This project simulates a realistic framework for **register-based population statistics**, inspired by methodologies used in official statistics (e.g. Destatis, Eurostat).

It demonstrates how multiple administrative data sources can be integrated to:
- identify **likely residents**
- detect **overcoverage in population registers**
- derive **population estimates based on activity signals ("Lebenszeichenansatz")**

The workflow reflects key challenges in official population statistics:
- incomplete or outdated register entries  
- absence of deregistration events  
- inconsistencies across administrative data sources  
- need for transparent and reproducible estimation methods  

---

## 🧠 Methodological Concept: Lebenszeichenansatz

The core idea of this project is the **activity-based population estimation approach**:

> Individuals are classified as *likely residents* if recent activity signals are observed across administrative registers.

Examples of activity signals:
- employment records  
- tax filings  
- education enrolment  

Individuals without observable activity are treated as potential **overcoverage cases**.

---

## 🇩🇪 Kurzbeschreibung (Deutsch)

Dieses Projekt demonstriert einen reproduzierbaren Ansatz zur registergestützten Bevölkerungsschätzung unter Verwendung des sogenannten **Lebenszeichenansatzes**.

Dabei werden mehrere administrative Register (z. B. Beschäftigung, Steuern, Bildung) integriert, um anhand von Aktivitätssignalen wahrscheinliche Einwohner zu identifizieren und Übererfassungen im Melderegister zu quantifizieren.

Der Workflow orientiert sich an typischen Herausforderungen der amtlichen Statistik, insbesondere:
- Registerüberdeckung (Overcoverage)
- fehlende Abmeldungen
- Inkonsistenzen zwischen Datenquellen
- Bedarf an transparenten und reproduzierbaren Methoden

## 🧪 Synthetic Data Sources

The project generates fully synthetic but realistic administrative datasets:

1. **Population Register**
   - demographic information (age, region, sex)
   - includes intentional overcoverage

2. **Employment Register**
   - employment status, income, working days

3. **Tax Register**
   - taxable income, filing indicators

4. **Education Register**
   - enrolment status and institution type

All datasets include:
- missing values  
- inconsistencies  
- realistic distributions  

---

## 🔄 Workflow Pipeline

```markdown
                 ┌───────────────────────────────────┐
                 │ 01_generate_synthetic_registers.R │
                 └────────────────┬──────────────────┘
                                  ▼
                 ┌───────────────────────────────────┐
                 │ 02_clean_and_validate_registers.R │
                 └────────────────┬──────────────────┘
                                  ▼
                 ┌───────────────────────────────────┐
                 │  03_integrate_activity_signals.R  │
                 └────────────────┬──────────────────┘
                                  ▼
                 ┌───────────────────────────────────┐
                 │  04_estimate_population_stock.R   │
                 └────────────────┬──────────────────┘
                                  ▼
                 ┌───────────────────────────────────┐
                 │      05_visualize_results.R       │
                 └───────────────────────────────────┘
```

---

### Key Steps

**1. Data Generation**
- creation of synthetic administrative registers

**2. Cleaning & Validation**
- plausibility checks (e.g. age constraints)
- handling missing and inconsistent values

**3. Multi-Source Integration**
- linking individuals across registers
- construction of activity signals

**4. Population Estimation**
- classification of likely residents
- estimation of overcoverage
- evaluation against ground truth (synthetic benchmark)

**5. Visualization**
- regional comparisons
- age-specific overcoverage patterns
- signal distribution analysis

---

## 📈 Key Results

### Overcoverage Estimation
- Estimated overcoverage rate: **~25.7%**
- True overcoverage rate (benchmark): **~4.8%**

👉 👉 The method is **conservative**, tending to **underestimate the true resident population** due to missing activity signals.

---

### Model Performance

| Metric        | Value |
|--------------|------|
| Accuracy     | 0.775 |
| Precision    | 0.989 |
| Recall       | 0.772 |
| Specificity  | 0.825 |

👉 High precision indicates reliable identification of active residents,  
while lower recall reflects missing activity signals.

---

### Methodological Limitations

- Activity signals may be incomplete or delayed  
- Certain population groups (e.g. children, elderly) are systematically underrepresented  
- Rule-based classification does not capture all behavioural patterns  

👉 These limitations are typical for register-based approaches and highlight the need for methodological refinement.

---

### Demographic Insights

- Very high estimated overcoverage among:
  - **young children (0–5)**
  - **elderly populations (65+)**
- Lower overcoverage among working-age groups

👉 This reflects realistic patterns in register-based statistics.

---

## 🛠 Technologies Used

- **R**
  - dplyr, tidyr — data manipulation  
  - readr — data I/O  
  - lubridate — date handling  
  - ggplot2 — visualization  
  - janitor — data cleaning  

---

## ▶️ How to Run

Run the pipeline step-by-step:

```r
source("R/01_generate_synthetic_registers.R")
source("R/02_clean_and_validate_registers.R")
source("R/03_integrate_activity_signals.R")
source("R/04_estimate_population_stock.R")
source("R/05_visualize_results.R")
```

Outputs will be generated in:

- data/ (intermediate datasets)
- output/tables/ (aggregated indicators)
- output/figures/ (visualizations)

## 📊 Example Outputs

The repository includes:

- aggregated population estimates by region and age group
- estimation quality metrics
- publication-style visualizations

All results are fully reproducible from synthetic data.

## 🔭 Possible Extensions

- probabilistic record linkage
- machine learning classification of residents
- time-series population estimation
- integration with census benchmarks
- advanced demographic modelling

## 📘 License

MIT License

## 👤 Author

**Golib Sanaev**
Applied Data Scientist | Official Statistics | Econometrics

**GitHub:** https://github.com/gsanaev  
**Email:** gsanaev80@gmail.com  
**LinkedIn:** https://www.linkedin.com/in/golib-sanaev/  

This project was developed as a portfolio demonstration of applied methods in official statistics, with a focus on register-based population estimation.