# 📊 Register-Based Population Estimation with Activity and Address Evidence

A reproducible R workflow for exploring population estimation from multiple synthetic administrative data sources.

![made-with-R](https://img.shields.io/badge/Made%20with-R-276DC2.svg)
![license](https://img.shields.io/badge/license-MIT-green.svg)

---

## 🚀 Project Overview

This project develops a fully synthetic methodological workflow for **register-based population estimation**.

It is motivated by typical challenges in administrative-data-based population statistics, including:

- outdated or incomplete population-register records
- population-register undercoverage and overcoverage
- persons appearing in auxiliary administrative sources but not in the population register
- inconsistent address information across data sources
- incomplete activity signals
- the need to distinguish data-quality problems from residence-status uncertainty

The workflow integrates synthetic population, address, employment, tax and education registers and demonstrates how observable administrative evidence can be used to construct population estimates.

The project is **not an implementation of an official Destatis production system**. All data, probabilities, clarification mechanisms and results are synthetic and are intended for methodological demonstration only.

---

## 🧠 Methodological Concept

The workflow combines three types of observable evidence:

1. **Population-register evidence**
   - whether a person is registered
   - registered address and demographic information

2. **Activity evidence**
   - employment activity
   - tax filing activity
   - education enrolment

3. **Address evidence**
   - source-specific administrative contact addresses
   - agreement or disagreement across auxiliary sources
   - comparison with the population-register address

Activity signals can be interpreted as synthetic **Lebenszeichen**: indications that a person appears in another administrative source.

However, the workflow deliberately avoids the rule:

> no activity signal = non-resident

Instead, missing or conflicting evidence is treated as **uncertainty that may require additional clarification**.

---

## 🇩🇪 Kurzbeschreibung

Dieses Projekt entwickelt einen reproduzierbaren, vollständig synthetischen Workflow zur **registerbasierten Bevölkerungsschätzung**.

Mehrere administrative Datenquellen werden miteinander verknüpft:

- Bevölkerungsregister
- Anschriftenregister
- Beschäftigungsdaten
- Steuerdaten
- Bildungsdaten

Dabei werden sowohl Aktivitätssignale („Lebenszeichen“) als auch Anschrifteninformationen berücksichtigt.

Ein fehlendes Aktivitätssignal wird **nicht automatisch als Hinweis auf einen fehlenden Wohnsitz interpretiert**. Stattdessen werden unterschiedliche Evidenzarten kombiniert und Fälle mit erhöhter Unsicherheit für einen synthetischen Klärungsschritt identifiziert.

Das Projekt bildet **kein offizielles Verfahren des Statistischen Bundesamtes** nach. Daten, Wahrscheinlichkeiten und Klärungsmechanismen sind vollständig synthetisch und dienen ausschließlich der methodischen Demonstration.

---

## 🧪 Synthetic Data Architecture

The project starts from a hidden synthetic population benchmark and generates imperfect observable administrative registers from it.

### Hidden synthetic benchmark

The simulation contains:

- **50,000 true residents**
- **3,000 former/non-residents**
- **53,000 persons in the synthetic universe**
- **23,825 households**
- **18,000 addresses**
- **12 synthetic regions**

The hidden benchmark contains the true residence and coverage status of each synthetic person.

These truth variables are kept separate from the operational register-processing pipeline.

### Observable administrative sources

#### Population Register

Contains:

- person ID
- household and address ID
- region and municipality
- age and age group
- sex
- citizenship group
- registration status and dates

The synthetic population register deliberately contains both:

- **undercoverage** — true residents missing from the population register
- **overcoverage** — former/non-residents still present in the register

#### Address Register

Contains synthetic information about:

- address ID
- region
- municipality
- urbanicity
- address type

#### Employment Register

Contains:

- employment status
- days employed
- annual employment income
- reference date
- source-specific administrative contact address

#### Tax Register

Contains:

- tax filing indicator
- taxable income
- tax year
- source-specific administrative contact address

#### Education Register

Contains:

- enrolment indicator
- institution type
- source-specific administrative contact address

The auxiliary registers are generated independently from the synthetic truth rather than being restricted to persons already present in the population register.

This allows true undercoverage cases to appear in auxiliary sources.

---

## 🔄 Workflow Pipeline

```text
┌───────────────────────────────────┐
│ 01_generate_synthetic_registers.R │
└────────────────┬──────────────────┘
                 │
                 ▼
Synthetic benchmark + imperfect observable registers
                 │
                 ▼
┌───────────────────────────────────┐
│ 02_clean_and_validate_registers.R │
└────────────────┬──────────────────┘
                 │
                 ▼
Clean registers + neutral activity/address indicators
                 │
                 ▼
┌───────────────────────────────────┐
│  03_integrate_activity_signals.R  │
└────────────────┬──────────────────┘
                 │
                 ▼
Person-level and address-level evidence
                 │
                 ▼
┌───────────────────────────────────┐
│  04_estimate_population_stock.R   │
└────────────────┬──────────────────┘
                 │
                 ▼
Baseline, evidence and clarification-assisted estimates
                 │
                 ▼
┌───────────────────────────────────┐
│      05_visualize_results.R       │
└───────────────────────────────────┘
```

---

## 🔍 Workflow Stages

### 1. Synthetic Data Generation

The first script creates:

- the hidden synthetic benchmark
- address register
- imperfect population register
- employment register
- tax register
- education register

Controlled imperfections include missing values, stale registrations, undercoverage, residual auxiliary activity and inconsistent source-specific addresses.

Simulation probabilities are illustrative methodological assumptions rather than empirical estimates for Germany.

### 2. Cleaning and Validation

The second script performs:

- key uniqueness checks
- referential-integrity checks
- plausibility checks
- register-specific cleaning
- address validation
- construction of neutral activity indicators
- identification of auxiliary-only persons

Importantly, hidden truth is not incorporated into the cleaned operational datasets.

### 3. Multi-Source Evidence Integration

The third script constructs an analysis-ready person-level evidence dataset.

It combines:

- population-register presence
- activity signals
- auxiliary-source presence
- contact-address agreement
- contact-address conflicts
- population-register versus auxiliary-address disagreement

It also creates an **analytical geography** for aggregation.

For registered persons, this comes from the population register.

For auxiliary-only persons, it is used only when auxiliary sources provide a consistent address.

An analytical address is not interpreted as verified residence.

### 4. Population Estimation and Synthetic Clarification

Three estimation approaches are compared.

#### Population-register baseline

Every person in the population register is counted as resident.

#### Evidence fallback

The population-register baseline is supplemented with auxiliary-only persons who have:

- at least one positive activity signal, and
- a consistent auxiliary address.

#### Clarification-assisted estimate

Cases with increased residence-status uncertainty are selected using observable evidence.

Residence clarification targets include:

- registered persons aged 18–64 without positive auxiliary activity
- registered persons with unknown age and no positive activity
- auxiliary-only persons

A synthetic clarification process is then simulated.

The clarification mechanism uses illustrative assumptions of:

- **90% response probability**
- **98% clarification-result accuracy**

These parameters are simulation assumptions and are not empirical estimates.

Address-quality cases and residence-status clarification are treated separately.

---

## 📈 Key Results

### Synthetic Population Structure

The hidden benchmark contains:

- true population: **50,000**
- population-register records: **51,447**
- true population-register undercoverage: **956**
- true population-register overcoverage: **2,403**

Across all observable registers, **52,253 unique persons** are observed.

There are **806 auxiliary-only persons**, demonstrating that auxiliary sources can provide evidence about people not present in the population register.

A further **747 persons** are absent from every observable source, including **233 true residents**.

These completely unobserved residents represent residual undercoverage that cannot be recovered through person-level linkage of the available sources alone.

---

## 📊 Population-Stock Estimates

| Estimation approach | Estimated population | Error vs. true population |
|---|---:|---:|
| Population-register baseline | 51,447 | +1,447 |
| Evidence fallback | 52,193 | +2,193 |
| Clarification-assisted | 50,592 | +592 |
| Hidden synthetic benchmark | 50,000 | — |

The evidence fallback recovers many undercovered true residents but also introduces additional false positives.

The clarification-assisted approach substantially reduces overcoverage while retaining high recall.

---

## 🎯 Person-Level Estimation Quality

| Method | Accuracy | Precision | Recall | Specificity |
|---|---:|---:|---:|---:|
| Population-register baseline | 0.937 | 0.953 | 0.981 | 0.199 |
| Evidence fallback | 0.948 | 0.953 | 0.994 | 0.177 |
| Clarification-assisted | **0.977** | **0.982** | **0.994** | **0.699** |

The results illustrate an important distinction:

> Better recovery of undercoverage does not automatically imply a better aggregate population estimate.

The evidence fallback improves recall but increases false-positive inclusion.

Targeted clarification substantially improves specificity while preserving very high recall.

---

## 🔎 Residence Clarification

The final workflow identifies **5,708 residence-clarification targets**.

The largest target group consists of registered persons aged 18–64 with no positive activity signal.

Importantly, absence of activity is used only as a **clarification trigger**, not as direct evidence of non-residence.

Address inconsistencies are retained separately as data-quality and address-clarification issues.

---

## 🗺 Address Evidence

The Version 2 workflow explicitly models address information.

Administrative contact addresses may:

- agree with the population-register address
- disagree with it
- disagree across auxiliary sources
- be unavailable

This allows the project to distinguish:

- residence-status uncertainty
- address inconsistency
- register undercoverage
- register overcoverage

without assuming that any single administrative source contains the definitive answer.

---

## 📁 Repository Structure

```text
register-population-estimation/
├── R/
│   ├── 01_generate_synthetic_registers.R
│   ├── 02_clean_and_validate_registers.R
│   ├── 03_integrate_activity_signals.R
│   ├── 04_estimate_population_stock.R
│   └── 05_visualize_results.R
│
├── data/
│   ├── raw/
│   ├── clean/
│   └── processed/
│
├── output/
│   ├── tables/
│   └── figures/
│
├── LICENSE
└── README.md
```

---

## 📄 Main Output Tables

The workflow generates:

- `population_estimation_overall.csv`
- `population_estimation_by_region.csv`
- `population_estimation_by_age_group.csv`
- `estimation_quality_summary.csv`
- `clarification_summary.csv`

A person-level estimation dataset is also written to:

- `data/processed/person_population_estimate.csv`

---

## 📊 Main Figures

Version 2 produces:

- `activity_signal_distribution.png`
- `address_evidence_distribution.png`
- `population_estimates_overall.png`
- `estimation_error_by_region.png`
- `estimation_quality_by_method.png`
- `residence_clarification_targets.png`

---

## 🛠 Technologies Used

The project is implemented in **R**.

Main packages include:

- `dplyr` — data transformation and integration
- `readr` — data input/output
- `purrr` — functional operations used in synthetic data generation
- `janitor` — variable-name standardization and data cleaning
- `tidyr` — reshaping data for reporting
- `ggplot2` — visualization
- `scales` — plotting labels and formatting

---

## ▶️ How to Run

From the repository root, run the scripts sequentially:

```r
source("R/01_generate_synthetic_registers.R")
source("R/02_clean_and_validate_registers.R")
source("R/03_integrate_activity_signals.R")
source("R/04_estimate_population_stock.R")
source("R/05_visualize_results.R")
```

Alternatively, from a shell:

```bash
Rscript R/01_generate_synthetic_registers.R
Rscript R/02_clean_and_validate_registers.R
Rscript R/03_integrate_activity_signals.R
Rscript R/04_estimate_population_stock.R
Rscript R/05_visualize_results.R
```

Generated files are stored in:

- `data/raw/`
- `data/clean/`
- `data/processed/`
- `output/tables/`
- `output/figures/`

Because the workflow uses fixed random seeds, the synthetic results are reproducible.

---

## ⚠️ Methodological Scope and Limitations

This repository is a synthetic methodological demonstration.

Key limitations include:

- simulation parameters are illustrative rather than empirically estimated
- administrative sources are simplified representations of real systems
- person identifiers allow deterministic linkage
- no probabilistic record linkage is required
- clarification outcomes are simulated
- some true residents are absent from every observable data source
- auxiliary-only persons generally lack operational demographic information such as age
- the workflow does not reproduce the institutional, legal or production architecture of official German population statistics

The project should therefore be interpreted as an applied data-integration and estimation exercise rather than as a replication of an official statistical procedure.

---

## 🔭 Possible Extensions

Potential methodological extensions include:

- sensitivity analysis for clarification-response and accuracy assumptions
- additional synthetic administrative data sources
- more complex temporal activity histories
- probabilistic or privacy-preserving record linkage
- longitudinal population-stock estimation
- alternative uncertainty and clarification rules

---

## 📘 License

MIT License

---

## 👤 Author

**Golib Sanaev**<br>
Applied Data Scientist | Official Statistics | Econometrics

**GitHub:** https://github.com/gsanaev<br>
**Email:** gsanaev80@gmail.com<br>
**LinkedIn:** https://www.linkedin.com/in/golib-sanaev/

This project was developed as a portfolio demonstration of reproducible data integration, statistical validation and population-estimation methods using synthetic administrative data.
