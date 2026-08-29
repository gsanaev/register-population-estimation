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

The core workflow integrates synthetic population, address, employment, tax and education registers and demonstrates how observable administrative evidence can be used to construct population estimates.

**Version 2.1** adds a separate educational-attainment extension. It demonstrates delivery validation, source-specific harmonisation, person-level evidence integration, longitudinal reconciliation and synthetic truth-based evaluation.

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

Dieses Projekt entwickelt einen reproduzierbaren, vollständig synthetischen Workflow zur **registerbasierten Bevölkerungsschätzung** und erweitert ihn um ein separates Modul zur **Integration von Bildungsstandsdaten**.

Mehrere administrative Datenquellen werden miteinander verknüpft:

- Bevölkerungsregister
- Anschriftenregister
- Beschäftigungsdaten
- Steuerdaten
- Bildungsdaten

Im Kernworkflow werden Aktivitätssignale („Lebenszeichen“) und Anschrifteninformationen kombiniert. Ein fehlendes Aktivitätssignal wird **nicht automatisch als Hinweis auf einen fehlenden Wohnsitz interpretiert**. Stattdessen werden Fälle mit erhöhter Unsicherheit transparent identifiziert.

Die Erweiterung zum Bildungsstand verarbeitet heterogene synthetische Datenlieferungen, prüft ihre Qualität, harmonisiert unterschiedliche Merkmalsausprägungen und führt Informationen auf Personenebene über Quellen und Berichtsjahre hinweg zusammen. Fehlende Bildungsangaben werden **nicht als niedriger Bildungsstand interpretiert**; widersprüchliche Angaben werden als Klärungsfälle ausgewiesen.

### Bezug zur amtlichen Statistik

Der methodische Schwerpunkt liegt auf Aufgaben, die für die amtliche Statistik relevant sind:

- Datenübernahme und Validierung
- Qualitätssicherung
- Harmonisierung heterogener Merkmale
- personbezogene Datenintegration
- Fortschreibung über Berichtsjahre
- Plausibilisierung widersprüchlicher Angaben
- transparente und reproduzierbare Entscheidungsregeln

Das Projekt bildet **kein offizielles Verfahren des Statistischen Bundesamtes** und kein produktives Registerzensus-System nach. Daten, Regeln, Wahrscheinlichkeiten und Ergebnisse sind vollständig synthetisch und dienen ausschließlich der methodischen Demonstration.

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

The education register used in the core population workflow represents **current education participation / enrolment activity** and contributes an activity signal. It is distinct from the Version 2.1 educational-attainment extension described below.

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

## 🎓 Education Attainment Extension

Version 2.1 adds an optional subworkflow for integrating **educational-attainment evidence** from heterogeneous synthetic administrative data deliveries.

The extension is deliberately separate from the `education_register.csv` used in the core population workflow:

- the core education register represents **current participation / enrolment activity**
- the Version 2.1 module represents **educational attainment (Bildungsstand)**

### Processing Design

```text
Hidden synthetic education truth
        │
        ├── generation and final evaluation only
        │
        ▼
Heterogeneous synthetic deliveries
        │
        ├── Zensus-like 2022 evidence
        ├── BA-like 2024 evidence
        └── Mikrozensus-like 2024 evidence
        │
        ▼
Delivery validation and record-level QA
        │
        ▼
Source-specific harmonisation
to common attainment ranges
        │
        ▼
Same-year person-level reconciliation
        │
        ▼
Longitudinal 2022 → 2024 reconciliation
        │
        ▼
accepted / review_required / not_reported
        │
        ▼
Synthetic truth-based evaluation
```

### Synthetic Attainment Scale

The harmonised scale contains six ordered synthetic levels:

1. low or none
2. school qualification
3. vocational or post-secondary
4. bachelor or equivalent
5. master or equivalent
6. doctorate

This scale is deliberately simplified. It is **not an official Destatis or ISCED classification**.

Some source observations are precise, while others are represented as ranges.

Examples include:

- precise Bachelor evidence → `4–4`
- precise Master evidence → `5–5`
- broad higher-education evidence → `4–6`
- broad school/vocational evidence → `2–3`

This allows heterogeneous source information to be retained without imposing artificial precision.

### Delivery Quality Assurance

The validation stage checks:

- required delivery schemas
- missing person identifiers
- invalid reference years
- duplicate person/year records
- missing attainment information
- unknown source codes
- validity of harmonised attainment ranges

All delivered records remain in the audit-oriented harmonised dataset. Only records satisfying the operational QA rules enter the reconciliation stage.

Across the three synthetic deliveries:

| QA outcome | Records |
|---|---:|
| Delivered | 29,932 |
| Usable | 29,107 |
| Flagged | 297 |
| Rejected | 528 |

Flagged records retain missing-attainment cases for transparent QA reporting, while structurally invalid or unmapped observations are excluded from operational reconciliation.

### Reconciliation Rules

The workflow deliberately avoids an arbitrary source hierarchy.

Within the same reference year:

- a single usable observation is retained
- compatible observations are intersected to retain their common information
- disjoint observations become `same_year_conflict`
- contradictory observations are not widened into an artificial compromise range

Across reference years:

- compatible later evidence can confirm an established state
- compatible more precise evidence can refine a broader state
- entirely higher later evidence is treated as `upward_progression`
- entirely lower later evidence is treated as `temporal_regression`
- temporal regressions retain the earlier accepted state and are routed to review
- an earlier state is carried forward when no later observation is available
- missing evidence is not interpreted as evidence of low attainment

Persons without usable attainment evidence therefore receive:

```text
not_reported
```

rather than being assigned to the lowest educational-attainment category.

### Operational Target Population

The education module uses a target population derived from **observable register evidence**.

The operational population is based on `evidence_fallback_resident`, which uses population-register presence, activity evidence and consistent auxiliary addresses.

The clarification-assisted residence estimate is deliberately not used as an education-module input because the synthetic clarification simulation depends on hidden residence truth.

For the 2024 educational-attainment result, persons must additionally have a known age corresponding to **age 15 or older in 2024**.

This produces an operational education target population of **43,004 persons**.

### Reconciliation Results

| Final status | Persons |
|---|---:|
| Accepted | 23,347 |
| Review required | 64 |
| Not reported | 19,593 |
| **Total** | **43,004** |

The review-required cases consist of:

- **48** temporal regressions
- **11** same-year conflicts without an earlier accepted state
- **5** same-year conflicts where the earlier state is retained pending review

There are **16 same-year conflicts** in total.

The distinction between `accepted`, `review_required` and `not_reported` keeps substantive attainment information separate from data-quality and completeness problems.

### Synthetic Evaluation

Hidden education truth is introduced only after the operational result has been completed.

The operational target population and hidden 2024 education-truth population overlap for **40,773 persons**.

Population mismatch is evaluated separately from attainment quality and is not automatically counted as an attainment-classification error.

Among the **23,400 persons** in the overlap with a consolidated attainment range:

- **22,392** contain the hidden true level within the consolidated range
- range agreement is **95.69%**

Among **11,136 exact attainment states**:

- **10,277** match the hidden true level exactly
- exact-state agreement is **92.29%**

Accepted states achieve approximately **95.77% range agreement**.

Review-required cases with a retained attainment range show substantially lower agreement, indicating that the review rules identify observations with materially greater uncertainty.

The evaluation is a **synthetic consistency check**, not an estimate of real-world administrative-data quality.

---

## 📁 Repository Structure

```text
register-population-estimation/
├── R/
│   ├── 01_generate_synthetic_registers.R
│   ├── 02_clean_and_validate_registers.R
│   ├── 03_integrate_activity_signals.R
│   ├── 04_estimate_population_stock.R
│   ├── 05_visualize_results.R
│   └── education/
│       ├── education_helpers.R
│       ├── 01_generate_education_evidence.R
│       ├── 02_validate_harmonize_education.R
│       ├── 03_consolidate_education_attainment.R
│       └── 04_evaluate_education_attainment.R
│
├── data/
│   ├── raw/
│   ├── clean/
│   ├── processed/
│   └── education/
│       ├── raw/
│       ├── clean/
│       └── processed/
│
├── output/
│   ├── tables/
│   ├── figures/
│   └── education/
│       └── tables/
│
├── tests/
│   └── test_education_helpers.R
│
├── LICENSE
└── README.md
```

---

## 📄 Main Output Tables

### Population Workflow

The core workflow generates:

- `population_estimation_overall.csv`
- `population_estimation_by_region.csv`
- `population_estimation_by_age_group.csv`
- `estimation_quality_summary.csv`
- `clarification_summary.csv`

A person-level estimation dataset is written to:

- `data/processed/person_population_estimate.csv`

### Education-Attainment Extension

Delivery QA and reconciliation outputs include:

- `education_delivery_qa_summary.csv`
- `education_population_scope_summary.csv`
- `education_reconciliation_summary.csv`
- `data/education/processed/education_attainment_person_year.csv`
- `data/education/processed/education_attainment_2024.csv`

Synthetic evaluation outputs include:

- `education_population_alignment_summary.csv`
- `education_attainment_evaluation_summary.csv`
- `education_evaluation_by_status.csv`
- `education_evaluation_by_decision_reason.csv`

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

The education extension additionally uses base-R helper functions and scenario tests for explicit reconciliation behavior.

---

## ▶️ How to Run

Run the core population workflow sequentially from the repository root:

```bash
Rscript R/01_generate_synthetic_registers.R
Rscript R/02_clean_and_validate_registers.R
Rscript R/03_integrate_activity_signals.R
Rscript R/04_estimate_population_stock.R
Rscript R/05_visualize_results.R
```

Then run the optional education-attainment extension:

```bash
Rscript R/education/01_generate_education_evidence.R
Rscript R/education/02_validate_harmonize_education.R
Rscript R/education/03_consolidate_education_attainment.R
Rscript R/education/04_evaluate_education_attainment.R
```

Run the education helper tests with:

```bash
Rscript tests/test_education_helpers.R
```

Generated files are stored in:

- `data/raw/`
- `data/clean/`
- `data/processed/`
- `data/education/`
- `output/tables/`
- `output/figures/`
- `output/education/tables/`

Because the workflow uses fixed random seeds, the synthetic results are reproducible.

The education generator creates the synthetic source deliveries and hidden benchmark. Scripts 02 and 03 form the operational, truth-free processing workflow for delivery validation, harmonisation and attainment reconciliation. Hidden education truth is used only during synthetic data generation and in the separate evaluation stage.

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
- the educational-attainment scale is simplified and synthetic rather than an official ISCED or Destatis classification
- education-source mappings and delivery-error rates are illustrative assumptions
- the education module models attainment integration rather than the institutional infrastructure of an educational register
- the workflow does not reproduce the institutional, legal or production architecture of official German population statistics

The project should therefore be interpreted as an applied **data-integration, quality-assurance and estimation exercise** rather than as a replication of an official statistical procedure.

---

## 🔭 Possible Extensions

Potential methodological extensions include:

- sensitivity analysis for clarification-response and accuracy assumptions
- additional synthetic administrative data sources
- more complex temporal activity histories
- probabilistic or privacy-preserving record linkage
- longitudinal population-stock estimation
- additional educational-attainment reference years
- more detailed educational classifications
- alternative uncertainty and reconciliation rules

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
