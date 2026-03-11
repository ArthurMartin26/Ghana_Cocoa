# Ghana CSSVD Reform & Women’s Economic Outcomes (DHS 2014 vs 2022/23)

**Project title:** *Did the October 2014 shift in CSSVD treatment financing and implementation alter women’s economic outcomes more in cocoa districts with higher baseline CSSVD exposure?*

This repository contains the **analysis code and reproducible workflow** for an MSc dissertation examining how a major institutional change in Cocoa Swollen Shoot Virus Disease (CSSVD) management in Ghana affected **women’s labour market outcomes and economic agency**, with impacts expected to be larger in **cocoa-dependent districts** with **higher baseline CSSVD outbreak exposure**.

---

## 1. Motivation (Why this matters)

Cocoa is one of Ghana’s most economically important agricultural sectors and a major source of rural livelihoods. Cocoa Swollen Shoot Virus Disease (CSSVD) is a severe and persistent threat: control relies primarily on **surveying/diagnosis**, **cutting-out infected trees (and surrounding trees)**, and **replanting**—which induces **immediate output losses** and a **multi-year production gap** until new trees mature. Because women are often overrepresented in **informal, low-paid, and vulnerable rural labour market segments**, prolonged shocks to cocoa production and local labour demand can translate into **gendered** welfare and agency impacts.

---

## 2. Policy shock (October 2014 reform)

From 1985 to October 2014, CSSVD-affected farms were treated **free of charge** through COCOBOD/Government support (including labour and inputs for cutting-out and replanting), and affected farmers received **cash compensation** in instalments.

In **October 2014**, this policy was **halted**, and the responsibility and costs of cutting-out and replanting were shifted to farmers **without compensation**. This reform plausibly increased the **private cost burden** of disease management and amplified the economic consequences of CSSVD in heavily infected areas.

---

## 3. Research question & hypothesis

### Research question
**Did the October 2014 shift in CSSVD treatment financing and implementation alter women’s economic outcomes more in cocoa districts with higher baseline CSSVD exposure?**

### Core hypothesis
The 2014 reform increased the **magnitude and duration** of income and liquidity shocks borne by cocoa households in high-burden districts, generating **gendered labour market adjustments** and changes in women’s economic agency—especially under rural credit constraints.

---

## 4. Identification strategy (Two-period DiD-in-intensity / Shift-share)

The empirical design uses **two DHS waves**:
- **Ghana DHS 2014** (pre-reform baseline)
- **Ghana DHS 2022/23** (post-reform)

The key variation is **cross-district heterogeneity** in **baseline CSSVD exposure** measured in 2014. The main model is a **two-period difference-in-differences in intensity** (equivalently a **two-period shift-share** design):


### Cocoa dependence conditioning (to sharpen interpretation)
To interpret effects in **cocoa-relevant local economies**, the design conditions on cocoa exposure using a cocoa footprint measure:
- **Trase district-level cocoa plantation area (hectares)**, optionally converted to **cocoa density** (hectares per district area),
and/or interacts baseline risk with cocoa exposure.

### Inference
Standard errors are clustered at an appropriate geographic level (district or cluster), reflecting correlated shocks within areas.

> **Note:** With only two DHS waves, direct pre-trend testing is not feasible. Credibility is strengthened by: predetermined baseline exposure, fixed effects, cocoa-intensity restrictions, alternative exposure definitions, and placebo/falsification checks where feasible.

---

## 5. Outcomes & mechanisms

### Primary outcomes (women)
From DHS women’s files (comparably defined across waves where possible):
- Current work status (employed/not employed)
- Employment type/sector (where comparable)
- Indicators related to economic agency / empowerment (e.g., decision-making modules when harmonisable)

### Secondary outcomes (household / child)
- Household asset/wealth proxies
- Selected child welfare measures (e.g., nutrition indicators), as feasible and consistent across waves

### Mechanisms (testable channels)
1. **Income & liquidity:** cutting-out and replanting implies immediate output loss and multi-year maturation gaps; post-2014 cost shifting likely magnifies constraints.
2. **Local labour demand & reallocation:** reduced cocoa output can compress local labour opportunities and shift household labour allocation, affecting women’s paid work and returns in informal labour markets.
3. **Implementation discontinuities:** funding gaps and farmer opposition can prolong disease pressure, extending the duration of economic strain in high-burden areas.

---

## 6. Data sources (high-level)

This analysis uses:
- **Ghana DHS 2014** microdata
- **Ghana DHS 2022/23** microdata
- DHS **GPS cluster** files (for spatial linkage)
- Ghana **ADM2/district boundary** polygons (for point-in-polygon assignment)
- **Baseline CSSVD exposure (2014)** from outbreak intensity information used in the dissertation’s key reference (Amon Armah et al., 2021): high outbreak defined as **>40% of cocoa area infected** vs low outbreak **<40%**
- **Cocoa exposure** from **Trase** cocoa plantation area aggregated to districts (hectares), optionally converted to density

> **Data access note:** DHS data require registration/permission. Please obtain data via official DHS channels. This repo does not distribute DHS microdata.

---

## 7. Workflow overview (replication steps)

1. **Harmonise DHS waves**
   - Align variable codings for outcomes and controls
   - Construct comparable indices (where needed and justified)

2. **Spatial linkage**
   - Assign DHS clusters to ADM2 districts using **point-in-polygon**
   - Robustness: buffered assignment to address DHS GPS displacement

3. **Construct exposure variables**
   - CSSVD risk: binary high/low and/or continuous intensity (2014 baseline)
   - Cocoa exposure: Trase hectares and/or cocoa density

4. **Merge exposures onto DHS**
   - Merge district-level exposure measures to individual outcomes by district IDs

5. **Estimate models**
   - Two-period DiD-in-intensity with district + wave fixed effects
   - Clustered standard errors
   - Robustness: cocoa-intensive subsamples, alternative exposure definitions, placebo tests

---

## 8. Repository structure & data policy

This repository contains **code and documentation only**. The following folders are intentionally excluded from version control via `.gitignore`:

- `Data/`  
  Raw and cleaned datasets (e.g., DHS, spatial files). Large and/or confidential.
- `Documents/`  
  Draft chapters, notes, literature PDFs, and working documents.
- `Outputs/`  
  Generated tables, figures, maps, and regression outputs.

