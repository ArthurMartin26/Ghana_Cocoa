## Conceptual Note (Updated): CSSVD Reform, Poverty, “Conversion Constraints,” and Female Labour Supply (Ghana DHS 2014 vs 2022/23)

### Why this note exists 
This repo started with a question about **sectoral reallocation** in women’s work after the 2014 CSSVD financing reform. The analysis has now produced a clearer empirical pattern:

1) **Women’s employment increases** in high-CSSVD districts post-2014 (cocoa-restricted sample).  
2) **No strong sector/occupation reallocation** shows up in the shares among employed women (effects look extensive-margin / broad-based).  
3) **The employment increase is *not* larger in poorer districts.** If anything, the increase is **concentrated in less-poor districts**, with attenuation in poorer districts (binary split strongest; terciles suggest similar direction but less power).

This shifts the conceptual emphasis from “which sectors?” toward **“who can translate the shock into employment?”**—i.e., *conversion constraints* (opportunities, liquidity, and possibly norms).

---

## 1) Updated Core Question
**How does a negative income shock (amplified by the post-2014 CSSVD reform) translate into women’s employment—and why is the response stronger in less-poor districts?**

This is no longer just “do women work more?” or “do they move into sector X?”  
It’s: **which local economies can convert necessity into observed female employment**, and which cannot.

---

## 2) What the current reduced-form evidence suggests
### (A) Extensive-margin response dominates
- We see an increase in women’s employment in high-CSSVD districts post-2014.
- We do **not** see a clean, consistent shift in occupation/sector shares among working women.

**Interpretation:** households appear to activate women’s work as a coping margin where feasible, but the composition of available jobs likely constrains where entry occurs.

### (B) Poverty heterogeneity goes the “wrong way” for pure necessity
A simple “necessity” logic would predict stronger entry in poorer places. Instead:
- Poorer districts do **not** show larger employment responses.
- The response is **stronger in less-poor districts** (attenuation in poor districts).

**Implication:** poverty is not just “shock severity”; it also proxies **constraints on adjustment**.

---

## 3) Revised mechanism menu (what can generate “wealthier districts respond more”?)

### Mechanism 1: **Opportunity / market thickness constraints**
Poorer districts may have:
- thinner labour markets,
- fewer market-facing opportunities for women,
- worse infrastructure/market access,
- lower demand for casual services/trade.

Even if the income shock is larger, the *local economy* may not provide feasible female jobs.  
Prediction: **attenuated female employment response in poor districts**, without a clean sector shift.

### Mechanism 2: **Liquidity / fixed-cost barriers to labour entry**
Female labour entry often has fixed costs:
- transport,
- childcare substitution,
- working capital for petty trade,
- job search and networks.

Poor households/districts may be **credit constrained**, making it harder to pay entry costs even when income falls.  
Prediction: **employment rises more where households can finance entry**, i.e., less-poor districts.

### Mechanism 3 (still possible but harder to detect here): **Norms as conversion constraints**
Norms can bind at the participation decision (fixed stigma/mobility constraints). But at the ADM2 level:
- agency proxies may be noisy (high missingness),
- power is limited for triple interactions,
- decision-making measures may not match the binding constraint for “work outside”.

Prediction (if detectable): **CSSVD-induced employment increases should be smaller where baseline norm constraints are higher**.

---

## 4) Updated conceptual framework

### Baseline benchmark (no constraints)
In a frictionless model, a negative income shock should increase female labour supply (necessity effect).  
If we observe muted/attenuated responses in some areas, the relevant question becomes:

> **What prevents households from converting the shock into female employment?**

This motivates *conversion constraints* rather than only “norms” narrowly defined.

### Revised identification of constraints (relative to Field et al. 2021)
- Field et al. infer norm barriers when labour entry rises after **women’s control over income increases** (sign reversal logic).
- Here: infer constraints from **distorted or heterogeneous coping responses** under **negative income pressure**.

The empirical signature is not “sign reversal,” but:
- broad-based entry without sector reallocation, and/or
- responses concentrated where constraints are weaker (e.g., less-poor districts).

---

## 5) Empirical strategy (reminder, consistent with two DHS waves)
- Two-period ADM2 panel: 2014 vs 2022/23
- DiD-in-intensity (high vs low CSSVD baseline exposure) with ADM2 fixed effects
- Cocoa-restricted sample for interpretation (cocoa-relevant local economies)

### Heterogeneity logic
Because individuals cannot be followed over time in DHS:
- treat constraints as **baseline (pre-period) ADM2-level characteristics**
- test whether the CSSVD × post effect differs by:
  - baseline poverty (e.g., share of women in bottom wealth quintiles)
  - baseline constraint proxies (agency/norm indices; ideally multi-item)

---

## 6) What to prioritise next (practical roadmap)

### (A) Improve the “HH norms/agency” measurement (if we want to learn from it)
- Build a multi-item agency index (e.g., v739 + v743a/b/d where harmonisable).
- Reduce noise and missingness by averaging across items.
- Re-run:
  - `post × cssvd_flag × agency_index_2014`  
  - and a “horse race” with baseline poverty.

### (B) Move to men’s employment as the next margin
If women’s response is muted in poorer places, households may adjust through:
- men increasing work (added-worker at household level),
- or men switching sectors,
- or migration/temporary work (if measurable).

Next step:
- Construct ADM2-level men’s employment outcomes from the MR file (same spatial join).
- Run the same DiD and poverty heterogeneity tests.

---

## 7) Updated “mental anchor” 
> **This project now asks why some local economies (especially less-poor cocoa districts) can convert a negative income shock into increased female employment, while poorer districts show attenuated adjustment—consistent with opportunity, liquidity, and potentially norm-based conversion constraints.**

---

## 8) Caution for interpretation (updated)
- Do **not** claim individual norm change.
- Do **not** interpret DHS wealth quintiles as comparable across waves; treat them as within-wave relative position and use **baseline** definitions for heterogeneity.
- Frame agency/norm results as **environmental constraints** measured at baseline; ADM2 aggregation and missingness can attenuate effects.
- Be precise: “effects concentrated in less-poor districts” is safer than “no effect in poor districts” unless consistently shown.

---

## 9) What to check when reopening later 
1) **Core DiD stability**
   - Does `post × cssvd_flag` remain positive and significant under minor spec changes?

2) **Sector/occupation robustness**
   - Confirm that employment rises without systematic shifts in occupation shares.

3) **Poverty heterogeneity**
   - Compare median split vs terciles vs continuous slope; check whether heterogeneity is threshold-like.

4) **Constraint proxies**
   - Replace single-item agency with a multi-item index; report sample coverage (non-missing counts).

5) **Cocoa intensity confounding**
   - Check correlation of baseline poverty with cocoa hectares; optionally include `post × hectares` (and `post × cssvd × hectares`) as controls.

6) **Men’s employment**
   - Build MR-based ADM2 outcomes and test whether men’s labour responds where women’s does not.

---
