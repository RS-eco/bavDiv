Species richness changes in Bavaria, Germany - Differences between
protected and non-protected areas
================

<!-- 

See ggcoefstats for nice forest plot with statistical details!!!
=> see also emmeans examples in ggstatsplot, search github repo!!!

- Sind die richness change Ergebnisse korrigiert für den observation effort?
- Ich denke, es wäre wichtig, die IUCN-Kategorien zu vermeiden und die deutschen Kategorien zu nutzen. Das könnte dann zwar etwas kleinteilig werden, aber auch aufschlussreich sein. Intuitiv würde ich auf jeden Fall die Landschaftsschutzgebiete (LSG) getrennt behandeln; außerdem würde ich FFH-/Vogelschutzgebiete als gemeinsame Kategorie nehmen, einmal mit Nationalparks und NSGs zusammen und einmal getrennt (wobei die oft auch beides sind). Schau aber mal, welche Kategorien es noch gibt und wir können dann mal überlegen, wie wir daraus eine sinnvolle Klassifizierung machen können…
- Was sind die non-designated areas?


- Add water to overall model???
- Show AIC, Adjusted R2, p-values for models! (Table 1)
- Add Main Figure on difference across Naturräume!

- Figure S4: Remove colour legend, split into two columns!
- Figure S5: Add 0 as grey background + Add outline of Bavaria! + Increase colour legend!

- Sort code into correct order and split into different Figures/Supp Figures, see Reptile1.5

- Use ggstatsplot/ggdist for Figures?

- Run multiple models (different variable combinations)
=> Create table with models against variables (p-values), Adj. R2 and AIC
=> Keep month in final model or not? (+ Filter out month == 0)

- Add water presence/lake variable to all models (significant for Odonata?)

- Check out Schafft et al. 2021 for Forest plot with mean + ci + effect as table on the side!

- Suppl: Add scale comparison (Tk4tel vs. TK25) (Ref: Chase et al. 2019 - Oikos)
- Suppl: Add time comparison => Not just upscaling, but smoothing (grainchanger)

Potential ideas:
- Change to Bayesian/Regression tree/BART model (Statistical Rethinking, stan_glmer, stan_lmer, brms: https://www.rensvandeschoot.com/tutorials/generalised-linear-models-with-brms/, ...)
- Look at non-linear relationships, plot model output/summary
- Tips on mixed effects models: Silk et al. 2020 - PeerJ
-->
<!--
**Running head:** Protected areas help to maintain Bavarian biodiversity

**Main message (in 25 words or less):** ...

## Working abstract:

**Context and aim:** Biodiversity is declining globally, but we still lack precise knowledge on regional biodiversity changes and their drivers. Here we assess a unique species occurrence data set for Bavaria and analyse species richness changes over time across four taxa (Aves, Odonata, Orthoptera and Lepidoptera). We assess how changes in species richness vary with protection to see if protected areas provide a save haven for biodiversity.

**Core results:** ...

**Interpretation in context:** ...

**Application:** ...

## Introduction

## Methods

### Species data

*  ASK data of 4 taxa (Aves, Lepidoptera, Odonata & Orthoptera)
*  Time period: 1985 - 2019

### Protected area data

* ProtectedPlanet data for Germany cropped by extent of Bavaria

### Data Analysis

*  Linear mixed effects model

*  We have dropped all data where month = 0. 
Need to consider this if month is included in the model!

## Results

## Discussion

## References
-->

## Tables

**Table 1.** Model performance (BIC) of different variable combinations
for each taxon (Aves, Lepidoptera, Odonata, Orthoptera) and all taxa
together (Total).

| formula                                                                                |     Aves | Lepidoptera |  Odonata | Orthoptera |   Total |
|:---------------------------------------------------------------------------------------|---------:|------------:|---------:|-----------:|--------:|
| sum\_art \~ jahr + (1 \| KARTE\_QUAD)                                                  | 614978.3 |     2970456 | 309624.5 |   303325.1 | 5314103 |
| sum\_art \~ perc\_cov + (1 \| KARTE\_QUAD)                                             | 624223.5 |     2995634 | 309734.8 |   303816.4 | 5329479 |
| sum\_art \~ jahr + monat + (1 \| KARTE\_QUAD)                                          | 613919.4 |     2968189 | 308449.0 |   292242.1 | 5281111 |
| sum\_art \~ jahr + perc\_cov + (1 \| KARTE\_QUAD)                                      | 614980.6 |     2970456 | 309583.2 |   303328.1 | 5314114 |
| sum\_art \~ jahr + monat + perc\_cov + (1 \| KARTE\_QUAD)                              | 613922.0 |     2968190 | 308407.8 |   292249.3 | 5281122 |
| sum\_art \~ jahr + monat + perc\_cov + naturraum + (1 \| KARTE\_QUAD)                  | 613996.1 |     2968169 | 308394.8 |   292170.9 | 5281113 |
| sum\_art \~ jahr + monat + perc\_cov + jahr:perc\_cov + (1 \| KARTE\_QUAD)             | 613060.5 |     2966406 | 308182.1 |   292243.1 | 5277582 |
| sum\_art \~ jahr + monat + perc\_cov + naturraum + jahr:perc\_cov + (1 \| KARTE\_QUAD) | 613133.3 |     2966386 | 308169.2 |   292164.8 | 5277575 |

**Table 2.** Model variance (R2.var.fixed) of different variable
combinations for each taxon (Aves, Lepidoptera, Odonata, Orthoptera) and
all taxa together (Total).

| formula                                                                                |  Aves | Lepidoptera | Odonata | Orthoptera | Total |
|:---------------------------------------------------------------------------------------|------:|------------:|--------:|-----------:|------:|
| sum\_art \~ jahr + (1 \| KARTE\_QUAD)                                                  | 0.031 |       0.011 |   0.001 |      0.002 | 0.004 |
| sum\_art \~ perc\_cov + (1 \| KARTE\_QUAD)                                             | 0.003 |       0.003 |   0.008 |      0.001 | 0.000 |
| sum\_art \~ jahr + monat + (1 \| KARTE\_QUAD)                                          | 0.034 |       0.012 |   0.005 |      0.052 | 0.013 |
| sum\_art \~ jahr + perc\_cov + (1 \| KARTE\_QUAD)                                      | 0.033 |       0.014 |   0.008 |      0.002 | 0.004 |
| sum\_art \~ jahr + monat + perc\_cov + (1 \| KARTE\_QUAD)                              | 0.037 |       0.015 |   0.012 |      0.052 | 0.014 |
| sum\_art \~ jahr + monat + perc\_cov + naturraum + (1 \| KARTE\_QUAD)                  | 0.084 |       0.075 |   0.034 |      0.071 | 0.050 |
| sum\_art \~ jahr + monat + perc\_cov + jahr:perc\_cov + (1 \| KARTE\_QUAD)             | 0.037 |       0.015 |   0.014 |      0.052 | 0.014 |
| sum\_art \~ jahr + monat + perc\_cov + naturraum + jahr:perc\_cov + (1 \| KARTE\_QUAD) | 0.085 |       0.074 |   0.035 |      0.071 | 0.051 |

## Figures

![](figures/Figure1-1.png)

**Figure 1.** Modelled mean species richness over time for min, median
and max percentage cover protection split by taxonomic group (Aves,
Lepidoptera, Odonata, Orthoptera and Total).
<!-- Significances to labels??? -->

![](figures/Figure2-1.png)

**Figure 2.** Modelled mean species richness over time for min, median
and max percentage cover protection split by taxonomic group (Aves,
Lepidoptera, Odonata, Orthoptera and Total).
