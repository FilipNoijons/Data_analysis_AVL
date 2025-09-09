11636 Tumor & Bodyweight Analysis

Author: Filip Noijons
Date: r format(Sys.Date())

This repository contains the full workflow and results for study 11636, investigating tumor growth and bodyweight changes in mice treated with:

Control

Tamoxifen

Dexamethasone

Tamoxifen + Dexamethasone

The workflow is scripted in RMarkdown, ensuring full reproducibility. This README narrates every step of the analysis, with tables and figures embedded.

⚙️ Setup

Data loaded from Excel sheets (tumor_mouse_and_group_means.xlsx, bodyweight_mouse_and_group_means.xlsx)

Helper functions for binning days and group mapping

Color palette for consistent plotting

📊 Tumor Analysis
1. Data loading

Tumor volumes per mouse per day were imported.

Preview (first 8 rows):

<Preview: tumor per mouse per day>

2. Quality control

Each mouse was checked for:

Number of measurement points

Minimum/maximum tumor size

Exclusion recommendation

QC table:

<Simplified tumor QC table per mouse>

3. Individual tumor growth

Each treatment group’s mice plotted separately:

Control


Tamoxifen


Dexamethasone


Tamoxifen + Dexamethasone


4. Exclusion of short follow-up

Mice with <30 days of follow-up were excluded.

Duration per mouse (excluded flagged):

<Mice with duration <30 days are excluded>

5. Group mean tumor growth

Unbinned (daily means ± SD):


Binned (3-day bins, day 0 separate):


6. Tumor burden (AUC)

AUC calculated using trapezoidal rule per mouse.

Preview (first 8 rows):

<Preview: AUC per mouse>


Tukey HSD post-hoc test:

<Tukey post-hoc test for pairwise group comparisons (AUC)>


Summary (mean ± SD per group):

<Summary of AUC per teatment group (mean ± SD, n mice)>


Boxplot with statistical comparisons:


⚖️ Bodyweight Analysis
1. Data loading

Bodyweight data per mouse per day.

Preview (first 8 rows):

<Preview: bodyweight per mouse per day>

2. Individual bodyweight trajectories

Control


Tamoxifen


Dexamethasone


Tamoxifen + Dexamethasone


3. Group mean bodyweight

Mean ± SD per day.

4. Maximal % bodyweight loss (waterfall)

For each mouse, maximal loss relative to baseline was calculated.

Nadir table:

<Nadir % bodyweight change per mouse>


Waterfall plot:


🧪 Statistical Analyses

ANOVA for AUC across all groups

Pairwise t-tests with Holm correction

Tukey HSD post-hoc test

All outputs are tabulated above.

📂 Repository Structure
├── 11636_Data_analysis.Rmd   # RMarkdown workflow
├── README.md                 # This summary
├── figures/                  # Saved plots
│   ├── tumor_growth_control.png
│   ├── tumor_growth_tamoxifen.png
│   ├── tumor_growth_dexamethasone.png
│   ├── tumor_growth_combo.png
│   ├── tumor_mean_unbinned.png
│   ├── tumor_mean_binned.png
│   ├── tumor_auc_boxplot.png
│   ├── bodyweight_control.png
│   ├── bodyweight_tamoxifen.png
│   ├── bodyweight_dexamethasone.png
│   ├── bodyweight_combo.png
│   ├── bodyweight_mean.png
│   └── bodyweight_waterfall.png
└── data/                     # Original Excel files

✅ Key Outcomes

QC flagged mice with insufficient data or short follow-up.

Group-wise differences observed in tumor growth patterns.

Bodyweight analysis revealed treatment-related changes.

Tumor burden (AUC) compared across groups with statistical tests.

This workflow ensures analysis is transparent, reproducible, and fully documented.

📌 When you knit your .Rmd with github_document, all the <...> placeholders will be replaced by the actual markdown tables.
