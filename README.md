# NFL Drive Pressure Index Analysis 🏈📊

[![R Version](https://img.shields.io/badge/R-4.2%2B-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)  
[![nflfastR](https://img.shields.io/badge/nflfastR-4.5.0-2B8CBE)](https://www.nflfastr.com/)  
[![ML](https://img.shields.io/badge/ML-caret%2Fgbm%2FrandomForest-D70206)](https://www.tidymodels.org/)

This repository contains the R implementation of novel pressure indices (PIO3 & PID) for quantifying clutch moments in NFL games, as described in our research paper.  
All detailed results are available in the paper; this repo holds the analysis code only.

---

## 📦 Requirements

- **R** (>= 4.2.0)  
- **Packages**: `nflreadr`, `nflfastR`, `dplyr`, `ggplot2`, `pROC`, `caret`, `gbm`, `randomForest`, `scales`

```r
install.packages(c(
  "nflreadr", "nflfastR", "dplyr", "ggplot2",
  "pROC", "caret", "gbm", "randomForest", "scales"
))

## 🚀 Getting Started

git clone https://github.com/agnivibes/nfl-pressure-index.git
cd nfl-pressure-index

# Run the full analysis:
source("Pressure_Index_NFL.R")

## 📖 Paper Citation

Aich, A., Bhattacharjee, D., & Saikia, H. (2025). Under Pressure: Integrating Machine Learning to Quantify Clutch Moments in American Football. International Journal of Sports Science and Coaching.

@article{Aich2025UnderPressure,
  title   = {Under Pressure: Integrating Machine Learning to Quantify Clutch Moments in American Football},
  author  = {Aich, Agnideep and Bhattacharjee, Dibyojyoti and Saikia, Hemanta},
  journal = {International Journal of Sports Science and Coaching},
  year    = {2025},
  note    = {Manuscript under review}
}

## 📝 License

This project is licensed under the [MIT License](LICENSE).
