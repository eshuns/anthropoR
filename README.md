# anthropoR

`anthropoR` is a light R package for calculating **Body Mass Index (BMI)** and classifying **obesity** for adults and children. It handles multiple height and weight formats and automatically computes BMI if not provided.

## Installation

```r
# Install dependencies if needed
install.packages(c("pedbp", "devtools"))

# Install anthropoR from GitHub
devtools::install_github("eshuns/anthropoR")
```

## Quick Start Example

```r
library(anthropoR)

# Minimal example dataset
df <- data.frame(
  id = 1:5,
  sex = c(1,0,1,0,1),       # 1 = male, 0 = female
  age_years = c(25, 12, 35, 18, 8),
  weight_lb = c(180, 70, 200, 150, 50),
  height_in = c(70, 55, 72, 65, 48)
)

# Compute BMI
df <- computeBMI(
  df,
  weight_lb = "weight_lb",
  height_in = "height_in"
)

# Classify obesity
df <- isObese(
  df,
  bmi = "bmi",
  sex = "sex",
  age_years = "age_years"
)

# View results
df[, c("id","age_years","bmi","obese")]
```

## Features

* Handles weight in **pounds** or **kilograms**
* Handles height in **meters**, **inches**, or **feet + inches**
* Computes BMI automatically if missing
* Classifies **adults** (BMI ≥ 30) and **children/adolescents** (CDC BMI-for-age percentile ≥ 95)
* Handles missing values

## Contributing

Contributions are welcome! Please open an issue or submit a pull request on GitHub.
