# How Health Insurance Premiums Vary by Age

An R project that looks at how ACA marketplace premiums change with age, and how they changed from 2014 to 2016, using the Health Insurance Marketplace data set from Kaggle.

**Report:** https://shaneghim.github.io/premiumsbyage/ (source: `interpretation.qmd`)

## What I did
- Joined the rate and plan files, and kept single-year ages 21 to 64, the "No Preference" tobacco category, and Bronze, Silver, Gold, and Platinum plans
- Averaged the quarterly rates in the file into one yearly premium per plan, rating area, and age, and checked that other choices (first, lowest, or highest quarter) changed the results very little
- Tested the $5,000 cutoff for implausible premiums and found it had no effect on the standard plans
- Compared the age curve (each age's average premium relative to age 21) overall, by metal level, and by year
- Repeated the year-to-year comparison with only the 331 plans offered in all three years

## Main findings
- Premiums are nearly flat through the mid-20s, rise slowly through the 30s, and climb steeply after 40. In 2016 the average premium at age 64 was about 2.9 times the age-21 premium, just under the ACA's 3:1 limit.
- The four metal levels follow nearly the same curve, but Platinum plans cost the most and Bronze the least at every age.
- Average premiums rose roughly 15% from 2014 to 2016 (age 21: $280 to $324, age 64: $825 to $946).
- The same 331 plans rose about 13.6%, so most of the increase was higher prices, not a change in which plans were offered.

## Files
- `R/01_load_data.R`: the full analysis script
- `interpretation.qmd`: the report source
- `index.html`: the rendered report
- `output/`: the charts and tables the script produces

## To rerun it
1. Download the data from https://www.kaggle.com/datasets/hhs/health-insurance-marketplace and put `Rate.csv` and `PlanAttributes.csv` in a `data/` folder.
2. Run the script. It needs the tidyverse, data.table, and janitor packages.
3. Render `interpretation.qmd` with Quarto.

## Limitations
Every plan counts equally because the data has no enrollment numbers. Premiums are regulated, so the age pattern reflects the rules as well as the underlying risk. The data covers 35 states, one tobacco category, and standard plans only, and 2014 to 2016 were the first years of the marketplace.
