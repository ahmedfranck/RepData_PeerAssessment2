# Reproducible Research: Peer Assessment 2

This repository contains a fully reproducible analysis of the NOAA Storm Database for the Coursera Reproducible Research course.

## Files

- `PA2_template.Rmd` — complete R Markdown source with all analysis code
- `PA2_template.md` — rendered Markdown report
- `PA2_template.html` — rendered HTML report
- `figure/` — two figures produced by the report

The source document starts from the raw compressed CSV file. When the dataset is not present locally, the R code downloads the assignment dataset before reading and processing it.

## Main findings

- Tornadoes have the greatest combined population-health impact, measured as fatalities plus injuries.
- Floods have the greatest combined reported property and crop damage.

The analysis preserves NOAA `EVTYPE` categories except for capitalization and surrounding whitespace. Economic values are nominal reported dollars and are not adjusted for inflation.
