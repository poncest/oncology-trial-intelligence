# OCTID: Oncology Competitive Trial Intelligence Dashboard

> R Shiny dashboard consuming the ClinicalTrials.gov API v2 to analyze the active
> oncology competitive landscape — hypothesis-driven, governance-aware, and built
> for multi-persona decision support.

## Status

🚧 In development

## Overview

OCTID provides situational awareness of the oncology clinical trial landscape using
public ClinicalTrials.gov data. It is a benchmarking tool, not a forecasting engine.

This tool supports oncology portfolio design discussions by surfacing comparative
trial design patterns across sponsors and phases — connecting landscape data to
decision moments before portfolio reviews, launch strategy meetings, and business
development conversations.

**What it answers:**
- Where is trial activity concentrating by phase and modality?
- How do primary endpoints differ across sponsors?
- Who is accelerating pipeline activity?
- Where is enrollment concentrating geographically?

**What it does NOT do:**
- Predict trial success or failure
- Rank or score assets for investment
- Make treatment recommendations
- Estimate NPV or financial returns

## Tech Stack

R · Shiny · shiny.semantic · ggiraph · reactable · leaflet · httr2

## Data Source

[ClinicalTrials.gov](https://clinicaltrials.gov) API v2 — NIH/NLM registry of 500,000+ clinical studies.
Overview data cached daily via GitHub Actions. Trial detail fetched live on demand.

## Portfolio Context

Part of a pharmaceutical commercial analytics portfolio. Complements:
- [Launch Curve Forecaster](https://github.com/poncest/launch-curve-forecaster)
- [Clinical Trial Forecaster](https://github.com/poncest/clinical-trial-forecaster)
- [Pharma R&D Pipeline Simulator](https://github.com/poncest/pharma-rd-pipeline)

## Disclaimer

Portfolio project using publicly available data. Not for commercial use.

---

*Steven Ponce · 2026*

