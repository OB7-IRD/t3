# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.9.0] - 2020-06-05
### Added
- Implentation of the model layer in R6class
- Implementation of all logbooks standardization sub-processes
- Implementation of all samples standardization sub-processes
- Implementation of sub-processes for assessing species composition in relation to non-sampled assemblages
  - Strict selection of samples used in the model training according to several criteria on sets and wells: school type, location, date, sample quality.      
  - Classification of unknown school type (no data available) by clustering according to major tuna species composition.      
  - Development of a statistical model accounting for:
      - Spatial and temporal structure of the fishery: location and date of the catch
      - Vessel specificity which impacts the species composition of the catch
      - Crew reporting information which enables more flexibility in the model when a marginal composition is caught
      - Several years of fishing in the training to increase the robustness of the estimates
  - Model checking to ensure reliability of the results
  - Computation of a confidence interval (bootstrap method) which give the trust     level on catch estimations. 
