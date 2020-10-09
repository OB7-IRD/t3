# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.9.1] - 2020-09-21
### Added
- Implementation for the import of annual dataset from outputs of the level 1 and 2 (period length customizable)
- Implementation of boostrap method as base for all confidence intervals
- Computation of confidence interval (bootstrap interval) for the nominal catches by species and by fishing mode
- Implementation of outputs for the he nominal catches by species and by fishing mode
- Fix minor bugs in all different processes (P1, P2, P3)

### Changed
- Improve modelling configuration (addition of new parameters)
- Rewrite several figures for the model checking
- Improve time computing using faster implementation of random forests for the species composition modeling in the level3 

### Removed
- Disabling task and effort computation while waiting for confident interval implementation
- Remove the documentation from the package. Stay available at https://ob7-ird.github.io/t3/index.html

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
