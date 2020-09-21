# Release notes

The release notes provide more details for users moving to the new release, along
with a link to the full release change log.

## 0.9.1 
The release is available: [https://github.com/OB7-IRD/t3/releases/tag/0.9.1](https://github.com/OB7-IRD/t3/releases/tag/0.9.1)

Major changes :
- Computation of confidence interval (bootstrap interval) for the nominal catches by species and by fishing mode
- Improve time computing using faster implementation of random forests for the species composition modeling in the level3 
- Disabling task and effort computation while waiting for confident interval implementation


## 0.9.0 
The release is available: [https://github.com/OB7-IRD/t3/releases/tag/0.9.0](https://github.com/OB7-IRD/t3/releases/tag/0.9.0)

List of all functionalities available in the package:

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
  - Computation of a confidence interval (bootstrap method) which give the trust level on catch estimations. 

