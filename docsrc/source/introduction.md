
# Introduction

The mission of the Observatory of Exploited Tropical Pelagic Ecosystems (Ob7) of the Institute of Research for Development (IRD) is to ensure the statistical monitoring of the tropical tuna seine fishery. and cane in the Atlantic and Indian Oceans. 
This monitoring consists of a continuous collection of data in the field, on the one hand, and a series of statistical treatments applied to these data in order to make them usable for expert questions (e.g. stock assessment) and for more academic research related to IRD's research themes (e.g. biodiversity conservation) on the other hand. 
Once validated, These data constitute France's official statistics and are communicated each year to the international tuna commissions in charge of managing tropical tuna stocks: the International Commission for the Conservation of Atlantic Tunas (ICCAT) and the Indian Ocean Tuna Commission (IOTC).
Several types of information are collected for this purpose:
- Logbooks: These give information on the total weight caught, by commercial categories (species and size class). The information provides almost exhaustive but approximate coverage, especially concerning specific composition.
- Landing slips: These give information on the total weight sold to canneries, by species and commercial categories. The tonnages collected are weighed.
- Well plans (or loading): Documents allowing correspondence to be made between wells and geolocalised fishing shots. They enable the well sampling plan to be established by selecting samples from the same stratum (as defined by (Pallarés et Hallier, 1998),
- Landing surveys (or samples): Conducted at the main landing ports (Abidjan, Dakar, Victoria and Antsiranana), they cover all landings and consist of sampling of specific size and composition carried out on certain wells of the tuna vessels during the landings.  They provide reliable information but only on a fraction of the landings.
- Estimates of quantities of fish destined for the local market (informal market).

Then, we cross-reference these data to deduce :

- An exhaustive coverage at the scale of the year and of the national fleet, geolocalised and "adjusted" in tonnage (i.e. adjusted to the landing weights) and in specific composition (i.e. corrected in applying the specific composition derived from the surveys to ports),
- Effort, expressed as search time, using relationships between set time and weight caught which are derived from data collected at sea by on-board observers,
- Size structures at the scale of total catches, using morphometric relationships that are derived from biological sampling carried out mainly at the Abidjan and Victoria canneries.


The original methodology developed in the Fortran language was previously described (Pallarés and Petit, 1998). The lack of a dedicated detailed documentation of the treatments performed was one of the major drawbacks. Reverse engineering work was carried out at the beginning of 2010 with the aim of replacing the tool with a documented program in a modern language, Java. A new version was developed reproducing the same methodology based on large sample areas, relying on T3 on the documentation of reverse engineering (Cauquil et al., 2018). However, the version 2 of T3 could not succeed completely because it did not resolve all the biases of the first version. Based on these facts, and in order to provide a functional version, we decided to completely review the program code.

The new version of T3 process is developed in R language and available through a package. One of the advantages of this language is that it is more understandable and manipulated by the fisheries’ scientific community rather than Java language. Moreover, this development follows the principles of the open and reproducible science: 

- Process transparency: the code is fully documented and the documentation of how works the program will be available.
- Open access and Open source: the code source is hosted on a github repository (https://github.com/OB7-IRD/t3) under a GPL version 3 license. In the end, it would be available on the Comprehensive R Archive Network (CRAN).
- Referencing of the software : enable result reproducibility and versioning  (Depetris et al., 2020)  : version 0.9.0 - [DOI: 10.5281/zenodo.3878125](https://doi.org/10.5281/zenodo.3878125).


## References

- (Cauquil et al., 2018):
- (Depetris et al., 2020): 
- (Pallarés et Hallier, 1998): 