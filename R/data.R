#' Referential conversion table LD1 to LF
#'
#' During the sampling process, fishes length can be collected and expressed in different standards.\cr
#' For example, regarding field constraints and more precisely the length of the different species,
#' sampling data covered in T3 can by express in first dorsal length (LD1) or curved fork length (LF).\cr
#' Generally, length of small individuals are provided in LF because it’s logistically possible and easier to measure the entire fish,
#' while length of bigger individuals are provided in LD1, for the same reciprocal reasons.\cr
#' To standardize this standard among sampling data and at the end have only length sampling data expressed in LF,
#' the process use this referential conversion table LD1 to LF.
#'
#' @format length_step: data frame with 3951 rows and 6 columns:
#' \describe{
#'   \item{ocean_code}{Ocean code, type \code{\link[base]{integer}}. For example \code{ocean_code=1} for the Atlantic Ocean and \code{ocean_code=2} the Indian Ocean}
#'   \item{species_code}{Species code, type \code{\link[base]{integer}}.}
#'   \item{species_fao_code}{Species FAO code, type \code{\link[base]{character}}.}
#'   \item{ld1_class}{First dorsal length (LD1) class (cm), type \code{\link[base]{double}}.}
#'   \item{lf_class}{Curved fork length (LF) class (cm), type \code{\link[base]{integer}}.}
#'   \item{ratio}{Percentage (\%) of samples to be allocated to the corresponding \code{lf_class}, type \code{\link[base]{double}}.
#'   For example, for the samples from the Atlantic Ocean (1), of the species YFT (*Thunnus albacares*), with a first  dorsal length class (LD1) measured at \code{ld1_class=8} (cm),
#'    a percentage of \code{ratio=50} \% will be assigned to the curved fork length class: \code{lf_class=32} (cm) and 50\% of these samples will obtain a \code{lf_class=34} (cm).}
#' }
#' @usage data(length_step, package="t3")
#' @details
#' This table are expressed and used through a stratification by ocean and specie.\cr
#' For one LD1 length class measured we can have several LF length classes associated, according to the distribution ratio.
#' That mean that in our data processed the number of sampling item could rise.\cr
#' Furthermore, tunas regional fisheries management organisations,
#' like International Commission for the Conservation of Atlantic Tunas (\href{https://www.iccat.int/}{ICCAT})
#'  or the Indian Ocean Tuna Commission (\href{https://iotc.org/}{IOTC}), have validated conversion factors for types of size measurements.
#' One of our next update will be to modify the process according theses formula.
#' @keywords conversion sample length class first dorsal length (LD1) curved fork length (LF)
"length_step"

#' Referential LWR (length weight relationship) table
#'
#' Conversion of the samples length measurements in weight by length weight relationship (LWR).\cr
#' Formula associated are the following one:\cr
#' \eqn{RWT=a \times LF^b}{RWT=a x LF^b} where:
#' \describe{
#'   \item{RWT}{is the round weight (kg)}
#'   \item{LF }{is the curved fork length (cm)}
#'   \item{a, b}{parameters from the reference table, dependent of of the species and potentially of the area (ocean or others) and the season.}
#' }
#'
#' @format length_weight_relationship: data frame with 40 rows and 7 columns:
#' \describe{
#'   \item{ocean_code}{Ocean code, type \code{\link[base]{integer}}. For example \code{ocean_code=1} for the Atlantic Ocean and \code{ocean_code=2} the Indian Ocean}
#'   \item{ocean_label}{Ocean label ("Atlantic" or "Indian"), type \code{\link[base]{character}}.}
#'   \item{species_code}{Species code, type \code{\link[base]{integer}}.}
#'   \item{species_fao_code}{Species FAO code, type \code{\link[base]{character}}.}
#'   \item{length_weight_formula}{LWR formula form, type \code{\link[base]{character}}.}
#'   \item{lwr_a}{Parameter a of LWR formula, type \code{\link[base]{double}}.}
#'   \item{lwr_b}{Parameter b of LWR formula, type \code{\link[base]{double}}.}
#' }
#' @usage data(length_weight_relationship, package="t3")
#' @details More detail information could be find on the regional fisheries management organisations (RFMOs) like \href{https://www.iccat.int/}{ICCAT} or \href{https://iotc.org/}{IOTC}.
#' @keywords length weight relationship
"length_weight_relationship"

#' Referential set duration table
#'
#' The set duration (min) is calculated from catch weight (t), according to a linear function with two parameters a and b:\cr
#' \eqn{set_duration = a \times catch_weight + b}{set_duration = a x catch_weight + b}.\cr
#' These are found through this reference table, for each year, ocean, fishing school and country.\cr
#' Furthermore, a specific value provided for a null set (a fishing set without any catches associated).\cr
#'
#' @format set_duration_ref: data frame with 819 rows and 9 columns:
#' \describe{
#'  \item{year}{Year, type \code{\link[base]{integer}}.}
#'   \item{flag_code}{Country code, type \code{\link[base]{integer}}.}
#'   \item{flag_code_iso_3}{3 letter ISO country codes, type \code{\link[base]{character}}.}
#'   \item{ocean_code}{Ocean code, type \code{\link[base]{integer}}. For example \code{ocean_code=1} for the Atlantic Ocean and \code{ocean_code=2} the Indian Ocean}
#'   \item{school_type_code_avdth}{School type code in AVDTH referential template, type \code{\link[base]{integer}}, (1 for floating object school, 2 for free school and 3 for undetermined school).}
#'   \item{school_type_code_observe}{School type code in Observe referential template, type \code{\link[base]{integer}}, (1 for floating object school, 2 for free school and 0 for undetermined school).}
#'   \item{parameter_a}{Slope of the linear function giving the duration of the set as a function of the catch weight, type \code{\link[base]{double}}.}
#'   \item{parameter_b}{Intercept of the linear function giving the duration of the set as a function of the catch weight, type \code{\link[base]{double}}.}
#'   \item{null_set_value}{Specific duration value (min) provided for a null set (a fishing set without any catches associated), type \code{\link[base]{double}}.}
#' }
#' @usage data(set_duration_ref, package="t3")
#' @keywords set duration
"set_duration_ref"

