% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardisedsampleset.R
\name{standardisedsampleset}
\alias{standardisedsampleset}
\title{R6 class standardisedsampleset}
\description{
Create R6 reference object class standardisedsampleset
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-standardisedsampleset-new}{\code{standardisedsampleset$new()}}
\item \href{#method-standardisedsampleset-clone}{\code{standardisedsampleset$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-standardisedsampleset-new"></a>}}
\if{latex}{\out{\hypertarget{method-standardisedsampleset-new}{}}}
\subsection{Method \code{new()}}{
Initialize function for R6 standardisedsampleset class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{standardisedsampleset$new(
  trip_id,
  activity_id,
  well_id,
  sample_id,
  sample_quality_code,
  sample_type_code,
  species_fao_code,
  sample_standardised_length_class_lf,
  sample_number_weighted,
  sample_weight,
  sample_weight_unit,
  sample_category
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{trip_id}}{Object of class {\link[base]{character}} expected. Trip identification.}

\item{\code{activity_id}}{Object of class {\link[base]{character}} expected. Activity identification.}

\item{\code{well_id}}{Object of class {\link[base]{character}} expected. Well identification.}

\item{\code{sample_id}}{Object of class {\link[base]{character}} expected. Sample identification.}

\item{\code{sample_quality_code}}{Object of class {\link[base]{integer}} expected. Sample quality identification.}

\item{\code{sample_type_code}}{Object of class {\link[base]{integer}} expected. Sample type identification.}

\item{\code{species_fao_code}}{Object of class {\link[base]{character}} expected. Specie code identification on 3 characters.}

\item{\code{sample_standardised_length_class_lf}}{Object of class {\link[base]{integer}} expected. Sample standardised length class length fork of measured individus.}

\item{\code{sample_number_weighted}}{Object of class {\link[base]{numeric}} expected. Sample number of measured individus extrapolated to all counted individus and weighted by set weight.}

\item{\code{sample_weight}}{Object of class {\link[base]{numeric}} expected. Weight (kg) of the sample number of measured individus extrapolated to all counted individus (conversion by length weight relationship: coeficient a * length class lf ^ coeficient b).}

\item{\code{sample_weight_unit}}{Object of class {\link[base]{numeric}} or NA expected. Weight (kg) of one individu calculated by the length weight relationship: coeficient a * length class lf ^ coeficient b).}

\item{\code{sample_category}}{Object of class {\link[base]{character}} expected. Sample category: -10kg or +10kg.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-standardisedsampleset-clone"></a>}}
\if{latex}{\out{\hypertarget{method-standardisedsampleset-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{standardisedsampleset$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
