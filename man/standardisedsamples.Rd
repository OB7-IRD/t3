% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardisedsamples.R
\name{standardisedsamples}
\alias{standardisedsamples}
\title{R6 class standardisedsamples}
\description{
Create R6 reference object class standardisedsamples
}
\section{Super class}{
\code{\link[t3:list_t3]{t3::list_t3}} -> \code{standardisedsamples}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{standardisedsamples$new()}}
\item \href{#method-add}{\code{standardisedsamples$add()}}
\item \href{#method-filter_by_trip}{\code{standardisedsamples$filter_by_trip()}}
\item \href{#method-clone}{\code{standardisedsamples$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="count">}\href{../../t3/html/list_t3.html#method-count}{\code{t3::list_t3$count()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="extract">}\href{../../t3/html/list_t3.html#method-extract}{\code{t3::list_t3$extract()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="extract_l1_element_value">}\href{../../t3/html/list_t3.html#method-extract_l1_element_value}{\code{t3::list_t3$extract_l1_element_value()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="filter">}\href{../../t3/html/list_t3.html#method-filter}{\code{t3::list_t3$filter()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="filter_l1">}\href{../../t3/html/list_t3.html#method-filter_l1}{\code{t3::list_t3$filter_l1()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="modification_l1">}\href{../../t3/html/list_t3.html#method-modification_l1}{\code{t3::list_t3$modification_l1()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="name">}\href{../../t3/html/list_t3.html#method-name}{\code{t3::list_t3$name()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="remove">}\href{../../t3/html/list_t3.html#method-remove}{\code{t3::list_t3$remove()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="t3" data-topic="list_t3" data-id="view">}\href{../../t3/html/list_t3.html#method-view}{\code{t3::list_t3$view()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize function for R6 standardisedsamples class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{standardisedsamples$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{(empty, list or R6-standardisedsample classes) Nothing, a list of object R6-standardisedsample classes or one object R6-standardisedsample classes.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add"></a>}}
\if{latex}{\out{\hypertarget{method-add}{}}}
\subsection{Method \code{add()}}{
Function for add a new standardisedsample in the object standardisedsamples.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{standardisedsamples$add(new_item)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{new_item}}{(list or R6-standardisedsample classes) A list of object R6-standardisedsample classes or one object R6-standardisedsample classes.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-filter_by_trip"></a>}}
\if{latex}{\out{\hypertarget{method-filter_by_trip}{}}}
\subsection{Method \code{filter_by_trip()}}{
Function for filter standardisedsamples by trip identification.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{standardisedsamples$filter_by_trip(trip_id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{trip_id}}{(character) Trip identification.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{standardisedsamples$clone(deep = FALSE)}\if{html}{\out{</div>}}
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