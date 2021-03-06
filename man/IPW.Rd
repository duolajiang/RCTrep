% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPW.R
\name{IPW}
\alias{IPW}
\title{R6 class: Inverse propensity score weighting estimator base class}
\description{
A base R6 class for inverse propensity score weighting estimator of average treatment effect that implements comment methods.
}
\section{Super class}{
\code{\link[RCTrep:TEstimator]{RCTrep::TEstimator}} -> \code{IPW}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{model:}}{a fitted model for treatment conditioning on covariates}

\item{\code{method:}}{a string specifying the method for fitting the propensity score}

\item{\code{formula:}}{an optional \code{formula} object. The formula is specified as \code{Z ~ X1+X2...}, where \code{Z} represents treatment variable and \code{X1} and \code{X2} are covariates.}

\item{\code{ps:}}{a numeric vector of length n where n is row number of \code{self$data}.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{model:}}{a fitted model for treatment conditioning on covariates}

\item{\code{method:}}{a string specifying the method for fitting the propensity score}

\item{\code{formula:}}{an optional \code{formula} object. The formula is specified as \code{Z ~ X1+X2...}, where \code{Z} represents treatment variable and \code{X1} and \code{X2} are covariates.}

\item{\code{ps:}}{a numeric vector of length n where n is row number of \code{self$data}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{IPW$new()}}
\item \href{#method-summary}{\code{IPW$summary()}}
\item \href{#method-clone}{\code{IPW$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="RCTrep" data-topic="TEstimator" data-id="diagnosis_t_overlap">}\href{../../RCTrep/html/TEstimator.html#method-diagnosis_t_overlap}{\code{RCTrep::TEstimator$diagnosis_t_overlap()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="RCTrep" data-topic="TEstimator" data-id="diagnosis_y_overlap">}\href{../../RCTrep/html/TEstimator.html#method-diagnosis_y_overlap}{\code{RCTrep::TEstimator$diagnosis_y_overlap()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="RCTrep" data-topic="TEstimator" data-id="get_CATE">}\href{../../RCTrep/html/TEstimator.html#method-get_CATE}{\code{RCTrep::TEstimator$get_CATE()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="RCTrep" data-topic="TEstimator" data-id="plot_CATE">}\href{../../RCTrep/html/TEstimator.html#method-plot_CATE}{\code{RCTrep::TEstimator$plot_CATE()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="RCTrep" data-topic="TEstimator" data-id="plot_y1_y0">}\href{../../RCTrep/html/TEstimator.html#method-plot_y1_y0}{\code{RCTrep::TEstimator$plot_y1_y0()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IPW$new(df, vars_name, name, treatment_method, treatment_formula, isTrial, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summary"></a>}}
\if{latex}{\out{\hypertarget{method-summary}{}}}
\subsection{Method \code{summary()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IPW$summary(stratification)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IPW$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
