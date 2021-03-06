% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/G_computation_psBART.R
\name{G_computation_psBART}
\alias{G_computation_psBART}
\title{R6 class: G_computation base class}
\description{
A base R6 class for G_computation estimator for average treatment effect
}
\section{Super class}{
\code{\link[RCTrep:TEstimator]{RCTrep::TEstimator}} -> \code{G_computation_psBART}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{G_computation_psBART$new()}}
\item \href{#method-summary}{\code{G_computation_psBART$summary()}}
\item \href{#method-clone}{\code{G_computation_psBART$clone()}}
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
\if{html}{\out{<div class="r">}}\preformatted{G_computation_psBART$new(
  df,
  vars_name,
  name,
  gc.method,
  gc.formula,
  var_approach = "Bias_adjusted",
  isTrial,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summary"></a>}}
\if{latex}{\out{\hypertarget{method-summary}{}}}
\subsection{Method \code{summary()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{G_computation_psBART$summary(stratification, stratification_joint = TRUE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{G_computation_psBART$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
