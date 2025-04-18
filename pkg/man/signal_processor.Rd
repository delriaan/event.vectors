% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_signal_processor.R
\name{signal_processor}
\alias{signal_processor}
\title{Signal Processor}
\usage{
signal_processor(object, ...)
}
\arguments{
\item{object}{A "break_signal" \code{\link{S7}} object}

\item{...}{Arguments for internal use}
}
\value{
The modified "break_signal" object with prescribed slots populated from the results of processing the "signal" (see \code{\link{break_signal}}).
}
\description{
\code{signal_processor} processes the "signal" of a "break_signal" object.
}
\seealso{
Other Signal Processor Functions: 
\code{\link{exec_algorithm}()},
\code{\link{geo_pmf}()},
\code{\link{info_encoder}()},
\code{\link{score_algorithm_output}()}
}
\concept{Signal Processor Functions}
