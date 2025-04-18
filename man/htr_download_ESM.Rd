% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/htr_download_ESM.R
\name{htr_download_ESM}
\alias{htr_download_ESM}
\title{Download ESM data}
\usage{
htr_download_ESM(hpc = NA, indir, outdir)
}
\arguments{
\item{hpc}{Indicates whether the user is working in a HPC (High Performance Computing) facility}

\item{indir}{Directory where input files are located}

\item{outdir}{Directory where output files will be saved}
}
\description{
Download ESM data
}
\examples{
\dontrun{
htr_download_ESM(
hpc = NA,
indir = file.path(base_dir, "data", "raw", "wget"), # input directory
outdir = file.path(base_dir, "data", "raw", "tos") # output directory
)
}
}
\author{
Dave Schoeman and Tin Buenafe
}
