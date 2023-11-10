#' MigrationDetectR: Segment-Based Migration Detection Algorithm
#'
#' @description Implementation of Migration detection algorithm,
#'  published by Chi et al. (2020) in *A general approach to detecting migration events in digital trace data* (\doi{10.1371/journal.pone.0239408} )
#' @section  Changes in the R implementation:
#' * Compared to the reference, this implementation works at the precision of seconds rather than days.
#' At the expense of speed, it is applicable at finer timescales.
#' * The segment detector has the additional option to allow for one of several overlapping segments to persist if it contains the majority of occurrences during the overlap period.
#' @section Usage:
#' First, transform your data into the format required by the package: Two aligned vectors, one with locations and one with timestamps.
#' Use the \link{detect_segments} function to identify segments of continuous residence.
#' Then, use the \link{identify_migrations} function on the segments to detect migrations.
#'  To optionally determine the best split time, pass the original locations and timestamps vectors.
#' @docType package
#' @name MigrationDetectR
#' @author Johannes Mast (R Implementation)
#' @author Guanghua Chi (Developer of the Algorithm)
#' @md
NULL
