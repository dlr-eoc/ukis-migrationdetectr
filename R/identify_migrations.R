#' identify_migrations
#'
#' @param segs A vector of \link[lubridate]{interval}, a vector of intervals which indicate residence segments, such as detected by \link{detect_segments}.
#' @param locs A vector of Character, locations which correspond to \code{segs}.
#' @param min_res_length (Optional) Numeric. A vector of days indicating the minimum number of days for \code{segs} to quality as viable origin or destination of a migration. Default is \code{90}.
#' @param occurrence_locs  (Optional) Character. See \code{occurrence_times}
#' @param occurrence_times (Optional) If provided along with the matching \code{occurrence_times}, will try to find the optimal point in time during which the migration occurred.
#' @param verbose (Optional) logical. Output optional messages? Default is \code{TRUE}.
#' @return A tibble containing the detected true relocations, if any. Contains the fields: \itemize{
#' \item{**from** Character, the location from which the relocations started.}
#' \item{**to** Character, the location from which the relocations started.}
#' \item{**movement_interval** \link[lubridate]{interval} of the transitionary period, starting with the end of the prior residency and ending with the start of the new residency.}
#' \item{**movement_length** The duration of the transitionary period in seconds.}
#' \item{**movement_midrange** The midpoint of the transitionary period.}
#' }
#' If \code{occurrence_locs} and \code{occurrence_times} were provided, \link{find_best_split} will be used to detect the optimum split time, and the following columns will be added:\itemize{
#' \item{**split_time**, a \link[lubridate]{POSIXct} which indicates the time of the optimal split. }
#' \item{**split_correctness**, a numeric which as a quality measure of \code{splot_time} reports the split's ratio of wrongly assigned days to the correctly assigned days.}
#' }
#' @author Johannes Mast \email{Johannes.Mast@@dlr.de}, based on the algorithm by Guanghua Chi \email{guanghua@@berkeley.edu}
#' @importFrom lubridate interval is.interval as.duration as_date int_start int_end int_length int_overlaps is.POSIXct %within%
#' @import dplyr
#' @references Chi, Guanghua, Fengyang Lin, Guangqing Chi, and Joshua Blumenstock. 2020. “A General Approach to Detecting Migration Events in Digital Trace Data.” Edited by Song Gao. PLOS ONE 15 (10): e0239408. https://doi.org/10.1371/journal.pone.0239408.
#' @export
#' @examples
#' trace <- MigrationDetectR::example_trace
#' # Detect segments
#' segments <-
#'   detect_segments(
#'       locs = trace$location,
#'       times = trace$timestamp,
#'       param_min_days = 3,
#'       param_prop_days = 0.06,
#'       param_window_size_days = 7)
#'  nrow(segments) # check the number of detected segments
#'
#'  migrations <-
#'  identify_migrations(
#'   segs = segments$segments,
#'   locs = segments$locs,
#'   min_res_length = 90,
#'   occurrence_locs = trace$location,
#'   occurrence_times = trace$timestamp
#'   )
#'   nrow(migrations) # check the number of identified migrations
#'   head(migrations) # check the detected migrations
identify_migrations <- function(segs,
                                  locs,
                                  min_res_length = 90,
                                  occurrence_locs = NULL,
                                  occurrence_times = NULL,
                                verbose=TRUE){

  assert_that(is.interval(segs), msg = "segs must be of class lubridate::interval")
  assert_that(is.character(locs), msg = "locs must be of class character")
  assert_that(length(locs) == length(segs), msg = "Lengths of segs and locs are not equal")
  assert_that(length(locs) >= 2, msg = "At least two segments are necessary")
  assert_that(n_distinct(locs) >= 2, msg = "At least two residencies must be in different locations")

  if(verbose & !is.null(occurrence_locs) & is.null(occurrence_times)){
    warning("occurrence_locs will only be used if occurrence_times are also provided.")
  }
  if(verbose & !is.null(occurrence_times) & is.null(occurrence_locs)){
    warning("occurrence_times will only be used if occurrence_locs are also provided.")
  }

  if(is.null(occurrence_times)){
    assert_that(length(occurrence_times) == length(occurrence_locs),
                msg = "Lengths of occurrence_times and occurrence_locs are not equal")
  }

  # combine segs and locs into a tibble
  # and filter residences that are not long enough
  residencies <-
    tibble(segs,locs) |>
    arrange(int_start(segs)) |>
    filter((int_length((segs))/86400)>min_res_length)

  if(!nrow(residencies)>=2){
    if(verbose) cat("No residencies of sufficient length could be found. Consider adjusting min_res_length.")
    return(NULL)
  }
  if(!n_distinct(residencies$locs)>=2){
    if(verbose) cat("No residencies of sufficient length could be found for different locations. Consider adjusting min_res_length.")
    return(NULL)
    }

  # make sure there are no overlaps
  any_overlaps <- sapply(1:nrow(residencies),FUN = function(i)(residencies$segs[i] |> int_overlaps(residencies$segs[-i])) ) |> any()
  assert_that(!any_overlaps,
              msg="Residencies must not be overlapping. Check overlap removal algorithm.")

  # calculate the relocations
  relocations <-
    residencies |>
    mutate(from=lag(locs),
           to=locs,
           #The movement begins at the end of the previous true residence segment
           movement_start=int_end(lag(segs)),
           #The movement ends at the beginning of the current true residence segsment
           movement_end=int_start(segs),
           movement=interval(movement_start,movement_end),
           movement_midrange=movement@start + as.duration(movement)/2) |>
    select(-locs) |>
    #filter for actual changes in location
    filter(from!=to) |>
    mutate(movement_length=int_length(movement)) |>
    select(from,
           to,
           movement_interval=movement,
           movement_length,
           movement_midrange)

  # if occurrence locs and timestamps were provided,
  # the optimal split time can be inferred as well
  if(!is.null(occurrence_locs) & !is.null(occurrence_times)){
    relocations <-
      relocations |>
      rowwise() |>
      #add an additional column for the interval length
      mutate(movement_split = find_best_split(
        locs = occurrence_locs,
        times = occurrence_times,
        movement = movement_interval,
        from = from,
        to = to))  |>
      ungroup() |>  #end row wise
      unnest(movement_split)
  }
  # if no relocations were detected, there is nothing to return
  if(length(relocations)==0) return(NULL)
  # otherwise, detect the relocations
  return(relocations)
}
