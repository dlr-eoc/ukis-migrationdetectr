
#' detect_segments
#' @description A three step algorithm to identify segments of continuous presence within trace data
#' @details \itemize{
#' The first step in detecting migration requires detecting periods of time when
#'  an individual is continuously present in a single location,
#'   allowing for some margin of travel from that location (Chi et al., 2020).
#'
#'  These segments are identified from the time series of \code{locs} and \code{times} in a
#' three-step procedure:
#' \item{Step 1: **Identify** contiguous segments, with no gap exceeding \code{param_window_size_days days},
#'  where the individual is present for at least \code{param_prop_days} percent of days in the segment,
#'  and the total length of the segment is at least \code{param_min_days}},
#' \item{Step 2: **Merge** segments if there are no segments in other locations between them.},
#' \item{Step 3: **Prune** overlapping time from segments,
#'  when an individual is associated with segments in multiple locations at a single point in time.
#'  overlapping segments may be allowed to persist if they
#'  contain at least \code{param_ol_min_frac} of all observations during the overlap period.}}
#' @param locs (character). A vector containing locations corresponding to \code{times }.
#' @param times (POSIXct). A vector containing timestamps corresponding to \code{locs}.
#' @param param_min_days (Optional) numeric. The minimum length in days of a segment. Smaller segments will be eliminated during stage 1. Default is \code{2}.
#' @param param_prop_days (Optional) numeric. The minimum fraction of days in a segment during which a user must have been observed at a location. Segments with a smaller proportion will be eliminated. Default is \code{0.05}.
#' @param param_window_size_days (Optional) numeric. The minimum forward window size in days for step 1. Observations separated by a smaller timespan will be connected. Default is \code{20}.
#' @param param_ol_min_frac (Optional) numeric. In step 3, do not remove the overlapped segment if the place it belongs to is the mode during the period of overlap and if the place contains more than this fraction of all occurrences. Default is 1 meaning that no segments can persist at overlaps.
#' @param return_intermediate_results (Optional) logical. Should the results of Step 1 and 2 be returned as well? If \code{TRUE} the result will be a list of length 3. Default is \code{FALSE}.
#' @return A \link[tibble]{tibble} containing the detected segments as \link[lubridate]{interval} with their location as Character.
#' @export
#' @importFrom lubridate interval as.duration as_date int_start int_end int_length int_overlaps is.POSIXct %within%
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom assertthat assert_that
#' @author Johannes Mast \email{Johannes.Mast@@dlr.de}, based on the algorithm by Guanghua Chi \email{guanghua@@berkeley.edu}
#' @references Chi, Guanghua, Fengyang Lin, Guangqing Chi, and Joshua Blumenstock. 2020. “A General Approach to Detecting Migration Events in Digital Trace Data.” Edited by Song Gao. PLOS ONE 15 (10): e0239408. https://doi.org/10.1371/journal.pone.0239408.
#' @examples
#' trace <- MigrationDetectR::example_trace
#'
#' # Detect segments
#' segments <-
#'   detect_segments(
#'       locs = trace$location,
#'       times = trace$timestamp,
#'       param_min_days = 3,
#'       param_prop_days = 0.06,
#'       param_window_size_days = 7,
#'       param_ol_min_frac= 0.5)
#'  nrow(segments) # check the number of detected segments
#'  head(segments) # check the segments
detect_segments <- function(locs,
                            times,
                            param_min_days = 2,
                            param_prop_days = 0.05,
                            param_window_size_days = 20,
                            param_ol_min_frac=1.0,
                            return_intermediate_results = FALSE)
{
  assert_that(is.POSIXct(times), msg = "times must be of class POSIXct")
  assert_that(is.character(locs), msg = "locs must be of class character")
  assert_that(length(locs) == length(times), msg = "Lengths of times and locs are not equal")
  assert_that(length(times) >= 1, msg = "At least two times are required")
  assert_that(!is.unsorted(times), msg = "times must be sorted")
  assert_that(!anyNA(locs), msg = "NA in locs are not allowed")
  assert_that(!anyNA(times), msg = "NA in times are not allowed")




  # === Step 1 === #
  # combine locs and times into a tibble
  hstry <- tibble(locs, times)

  segments <-
    hstry |>
    group_by(locs) |>
    #Calculate the time until that location was visited again
    mutate(tdiff_to_revisit =
             c(as.numeric(diff(times), units = "days"), 9999999)) |>
    #Was this location visited within the timeframe?
    mutate(revisit_in_time = tdiff_to_revisit <= param_window_size_days) |>
    #If not, that observation marks the end of the segment. Give it a unique row number
    group_by(revisit_in_time) |>
    mutate(segment = ifelse(revisit_in_time, NA, row_number(revisit_in_time))) |>
    ungroup() |>
    group_by(locs) |>
    #  # Assign that row number as a segment ID to all observations preceeding that observation,
    # until another segment is reached
    fill(segment, .direction = "up") |>
    group_by(segment) |>
    # calculate statistics for each segment
    summarise(
      locs = head(locs, 1),
      segments = interval(min(times), max(times)),
      unique_days = times |> as.Date() |> as.integer() |> n_distinct(),
      duration_days = int_length(segments) / 86400,
      n_days_span = as.integer(as.Date(int_end(segments))) -
        as.integer(as.Date(int_start(segments))) +
        1  # add 1 day because every interval occurs on at least 1 day in time
    )

  # Apply the filter criteria to the segments
  segments_step1 <-
    segments |>
    filter(duration_days >= param_min_days) |>
    filter(unique_days >= (param_prop_days * n_days_span)) |>
    select(segments, locs)

  if(nrow(segments_step1) == 0){
    # if(verbose)cat("No segments were detected in Step 1");
    return(NULL)}

  # === Step 2 === #
  # aux function for getting rle ids # credit https://stackoverflow.com/a/36925115
  aux_rleid <- function(vector){rep(seq_along(rle(vector)$values), rle(vector)$lengths)}

  if (nrow(segments_step1) == 1) {
    segments_step2 <- segments_step1
  } else{
    segments_step2 <-
      segments_step1 |>
      # arrange by segment start dates
      arrange(int_start(segments)) |>
      # use run length encoding to Give each series of
      # segments in the same location the same id
      mutate(new_seg = mergsec <- aux_rleid(locs)) |>
      # collapse the series of segments into one
      group_by(new_seg) |>
      summarize(locs = head(locs, 1),
                segments = interval(min(int_start(segments)),
                                    max(int_end(segments)))) |>
      select(-new_seg)
  }
  if(return_intermediate_results){
    if(nrow(segments_step1) == 0){
      # cat("No segments were detected after Step 2");
      return(list(step1=segments_step1))}
  }

  # === Step 3 === #

  if (nrow(segments_step2) == 1) {
    segments_step3 <- segments_step2 |>
      rownames_to_column(var = "seg_id")
  } else{
    segments_step3 <-
      segments_step2 |>
      arrange(int_start(segments))  |>
      rownames_to_column(var = "seg_id")

    # In a loop over all segments, find the all overlaps
    # Usually, overlaps will be detected twice
    overlaps <- list()
    for (i in 1:nrow(segments_step3)) {
      seg <- segments_step3[i, ] # current segment
      seg_other <- segments_step3[-i, ] # all other segments
      overlaps[[i]] <-
        seg_other |>
        filter(int_overlaps(segments, seg$segments)) |>
        # construct the overlap from the later start of the two overlapping segments,
        # and the earlier end of the two overlapping segments
        mutate(ol_int = interval(pmax(
          int_start(segments), int_start(seg$segments)
        ), pmin(
          int_end(segments), int_end(seg$segments)
        )))
    }

    # If any overlaps were detected
    if (any(sapply(overlaps, nrow))) {
      # Combine the overlaps into an ordered vector of overlaps
      overlaps <-
        overlaps |>
        bind_rows() |>
        arrange(int_start(ol_int)) |>
        # merge overlapping overlaps
        mutate(ol_idx = util_int_overlapping(segs = ol_int)) |>
        group_by(ol_idx) |>
        summarize(ol_seg =
                    interval(min(int_start(ol_int)), max(int_end(ol_int)))) |>
        select(-ol_idx)
      # sort overlaps
      overlaps <-
        overlaps |>
        arrange(int_start(ol_seg))

      # function to add a modal place,
      # which persists through the overlap removal,
      # if there is one that meets the threshold
      # (This place will not be eliminated)
      modal_within_interval <- function(x_int,min_frac=0.5) {
        x <- hstry |>
          filter(times %within% x_int) |>
          pull(locs)

        x <- util_calculate_mode(x,min_frac = min_frac)
        return(ifelse(is.na(x), "NoSuitableModeDetected", x))
      }
      overlaps <-
        overlaps |>
        rowwise()  |>
        mutate(modal_place = modal_within_interval(ol_seg,min_frac=param_ol_min_frac)) |>
        ungroup()

      # loop over overlaps
      # splitting all segments which the overlap intersects
      # unless that segment is of the modal location
      for (i in 1:nrow(overlaps)) {
        ol_i <- overlaps[i, ]
        #assert that this overlap is not overlapping any other overlaps
        # This should not happen if the merge worked well, but we better make sure.
        if (nrow(overlaps) > 1) {
          assert_that(!any(int_overlaps(ol_i$ol_seg, overlaps[-i, ]$ol_seg)),
                      msg =
                        "Overlap intervals should not be overlapping. Something went wrong.")
        }
        #Make new segments that result as a split by the old segments
        # Every overlap creates
        # for every segment it overlaps
        # two new segments on both sides
        # If the overlap exceeds the segment on either side, one or both
        # of the new segments have a length <= 0 and are deleted
        seg_new <- segments_step3 |>
          filter(int_overlaps(segments, ol_i$ol_seg),
                 locs != ol_i$modal_place) |>
          mutate(
            seg1 = interval(int_start(segments), int_start(ol_i$ol_seg)),
            seg2 = interval(int_end(ol_i$ol_seg), int_end(segments))
          ) |>
          select(-segments) |>
          pivot_longer(
            cols = c(seg1, seg2),
            names_to = "seg_pos",
            values_to = "segments"
          ) |>
          select(-seg_pos)
        #if any new segments are created, replace the input segments with them
        if (nrow(seg_new) > 0) {
          #remove the first and last second from each new segment to
          # make sure there is no renewed overlap
          int_end(seg_new$segments) <- int_end(seg_new$segments) - 1
          int_start(seg_new$segments) <-
            int_start(seg_new$segments) + 1

          segments_step3 <-
            segments_step3 |>
            #Remove those segments that were just split
            filter(!seg_id %in% seg_new$seg_id) |>
            # replace them with the split segments
            bind_rows(seg_new) |>
            # ensure the new order is correct
            arrange(int_start(segments)) |>
            #Some of the newly added segments will have negative length; remove those
            filter(int_length(segments) > 0) |>
            # make new seg ids
            select(-seg_id) |>
            rownames_to_column(var = "seg_id")
        }# end if any new segments are created
      }# end loop over overlaps
    }# end if any overlaps were detected
  } # end else if nrow(segments_step2)==1

  if(nrow(segments_step3)==0){
    # if(verbose) print("No segments remaining after Step 3.")
    return(NULL)
  }
  # Apply the filters one last time
  segments_step3 <-
    segments_step3 |>
    group_by(seg_id) |>
    mutate(
      unique_days = util_count_unique_days_in_interval(hstry$locs,
                                                       hstry$times,
                                                       locs,
                                                       segments),
      duration_days = int_length(segments) / 86400,
      n_days_span = as.integer(as.Date(int_end(segments))) -
        as.integer(as.Date(int_start(segments))) +
        1  # add 1 day because every interval occurs on at least 1 day in time
    ) |>
    filter(duration_days >= param_min_days) |>
    filter(unique_days >= (param_prop_days * n_days_span)) |>
    ungroup() |>
    select(segments, locs)

  # return the results, if any
  if(return_intermediate_results)  return(list(step1=segments_step1,
                                               step2=segments_step2,
                                               step3=segments_step3))
  if(length(segments_step3)==0) return(NULL)
  return(segments_step3)
}
