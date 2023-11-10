#' Find the most common value in a character vector
#' @noRd
#' @param x Input vector of strings
#' @param min_frac Minimum fraction of non-NA inputs that the mode has to surpass to be returned. If the mode is below the threshold, NA will be returned. Default is \code{0.5}.
#' @return The most common string that also surpasses \code{min_frac}
#' x <- c("Ikeja", "Ikeja",NA,NA,"Ikeja", "Abuja",NA,NA,"Abuja", "Ikeja",NA,NA,"Abuja", "Ikeja",NA,NA,"Abuja", "Ikeja",NA,NA,"Lagos","Lagos","Port Harcourt")
#' mw_util_calculate_mode(x)
util_calculate_mode <- function(x, min_frac = 0.5) {
  # Return NA if all inputs are NA
  if (!any(!is.na(x))) {
    return(NA)
  }

  #min_frac = 0.5
  x_ <- as_tibble(table(x)) |>
    mutate(frac = n / sum(n)) |>
    filter(frac > min_frac) |>
    slice_max(order_by = frac) |>
    pull(x)
  if (identical(x_, character(0))) {
    return(NA)
  } else{
    return(x_)
  }
}

#' Find overlapping time ranges
#' @param segs A vector of intervals
#' @param group_var (Optional) A vector of grouping variables, for example locations
#' @return A vector of the same length as segments, containing indices as integer of overlapping segments
#' @noRd
util_int_overlapping <- function(segs, group_var = NULL) {
  if (is.null(group_var)) {
    group_var = rep(1, length(segs))
  }
  indx <-
    tibble(segs = segs, groups = group_var) |>
    mutate(start = int_start(segs),
           end = int_end(segs)) |>
    group_by(groups) |>
    mutate(indx = c(0, cumsum(as.numeric(lead(start)) >
                                cummax(as.numeric(end)))[-n()])) %>%
    # mutate(next_start = as.numeric(lead(start)),
    #        current_end = as.numeric(end)) |>
    # mutate(indx = c(0, cumsum(next_start > current_end)[-n()])) |>
    pull(indx)
  return(indx)
}


#' Count the unique days
#' @noRd
#' @param location Character, the location name which the input tweets should match
#' @param locs Character, see \code{times}
#' @param times POSIXct, matching \code{locs}, giving the occurrences, the unique days of which are to be counted within the interval
#' @param interval Interval, the interval within which the input tweets should fall
#' @return Count of unique days matching the location and timespan
util_count_unique_days_in_interval <-
  function(locs, times, location, interval) {
    tibble(locs, times) %>%
      filter(locs == location,
             times %within% interval) %>%
      summarize(n_unique_days = as.numeric(as_date(times)) %>% n_distinct) %>% pull(n_unique_days)
  }



