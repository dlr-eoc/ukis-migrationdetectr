#' @title find_best_split
#' @param locs character, A vector of the locations of occurrences.
#' @param times POSIXct, A vector of the occurrences corresponding timestamps.
#' @param movement A lubridate interval for which the best split is to be found.
#' @param from character,  the name of the location the \code{movement} ends at. Must match an element in \code{locs}.
#' @param to character, the name of the location the \code{movement} ends at. Must match an element in \code{locs}.
#' @description Find the optimal point in time to split a migration interval
#' @details The function checks which occurrences by \code{locs} and \code{times} fall within the interval given by \code{movement}.
#' It then tries to find the optimal point in time to split the interval, using the following two criteria:\itemize{
#' \item{The point which minimizes the number of misclassified **days**, i.e., the number of days when the migrant
#' appears at \code{to} before the migration date and days when the migrant appears at
#' \code{from} after the migration date.}
#' \item{In cases where multiple days yield the same number of misclassifications, we select the last timestamp as the migration date}
#' }
#' @return A tibble of 2 fields: \itemize{
#' \item{**split_time**, a lubridate date time which indicates the time of the optimal split found}
#' \item{**split_correctness**, a numeric date time which indicates the split's ratio of the wrongly assigned days to the correctly assigned days}
#' }
#' @export
#' @importFrom dplyr arrange rowwise slice_min mutate select desc tibble ungroup
#' @importFrom assertthat assert_that
#' @import lubridate
#' @author Johannes Mast \email{Johannes.Mast@@dlr.de}, based on the algorithm by Guanghua Chi \email{guanghua@@berkeley.edu}
#' @references Chi, Guanghua, Fengyang Lin, Guangqing Chi, and Joshua Blumenstock. 2020. “A General Approach to Detecting Migration Events in Digital Trace Data.” Edited by Song Gao. PLOS ONE 15 (10): e0239408. https://doi.org/10.1371/journal.pone.0239408.
find_best_split <- function(locs,times,movement,from,to){
  assert_that(is.POSIXct(times), msg = "times must be of class POSIXct")
  assert_that(is.character(locs), msg = "locs must be of class character")
  assert_that(length(times) >= 1, msg = "At least two times are required")
  assert_that(length(locs) == length(times), msg = "Lengths of times and locs are not equal")
  assert_that(!is.unsorted(times), msg = "times must be sorted")
  assert_that(!anyNA(locs), msg = "NA in locs are not allowed")
  assert_that(from %in% locs, msg = "from must be an element of locs")
  assert_that(to %in% locs, msg = "to must be an element of locs")
  assert_that(is.interval(movement), msg = "movement must be of class lubridate::interval")

  x <- tibble(locs,times) |>
    #subset to only those times and locs which fall into the interval and are either the from or to location
    filter(times %within% movement,locs %in% c(from,to))

  y <-
    x |>
    arrange(times) |>
    dplyr::rowwise()|>
    # we take the intervals before and after the tweet, shifted by 1 second forwards and backwards as to make sure the tweet in question counts in both parts
    #as such, there will always be at least one misclassified day, the one of the tweet in question
    mutate(before=int_start(movement)%--%(times+1),
           after = (times-1)%--%int_end(movement),
           #count the number of misclassified days if the date was the migration date
           wrong_before=util_count_unique_days_in_interval(locs = x$locs,x$times,location = to,interval = before),
           wrong_after=util_count_unique_days_in_interval(locs = x$locs,x$times,location = from,interval = after),
           correct_before=util_count_unique_days_in_interval(locs = x$locs,x$times,location = from,interval = before),
           correct_after=util_count_unique_days_in_interval(locs = x$locs,x$times,location = to,interval = after),
           wrong_total=wrong_before+wrong_after) |>
    ungroup() #end row wise
  best_split <-
    y |>
    arrange(desc(times)) |>
    slice_min(order_by = wrong_total,
              n=1,with_ties = F) |>
    #calculate the correctness
    mutate(correctness=(correct_before+correct_after)/(wrong_before+wrong_after)) |>
    select(split_time=times,split_correctness=correctness)
  return(best_split)
}
