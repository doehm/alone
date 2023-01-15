#' Survivors
#'
#' Contains details of each survivor including demographics and results.
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{version}}{Country code for the version of the show}
#'   \item{\code{season}}{The season number}
#'   \item{\code{name}}{Name of the survivor}
#'   \item{\code{age}}{Age of survivor}
#'   \item{\code{gender}}{Gender}
#'   \item{\code{city}}{City}
#'   \item{\code{state}}{State}
#'   \item{\code{country}}{Country}
#'   \item{\code{result}}{Place the survivor finished in the season}
#'   \item{\code{days_lasted}}{The number of days lasted in the game before tapping out or winning}
#'   \item{\code{medically_evacuated}}{Logical. If the survivor was medically evacuated from the game}
#'   \item{\code{reason_tapped_out}}{The reason the survivor tapped out of the game. \code{NA} means
#'   they were the winner}
#'   \item{\code{reason_category}}{A simplified category of the reason for tapping out}
#'   \item{\code{team}}{The team they were associated with (only for season 4)}
#'   \item{\code{day_linked_out}}{Day the team members linked up}
#'   \item{\code{profession}}{Profession}
#'   \item{\code{url}}{URL of castaway page on the history channel website. Prefix URL with https://www.history.com/shows/alone/cast}
#' }
#' @source \url{https://www.history.com/shows/alone/cast}
"survivors"
