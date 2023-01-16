#' Survivalists
#'
#' Contains details of each survivalist including demographics and results.
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{season}}{The season number}
#'   \item{\code{name}}{Name of the survivalist}
#'   \item{\code{age}}{Age of survivor}
#'   \item{\code{gender}}{Gender}
#'   \item{\code{city}}{City}
#'   \item{\code{state}}{State}
#'   \item{\code{country}}{Country}
#'   \item{\code{result}}{Place the survivalist finished in the season}
#'   \item{\code{days_lasted}}{The number of days lasted in the game before tapping out or winning}
#'   \item{\code{medically_evacuated}}{Logical. If the survivalist was medically evacuated from the game}
#'   \item{\code{reason_tapped_out}}{The reason the survivalist tapped out of the game. \code{NA} means
#'   they were the winner}
#'   \item{\code{reason_category}}{A simplified category of the reason for tapping out}
#'   \item{\code{team}}{The team they were associated with (only for season 4)}
#'   \item{\code{day_linked_up}}{Day the team members linked up}
#'   \item{\code{profession}}{Profession}
#'   \item{\code{url}}{URL of castaway page on the history channel website. Prefix URL with https://www.history.com/shows/alone/cast}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_Alone_episodes#Season_1_(2015)_-_Vancouver_Island}
"survivalists"

#' Episodes
#'
#' Contains details of each episode including the title, number of viewers, beginning quote
#' and IMDb rating
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{version}}{Country code for the version of the show}
#'   \item{\code{season}}{The season number}
#'   \item{\code{episode_number_overall}}{Episode number across seasons}
#'   \item{\code{episode}}{Episode number}
#'   \item{\code{title}}{Episode title}
#'   \item{\code{air_date}}{Date the episode originally aired}
#'   \item{\code{viewers}}{Number of viewers in the US (millions)}
#'   \item{\code{quote}}{The beginning quote}
#'   \item{\code{author}}{Author of the beginning quote}
#'   \item{\code{imdb_rating}}{IMDb rating of the episode}
#'   \item{\code{n_ratings}}{Number of ratings given for the episode}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_Alone_episodes#Season_1_(2015)_-_Vancouver_Island}
"episodes"

#' Seasons
#'
#' Season summary includes location and other season level information
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{version}}{Country code for the version of the show}
#'   \item{\code{season}}{The season number}
#'   \item{\code{location}}{Location}
#'   \item{\code{country}}{Country}
#'   \item{\code{n_survivors}}{Number of survivors. Season 4 there were 7 teams of 2.}
#'   \item{\code{lat}}{Latitude}
#'   \item{\code{lon}}{Longitude}
#'   \item{\code{date_drop_off}}{Date the survivors where dropped off}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Alone_(TV_series)}
"seasons"

#' Loadouts
#'
#' Information on each survivalists loadout of 10 items
#'
#' @format This data frame contains the following columns:
#' \describe{
#'   \item{\code{version}}{Country code for the version of the show}
#'   \item{\code{season}}{The season number}
#'   \item{\code{name}}{Name of the survivalist}
#'   \item{\code{item_number}}{Item number}
#'   \item{\code{item_detailed}}{Detailed loadout item description}
#'   \item{\code{item}}{Loadout item. Simplified for aggregation}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Alone_(TV_series)}
"loadouts"
