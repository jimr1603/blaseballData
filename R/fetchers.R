# One function for each API endpoint goes here.

#' Download all raw event data.
#'
#' @param season number season to get event data for
#' @return A JSON file with the raw event data.
#' @export

all_events <- function(season) {
  assertthat::is.number(season) # Validating input
  season <- season - 1 # We do not 0-index in R.

  get_request("data/events", list(season = season))
}

#' Query for game events.
#'
#' @param playerId string The ID of a player that must be the batter or the pitcher in each event.
#' @param gameId string The ID of the game by which to filter results.
#' @param pitcherId string The ID of the pitcher that must be in each event.
#' @param batterId string The ID of the batter that must be in each event.
#' @param type string The type of event by which to filter.
#' @param outcomes boolean Include child player event records.
#' @param baseRunners boolean Include child base runner records.
#' @param sortBy string The field by which to sort. Most text and numeric columns are supported.
#' @param sortDirection string The direction by which to sort. Must be one of ASC, DESC.
#' @return The events that match the query and the number of matching events.
#' @export

events <- function(playerId = NULL,
                   gameID = NULL,
                   pitcherId = NULL,
                   batterId = NULL,
                   type = NULL,
                   outcomes = TRUE,
                   baseRunners = TRUE,
                   sortBy = NULL,
                   sortDirection = "ASC") {
  assertthat::assert_that(!all(is.null(playerId, gameId, pitcherId, batterId)),
    msg = "At least one of playerId, gameId, pitcherId and batterId must be specified"
  )
  # TODO: check what happens when other args are left undefined.

  get_request("events", list(
    playerId = playerId,
    gameId = gameId,
    pitcherId = pitcherId,
    batterId = batterId,
    type = type,
    outcomes = outcomes,
    baseRunners = baseRunners,
    sortBy = sortBy,
    sortDirection = sortDirection
  ))
}

#' Calculate the number of events for each batter or pitcher.
#'
#' @param eventType string The type of event to count.
#' @param batterId string The ID of the batter(s) by which to filter.
#' @param pitcherId string The ID of the pitcher(s) by which to filter.
#' @return The number of events the batter was involved in with the specified type.
#' @export

count_by_type <- function(eventType, batterId = NULL, pitcherId = NULL) {
  get_request("countByType", list(eventType = eventType, batterId = batterId, pitcherId = pitcherId))
}

#' Get a list of all currently deceased players
#'
#' @return data.frame of player objects
#' @export

deceased <- function() {
  get_request("deceased")
}

#' Get all player IDs matching a given player name
#'
#' @param name string The name of the player
#' @param current boolean If true, only players currently using this name will be returned
#' @return data.frame of objects with player IDs matching that name, including the name and timestamps for when that name started and ended being used for that player
#' @export

player_ids_by_name <- function(name, current = TRUE) {
  get_request("playerIdsByName", list(name = name, current = current))
}

#' Get extended info for a given player - their name, attributes, ratings, and stars
#'
#' @param playerId string The player ID of the player (takes precedence if other params are specified)
#' @param name string The name of the player (takes precedence if slug is specified)
#' @param slug string The url_slug of the player
#' @param all boolean (default:false) If true, all historical info for the player will be returned, rather than just the current info
#' @return data.frame of objects
#' @export

player_info <- function(playerId = NULL,
                        name = NULL,
                        slug = NULL,
                        all = FALSE) {
  assertthat::assert_that(!all(is.null(c(playerId, name, slug))), msg = "You must identify the player by ID, name or slug")
  get_request("playerInfo", list(
    playerId = playerId,
    name = name,
    slug = slug,
    all = all
  ))
}

#' Get the list of all players with a modification tag
#'

#' @return data.frame of player information including current team id/nickname
#' @export

tagged_players <- function() {
  get_request("taggedPlayers")
}

#' Get the current roster for a given team
#'
#' @param teamId string The ID of the team (takes precedence if slug is given)
#' @param slug string The slug of the team
#' @return data.frame of roster information
#' @export

current_roster <- function(teamId = NULL, slug = NULL) {
  assertthat::assert_that(!all(is.null(c(teamId, slug))), "You need to speficy the team by ID or slug")
  get_request("currentRoster", list(teamId = teamId, slug = slug))
}

#' Get the list of all current players, optionally including players in the Shadows
#'
#' @param includeShadows boolean whether to include players in the Shadows
#' @return list of players
#' @export

all_players <- function(includeShadows = FALSE) {
  get_request("allPlayers", list(includeShadows = includeShadows))
}

#' Get the list of all players as of a specific Season and Gameday
#'
#' @param season number Season to query
#' @param day number Day to query
#' @return data.frame of player information including current team id/nickname
#' @export

all_players_for_gameday <- function(season, day) {
  assertthat::is.number(season)
  assertthat::is.number(day)

  season <- season - 1
  day <- day - 1
  get_request("allPlayersForGameday", list(season = season, day = day))
}

#' Get the list of all current teams
#'

#' @return list of current teams
#' @export

all_teams <- function() {
  get_request("allTeams")
}


#' Get current star values for all teams
#'

#' @return list of current team stars
#' @export

all_team_stars <- function() {
  get_request("allTeamStars")
}

#' Get the season leaders for a given category and stat
#'
#' @param season number Season to query
#' @param category string Stat category (batting, pitching, running, or fielding)
#' @param stat string Stat to get the leaders for (as returned from /playerStats)
#' @param order string Ordering to use for ranking (ASC or DESC)
#' @param limit number Number of leaders to return
#' @return list of leaders
#' @export

season_leaders <- function(season,
                           category = NULL,
                           stat = NULL,
                           order = "ASC",
                           limit = NULL) {
  assertthat::is.number(season)
  assertthat::assert_that(order %in% c("ASC", "DSC")) # TODO do this for all ORDER args.

  season <- season - 1 # Convert to 0-indexing for the API.

  get_request("seasonLeaders", list(
    season = season,
    category = category,
    stat = stat,
    order = order,
    limit = limit
  ))
}
