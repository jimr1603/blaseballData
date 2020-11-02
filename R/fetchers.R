# One function for each API endpoint goes here.

#' Gets all current teams.
#'
#' @return data.frame of the current blaseball teams.
#' @export

all_teams <- function() {
  get_request("allTeams")
}
