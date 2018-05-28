#' Snooker events
#'
#' Data set describing snooker events in seasons 2016/2017 and 2017/2018.
#'
#' @details Data is taken from snooker.org (http://www.snooker.org/) API.
#'
#' This data set has information about events that have all following qualities:
#' - It has "Invitational", "Qualifying", or "Ranking" type.
#' - It describes traditional snooker (not 6 Red or Power Snooker) between
#' individual players (not teams).
#' - Both genders can take part (not only men or women).
#' - Players of all ages can take part (not only seniors or under 21).
#' - It is not "Shoot-Out" as those events are treated differently in
#' snooker.org database.
#'
#' @format A [tibble][tibble::tibble] with one row per event and the following
#' columns:
#' - __id__ <int> : Event identifier in snooker.org database (used in `eventId`
#' column of [snooker_matches]).
#' - __season__ <int> : Season identifier (by the year of season start).
#' - __name__ <chr> : Event name.
#' - __startDate__ <dttm> : Start date of event.
#' - __endDate__ <dttm> : End date of event.
#' - __sponsor__ <chr> : Event sponsor name.
#' - __type__ <chr> : Event type ("Invitational", "Qualifying", or "Ranking").
#' - __venue__ <chr> : Venue name event was played.
#' - __city__ <chr> : City name event was played.
#' - __country__ <chr> : Country name event was played.
#'
#' @seealso [Snooker players][snooker_players],
#' [snooker matches][snooker_matches]
"snooker_events"

#' Snooker players
#'
#' Data set describing snooker players in seasons 2016/2017 and 2017/2018.
#'
#' @details Data is taken from snooker.org (http://www.snooker.org/) API.
#'
#' Data is present only for players who played at least one game in tracked
#' [snooker events][snooker_events] in seasons 2016/2017 and 2017/2018.
#'
#' @format A [tibble][tibble::tibble] with one row per player and the following
#' columns:
#' - __id__ <int> : Player identifier in snooker.org database (used in
#' `player1Id`, `player2Id` and `winnerId` columns of [snooker_matches]).
#' - __name__ <chr> : Player full name.
#' - __nationality__ <chr> : Player nationality.
#' - __sex__ <chr> : Player gender ("F" for female, "M" for male, and
#' "Unknown").
#' - __born__ <dttm> : Player date of birth.
#' - __status__ <chr> : Player status in season 2017/2018 ("pro" for
#' professional, "ama" for amateur).
#'
#' @seealso [Snooker events][snooker_events], [snooker matches][snooker_matches]
"snooker_players"

#' Snooker matches
#'
#' Data set describing snooker matches in seasons 2016/2017 and 2017/2018.
#'
#' @details Data is taken from snooker.org (http://www.snooker.org/) API.
#'
#' Matches are present only for tracked [snooker events][snooker_events].
#'
#' @format A [tibble][tibble::tibble] with one row per match and the following
#' columns:
#' - __id__ <int> : Match identifier in snooker.org database.
#' - __eventId__ <int> : Match's event identifier (taken from `id` column of
#' [snooker_events])
#' - __round__ <int> : Round number of event in which match was played.
#' _Usually_ event's structure is organized in rounds: sets of matches with
#' roughly "the same importance". _Usually_ the more round number the "more
#' important" matches are played. However, there are many exceptions.
#' - __player1Id__ <int> : Identifier of first player in match (taken from `id`
#' column of [snooker_players]).
#' - __score1__ <int> : Number of won frames (individual games) by first player.
#' - __walkover1__ <lgl> : Whether the win of first player was scored by the
#' technical reasons.
#' - __player2Id__ <int> : Identifier of second player in match (taken from `id`
#' column of [snooker_players]).
#' - __score2__ <int> : Number of won frames (individual games) by second
#' player.
#' - __walkover2__ <lgl> : Whether the win of second player was scored by the
#' technical reasons.
#' - __winnerId__ <int> : Identifier of match's winner (taken from either
#' `player1Id` or `player2Id` columns).
#' - __startDate__ <dttm> : Time at which match started.
#' - __endDate__ <dttm> : Time at which match ended.
#' - __scheduledDate__ <dttm> : Time at which match was scheduled to start.
#' - __frameScores__ <chr> : Scores of players in frames. Usually is missing,
#' present only for important matches.
#'
#' @seealso [Snooker events][snooker_events], [snooker players][snooker_players]
"snooker_matches"
