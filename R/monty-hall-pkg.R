#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title Player selects a door
#'
#' @description
#' Randomly selects one of three doors
#' @details
#' Simulates a player making an initial choice between three doors
#'
#' @param ... No arguments are required
#'
#' @return An integer between 1 and 3 representing the selected door
#'
#' @examples 
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Host opens a door revealing a goat
#'
#' @description
#' Determines which door the host opens to reveal a goat
#'
#' @details
#' If the player's pick is the car, the host randomly pick one of the  remaining goat doors. If it's a goat, the host opens the only remaining goat door.
#'
#' @param game A character vector representing the game setup
#' @param a.pick An integer indicating the door selected by the player.
#'
#' @return An integer representing the door number opened by the host.
#'
#' @examples
#' open_goat_door(c("goat", "car", "goat"), 2)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Determine final door choice
#' @description Decides whether the player stays with the initial choice or switches
#' @details If stays is TRUE, returns the initial pick; otherwise returns the other unopened door
#' @param stay Logical value. TRUE if player stays with the original pick; FALSE to switch
#' @param opened.door An integer of the door opened by the host
#' @param a.pick An integer of the initial door picked by the player
#' @return An integer of the final door picked by the player
#' @examples change_door(TRUE, 3, 1)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine game result
#' @description Reveals if the player has won a car or a goat
#' @details Checks if the final door picked by the player contains the car
#' @param final.pick An integer of the door picked by the player
#' @param game A character vector of the game setup
#' @return A string: "WIN" if car, "LOSE" if goat
#' @examples determine_winner (2, c("goat", "car", "goat"))
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Simulate one game
#' @description Simulates one round of Monty Hall game using both strategies: stay and switch
#' @details Simulates full game process and returns a data frame showing the outcome of both strategies
#' @param ... No argument are required
#' @return A data frame with columns: strategy and outcome
#' @examples play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Simulate multiple games
#' @description Plays the Monty Hall game multiple times and summarizes the results.
#' @details Useful for observing the statistical advantage of switching doors over multiple trials
#' @param n Number of games to play. Default is 100
#' @return A data frame of all outcomes
#' @examples play_n_games(10)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
