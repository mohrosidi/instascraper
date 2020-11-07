#' Search location id
#'
#' \code{search_location} returns location id based on location name.
#'
#' @param name  Character that specify a location name .
#'
#' @return A sting
#'
#' @examples
#' \dontrun{
#' search_location("Bandung")
#' }
#'
#' @export
search_location <- function(name = NULL){

  if(is.null(name))
    stop(paste("Please specify name"))

  cmd <- paste("instagram-scraper --search-location", name)

  system(cmd)


}




