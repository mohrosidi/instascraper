#' Set up instagram scraper
#'
#' \code{instascraper_install} will install instagram scraper from pip.
#'
#'
#'
#' @importFrom reticulate conda_binary install_miniconda
#'
#' @examples
#' \dontrun{
#' instascraper_install()
#' }
#'
#' @export

instascraper_install <- function(){

  if(is.null(conda_binary()))
    reticulate::install_miniconda()

  system("pip install instagram-scraper")

}
