# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' A kind function
#'
#' @return NULL
#' @export
#'
#' @examples
#' hello()
hello <- function() {
  print("Hello, world!")
}


#' A polite function
#'
#' @param n Integer with default value of 1.
#' @return Nothing, used for greating only.
#' @export
#'
#' @examples
#' hello2()
hello2 <- function(n = 1) {
  print("Hello2, world!")
}
