# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c('shiny', 'shinyjs', 'promises', 'future', 'DT', 'markdown','data.table','dplyr','tidyr','readr','tibble',
                'showtext','bs4Dash','waiter','purrr','devtolls')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))