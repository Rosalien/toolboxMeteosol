# https://www.imo.universite-paris-saclay.fr/~goude/Materials/ProjetMLF/editer_package_R.html
# https://thinkr.fr/creer-package-r-quelques-minutes/
# https://rtask.thinkr.fr/fr/quand-le-developpement-commence-par-la-documentation/
# https://github.com/ThinkR-open/golem

# Tous les paquets utilisés
usethis::use_package("dplyr")
usethis::use_package("data.table")
usethis::use_package("lubridate")
usethis::use_package("testthat") # for unit test
usethis::use_package("dygraphs")
usethis::use_package("reshape")
usethis::use_package("xts")
usethis::use_package("wesanderson") # for color palette 

# Pour ne pas prendre en compte le fichier
usethis::use_build_ignore("devtools_history.R")

