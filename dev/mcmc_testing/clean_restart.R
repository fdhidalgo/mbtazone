remove.packages("mbtazone")
.rs.restartR()
# Wait until the restart has finished then run:
devtools::document()
devtools::install()
