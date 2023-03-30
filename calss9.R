install.packages("MASS")
library("MASS")

snails <- MASS::snails %>% as_tibble()
snails

library( skimr )
skim(snails)
skim_without_charts( snails )

(help(snails))

cbind( snails$Deaths,snails$N - snails$Deaths )[1:10,]

summary( snails_logistic )
