ncaa2005 <- read.csv(file = file.path("data-raw", "ncaa2005.csv"),
                     header = TRUE, stringsAsFactors = FALSE)
ncaa2005 <- to_longcr(ncaa2005)
save(ncaa2005, file = file.path("data", "ncaa2005.rda"), compress = "bzip2")
