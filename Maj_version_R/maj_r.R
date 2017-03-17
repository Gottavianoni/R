
rep1 <- "C:/R/R-3.2.4revised/library/"
rep2 <- "A:/R/R/library/"
pckg1 <- list.files(rep1)
pckg2 <- list.files(rep2)
pckg <- pckg2[which(is.na(match(pckg2,pckg1)))]

for(pck in pckg) try(install.packages(pck))

