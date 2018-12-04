## =================================================================
## initialize
## =================================================================
library(reporttools)
path <- "C:/rufibach/01 personal/sport/kieser/analyse/"

## =================================================================
## input kieser_uebungen data
## update .xls file, go to "Daten", copy everything and paste as text (not formulas!), choose "Standardformatierung" and save as .csv
## =================================================================
kieser_uebungen <- read.csv(paste(path, "kieser_uebungen.csv" , sep = ''), sep = ";", as.is = TRUE)
date <- as.Date(kieser_uebungen$Datum, origin = "1899-12-30")
kieser <- kieser_uebungen[date > "2006-12-30", ]
date <- as.Date(kieser$Datum, origin = "1899-12-30")


## =================================================================
## generation plots
## =================================================================
file <- paste(path, "plots.pdf", sep = "")
pdf(file, width = 7, height = 10, onefile = TRUE, family = "Helvetica") 
par(mfrow = c(3, 3), oma = c(3, 3, 3, 3), cex = 0.8, mar = c(4, 4, 4, 1), las = 2)

for (i in 2:ncol(kieser_uebungen)){
    v <- kieser[, i]
    dat <- data.frame(v, date)
    dat[dat == 0] <- NA

    if (all(is.na(dat$v)) == FALSE){
          plot(dat$dat, dat$v, type = "l", pch = 1, main = paste("Uebung ", colnames(kieser)[i], sep = ""), xlab = "", ylab = "", col = 2)
          points(dat$dat, dat$v, pch = 19, col = 2)
          }
    if ((i %% 9) == 0){title("Gewichte Uebungen Kieser/Exersuisse Kaspar Rufibach", outer = TRUE, line = 0)}
} # end i

par(cex = 1)
dev.off()
graphics.off()


