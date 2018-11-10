## =================================================================
## initialize
## =================================================================
library(reporttools)
path <- "C:/rufibach/01 personal/sport/jahreszusammenfassungen/"

## =================================================================
## input sportKR data
## update .xls file, go to "Daten", copy everything and paste as text (not formulas!), choose "Standardformatierung" and save as .csv
## =================================================================
sportKR <- read.csv(paste(path, "sportKR.csv" , sep = ''), sep = ";", as.is = TRUE)
transformVarNames(sportKR, "sportKR")

sport   <- as.factor(sportKR[ ,"Sport"])
einheit <- as.factor(sportKR[ ,"Einheit"])
x2013   <- sportKR[ ,"X2013"]
x2014   <- sportKR[ ,"X2014"]
x2015   <- sportKR[ ,"X2015"]
x2016   <- sportKR[ ,"X2016"]
x2017   <- sportKR[ ,"X2017"]
x2018   <- sportKR[ ,"X2018"]

dat <- cbind(x2013, x2014, x2015, x2016, x2017, x2018)
y <- 2013:2018

# choose scaling
skal <- c(1, 52)
skal.nam <- c(" pro Jahr", " pro Woche")
s <- 1

## =================================================================
## generation plots
## =================================================================
file <- paste(path, "plots", skal.nam[s], ".pdf", sep = "")
pdf(file, width = 7, height = 10, onefile = TRUE, family = "Helvetica") 
par(mfrow = c(3, 3), oma = c(3, 3, 3, 3), cex = 0.75, mar = c(4, 4, 4, 1), las = 1)

for (i in 1:nrow(sportKR)){
     dat.i <- dat[i, ] / skal[s]
     barplot(dat.i, names = y, main = paste(sport[i], "\n(", tolower(einheit[i]), skal.nam[s], ")", sep = ""))     
     } # end i

par(cex = 1)
dev.off()
graphics.off()


apply(dat[einheit == "Zeit", ], 2, sum)









