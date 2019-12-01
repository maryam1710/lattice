BMI = read.table("c:/Users/a/BMI_IRAN.txt", header = TRUE, sep = " ")
#
> head(BMI)
   fat      bmi   wt heigh   wc hp   ac tv.1      age eduf edum sex obesity
1  6.5 18.12130 44.1 156.0 62.0 77 22.0   60 16.00000    4    4   2       0
2 13.3 21.16528 44.5 145.0 66.0 85 25.0  120 16.86575    3    2   2       1
3 15.5 20.61522 45.4 148.4 64.0 82 21.5  300 14.69589    3    2   2       0
4 29.2 25.92867 63.1 156.0 70.0 97 25.0  240 14.63562    4    3   2       1
5 17.6 20.70343 54.2 161.8 66.5 90 23.0  120 16.70685    3    3   2       1
6 25.4 25.04276 57.1 151.0 67.0 92 24.0  300 14.72877    4    2   2       0
  three abdomian     t  systolic diastolic      map time  variable city
1   8.0     10.6 13.60 102.33333  72.00000 82.11111    3 0.8051948    1
2  26.6     15.6 20.90  99.66667  68.00000 78.55556    4 0.7764706    1
3  13.1     12.6 11.20 105.00000  74.33333 84.55556    3 0.7804878    1
4  13.6     21.6 21.60  98.66667  63.33333 75.11111    3 0.7216495    1
5  14.6     12.6 10.05 110.66667  77.33333 88.44444    3 0.7388889    1
6  17.6     22.1 14.60  93.33333  67.00000 75.77778    3 0.7282609    1
> tail(BMI)
      fat      bmi   wt heigh    wc  hp   ac tv.1      age eduf edum sex
2348 36.5 30.74807 82.7 164.0 102.0 104 27.0    1 15.46027    2    2   1
2349 27.8 24.96190 69.2 166.5  85.5  99 27.0    4 15.59726    4    2   1
2350 16.4 20.64279 56.2 165.0  72.0  85 23.5    3 15.36712    3    3   1
2351 23.6 24.44180 74.0 174.0  80.0  94 26.0    1 20.09589    2    2   1
2352 28.0 30.78776 81.8 163.0  91.0 104 32.5    3 17.67123    2    2   1
2353 11.4 19.90356 58.2 171.0  70.0  86 24.0    1 15.17808    4    2   1
     obesity three abdomian    t  systolic diastolic      map time  variable
2348       1  28.5    77.50 57.5  96.33333  64.66667 75.22222    3 0.9807692
2349       0  29.5    55.00 31.5 121.66667  73.00000 89.22222    1 0.8636364
2350       0  21.5    30.50 11.5 121.66667  73.00000 89.22222    1 0.8470588
2351       1  15.5    24.00 17.0 112.66667  65.00000 80.88889    1 0.8510638
2352       0  30.0    38.75 21.5 119.00000  79.33333 92.55556    1 0.8750000
2353       0  16.0    16.50 14.0 121.33333  71.00000 87.77778    3 0.8139535
     city
2348    2
2349    2
2350    2
2351    2
2352    2
2353    2
#
xtabs(~ time, data = BMI)
time
  1   2   3   4 
713 532 777 331 
#
library(lattice)
xyplot(bmi ~ wc | edum , data = BMI, type = "p", layout = 
              c(2, 4), xlim = c(40, 130),  aspect = "iso", strip = F))
#
histogram( ~ bmi | time, data = BMI, nint = 10, 
 equal.widths = T, breaks = NULL,
 type = "density", layout = c(2,2), aspect = 1,
 xlab = "time")
#
library("lattice")
densityplot(~ bmi | factor(time), data = BMI, 
 plot.points = FALSE, ref = TRUE)
#
densityplot(~ bmi,  data = BMI, groups = eduf,  
 plot.points = FALSE, ref = TRUE, 
 auto.key = list(columns = 3))
#
require(tigerstats)
#
bwplot(~bmi,data=BMI,
      xlab="bmi",
      main="IRAN BMI")
#
bwplot(eduf ~ bmi, data = BMI,
        xlab = "bmi", notch = T,
        main="Checking IRAN BMI \nby Father's education of Subject",
        pch = "|", box.ratio = 2,
        panel = function(...) {
            panel.bwplot(...)
        },
        par.settings = list(plot.symbol = list(pch = 7, col = 171)))
#
bwplot(ac~bmi,data=BMI,
        ylab="sex",
        main="Checking IRAN BMI \nby Ac")
#
barchart(sex~bmi | time+city , data = BMI)
#
barchart(sex~bmi | time+city , data = BMI, panel = panel.barchart)
#
gdURL = "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/
 gapminder/data/gapminderDataFiveYear.txt"
gDat = read.delim(file = gdURL)
#
str(gDat)
'data.frame':   1704 obs. of  6 variables:
$ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
$ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
$ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
$ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
$ lifeExp  : num  28.8 30.3 32 34 36.1 ...
$ gdpPercap: num  779 821 853 836 740 ...
#
jDat = droplevels(subset(gDat, continent != "Oceania"))
#
library(lattice)
library(plyr)
#
myAwesomePlot =
 xyplot(lifeExp ~ gdpPercap | continent, jDat,
 scales = list(x = list(log = 10, equispaced.log = T)),
 type = c("p", "smooth"), grid = TRUE, col.line = "darkorange", lwd = 4)
#
print(myAwesomePlot)
#
myOtherPlot = stripplot(lifeExp ~ reorder(continent, lifeExp),
  subset(jDat, subset = year %in% c(1952, 1977, 2007)),
  groups = year, auto.key = list(reverse.rows = TRUE),
  jitter.data = TRUE, type = c("p", "a"), fun = median)
#
print(myAwesomePlot, split = c(1, 1, 2, 1))
print(myOtherPlot, split = c(2, 1, 2, 1), newpage = F)
#
print(myAwesomePlot, pos = c(0, 0, 0.52, .95), more = TRUE)
print(myOtherPlot, pos = c(0.48, 0, 1, 1))
#
lifeExpSpread <- ddply(jDat, ~ continent + year, summarize,
 sdGdpPercap = sd(gdpPercap), iqrGdpPercap = IQR(gdpPercap),
 madGdpPercap = mad(gdpPercap))
#
xyplot(sdGdpPercap + iqrGdpPercap + madGdpPercap ~ year, lifeExpSpread,
 subset = continent == "Africa", type = "b", ylab = "measure of spread",
 auto.key = list(x = 0.07, y = 0.85, corner = c(0, 1)) )
#
xyplot(sdGdpPercap + iqrGdpPercap + madGdpPercap ~ year, lifeExpSpread,
 subset = continent == "Africa", type = "b", ylab = "measure of spread",
 outer = TRUE, layout = c(3, 1), aspect = 1)
#
xyplot(lifeExp ~ gdpPercap, jDat,
 grid = TRUE,
 scales = list(x = list(log = 10, equispaced.log = FALSE)),
 type = c("p", "smooth"), col.line = "darkorange", lwd = 3)
#
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples
           /gapminder/data/gapminderWithColorsAndSorted.txt"
kDat <- read.delim(file = gdURL, as.is = 7) 
str(kDat)
#
jYear <- c(1952, 2007)
yDat <- subset(kDat, year %in% jYear)
#
str(yDat)
'data.frame':   284 obs. of  7 variables:
 $ country  : Factor w/ 142 levels "Afghanistan",..: 25 59 135 67 60 48 15 134 65 9 ...
 $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 2 3 3 4 2 4 4 3 ...
 $ year     : int  1952 1952 1952 1952 1952 1952 1952 1952 1952 1952 ...
 $ pop      : num  5.56e+08 3.72e+08 1.58e+08 8.65e+07 8.21e+07 ...
 $ lifeExp  : num  44 37.4 68.4 63 37.5 ...
 $ gdpPercap: num  400 547 13990 3217 750 ...
 $ color    : chr  "#40004B" "#460552" "#A50026" "#611A6D" ...
#
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)))
#
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
 grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
 panel = panel.xyplot)
#
jCexDivisor <- 1500                     
jPch <- 21
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
       grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
       cex = sqrt(yDat$pop/pi)/jCexDivisor, 
       panel = function(x, y, ..., cex, subscripts) {
        panel.xyplot(x, y, cex = cex[subscripts], pch = jPch, ...)
         })
jDarkGray <- 'grey20'
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
 grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
 cex = sqrt(yDat$pop/pi)/jCexDivisor, fill.color = yDat$color,
 col = jDarkGray,
 panel = function(x, y, ..., cex, fill.color, subscripts) {
 panel.xyplot(x, y, cex = cex[subscripts],
 pch = jPch, fill = fill.color[subscripts], ...)
          })
#
gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A
       /examples/gapminder/data/gapminderContinentColors.txt"
continentColors <- read.delim(file = gdURL, as.is = 3) 
continentKey <-
 with(continentColors,
 list(x = 0.95, y = 0.05, corner = c(1, 0),
 text = list(as.character(continent)),
 points = list(pch = jPch, col = jDarkGray, fill = color)))
xyplot(lifeExp ~ gdpPercap | factor(year), yDat, aspect = 2/3,
 grid = TRUE, scales = list(x = list(log = 10, equispaced.log = FALSE)),
 cex = sqrt(yDat$pop/pi)/jCexDivisor, fill.color = yDat$color,
 col = jDarkGray, key = continentKey,
 panel = function(x, y, ..., cex, fill.color, subscripts) {
 panel.xyplot(x, y, cex = cex[subscripts],
 pch = jPch, fill = fill.color[subscripts], ...)
          })
#
library(lattice) 
BMI$transmission <- factor(BMI$sex,                                
 labels=c("man",  "woman")) 
panel.smoother <- function(x, y) {                     
 panel.grid(h=-1, v=-1)                     
 panel.xyplot(x, y)                      
 panel.loess(x, y, col = 2)                     
 panel.abline(h=mean(y), lwd=2, lty=2, col="green")                  
  } 
xyplot(bmi~fat|transmission,data = BMI,        
 scales=list(cex=.8, col="red"),        
 panel=panel.smoother,
 xlab ="fat", ylab = "bmi" ,            
 main="bmi vs fat by Transmission Type",        
 sub = "Dotted lines are Group Means", aspect=1)
#
