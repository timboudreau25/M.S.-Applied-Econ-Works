## Tim Boudreau
## Exam 2 - Take Home
## Econ 217
## February 19th 2016

library(foreign)

##############
## Question 1
##############

# clean R

rm(list = ls())

# clean data

wages.df <- read.table("http://people.ucsc.edu/~aspearot/Econ_217/Wages1983.csv",
                       sep=",", header = TRUE, fill = TRUE)
wages.df <- subset(wages.df, is.na(wage) == FALSE)
wages.df <- subset(wages.df, is.na(educ) == FALSE)
wages.df <- subset(wages.df, is.na(exper) == FALSE)
wages.df <- subset(wages.df, is.na(feduc) == FALSE)
wages.df <- subset(wages.df, is.na(meduc) == FALSE)

## Part a ##

library(gam)

# run a GAM regression and plot the results, with lines to describe the data

gamresults <- gam(log(wage)~s(educ)+s(exper)+s(feduc)+s(meduc), data = wages.df)
summary(gamresults)
#tiff(file = "/Users/Tim/Desktop/UCSC/Winter 2016/Econ 217/Exams/Exam1a.tiff")
par(mfrow=c(2,2))
plot(gamresults, se=TRUE,rug=FALSE,terms="s(educ)", main="Education GAM",
     xlab="Education Level (years)", ylab="Deviation From Mean")
abline(v = 0)
abline(v = ceiling(mean(wages.df$educ)), col ="blue")
abline(v = mean(wages.df$educ))
abline(v = floor(mean(wages.df$educ)), col ="red")
abline(h = 0)
text(16, -.2, paste("Mean (Black)\n", round(mean(wages.df$educ),1)))
plot(gamresults, se=TRUE,rug=FALSE,terms="s(exper)", main="Experience GAM",
     xlab="Experience Level (years)", ylab="Deviation From Mean")
abline(v = 0)
abline(v = ceiling(mean(wages.df$exper)), col ="blue")
abline(v = mean(wages.df$exper))
abline(v = floor(mean(wages.df$exper)), col ="red")
abline(h = 0)
text(16.5, -.1, paste("Mean (Black)\n", round(mean(wages.df$exper),1)))
plot(gamresults, se=TRUE,rug=FALSE,terms="s(feduc)", 
     main="Father's Education GAM",
     xlab="Father's Education Level (years)", ylab="Deviation From Mean")
abline(v = 0)
abline(v = ceiling(mean(wages.df$feduc)), col ="blue")
abline(v = mean(wages.df$feduc))
abline(v = floor(mean(wages.df$feduc)), col ="red")
abline(h = 0)
text(15, -.15, paste("Mean (Black)\n", round(mean(wages.df$feduc),1)))
plot(gamresults, se=TRUE,rug=FALSE,terms="s(meduc)", 
     main="Mother's Education GAM", xlab="Mother's Education Level (years)", 
     ylab="Deviation From Mean")
abline(v = 0)
abline(v = ceiling(mean(wages.df$meduc)), col ="blue")
abline(v = mean(wages.df$meduc))
abline(v = floor(mean(wages.df$meduc)), col ="red")
abline(h = 0)
text(15, -.25, paste("Mean (Black)\n", round(mean(wages.df$exper),1)))
#dev.off()

# show statistical summary of the results

summary(gamresults)


##############
## Question 2
##############

# clean R

rm(list = ls())

# read data from website

## since completion of my program, data extract has been made private. Below,
## I generate the same data my professor provided me.

# tim.df <- read.table("http://people.ucsc.edu/~aspearot/Econ_217/Exam_2_Data/Boudreau_Tim.csv",
#                      sep=",", header = TRUE, fill = TRUE)

# data generation (sloppy but the above commneted code would work under 
# normal circumstances)

x <- seq(0,10,10/99)

y <- c(
  -1.08430914035311,
  0.75747046579542,
  0.109865303717206,
  0.500640464348417,
  0.221941724730752,
  1.44317094577423,
  1.53737464550003,
  -0.529792199681503,
  2.05878541958831,
  0.977319321082073,
  3.20925060747536,
  0.0133252516403253,
  3.33433536448316,
  2.28238626718977,
  5.30654405481833,
  1.87848743654277,
  2.58027868557316,
  1.62673986030924,
  3.44599885791156,
  1.59097785795173,
  6.28572571314876,
  2.21138775283771,
  1.77758181974261,
  2.31769034489577,
  1.78732485017203,
  -1.43771273435395,
  2.26925100949774,
  3.22747662141231,
  7.48073369903557,
  6.07614072486783,
  7.87169756846872,
  6.11733402600087,
  1.46966401828417,
  6.95155515902841,
  8.01452029837,
  10.6391064605964,
  7.99574167287241,
  12.2263937627621,
  10.7709854438542,
  8.47687434260935,
  10.9377008702179,
  5.86133468854765,
  15.7583325416508,
  13.5920642114491,
  13.1200267769064,
  9.06393170392642,
  9.69774037229323,
  13.8388790680879,
  16.8662326619036,
  17.3287439616262,
  13.2822508649708,
  13.7119273157966,
  15.2297981202861,
  16.3179076183637,
  15.7127828406941,
  18.1392243049885,
  18.0667065645482,
  21.3886383426372,
  18.3541846231979,
  20.1878439225866,
  18.1404426791457,
  20.4482118931181,
  20.8022453869528,
  20.0207246120836,
  21.875901781655,
  20.5507219354805,
  22.8731637905255,
  24.7075453930425,
  26.2272056438761,
  23.8740593894415,
  26.26677517701,
  21.6944076574709,
  24.5599616289293,
  24.4372143936338,
  26.9125647514494,
  26.621741287422,
  28.2501419666132,
  28.5992769902814,
  31.2689371778424,
  31.574737686597,
  28.7172951126281,
  27.2100780077814,
  32.7057995758225,
  29.1666848094499,
  32.8255960123761,
  31.9952843639414,
  30.0794709112794,
  32.4382903438773,
  32.7367197775855,
  31.6483757778578,
  34.112180576314,
  36.2061354075877,
  33.3190219051793,
  32.002810460436,
  37.2775754327722,
  35.8566586711056,
  38.1960520632292,
  37.1840209759874,
  38.3473392822284,
  38.4164134206041)

tim.df <- data.frame(x, y)

## Part a ##

# predict values for artificially missing data and store them, cross validation
  
for (h in seq(1,10,.5)) {   # estimated kink locations
  for (i in 1:nrow(tim.df)) {
    timdrop <- tim.df[i,]   #exlude a data point each time
    timkeep <- tim.df[-i,]  
    timkeep$timones <- ifelse(timkeep$x>h, 1, 0)  #set 1 (true) if x > kink,
    timdrop$timones <- ifelse(timdrop$x>h, 1, 0)  #set 0 else
    fit <- glm(y~timones*I(x-h), offset = x, data = timkeep)  #fit missing data
    dropfit <- predict(fit, timdrop, se=FALSE)
    sqerr <- (timdrop$y-as.numeric(dropfit))^2  #calculate SSE for prediction
    if (i*h == 1) {results <- data.frame(h,i,sqerr)}  #store SSE for each h
    if (i*h > 1) {results <- rbind(results, data.frame(h,i,sqerr))}
  }
}

# use the sum of squared errors for each kink value (h)
# to find the h with the smallest SSE as best predicting the kink value

tap.a <- tapply(results$sqerr, results$h, FUN=sum, na.rm=TRUE) #sum SSE for each h 
minSSE.a <- tap.a[as.numeric(which.min(tap.a))] #find minimum kink value h 
kink.a <- as.numeric(names(tap.a)[as.numeric(which.min(tap.a))])
tim.df$ones <- ifelse(tim.df$x>kink.a, 1, 0)  #find x is greater than kink
tim.df$xkink <- tim.df$x - kink.a  #focus only on x > kink values
tim.df$regkink <- tim.df$xk*tim.df$ones  #interact ones and kink/x differential
timreg <- glm(y~regkink, offset = x, data = tim.df)  #generalized linear regression
summary(timreg)

cat("\n\nCross-Validation to find the kink:",
    "\nKink: ", kink.a, "\nSSE: ", minSSE.a)

# print graph with kink, and store the image locally

# tiff(file = "/Users/Tim/Desktop/UCSC/Winter 2016/Econ 217/Exams/Exam2a.tiff")
plot(tim.df$x, tim.df$y, main = "Exam 2 Question 2 Data", 
     xlab = "x", ylab = "y")
abline(v = kink.a)
dev.off()

## Part b ##

# created a function that randomizes the order of the data frame used

randomSample = function(df,n) {
  return (df[sample(nrow(df), n, replace = TRUE),])
}

# number of total bootsrap iterations

B <- 20

# boostrapping loop *** MAY TAKE 10+ MINUTES TO FULLY RUN ***
# identical loop to part a, where we estimate the location of a kink, but
# with bootstrapping, so randomizing the data frame and running 20 times

for (rep in 1:B) {
  rand.df <- randomSample(tim.df, nrow(tim.df)) #randomize data frame, make procedure modular
  for (h in seq(.1,10,.1)) {    # same loop as above, estimate kink location
    for (i in 1:nrow(tim.df)) {   # for each row in the data frame
      timdrop <- rand.df[i,]
      timkeep <- rand.df[-i,]
      timkeep$timones <- ifelse(timkeep$x>h, 1, 0)
      timdrop$timones <- ifelse(timdrop$x>h, 1, 0)
      fit <- glm(y~timones*I(x-h), offset = x, data = timkeep)
      dropfit <- predict(fit, timdrop, se=FALSE)
      sqerr <- (timdrop$y-as.numeric(dropfit))^2
      if (i*h == .1) {results <- data.frame(h,i,sqerr)}
      if (i*h > .1) {results <- rbind(results, data.frame(h,i,sqerr))}
    }
  }
  tap.b <- tapply(results$sqerr, results$h, FUN=sum, na.rm=TRUE)
  minSSE.b <- tap.b[as.numeric(which.min(tap.b))]
  kink.b <- as.numeric(names(tap.b)[as.numeric(which.min(tap.b))])
  if (rep == 1) {results.kink <- data.frame(rep, kink.b)}
  if (rep > 1) {results.kink <- rbind(results.kink, data.frame(rep, kink.b))}
}

# confidence interval for the kink estimation

ci <- quantile(results.kink$kink, prob = c(.05, .95), na.rm=TRUE)

# create graph and save graph

#tiff(file = "/Users/Tim/Desktop/UCSC/Winter 2016/Econ 217/Exams/Exam2b.tiff")
plot(density(results.kink$kink), main = "Distribution of Estimated Kink Values",
     xlab = "Kink Value", ylab = "Density")
abline(v = mean(results.kink$kink), col = "red")
abline(v = kink.a, col = "blue")
text(3.5, .75, paste("Original Value:\n", 
                     kink.a, " (Blue)"))
text(3.5, .65, paste("Mean of Distribution:\n", 
                    mean(results.kink$kink), " (Red)"))
dev.off()

cat("\n\nBootstrap to validate kink value:",
    "\nEstimated Kink: ", mean(results.kink$kink), "\nOriginal Kink: ",
    kink.a, "\n90% Confidence Interval: ", ci, "\n\n")