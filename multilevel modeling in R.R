#########################################################
#                 MULTILEVEL MODELING IN R              #
#########################################################

# first, install and load required packages:
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("lme4")
usePackage("lmerTest")
usePackage("plotrix")
usePackage("RLRsim")
usePackage("RCurl")
usePackage("scales")

# load dataset
dat <- read.csv(text = getURL("https://raw.githubusercontent.com/paulrconnor/mlm/master/meat%2Bhealth.csv"))

# our first dataset includes observations of 1300 individuals
# from 13 countries. It includes two individual-level
# variables and two cuntry-level variables 
# meat_consumption - a measure of annual meat consumption (in pounds)
# health - a standardised measure of physical health
# country - country name (our grouping variable)
# GDP_pc - gross domestic product per capita
str(dat)
head(dat)

# We are interested in the relationship between meat
# consumption and overall health. We can plot this relationship
# with a basic scatter-plot
plot(dat$health ~ dat$meat_consumption, # health is on our Y-axis, meat_consumption is on our X-axis
     pch=16, # this makes the points solid
     bty="n", # this removes the oplot outline (a personal preference)
     cex=0.5, # this shrinks the points
     xlab="Meat consumption (lb/year)", # this labels the x axis
     ylab="Standardized health score",
     main="Health and meat consumption") # this labels the y axis

# From this plot we can see that meat consumption is highly positively skewed.
# But it is difficult to determine whether the relationship is positive, negative, or
# there is no relationship. 

# 1. LINEAR-MODEL

# We could just fit a linear model to the data, and ignore the nested 
# structure of the data: Y_i = a + b*X_i + e_i
# Where Y_i is individual-level health
# X_i is individual level meat consumption
# e_i is random individual-level error
mod0 <- lm(health ~ meat_consumption, data=dat)
summary(mod0)
# Our fitted model is: health_i = -0.149 + 0.002*meat_consumption_i + e_i

# the slope on meat_consumption is significantly higher than 0, 
# suggesting that there is an overall positive relationship between meat 
# consumption and health. We can add the line of best fit to our plot:
ablineclip(mod0, # specifies the intercept and slope
           x1=0,x2=600, # specifies where to clip line horizontally
           y1=-3, y2=3, # specifies where to clip line vertically
           col="red", # specifies a color
           lwd=2) # sets line width

# 2. RANDOM INTERCEPT MULTI-LEVEL MODEL

# BUT we have nested data, and we should probably take this into account. 
# Let's add a random intercept to our model. 
# We now fit: Y_ij = a + b*X_ij + e_j + e_ij
# Where Y_ij is individual-level health of individual i in country j
# X_ij is individual level meat consumption of individual i in country j
# e_j is random country-level error of country j
# e_ij is random individual-level error of individual i in country j
mod1 <- lmer(health ~ meat_consumption + (1|country), data=dat) # we now use lmer() not lm(),
# and add + (1|country) to the model
summary(mod1)
# Our fitted model is: health_ij = 0.04 - 0.00076*meat_consumption_ij + e_j + e_i
# Now there is a significant negative slope on meat_consumption. This
# suggests that the previously observed positive effect may have been
# related to country-level differences.

# We can view our random effects using coef()
coef(mod1) # different intercepts, same slopes (we haven't allowed slopes to vary yet)

# It is normal to report fixed effects and also the variance of random effects.
# The variance of the random intercepts is estimated to be 0.67, the 
# variance of the residuals is estimated to be 0.43.
# That makes the intra-class correlation ICC = 0.67/(0.67 + 0.43) = 0.61

# Another question we may have is whether the variance of the
# random intercepts is significantly different from zero.
# We can use the exactRLRT() function for this:
exactRLRT(mod1)
# The test is significant, suggesting that a significant amount
# of variance is accounted for by between-country differences

# To plot this result:
palette(rainbow(13,s = 0.7)) # creates a color palette with 13 levels
dat$col <- sort(rep(1:13,100)) # adds a color for each country
par(oma=c(0,0,0,7),mar=c(3,3,2,0)) # sets size of outer margins and inner margins (bottom, left, top, right)
plot(dat$health ~ dat$meat_consumption, # health is on our Y-axis, meat_consumption is on our X-axis
     col=alpha(dat$col,0.5), # we now color the points according to country
     pch=16, # this makes the points solid
     bty="n", # this removes the oplot outline (a personal preference)
     cex=0.5, # this shrinks the points
     cex.main=0.9, # adjusts size of title
     cex.lab=0.9, # adjusts size of axis labels
     cex.axis=0.8, # adjusts size of axis text
     xlab="", # this makes x axis blank
     ylab="") # this makes y axis label blank
title(ylab="Standardized health score", # this adds a y axis label
      line=2, # sets distance to plot
      cex.lab=0.9) # sets size of label
title(xlab="Meat consumption (lb/year)", # this adds a x axis label
      line=2, # sets distance to plot
      cex.lab=0.9) # sets size of label
title(main="Meat consumption and health", # this adds a title
      line=0.7, # sets distance to plot
      font.main=1, # sets font to be non-bold
      cex.main=1) # sets size of title
legend(x=620,y=3.2, # setds location of legend
       cex=0.7, # sets size of text
       legend=unique(dat$country), # adds labels
       col=unique(dat$col), # sets colors
       bty="n", # removes border
       xpd=NA, # allows legend to display outside plot
       lty=1, # specifies that we want lines
       lwd=2) # increases width of lines
# We can plot the model's prediction lines for each of the countries like so
fixed.intercept <- summary(mod1)$coef[,1]["(Intercept)"] # saves the fixed intercept
for(i in 1:13){ # this for loop cycles through each country
  country <- row.names(coef(mod1)$country)[i] # specifies which country to plot a line for
  ablineclip(a= fixed.intercept + coef(mod1)$country[i,1], # sets the intercept for the country (fixed intercept + random intercept)
             b= coef(mod1)$country[i,2],# sets the slope for the country
             x1=0,x2=600, # specifies where to clip line horizontally
             y1=-3, y2=3, # specifies where to clip line vertically
             col=unique(dat$col[dat$country==country]), # specifies the color we have attached to the country
             lwd=1) # sets line width
} # closes loop
# we can also add a line for the fixed effect of meat consumption like so:
fixed.slope <- summary(mod1)$coef[,1]["meat_consumption"] # saves the fixed effect estimates
ablineclip(a=fixed.intercept, # sets the intercept 
           b= fixed.slope,# sets the slope 
           x1=0,x2=600, # specifies where to clip line horizontally
           y1=-3, y2=3, # specifies where to clip line vertically
           col="black", # specifies the color we have attached to the country
           lwd=2)
legend("bottomright", # sets location for legend
       cex=0.8, # sets text size
       legend="Fixed effect", # adds label
       col="black", # sets color 
       bty="n", # removes border
       lty=1, # specifies to add line
       lwd=2) # adjusts line width

# 3. RANDOM SLOPE MODEL

# If we want to test whether the relationship between meat consumption
# and health varies between countries, we can add random slopes
# on meat consumption. 
# We now fit: Y_ij = a + b*X_ij + b(r)_j*X_ij  e_j + e_ij 
# Where Y_ij is individual-level health of individual i in country j
# X_ij is individual level meat consumption of individual i in country j
# b(r)_j indicates countries' random slopes on X_ij
# e_j is random country-level error of country j
# e_ij is random individual-level error of individual i in country j
mod2 <- lmer(health ~ meat_consumption + (meat_consumption|country), data=dat)
# adding "+ (meat_consumption|country)" adds random slopes on meat_consumption and random intercepts for countries
summary(mod2)
# Our fitted model is: health_ij = 0.03 + 0.001*meat_consumption_ij + b(r)_j*meat_consumption_ij + e_j + e_i
# After taking account of the different slopes in different countries, 
# We now estimate a non-significant positive fixed effect for meat consumption

# We also now get some warnings from the function. These are mainly caused by having variables
# on different scales (meat consumption ranges from 0 to 600, health from -3 to 3), 
# which causes R some problems in fitting the model. It is relatively
# safe to ignore these, I believe. If we z-score our variables we don't get these warnings, 
# but all t and p values are the same:
summary(lmer(scale(health) ~ scale(meat_consumption) + (scale(meat_consumption)|country), data=dat))

# Here a key question is whether the random slopes acocunted for a significant
# amount of variance. We can test this by comparing a model without the random
# slopes to a model with the random slopes, using anova().
anova(mod1,mod2) # this suggests the random slopes accounted for a significant amount of variance

# To plot this result:
plot(dat$health ~ dat$meat_consumption, # health is on our Y-axis, meat_consumption is on our X-axis
     col=alpha(dat$col,0.5), # we now color the points according to country
     pch=16, # this makes the points solid
     bty="n", # this removes the oplot outline (a personal preference)
     cex=0.5, # this shrinks the points
     cex.main=0.9, # adjusts size of title
     cex.lab=0.9, # adjusts size of axis labels
     cex.axis=0.8, # adjusts size of axis text
     xlab="", # this makes x axis blank
     ylab="") # this makes y axis label blank
title(ylab="Standardized health score", # this adds a y axis label
      line=2, # sets distance to plot
      cex.lab=0.9) # sets size of label
title(xlab="Meat consumption (lb/year)", # this adds a x axis label
      line=2, # sets distance to plot
      cex.lab=0.9) # sets size of label
title(main="Meat consumption and health", # this adds a title
      line=0.7, # sets distance to plot
      font.main=1, # sets font to be non-bold
      cex.main=1) # sets size of title
legend(x=610,y=3.2, # setds location of legend
       cex=0.7, # sets size of text
       legend=unique(dat$country), # adds labels
       col=unique(dat$col), # sets colors
       bty="n", # removes border
       xpd=NA, # allows legend to display outside plot
       lty=1, # specifies that we want lines
       lwd=2) # increases width of lines
# We can plot the model's prediction lines for each of the countries like so
for(i in 1:13){ # this for loop cycles through each country
  country <- row.names(coef(mod2)$country)[i] # specifies which country to plot a line for
  ablineclip(a=coef(mod2)$country[i,1], # sets the intercept for the country
             b= coef(mod2)$country[i,2],# sets the slope for the country
             x1=0,x2=600, # specifies where to clip line horizontally
             y1=-3, y2=3, # specifies where to clip line vertically
             col=unique(dat$col[dat$country==country]), # specifies the color we have attached to the country
             lwd=1) # sets line width
} # closes loop
# we can also add a line for the fixed effect of meat consumption like so:
fixed.intercept <- summary(mod2)$coef[,1]["(Intercept)"] # saves the fixed intercept
fixed.slope <- summary(mod2)$coef[,1]["meat_consumption"] # saves the fixed slope
ablineclip(a=fixed.intercept, # sets the intercept 
           b= fixed.slope,# sets the slope 
           x1=0,x2=600, # specifies where to clip line horizontally
           y1=-3, y2=3, # specifies where to clip line vertically
           col="black", # specifies the color we have attached to the country
           lwd=2)
legend("bottomright", # sets location for legend
       cex=0.8, # sets text size
       legend="Fixed effect", # adds label
       col="black", # sets color 
       bty="n", # removes border
       lty=1, # specifies to add line
       lwd=2) # adjusts line width

# 4. CROSS-LEVEL INTERACTION MODEL

# Based on these results, we know there is significant heterogeneity in the
# effect of meat consumption between countries. Now that we know this, 
# we might try to explain that heterogeneity with a cross-level interaction. 
# Perhaps, for example, the relationship between meat consumption
# and health is positive in poorer countries (where meat consumption is
# a marker of socioeconomic status), but negative in wealthier countries
# (where meat consumption might be indicative of a bad diet or over-eating).
# We now fit: Y_ij = a + b1*X_ij +  b2*Z_j + b3*X_ij*Z_j + b(r)_j*X_ij  e_j + e_ij 
# Where Y_ij is individual-level health of individual i in country j
# X_ij is individual level meat consumption of individual i in country j
# Z_j is a country-level measure of income per capita
# X_ij*Z_ij are the products of multiplying individual meat consumption by country-level income per capita
# b(r)_j indicates countries' random slopes on X_ij
# e_j is random country-level error of country j
# e_ij is random individual-level error of individual i in country j
mod3 <- lmer(health ~ meat_consumption*GDP_pc + (meat_consumption|country), data=dat)
summary(mod3)
# Our fitted model is: health_ij = -0.96 + 0.004*meat_consumption_ij + 0.00004*GDP_pc_j + 
#                                  -0.0000001*meat_consumption_ij*GDP_pc_j + 
#                                  b(r)_j*meat_consumption_ij + e_j + e_i
# there is a significant cross-level interaction effect. 

# It is possible that including GDP per capita and the cross-level interaction
# explained all the variance in slopes. In this case, the random slopes
# would no longer be accounting for significant amounts of variance.
# We can test this by fitting a reduced model without the random slopes
# and performing a model comparison
mod3.reduced <- lmer(health ~ meat_consumption*GDP_pc + (1|country), data=dat)
anova(mod3.reduced,mod3) # the random slopes are now not significantly improving the model fit

# We can plot this interaction like so:
  # reset outer margins
# create a new colour variable that follows a red -> blue gradient according to GDP per capita variable
dat$col <- colorRampPalette(c('red','blue'))(20)[as.numeric(cut(dat$GDP_pc,breaks = 20))] 
dat <- dat[order(-dat$GDP_pc),] # order the data by GDP per capita
legend_image <- as.raster(matrix(dat$col, ncol=1))
plot(dat$health ~ dat$meat_consumption, # health is on our Y-axis, meat_consumption is on our X-axis
     col=scales::alpha(dat$col,0.3), # we now color the points according to GDP per capita
     pch=16, # this makes the points solid
     bty="n", # this removes the oplot outline (a personal preference)
     cex=0.4, # this shrinks the points
     cex.main=0.9, # adjusts size of title
     cex.lab=0.9, # adjusts size of axis labels
     cex.axis=0.8, # adjusts size of axis text
     xlab="", # this makes x axis blank
     ylab="") # this makes y axis label blank
title(ylab="Standardized health score", # this adds a y axis label
      line=2, # sets distance to plot
      cex.lab=0.9) # sets size of label
title(xlab="Meat consumption (lb/year)", # this adds a x axis label
      line=2, # sets distance to plot
      cex.lab=0.9) # sets size of label
title(main="Meat consumption and health", # this adds a title
      line=0.7, # sets distance to plot
      font.main=1, # sets font to be non-bold
      cex.main=1) # sets size of title
country.col.list <- c() # creates empty list for country colors
for(i in 1:13){ # this for loop cycles through each country
  country <- unique(dat$country)[i] # specifies which country to plot a line for
  country.GDP <- unique(dat$GDP_pc[dat$country==country]) # stores country's GDP per capita
  country.coefs <- coef(mod3)$country[i,] # stores country-specific intercept and slopes
  country.col <- unique(dat$col[dat$country==country]) # stores the country's color
  country.col.list <- c(country.col.list, country.col) # adds country's color to color list
  ablineclip(a= as.numeric(country.coefs["(Intercept)"] + country.coefs["GDP_pc"]*country.GDP), # sets the intercept for the country
             b= as.numeric(country.coefs["meat_consumption"] + country.coefs["meat_consumption:GDP_pc"]*country.GDP),# sets the slope for the country
             x1=0,x2=600, # specifies where to clip line horizontally
             y1=-3, y2=3, # specifies where to clip line vertically
             col=alpha(country.col,0.5), # specifies the color we have attached to the country
             lwd=1,lty=3) 
} # closes loop
legend(x=640,y=2.8, # setds location of legend
       cex=0.7, # sets size of text
       legend=unique(dat$country), # adds labels
       col=alpha(country.col.list,0.7), # sets colors
       bty="n", # removes border
       xpd=NA, # allows legend to display outside plot
       lty=3, # specifies that we want lines
       lwd=2) # increases width of lines
fixed <- summary(mod3)$coef[,1] # stores the fixed effect estimates
plus1sdgdp <- mean(dat$GDP_pc) + sd(dat$GDP_pc) # adds one SD GDP to the mean of GDP
meangdp <- mean(dat$GDP_pc) # stores the mean of GDP
minus1sdgdp <- mean(dat$GDP_pc)-sd(dat$GDP_pc) # subtracts one SD GDP from the mean of GDP
# curve() adds lines (in this case a straight line) to plots that are a function of
# the x axis. We can use our model coefficients to fit prediction lines 
# at different levels of GDP per capita
curve(fixed["(Intercept)"] + fixed["meat_consumption"]*x + fixed["GDP_pc"]*plus1sdgdp +
        fixed["meat_consumption:GDP_pc"]*x*plus1sdgdp, # the function of x
      from=0,to=600, # the horizontal limits of the line
      add=T, # specifies that we don;t want a whole new plot
      col="blue", # sets color
      lwd=2) # sets line width
curve(fixed["(Intercept)"] + fixed["meat_consumption"]*x + fixed["GDP_pc"]*meangdp +
        fixed["meat_consumption:GDP_pc"]*x*meangdp, # the function of x
      from=0,to=600, # the horizontal limits of the line
      add=T, # specifies that we don;t want a whole new plot
      col="#5500AA", # sets color
      lwd=2) # sets line width
curve(fixed["(Intercept)"] + fixed["meat_consumption"]*x + fixed["GDP_pc"]*minus1sdgdp +
        fixed["meat_consumption:GDP_pc"]*x*minus1sdgdp, # the function of x
      from=0,to=600, # the horizontal limits of the line
      add=T, # specifies that we don;t want a whole new plot
      col="red", # sets color
      lwd=2) # sets line width
legend("bottomright", # sets legend location
       c("+1SD GDPpc", "Mean GDPpc", "-1SD GDPpc"), # sets labels
       lty=1, # specifies lines
       cex=0.7, # sets size of text
       col=c("blue","#5500AA","red"), # specifies colors
       lwd=2, # changes line width
       bty = "n") # removes border

##### EXAMPLE 2 - DAILY DIARY STUDY

# A lot of the time you will need to restructure your data in order to
# use multilevel models. The next datafile we're going to
# work with is longitudinal data from a diary study, where 200 individuals
# were first measured on trait neuroticism, and then reported
# (a) daily number of negative events and (b) daily happiness, over a 
# seven day period
diary.dat <- read.csv(text = getURL("https://raw.githubusercontent.com/paulrconnor/mlm/master/diarystudy.csv"))
str(diary.dat)
# 'id' identifies the participant
# 'neur' is a standardised measure of trait neuroticism
# neg.day1, neg.day2, etc are daily reports of negative experiences
# happy.day1, happy.day2 etc are daily reports of feelings of happiness on a 0-7 scale.

# Here we have nested data, because we have observations nested within individuals. But
# the data is currently in 'wide' format, with all data for each participant in 
# each single row:
head(diary.dat)

# We need to use reshape() to make this data into 'long' format, similar to
# the country data we used above, where each row represents a single observation
# nested within participants (i.e., each participant will now have seven rows, 
# like how each country had many rows above):
diary.long <- reshape(diary.dat, # the dataset we want to reshape
                      varying = c("neg.day1","neg.day2", # the variables nested within individuals
                                  "neg.day3","neg.day4",
                                  "neg.day5","neg.day6",
                                  "neg.day7",
                                  "happy.day1","happy.day2",
                                  "happy.day3","happy.day4",
                                  "happy.day5","happy.day6",
                                  "happy.day7"), 
                      idvar="id", # the id variable
                      direction="long", # the shape of data we want
                      timevar="day") # a name for the 'time variable'

# order the data by the id variable (not essential!)
diary.long <- diary.long[order(diary.long$id),] 

# we can now see that we have multiple rows per participant (long format).
# This enables us to perform multilevel models.
head(diary.long) 

# Now you try! 

# 1. Using the techniques above, can you assess the impact of daily neagtive 
#    experiences on daily happiness?
#    Can you plot this?
# 2. Is it necessary to account for individual differences in trait neuroticism?
#    Can you plot this?
# 3. What is the impact of daily neagtive experiences on daily happiness accounting for individual 
#    differences in trait neuroticism?
#    Can you plot this?
# 4. Is it necessary to account for individual differences in the effect of negative experiences?
#    Can you plot this?
# 5. Does trait neuroticism help account for individual differences in the effect of negative experiences?
#    Can you plot this?
