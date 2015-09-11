setwd("~/vosd.org/215-opendsd")
df = read.csv('bayes_data_BP.csv')
#df = subset(df,df$Type == 'Conditional Use Permit')
df$X = NULL



# make issued an actual logical
df$Issued = as.logical(df$Issued)

# create some subsets
issued = subset(df, df$Issued == TRUE)
unissued = subset(df, df$Issued == FALSE)

# isolating the integer days for minimum time to issue
# this is # days between application date and 12-05-2014 (the last day in our data)
unissued$minTTI = as.character(unissued$minTTI)
splat = strsplit(unissued$minTTI, split = " ")
# use lapply to pull out just the days, after the string split
min.days = lapply(splat, function(x)(x = x[1]))
# make it a numeric
min.days = as.numeric(min.days)


rm(splat)
# reassign minTTI
unissued$minTTI = min.days
rm(min.days)

# fill in the 'issued' tti (which is sort of nonsensical) w/ # days it actually took to issue
issued$minTTI = issued$TimetoIssue
# pull df back together
df = rbind(issued,unissued)
str(df)

# subset the data we need: 2004-2008
past = subset(df, df$ApplicationYear > 2003 & df$ApplicationYear < 2009)
# we will look only at TTI less than 2000 days, so all 5 of these years are 'complete'
# this is because there are ~2000 days between the end of 2008 and early December 2014 (the end of our data)
past = subset(past, past$Issued == FALSE | past$TimetoIssue < 2001)
# some more subsets
past.issued = past[past$Issued == TRUE,]
past.unissued = past[past$Issued == FALSE,]

# set up bayes dataframe
n = max(df$minTTI)
# we need 0 through n because it is possible to have 0 days to issue
bayes = data.frame(days=c(0:n))
m = nrow(bayes)
bayes$count = c(rep(0, times=m))

# do an rle to count up how many permits took x days to issue for all x between the min and max
# of course, some will be 0 but we need them in the table anyway (bc they will still get a probability)
count = rle(sort(past.issued$TimetoIssue))
bayes$count = count[[1]][match(bayes$days, count[[2]])]
# convert the NAs to zeroes
bayes$count[is.na(bayes$count) == TRUE] = 0

bayes$sum = c(rep(0, times=m))

# this computes how many permits have been issued in x days up to max x
for (i in 1:m) {
  bayes$sum[i] = sum(bayes$count[1:i])
}

total = nrow(past)
total.issued = nrow(past.issued)

bayes$numerator = c(rep(total.issued, times = m))


# the Bayes numerator (it's a simplified one) is the total number issued in AT LEAST x days
# so we have to subtract all the ones issued more quickly from the total issued
for (i in 2:m) {
  bayes$numerator[i] = bayes$numerator - bayes$sum[i-1]
}


# now computing the probability is simple:
# it is the ratio of permits issued in at least x days to the total number of applications
# this gives us P(a|d), the probability that a permit will be issued ('a' for 'approved'), 
# given that d days have passed since its application.
bayes$p = bayes$numerator / total

# we now have our p distribution
# it remains to compute how many outstanding permits in each year are likely someday to be issued
# we will index minTTI for each to bayes$days, and sum the indexed probabilities


# set up our projection dataframe
p.cohort = data.frame(year = c(2004:2013))

# get the total applications for each year using rle (such a godsend function!)
count = rle(sort(df$ApplicationYear))
p.cohort$total = count[[1]][match(p.cohort$year, count[[2]])]

# now do the same with total issued
count = rle(sort(issued$ApplicationYear))
p.cohort$issued = count[[1]][match(p.cohort$year, count[[2]])]

# now unissued
count = rle(sort(unissued$ApplicationYear))
p.cohort$unissued = count[[1]][match(p.cohort$year, count[[2]])]

# proportion issued
p.cohort$prop.issued = p.cohort$issued / p.cohort$total

# initialize some variables to be adjusted later
# to be exact, only 2009-2013 will be adjusted
p.cohort$p.issued = p.cohort$issued
p.cohort$p.unissued = p.cohort$unissued
p.cohort$p.prop.issued = p.cohort$prop.issued

# adding in the probabilities from our model using a match (it's like a join, essentially)
unissued$p = bayes$p[match(unissued$minTTI, bayes$days)]

# initialize some dataframes just to make things easier
ui.2009 = unissued[unissued$ApplicationYear == 2009,]
ui.2010 = unissued[unissued$ApplicationYear == 2010,]
ui.2011 = unissued[unissued$ApplicationYear == 2011,]
ui.2012 = unissued[unissued$ApplicationYear == 2012,]
ui.2013 = unissued[unissued$ApplicationYear == 2013,]

# initialize an 'extra' column in p.cohort which will be sum of the outstanding permit probabilities
p.cohort$extra = c(rep(0, times = nrow(p.cohort)))

# assign probs in 2009 that are above 2000 days to zero (it's a little inaccurate, but our model does not
# assign probabilities to permits older than 2000 days)
ui.2009$p[is.na(ui.2009$p) == TRUE] = 0


# finally: adding in our projected extra permits!
p.cohort$extra[p.cohort$year == 2009] = round(sum(ui.2009$p))
p.cohort$extra[p.cohort$year == 2010] = round(sum(ui.2010$p))
p.cohort$extra[p.cohort$year == 2011] = round(sum(ui.2011$p))
p.cohort$extra[p.cohort$year == 2012] = round(sum(ui.2012$p))
p.cohort$extra[p.cohort$year == 2013] = round(sum(ui.2013$p))

# now update other variables
p.cohort$p.issued = p.cohort$issued + p.cohort$extra
p.cohort$p.unissued = p.cohort$total - p.cohort$p.issued
p.cohort$p.prop.issued = p.cohort$p.issued / p.cohort$total

# okay, so no huge changes, but we now have a principled estimation of each year's cohort
# now it remains to compute some deciles on the basis of our predicted cohort size

# As a kind of aside: the motivation for doing what we're doing here is that we want to be able
# to confidently report a genuine median for each year (and maybe some other deciles, too).
# But we couldn't do that before, because we didn't know the actual size of the set of issued permits
# for each year. After all, D permits can take years to get issued, so recent years are not 'complete' 
# yet. However, we know that if any outstanding applications get issued henceforth, they will live in 
# the higher deciles of each year's distribution. Since we are looking most recently at 2013, any outstanding
# application has to have at minimum a TTI of the difference between Dec 31, 2013, and Dec 5, 2014.
# That is 

31+28+31+30+31+30+31+31+30+31+30+5

# 339 days. So, no matter when any of the outstanding permits get issued, they will have at least a 339 day TTI.
# Depending on where exactly that is in 2013's distribution, we can look at deciles for every year reasonably
# close to the median (the 50th decile). So essentially, we can compute the median for each year, even though
# we don't have all the data. Neat, huh?


## Computing deciles
# First, let's define a function to compute deciles:

ytile = function(x,y,n) {
  index = round(n * y)
  x = sort(x)
  return(x[index])  
}

# Now we can start building a dataframe with all of our deciles

# First, although we will use our projected total as the denominator in our median calculation,
# we are only actually looking at issued permits (and only those below 2001 days).

issued = issued[issued$TimetoIssue < 2001,]

# Our 'n' for the overall calculation is just the sum of all p.issued in p.cohort:

sum(p.cohort$p.issued)

# Okay, so we are going to build a dataframe for D overall.
# We know that our n is 4431. We want as many deciles as we can get, but 
# some will not compute because in the highest deciles, we will be indexing
# a vector that is shorter than the vector of all p.issued permits (that is,
# all the permits that will ever be issued, from among those applied for in 
# our data). 

overall = data.frame(year = c(2004:2013),
                     ten = c(1:10),
                     twenty = c(1:10),
                     thirty = c(1:10),
                     forty = c(1:10),
                     fifty = c(1:10),
                     sixty = c(1:10),
                     seventy = c(1:10),
                     eighty = c(1:10),
                     ninety = c(1:10))

sum(p.cohort$p.issued)


ten = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .1, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .1, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .1, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .1, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .1, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .1, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .1, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .1, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .1, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .1, p.cohort$p.issued[p.cohort$year == 2013]))

twenty = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .2, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .2, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .2, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .2, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .2, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .2, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .2, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .2, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .2, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .2, p.cohort$p.issued[p.cohort$year == 2013]))

thirty = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .3, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .3, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .3, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .3, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .3, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .3, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .3, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .3, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .3, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .3, p.cohort$p.issued[p.cohort$year == 2013]))

forty = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .4, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .4, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .4, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .4, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .4, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .4, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .4, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .4, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .4, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .4, p.cohort$p.issued[p.cohort$year == 2013]))

fifty = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .5, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .5, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .5, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .5, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .5, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .5, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .5, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .5, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .5, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .5, p.cohort$p.issued[p.cohort$year == 2013]))

sixty = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .6, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .6, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .6, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .6, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .6, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .6, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .6, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .6, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .6, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .6, p.cohort$p.issued[p.cohort$year == 2013]))

seventy = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .7, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .7, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .7, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .7, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .7, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .7, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .7, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .7, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .7, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .7, p.cohort$p.issued[p.cohort$year == 2013]))

eighty = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .8, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .8, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .8, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .8, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .8, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .8, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .8, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .8, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .8, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .8, p.cohort$p.issued[p.cohort$year == 2013]))

ninety = c(ytile(issued$TimetoIssue[issued$ApplicationYear == 2004], .9, p.cohort$p.issued[p.cohort$year == 2004]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2005], .9, p.cohort$p.issued[p.cohort$year == 2005]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2006], .9, p.cohort$p.issued[p.cohort$year == 2006]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2007], .9, p.cohort$p.issued[p.cohort$year == 2007]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2008], .9, p.cohort$p.issued[p.cohort$year == 2008]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2009], .9, p.cohort$p.issued[p.cohort$year == 2009]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2010], .9, p.cohort$p.issued[p.cohort$year == 2010]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2011], .9, p.cohort$p.issued[p.cohort$year == 2011]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2012], .9, p.cohort$p.issued[p.cohort$year == 2012]),
        ytile(issued$TimetoIssue[issued$ApplicationYear == 2013], .9, p.cohort$p.issued[p.cohort$year == 2013]))


overall$ten = ten
overall$twenty = twenty
overall$thirty = thirty
overall$forty = forty
overall$fifty = fifty
overall$sixty = sixty
overall$seventy = seventy
overall$eighty = eighty
overall$ninety = ninety

overall


# Now we need to consider which deciles we can report
# This issue here is that some of our low unknowns may actually be 
# lower than some existing data points in the distribution
# Take 2013 for example. The minimum TTI for an unissued 2013 permit is 339 days.
# But there are some permits applied for in 2013 that were issued after more days than that, because
# they were applied for early in the year, and not approved until late 2014. We know this
# because the seventieth percentile for 2013 is already 354. So, we can't actually report that 
# percentile, because we don't know whether one of the unknowns would end up HIGHER than everything
# else in 2013. 

#339                  = 339
#339+365              = 704
#339+365+365          = 1069
#339+365+365+365      = 1434
#339+365+365+365+365  = 1799

# Okay so, here's the breakdown of deciles we can report:
# 2013: through sixty
# 2012: through eighty
# 2011: all
# all from the rest, too

# So, the good news is that we always report the median. I think it'd be nice and readable just to look at the middle band:
# The fortieth, fiftieth, and sixtieth percentiles for each year.

plot = data.frame(year=overall$year, forty = overall$forty, fifty = overall$fifty, sixty = overall$sixty)

#write.csv(plot, '/home/damoncrockett/vosd.org/215-opendsd/tiles_CUP.csv')
#write.csv(overall, '/home/damoncrockett/vosd.org/215-opendsd/tiles_overall_BP.csv')





