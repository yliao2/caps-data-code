
#day of week screening
#use function: obs.rank (create)
#level set plot for DOW (alpha = 0.1)
#-------------------------------
time <- "DayOfWeekUSA"
week <- obs.rank(2015, 71, time)
tag <- c("sun","mon","tue","wed","thu","fri","sat")
alpha <- 0.05

#subset "data" into "tag" given "time" in a ranked order (implicit)
#------------------------------------------------------------------
rank.time.dt <- function(data, tag, time)
{
  for (var in tag)
  {assign(var, data %>% filter(get(time)==which(tag==var)) %>% arrange(rank), inherits = TRUE)}
}
#create separate data sets based on the setting "Line 5-9"
#generate data: sun, mon, tue, wed, thu, fri, sat
rank.time.dt(data = week, tag = tag, time = time)


#create lvl set results from subset "data" from "time"
#use data generated from "Line 18"
#-----------------------------------------------------
lvl.set.time <- function(tag, alpha)
{
  for (var in tag)
  {
    out <- NULL
    for (i in 1:length(alpha))
    {
      d <- data.frame(lvl.set(data = get(var), alpha = alpha[i]),
                      time = which(tag==var))
      out <- rbind(out, d)
    }
    assign(paste(var,".lvl",sep = ""), out, inherits = TRUE)
  }
}
#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- rbind(sun.lvl, mon.lvl, tue.lvl, wed.lvl, thu.lvl, fri.lvl, sat.lvl)


#fn. for paired comparison 
#(x = which groups to compare (based on combn function), 
# alpha = significance level)
#---------------------------------------------------------
origin.pair <- function(x, alpha, var)
{
  a <- get(x[1])
  b <- get(x[2])
  if(sum(a[[var]])==1) {out <- ks.results(s1 = cumsum(a[[var]]), s2 = cumsum(b[[var]]), alpha = alpha)}
  else {out <- ks.results(s1 = a[[var]], s2 = b[[var]], alpha = alpha)}
  return(out$out)
}


#fn. for ks result for multiple comparison
#(data has to include all subsets)
#-----------------------------------------
pair.data <- function(data, tag, time, var, alpha)
{
  #combination of tag (for paired comparisons)
  comb <- combn(x = tag, m = 2)
  
  for (i in 1:length(tag))
  {assign(tag[i], data[which(data[[time]]==i),], inherits = TRUE)}
  
  #ks results for each pair
  out <- NULL
  for (i in 1:ncol(comb))
  {out <- rbind(out, origin.pair(comb[,i], alpha = alpha, var = var))}
  cbind(t(comb), out)
}

#=================================================



hline.dat <- data.frame(DayOfWeekUSA = out$time, hl = out$thres)

point.dat <- data.frame(rbind(sun[which(sun$p>=out$thres[1]), ],
                              mon[which(mon$p>=out$thres[2]), ],
                              tue[which(tue$p>=out$thres[3]), ],
                              wed[which(wed$p>=out$thres[4]), ],
                              thu[which(thu$p>=out$thres[5]), ],
                              fri[which(fri$p>=out$thres[6]), ],
                              sat[which(sat$p>=out$thres[7]), ]))

ggplot(data = week, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(DayOfWeekUSA~.)


#tests for surveillance plot setting for DOW data
#(size of alpha vs. cumulative % crashes: largest -> smallest, in rank order)
#----------------------------------------------------------------------------
#time <- "DayOfWeekUSA"
#week <- obs.rank(2015, 71, time)
#tag <- c("sun","mon","tue","wed","thu","fri","sat")
#alpha <- 0.05

out1 <- NULL
for (var in tag)
{out1 <- rbind(out1, get(var))}

pair.data(data = out1, time = time, 
          tag = tag, var = "p", alpha = 0.05)

#ks test results for surveillance plot (sun ~ sat):
# same:
#  mon - tue, wed, thu, fri
#  tue - wed, thu, fri
#  wed - thu, fri
# diff: (the rest)
#
#may suggest weekend vs. weekdays
#--------------------------------


#level set setting for DOW data (% crashes vs. size of alpha)
#------------------------------------------------------------
#tag <- c("sun","mon","tue","wed","thu","fri","sat")
#step 1: create a list of level set results given "tag"
alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
out2 <- rbind(sun.lvl,mon.lvl,tue.lvl,wed.lvl,thu.lvl,fri.lvl,sat.lvl)


#step 2: rename tag to .lvl data
#compare every combination of .lvl data
tag <- c("sun.lvl","mon.lvl","tue.lvl","wed.lvl","thu.lvl","fri.lvl","sat.lvl")
pair.data(data = data.frame(out2), time = "time", 
          tag = tag, var = "pct.events", alpha = 0.05)

#only wed - thu has no difference in distributions
#-------------------------------------------------


#only consider top 10% of segments
#---------------------------------
#find % matched segments for every combination of "tag"
#(ex. sun - mon, sun - tue, sun - wed, ...)
#-------------------------------------------------------
pair.match <- function(tag, alpha)
{
  comb <- combn(x = tag, m = 2)
  m1 <- NULL
  for (i in 1:ncol(comb))
  {
    a <- get(comb[1, i])
    b <- get(comb[2, i])
    
    n1 <- out[out$time==which(tag==comb[1,i]),]$nseg
    n2 <- out[out$time==which(tag==comb[2,i]),]$nseg
    
    max <- length(unique(c(a[1:n1,]$Logmile, b[1:n2,]$Logmile)))
    cond <- a[1:n1,]$Logmile %in% b[1:n2,]$Logmile
    
    m1 <- c(m1, sum(cond)/max)
  }
  data.frame(t(comb), match = m1)
}
#-------------------------------------------

alpha <- 0.05
tag <- c("sun","mon","tue","wed","thu","fri","sat")
lvl.set.time(tag = tag, alpha = alpha)
out3 <- rbind(sun.lvl,mon.lvl,tue.lvl,wed.lvl,thu.lvl,fri.lvl,sat.lvl)
pair.match(tag = tag, alpha = alpha)




#figure 3.2
#ordered percentages of crashes for days of week
#-------------------------------------------------
ord.plot <- function(data, var, xlim, ylim)
{
  d <- data
  plot.new()
  plot.window(xlim = xlim, ylim = ylim)
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2)
  lines(x = 1:max(xlim), y = d[[var]][1:max(xlim)], type = "s")
  box(which = "plot")
}

tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(1, 400)
ylim <- c(0, max(week$p))
par(mfrow = c(3, 3), family = "serif", cex.axis = 0.7, 
    mar = c(3.5, 2.5, 1, 1), oma = c(2, 0, 0, 0), las = 1)
for (obj in tag)
{
  ord.plot(get(obj), "p", xlim, ylim)
  mtext(toupper(obj), side = 1, line = 2, cex = 0.7, adj = 0.5)
}
fig.des <- expression(paste("Figure 3.2: ordered percentages of crashes for days of week", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)

















#data separation given different time
#only for original data (without any setting)
#============================================
#ks test for months (jan ~ dec)
#------------------------------
time <- "Month" #set up "time" var
moy <- obs.rank(2015, 71, time = time) #observed data given "time"
tag <- c("jan","feb","mar","apr",
         "may","jun","jul","aug",
         "sep","oct","nov","dec") #set up name tag for each group
var <- "p"

pair.data(time = time, data = moy, tag = tag, var = var, alpha = 0.05)

#ks test: route 71, 2015 jan ~ dec (crtical point: 0.0449)
# same:
#  jan: jul
#       sep
#       oct
#  mar: sep
#       oct
#  may: aug
#  jul: sep
#  sep: oct
#  nov: dec
#----------------------------------------



#ks test for days of week (sun ~ sat)
#------------------------------------
time <- "DayOfWeekUSA" #set up "time" var
dow <- obs.rank(2015, 71, time = time) #observed data given "time"
tag <- c("sun.obs","mon.obs","tue.obs","wed.obs",
         "thu.obs","fri.obs","sat.obs") #set up name tag for each group

pair.data(time = time, data = dow, tag = tag, var = var, alpha = 0.05)

#ks test: route 71, 2015 sun ~ sat (crtical point: 0.0449)
#same: 
# mon - tue (0.0304), wed (0.0407), fri (0.0286) 
# tue - wed (0.0418), thu (0.0433), fri (0.0314), sat (0.0399)
# wed - thu (0.0366), fri (0.0427), sat (0.0371)
# 
#different:
# mon - thu (0.0484), sat (0.0513), sun (0.0746)
# tue - sun (0.0673)
# wed - sun (0.0831)
# thu - fri (0.0470), sat (0.0470), sun (0.0961)
# fri - sat (0.0497), sun (0.0644)
# sat - sun (0.0842)
#---------------------------------------------------------



#ks test for time of day (shift12, shift4, shift3, shift2)
#---------------------------------------------------------
time <- "shift12" #set up "time" var
tod <- obs.rank(2015, 71, time = time) #observed data given "time"
tag <- c("shift1","shift2","shift3","shift4","shift5","shift6",
         "shift7","shift8","shift9","shift10","shift11","shift12") #set up name tag for each group

pair.data(time = time, data = tod, tag = tag, var = var, alpha = 0.05)


#result 1 (tod: 2 groups) (critical point = 0.0449): 
# diff: 0am-12pm, 12pm-0am (0.0494)
#----------------------------------
#result 2 (tod: 3 groups) (critical point = 0.0449):
# diff: 0-8, 8-16   (0.0952)
#       0-8, 16-24  (0.1077)
# same: 8-16, 16-24 (0.0370)
#---------------------------
#result 3 (tod: 6 groups) (critical point = 0.0449):
# diff: 
#  0-4: 4-8 (0.0863)
#       8-12 (0.1338)
#       12-16 (0.1469)
#       16-20 (0.1593)
#       20-24 (0.1481)
#  4-8: 8-12 (0.0918)
#       12-16 (0.0844)
#       16-20 (0.1009)
#       20-24 (0.0730)
#  8-12: 12-16 (0.0588)
#        20-24 (0.0642)
#  12-16: 20-24 (0.0499)
#  16-20: 20-24 (0.0639)
# same:
#  8-12: 16-20 (0.0413)
#  12-16: 16-20 (0.0427)
#------------------------
#result 4 (tod: 12 groups) (critical point = 0.0449):
# same: 10-12: 12-14
#       12-14: 16-18
#              18-20
#       16-18: 18-20
#       18-20: 20-22
#-------------------
