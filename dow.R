function.path = "G:/caps/function_code.R"
source(file = function.path)

#day of week screening
#use function: obs.rank (create)
#level set plot for DOW (alpha = 0.1)
#-------------------------------
time <- "DayOfWeekUSA"
week <- obs.rank(2015, 71, time)
tag <- c("sun","mon","tue","wed","thu","fri","sat")
alpha <- 0.05



#generate data: sun, mon, tue, wed, thu, fri, sat
#------------------------------------------------
rank.time.dt(data = week, tag = tag, time = time)



#create separate data sets based on setting "Line 8-11"
#combine the results into "out"
#-------------------------------------------------------
lvl.set.time(tag = tag, alpha = alpha)
out <- rbind(sun.lvl, mon.lvl, tue.lvl, wed.lvl, thu.lvl, fri.lvl, sat.lvl)



#plot: distributions of crashes given different time
#-------------------------------------------------------------
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
dow.lvl.result <- pair.data(data = out1, time = time, 
                            tag = tag, var = "p", alpha = 0.05)

#ks test results for surveillance plot (sun ~ sat):
# same:
#  mon - tue, wed, thu, fri
#  tue - wed, thu, fri
#  wed - thu, fri
# diff: (the rest)
#
#may suggest weekend vs. weekdays
#-----------------------------------------------------------------



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
dow.lvl.result2 <- pair.data(data = data.frame(out2), time = "time", 
                             tag = tag, var = "pct.events", alpha = 0.05) %>% 
  mutate(Reference = toupper(substr(Reference, 1, 3)),
         Compare = toupper(substr(Compare, 1, 3)))

#mon - tue, mon - wed, tue -wed, have no difference in distributions
#-------------------------------------------------------------



#only consider top 10% of segments
#---------------------------------
#find % matched segments for every combination of "tag"
#(ex. sun - mon, sun - tue, sun - wed, ...)
#-------------------------------------------------------
#alpha <- 0.05
#tag <- c("sun","mon","tue","wed","thu","fri","sat")
#lvl.set.time(tag = tag, alpha = alpha)
#out3 <- rbind(sun.lvl,mon.lvl,tue.lvl,wed.lvl,thu.lvl,fri.lvl,sat.lvl)
#pair.match(tag = tag, alpha = alpha)



#============================
#-- markdown figure format --
#============================
#figure 3.3
tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(0, 0.3)
ylim <- c(0, 1)
group1 <- tag[2:4]
group2 <- tag[!(tag %in% group1)]
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in group1)
{dt <- get(paste(var, ".lvl", sep = ""))
 lines(x = dt$alpha, y = dt$pct.events, type = "s", col = which(group1==var))}
box(which = "plot")
legend("bottomright", toupper(substr(group1, 1, 3)), 
       col = 1:length(group1), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in group2)
{dt <- get(paste(var, ".lvl", sep = ""))
 lines(x = dt$alpha, y = dt$pct.events, type = "s", col = which(group2==var))}
box(which = "plot")
legend("bottomright", toupper(substr(group2, 1, 3)), 
       col = 1:length(group2), lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.3: level sets for days of week", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#-------------------------------------------------

#============================
#-- markdown figure format --
#============================
#figure 3.4
#ordered percentages of crashes for days of week
#-------------------------------------------------
tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(1, 400)
ylim <- c(0, 1)
group1 <- tag[2:4]
group2 <- tag[!(tag %in% group1)]
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered Segments", ylab="Cumulative % Crashes", cex = 0.7)
for (var in group1)
{lines(x = 1:max(xlim), y = cumsum(get(var)$p[1:max(xlim)]), 
       type = "s", col = which(group1==var))}
box(which = "plot")
legend("bottomright", toupper(group1), col = 1:length(group1), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered Segments", ylab="Cumulative % Crashes", cex = 0.7)
for (var in group2)
{lines(x = 1:max(xlim), y = cumsum(get(var)$p[1:max(xlim)]), 
       type = "s", col = which(group2==var))}
box(which = "plot")
legend("bottomright", toupper(group2), col = 1:length(group2), lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.4: ordered segments by percentages of crashes for days of week", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#------------------------------------------------




#segments binnings (days of week)
#----------------------------------
time <- "DayOfWeekUSA"
tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(0, ceiling(max(week$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
tag.bin <- paste(tag, ".bin", sep = "")
tag.bin.lvl <- paste(tag.bin, ".lvl", sep = "")
bin2rank.time(data = week, time = time, tag = tag.bin, brks = brks)
dow.bin <- NULL
for (var in tag.bin)
{dow.bin <- rbind(dow.bin, get(var))}

#binnings (bin size = 1) using level sets
#----------------------------------------
lvl.set.time(tag = tag.bin, alpha = seq(0, 1, by = 0.01))

#multiple comparisons for DOW binnings
#----------------------------------------
out4 <- rbind(sun.bin.lvl, mon.bin.lvl, tue.bin.lvl, 
              wed.bin.lvl, thu.bin.lvl, fri.bin.lvl, 
              sat.bin.lvl)
dow.lvl.result3 <- pair.data(data = data.frame(out4), time = "time", 
                             tag = tag.bin, var = "pct.events", alpha = 0.05) %>% 
  mutate(Reference = toupper(substr(Reference, 1, 3)),
         Compare = toupper(substr(Compare, 1, 3)))


#========================================
#--------    markdown format    ---------
#========================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(dow.bin$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin)
{tb <- get(var)
 lines(x = tb$rank, y = cumsum(tb$p), type = "s", col = which(tag.bin==var))}
box(which = "plot")
legend("bottomright", toupper(substr(tag.bin, 1, 3)), col = 1:7, lwd = 2, lty = 1)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin.lvl)
{tb <- get(var)
 lines(x = tb$alpha, y = tb$pct.events, type = "s", col = which(tag.bin.lvl==var))}
box(which = "plot")
legend("bottomright", toupper(substr(tag.bin.lvl, 1, 3)), 
       col = 1:7, lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.8: ordered bins with bin size = 1 for days of week", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)





#bin size of 0.5
#---------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = week, time = time, tag = tag.bin, brks = brks)
dow.bin <- NULL
for (var in tag.bin)
{dow.bin <- rbind(dow.bin, get(var))}

#binnings (bin size = 0.5) using level sets
#----------------------------------------
lvl.set.time(tag = tag.bin, alpha = seq(0, 1, by = 0.01))

#multiple comparisons for DOW binnings
#----------------------------------------
out5 <- rbind(sun.bin.lvl, mon.bin.lvl, tue.bin.lvl, 
              wed.bin.lvl, thu.bin.lvl, fri.bin.lvl, 
              sat.bin.lvl)
dow.lvl.result4 <- pair.data(data = data.frame(out5), time = "time", 
                             tag = tag.bin, var = "pct.events", alpha = 0.05) %>% 
  mutate(Reference = toupper(substr(Reference, 1, 3)),
         Compare = toupper(substr(Compare, 1, 3)))


#========================================
#--------    markdown format    ---------
#========================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(dow.bin$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin)
{tb <- get(var)
lines(x = tb$rank, y = cumsum(tb$p), type = "s", col = which(tag.bin==var))}
box(which = "plot")
legend("bottomright", toupper(substr(tag.bin, 1, 3)), col = 1:7, lwd = 2, lty = 1)

tag.bin.lvl <- paste(tag.bin, ".lvl", sep = "")
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin.lvl)
{tb <- get(var)
lines(x = tb$alpha, y = tb$pct.events, type = "s", col = which(tag.bin.lvl==var))}
box(which = "plot")
legend("bottomright", toupper(substr(tag.bin.lvl, 1, 3)), 
       col = 1:7, lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.9: ordered bins with bin size = 0.5 for days of week", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
------------------------------------------------



#based on survillance plots and KS results:
#combine Mon, Tue, Wed (similar) and use as a model
#to predict: Thu ~ Sun
#(original segments, without binning)
#run line 8:17 to get data
#---------------------------------------------------
b4mid <- rbind(mon, tue, wed) %>% select(Logmile, n) %>% 
  group_by(Logmile) %>% summarise(n = sum(n)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

dow.mod <- data2pred(ref = b4mid, compr = thu, "Logmile")
for (var in c("fri", "sat", "sun"))
{
  dow.mod <- cbind(dow.mod, 
                   data2pred(ref = b4mid, compr = get(var), "Logmile")[,-1])
}
colnames(dow.mod) <- c("ref.rank", "thu", "thu.n", "fri", "fri.n", 
                       "sat", "sat.n", "sun", "sun.n")

#========================================
#--------    markdown format    ---------
#========================================
par(family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)
plot.new()
plot.window(xlim = range(b4mid$rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
matlines(x = dow.mod[,1], y = dow.mod[,c(2,4,6,8)])
box(which = "plot")
legend("bottomright", c("Thu","Fri","Sat","Sun"), col = 1:4, lwd = 2)
fig.des <- expression(paste("Figure 3.16: Predictive performace using Mon-Wed model", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#----------------------------------------






#days of week with bin size 1
#line 359:363, to get data
#-----------------------------------------
time <- "DayOfWeekUSA"
tag <- c("sun","mon","tue","wed","thu","fri","sat")
xlim <- c(0, ceiling(max(week$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = week, time = time, tag = tag, brks = brks)

week.bin <- rbind(mon, tue, wed, thu, fri, sat) %>% select(breaks, counts) %>% 
  group_by(breaks) %>% summarise(n = sum(counts)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

dow.mod <- data2pred(ref = week.bin, compr = sun, "breaks")

colnames(dow.mod) <- c("ref.rank", "sun", "sun.n")

#days of week with bin size 0.5
#rerun 376:377, to get data
#-----------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = week, time = time, tag = tag, brks = brks)

dow.bin <- NULL
for (var in tag)
{dow.bin <- rbind(dow.bin, get(var))}

week.bin <- dow.bin %>% select(breaks, counts) %>% 
  group_by(breaks) %>% summarise(n = sum(counts)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

dow.mod <- data2pred(ref = week.bin, compr = sun, "breaks")
for (var in tag[-1])
{
  dow.mod <- cbind(dow.mod, 
                   data2pred(ref = week.bin, compr = get(var), "breaks")[,-1])
}
colnames(dow.mod) <- c("ref.rank","sun", "sun.n", "mon", "mon.n", "tue", "tue.n", 
                       "wed", "wed.n", "thu", "thu.n", "fri", "fri.n", 
                       "sat", "sat.n")
#========================================
#--------    markdown format    ---------
#========================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

dow.y <- c(2) #bin size = 1
dow.y <- c(2,4,6,8,10,12,14) #bin size = 0.5

plot.new()
plot.window(xlim = range(week.bin$rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
matlines(x = dow.mod[,1], y = dow.mod[,dow.y])
box(which = "plot")
legend("bottomright", c("sun"), col = 1, lwd = 2) #bin size = 1
legend("bottomright", tag, col = 1:length(tag), lwd = 2) #bin size = 0.5
fig.des <- expression(paste("Figure 3.20: Predictive performace with bin size 1 and 0.5 for days of week", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#----------------------------------------