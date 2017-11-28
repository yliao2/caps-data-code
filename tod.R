function.path = "G:/caps/function_code.R"
source(file = function.path)

#time of day screening
#use function: obs.rank (create)
#level set plot for TOD (alpha = 0.05)
#-------------------------------
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
alpha <- 0.05


#create separate data sets based on the setting "Line 8-11"
#generate data: 0-2, 2-4, 4-6,... 20-22, 22-24
rank.time.dt(data = tod, tag = tag, time = time)


#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- NULL
for (i in 1:12) {out <- rbind(out, get(paste(tag[i], ".lvl", sep = "")))}



hline.dat <- data.frame(shift12 = out$time, hl = out$thres)

point.dat <- data.frame(rbind(s1[which(s1$p>=out$thres[1]), ],
                              s2[which(s2$p>=out$thres[2]), ],
                              s3[which(s3$p>=out$thres[3]), ],
                              s4[which(s4$p>=out$thres[4]), ],
                              s5[which(s5$p>=out$thres[5]), ],
                              s6[which(s6$p>=out$thres[6]), ],
                              s7[which(s7$p>=out$thres[7]), ],
                              s8[which(s8$p>=out$thres[8]), ],
                              s9[which(s9$p>=out$thres[9]), ],
                              s10[which(s10$p>=out$thres[10]), ],
                              s11[which(s11$p>=out$thres[11]), ],
                              s12[which(s12$p>=out$thres[12]), ]))

ggplot(data = tod, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(shift12~.)


#tests for surveillance plot setting for TOD data
#(size of alpha vs. cumulative % crashes: largest -> smallest, in rank order)
#----------------------------------------------------------------------------
#time <- "shift12"
#tod <- obs.rank(2015, 71, time)
#tag <- c(paste("s", 1:12, sep = ""))
#alpha <- 0.05

out1 <- NULL
for (var in tag)
{out1 <- rbind(out1, get(var))}

pair.data(data = out1, time = time, 
          tag = tag, var = "p", alpha = 0.05)

#ks test results for surveillance plot (00-02 ~ 22-24):
# same:
#  06-08 - 08-10
#  08-10 - 10-12, 18-20
#  10-12 - 18-20
#  12-14 - 14-16
#  14-16 - 16-18
# diff: (the rest)
#
#may suggest 4-hour period
#-------------------------


#level set setting for TOD data (% crashes vs. size of alpha)
#------------------------------------------------------------
time <- "shift12"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:12, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

time <- "shift6"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:6, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

time <- "shift3"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:3, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

time <- "shift2"
tod <- obs.rank(2015, 71, time)
tag <- c(paste("s", 1:2, sep = ""))
rank.time.dt(data = tod, tag = tag, time = time)

#step 1: create a list of level set results given "tag"

alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
out2 <- NULL
for (var in tag) {out2 <- rbind(out2, get(paste(var, ".lvl", sep = "")))}

#step 2: compare every combination of .lvl data
tod.lvl.result <- pair.data(data = data.frame(out2), time = "time", 
                            tag = tag, var = "pct.events", alpha = 0.05)
#-------------------------------------------------


#=================================
#--------   md format   ----------
#=================================
#---------------------------------
par(mfrow = c(2, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
tag.lvl <- paste(tag, ".lvl", sep = "")
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.lvl)
{tb <- get(var)
lines(x = tb$alpha, y = tb$pct.events, type = "s", col = which(tag.lvl==var))}
box(which = "plot")
legend("bottomright", tag, col = 1:12, lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.5: level sets for time of day in various periods", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)





#segments binnings (time of day). use tod data (Line 79:97)
#-----------------------------------------------------------
xlim <- c(0, ceiling(max(tod$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
brks <- seq(0, xlim[2], by = 0.5)
tag.bin <- paste(tag, ".bin", sep = "")
tag.bin.lvl <- paste(tag.bin, ".lvl", sep = "")

bin2rank.time(data = tod, time = time, tag = tag.bin, brks = brks)
tod.bin <- NULL
for (var in tag.bin)
{tod.bin <- rbind(tod.bin, get(var))}
lvl.set.time(tag = tag.bin, alpha = seq(0, 1, by = 0.01))
#------------------     markdown format     -------------------
par(mfrow = c(2, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(tod.bin$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin)
{tb <- get(var)
lines(x = tb$rank, y = cumsum(tb$p), type = "s", col = which(tag.bin==var))}
box(which = "plot")
legend("bottomright", tag, col = 1:length(tag.bin), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin.lvl)
{tb <- get(var)
lines(x = tb$alpha, y = tb$pct.events, type = "s", col = which(tag.bin.lvl==var))}
box(which = "plot")
legend("bottomright", tag, col = 1:length(tag.bin), lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.11: ordered bins with bin size = 1 and 0.5 for time of day in 12 shifts", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#-----------------------------------------------------------


#ks table for time of day with binning results
#------------------------------------------------------------
out4 <- NULL
for (var in tag.bin.lvl)
{out4 <- rbind(out4, get(var))}
tod.lvl.result2 <- pair.data(data = data.frame(out4), time = "time", 
                             tag = tag, var = "pct.events", alpha = 0.05)
#-------------------------------------------------------------




#based on survillance plots and KS results:
#combine some shifts with similar pattern and use as a model
#to predict the rest shifts
#(use original segments, without binning)
#run line 79:82, 84:87, 89:92, 94:97 for each divisions, to get data
#-------------------------------------------------------------------
#1. 12 shifts (2h): s7~s9
tod.same <- rbind(s7, s8, s9) %>% select(Logmile, n) %>% 
  group_by(Logmile) %>% summarise(n = sum(n)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

tod.mod <- data2pred(ref = tod.same, compr = s1, "Logmile")
for (var in tag[!(tag==c("s7", "s8", "s9"))])
{
  tod.mod <- cbind(tod.mod, 
                   data2pred(ref = tod.same, compr = get(var), "Logmile")[,-1])
}
colnames(tod.mod) <- c("ref.rank", "s1", "s1.n", "s2", "s2.n", 
                       "s3", "s3.n", "s4", "s4.n", "s5", "s5.n",
                       "s6", "s6.n", "s10", "s10.n", "s11", "s11.n",
                       "s12", "s12.n")

#2. 6 shifts (4h): s2~s5
tod.same <- rbind(s2, s3, s4, s5) %>% select(Logmile, n) %>% 
  group_by(Logmile) %>% summarise(n = sum(n)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

tod.mod <- data2pred(ref = tod.same, compr = s1, "Logmile")
for (var in "s6")
{
  tod.mod <- cbind(tod.mod, 
                   data2pred(ref = tod.same, compr = get(var), "Logmile")[,-1])
}
colnames(tod.mod) <- c("ref.rank", "s1", "s1.n",
                       "s6", "s6.n")

#3. 3 shifts (8h): s2~s3
tod.same <- rbind(s2, s3) %>% select(Logmile, n) %>% 
  group_by(Logmile) %>% summarise(n = sum(n)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

tod.mod <- data2pred(ref = tod.same, compr = s1, "Logmile")
colnames(tod.mod) <- c("ref.rank", "s1", "s1.n")

#========================================
#--------    markdown format    ---------
#========================================
tod.y <- seq(2, 18, by = 2) #12 shifts (2h)
tod.y <- c(2, 4) #6 shifts (4h)
tod.y <- c(2) #3 shifts (8h)

par(mfrow = c(2, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = range(tod.same$rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
matlines(x = tod.mod[,1], y = tod.mod[,tod.y])
box(which = "plot")

legend("bottomright", tag[!(tag==c("s7", "s8", "s9"))], col = 1:9, lwd = 2) #12 shifts (2h)
legend("bottomright", c("s1", "s6"), col = 1:2, lwd = 2) #6 shifts (4h)
legend("bottomright", c("s1"), lwd = 2) #3 shifts (8h)

fig.des <- expression(paste("Figure 3.18: Predictive performace under various divisions: 2-hour, 4-hour and 8-hour shifts", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#-----------------------------------------------------------------






#combine some shifts with similar pattern and use as a model
#to predict the rest shifts
#(use original segments, without binning)
#run line 79:82, 84:87, 89:92, 94:97 for each divisions, to get data
#-------------------------------------------------------------------
#1. (data: 79:82): 12 shifts (2h), (1, 4:9, 12) (similar) -> model 
xlim <- c(0, ceiling(max(tod$Logmile))+1)
brks <- seq(0, xlim[2], by = 1) #bin size = 1
brks <- seq(0, xlim[2], by = 0.5) #bin size = 0.5

grp <- c(1, 4:9, 12) #bin size = 1
grp <- c(3:12) #bin size = 0.5

bin2rank.time(data = tod, time = time, tag = tag, brks = brks)

tod.bin <- NULL
for (var in tag[grp])
{tod.bin <- rbind(tod.bin, get(var))}

tod.bin <- tod.bin %>% select(breaks, counts) %>% 
  group_by(breaks) %>% summarise(n = sum(counts)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

tod.mod <- data2pred(ref = tod.bin, compr = get(tag[-grp][1]), "breaks")
for (var in tag[-grp][-1])
{tod.mod <- cbind(tod.mod, 
                  data2pred(ref = tod.bin, compr = get(var), "breaks")[,-1])}

colnames(tod.mod) <- c("ref.rank", "s2", "s2.n", "s3", "s3.n", 
                       "s10", "s10.n", "s11", "s11.n") #bin size = 1

colnames(tod.mod) <- c("ref.rank", "s1", "s1.n", "s2", "s2.n") #bin size = 0.5

#2. (data: 84:87, 275:282): 6 shifts (4h), (1, 3:5) (similar) -> model
grp <- c(1, 3:5) #bin size = 1
grp <- c(1:6) #bin size = 0.5

#for bin size = 0.5
tod.mod <- data2pred(ref = tod.bin, compr = get(tag[grp][1]), "breaks")
for (var in tag[grp][-1])
{tod.mod <- cbind(tod.mod, 
                  data2pred(ref = tod.bin, compr = get(var), "breaks")[,-1])}

colnames(tod.mod) <- c("ref.rank", "s2", "s2.n", "s6", "s6.n") #bin size = 1
colnames(tod.mod) <- c("ref.rank", "s1", "s1.n", "s2", "s2.n", 
                       "s3", "s3.n", "s4", "s4.n", "s5", "s5.n",
                       "s6", "s6.n") #bin size = 0.5

#3. (data: 89:92, 275:279): 3 shifts (8h), (use all) (similar) -> model
grp <- c(1:3) #bin size = 1
grp <- c(1:3) #bin size = 0.5

#rerun 304:307 for both sizes
colnames(tod.mod) <- c("ref.rank", "s1", "s1.n", "s2", "s2.n", "s3", "s3.n")

#========================================
#--------    markdown format    ---------
#========================================
tod.y <- c((1:length(tag[-grp])*2))
tod.y <- c((1:length(tag[grp])*2)) #for 6 shifts size 0.5, for 3 shifts both sizes

par(mfrow = c(3, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = range(tod.bin$rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
matlines(x = tod.mod[,1], y = tod.mod[,tod.y], col = 1:length(tod.y))
box(which = "plot")

legend("bottomright", tag[-grp], col = 1:length(tod.y), lwd = 2)
legend("bottomright", tag[grp], col = 1:length(tod.y), lwd = 2) #for 6 shifts size 0.5, for 3 shifts borth sizes

fig.des <- expression(paste("Figure 3.22: Various divisions with bin size 1 and 0.5:",
                            " 2-hour, 4-hour and 8-hour shifts ", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#-----------------------------------------------------------------




#only consider top 10% of segments
#---------------------------------
#find % matched segments for every combination of "tag"
#(ex. 00-02 - 02-04, 00-02 - 04-06, 00-02 - 04-06, ...)
#-------------------------------------------------------

#alpha <- 0.05
#tag <- c(paste("s", 1:12, sep = ""))
#lvl.set.time(tag = tag, alpha = alpha)
#out3 <- NULL
#for (var in tag) {out3 <- rbind(out2, get(paste(var, ".lvl", sep = "")))}
#pair.match(tag = tag, alpha = alpha)