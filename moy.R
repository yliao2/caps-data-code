function.path = "G:/caps/function_code.R"
source(file = function.path)

#mon of year screening
#use function: obs.rank (create)
#level set plot for moy (alpha = 0.05)
#-------------------------------
time <- "Month"
month <- obs.rank(2015, 71, time)
tag <- c("jan","feb","mar","apr","may","jun","jul", "aug",
         "sep","oct","nov","dec")
alpha <- 0.05


#create separate data sets based on the setting "Line 8-12"
#generate data: jan ~ dec
rank.time.dt(data = month, tag = tag, time = time)


#create separate data sets based on setting "Line 5-9"
#combine the results into "out"
lvl.set.time(tag = tag, alpha = alpha)
out <- rbind(jan.lvl, feb.lvl, mar.lvl, apr.lvl, may.lvl, jun.lvl, 
             jul.lvl, aug.lvl, sep.lvl, oct.lvl, nov.lvl, dec.lvl)



hline.dat <- data.frame(Month = out$time, hl = out$thres)

point.dat <- data.frame(rbind(jan[which(jan$p>=out$thres[1]), ],
                              feb[which(feb$p>=out$thres[2]), ],
                              mar[which(mar$p>=out$thres[3]), ],
                              apr[which(apr$p>=out$thres[4]), ],
                              may[which(may$p>=out$thres[5]), ],
                              jun[which(jun$p>=out$thres[6]), ],
                              jul[which(jul$p>=out$thres[7]), ],
                              aug[which(aug$p>=out$thres[8]), ],
                              sep[which(sep$p>=out$thres[9]), ],
                              oct[which(oct$p>=out$thres[10]), ],
                              nov[which(nov$p>=out$thres[11]), ],
                              dec[which(dec$p>=out$thres[12]), ]))

ggplot(data = month, mapping = aes(x = Logmile, y = p)) +
  geom_segment(aes(xend = Logmile, yend = 0)) + 
  geom_hline(aes(yintercept=hl), data=hline.dat, col = "darkgray") + 
  geom_point(data = point.dat, aes(x = Logmile, y = p), col = 4) + 
  facet_grid(Month~.)


#tests for surveillance plot setting for MOY data
#(size of alpha vs. cumulative % crashes: largest -> smallest, in rank order)
#----------------------------------------------------------------------------
#time <- "Month"
#week <- obs.rank(2015, 71, time)
#tag <- tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
#                "sep","oct","nov","dec")
#alpha <- 0.05

out1 <- NULL
for (var in tag)
{out1 <- rbind(out1, get(var))}

month.lvl.result <- pair.data(data = out1, time = time, tag = tag, var = "p", alpha = 0.05) %>% 
  mutate(Reference = substr(Reference, 1, 3),
         Compare = substr(Compare, 1, 3)) %>% filter(p.value>=0.05)

#ks test results for surveillance plot (jan ~ dec):
# same:
#  jan - jun, jul
#  feb - mar ~ dec
#  mar - apr, may, jul ~ oct, dec
#  apr - may, aug ~ oct, dec
#  may - jun ~ sep, nov, dec
#  jun - jul, sep, nov
#  jul - aug, sep, nov, dec
#  aug - sep, oct, dec
#  sep - oct ~ dec
#  oct - dec
#  nov - dec
#
# break (winter & summer) vs. non-break
#--------------------------------------


#level set setting for MOY data (% crashes vs. size of alpha)
#------------------------------------------------------------
#tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
#         "sep","oct","nov","dec")
#step 1: create a list of level set results given "tag"
alpha = seq(0, 1, by = 0.01)
lvl.set.time(tag = tag, alpha = alpha)
out2 <- rbind(jan.lvl,feb.lvl,mar.lvl,apr.lvl,may.lvl,jun.lvl,jul.lvl,
              aug.lvl,sep.lvl,oct.lvl,nov.lvl,dec.lvl)


#step 2: rename tag to .lvl data
#compare every combination of .lvl data
tag <- c("jan.lvl","feb.lvl","mar.lvl","apr.lvl","may.lvl","jun.lvl",
         "jul.lvl","aug.lvl","sep.lvl","oct.lvl","nov.lvl","dec.lvl")
month.lvl.result2 <- pair.data(data = data.frame(out2), time = "time", 
                               tag = tag, var = "pct.events", alpha = 0.05) %>% 
  mutate(Reference = toupper(substr(Reference, 1, 3)),
         Compare = toupper(substr(Compare, 1, 3))) %>% filter(p.value>=0.05)
#-------------------------------------------------


#only consider top 10% of segments
#---------------------------------
#find % matched segments for every combination of "tag"
#(ex. jan - feb, jan - mar, jan - apr, ...)
#-------------------------------------------------------
#
#alpha <- 0.05
#tag <- c("jan","feb","mar","apr","may","jun","jul", "aug", 
#         "sep","oct","nov","dec")
#lvl.set.time(tag = tag, alpha = alpha)
#out3 <- rbind(jan.lvl,feb.lvl,mar.lvl,apr.lvl,may.lvl,jun.lvl,jul.lvl,
#              aug.lvl,sep.lvl,oct.lvl,nov.lvl,dec.lvl)
#pair.match(tag = tag, alpha = alpha) %>% arrange(-match)



#figure 3.1: level sets for months of year
#-----------------------------------------
xlim <- c(0, 0.2)
ylim <- c(0, 1)
group1 <- tag[c(2:5,8:10)]
group2 <- tag[!(tag %in% group1)]
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in group1)
{lines(x = get(paste(var, ".lvl", sep = ""))$alpha, 
       y = get(paste(var, ".lvl", sep = ""))$pct.events, 
       type = "s", col = which(group1==var))}
box(which = "plot")
legend("bottomright", toupper(substr(group1, 1, 3)), col = 1:length(group1), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab=expression(alpha), ylab="Cumulative % Crashes", cex = 0.7)
for (var in group2)
{lines(x = get(paste(var, ".lvl", sep = ""))$alpha, 
       y = get(paste(var, ".lvl", sep = ""))$pct.events, 
       type = "s", col = which(group2==var))}
box(which = "plot")
legend("bottomright", toupper(substr(group2, 1, 3)), col = 1:length(group2), lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.1: level sets for months of year", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)



#figure 3.2
#ordered percentages of crashes for months of year
#-------------------------------------------------
xlim <- c(1, 300)
ylim <- c(0, 1)
group1 <- tag[c(2:5,8:10)]
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
legend("bottomright", toupper(substr(group1, 1, 3)), col = 1:length(group1), lwd = 2, lty = 1)

plot.new()
plot.window(xlim = xlim, ylim = ylim)
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered Segments", ylab="Cumulative % Crashes", cex = 0.7)
for (var in group2)
{lines(x = 1:max(xlim), y = cumsum(get(var)$p[1:max(xlim)]), 
       type = "s", col = which(group2==var))}
box(which = "plot")
legend("bottomright", toupper(substr(group2, 1, 3)), col = 1:length(group1), lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.2: ordered percentages of crashes for months of year", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)



#segments binnings (months of year)
#----------------------------------
time <- "Month"
tag <- c("jan","feb","mar","apr","may","jun","jul","aug", 
         "sep","oct","nov","dec")
xlim <- c(0, ceiling(max(month$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
tag.bin <- paste(tag, ".bin", sep = "")
tag.bin.lvl <- paste(tag.bin, ".lvl", sep = "")
bin2rank.time(data = month, time = time, tag = tag.bin, brks = brks)
moy.bin <- NULL
for (var in tag.bin)
{moy.bin <- rbind(moy.bin, get(var))}

#binnings (bin size = 1) using level sets
#----------------------------------------
lvl.set.time(tag = tag.bin, alpha = seq(0, 1, by = 0.01))

#multiple comparisons for MOY binnings
#----------------------------------------
tag.bin.lvl <- paste(tag.bin, ".lvl", sep = "")
out4 <- NULL
for (var in tag.bin.lvl)
{out4 <- rbind(out4, get(var))}

moy.lvl.result3 <- pair.data(data = data.frame(out4), time = "time", 
                             tag = tag.bin, var = "pct.events", alpha = 0.05) %>% 
  mutate(Reference = toupper(substr(Reference, 1, 3)),
         Compare = toupper(substr(Compare, 1, 3)))


#========================================
#--------    markdown format    ---------
#========================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(moy.bin$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin)
{tb <- get(var)
lines(x = tb$rank, y = cumsum(tb$p), type = "s", col = which(tag.bin==var))}
box(which = "plot")
legend("bottomright", toupper(substr(tag.bin, 1, 3)), col = 1:12, lwd = 2, lty = 1)

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
       col = 1:12, lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.10: ordered bins with bin size = 1 for months of year", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)




#bin size of 0.5
#--------------------------
brks <- seq(0, xlim[2], by = 0.5)
tag.bin <- paste(tag, ".bin", sep = "")
tag.bin.lvl <- paste(tag.bin, ".lvl", sep = "")
bin2rank.time(data = month, time = time, tag = tag.bin, brks = brks)
moy.bin <- NULL
for (var in tag.bin)
{moy.bin <- rbind(moy.bin, get(var))}

#binnings (bin size = 0.5) using level sets
#----------------------------------------
lvl.set.time(tag = tag.bin, alpha = seq(0, 1, by = 0.01))

#multiple comparisons for MOY binnings
#----------------------------------------
out5 <- NULL
for (var in tag.bin.lvl)
{out5 <- rbind(out5, get(var))}

moy.lvl.result4 <- pair.data(data = data.frame(out5), time = "time", 
                             tag = tag.bin, var = "pct.events", alpha = 0.05) %>% 
  mutate(Reference = toupper(substr(Reference, 1, 3)),
         Compare = toupper(substr(Compare, 1, 3)))


#========================================
#--------    markdown format    ---------
#========================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

plot.new()
plot.window(xlim = c(1, max(moy.bin$rank)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Ordered bins", ylab="Cumulative % Crashes", cex = 0.7)
for (var in tag.bin)
{tb <- get(var)
lines(x = tb$rank, y = cumsum(tb$p), type = "s", col = which(tag.bin==var))}
box(which = "plot")
legend("bottomright", toupper(substr(tag.bin, 1, 3)), col = 1:12, lwd = 2, lty = 1)

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
       col = 1:12, lwd = 2, lty = 1)

fig.des <- expression(paste("Figure 3.11: ordered bins with bin size = 0.5 for months of year", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)







#based on survillance plots and KS results:
#combine feb~may and aug~oct (similar) and use as a model
#to predict: jan, jun, july, nov, dec
#(original segments, without binning)
#run line 8:17 to get data
#----------------------------------------
sems <- rbind(feb, mar, apr, may, aug, sep, oct) %>% select(Logmile, n) %>% 
  group_by(Logmile) %>% summarise(n = sum(n)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

moy.mod <- data2pred(ref = sems, compr = jan, "Logmile")
for (var in c("jun", "jul", "nov", "dec"))
{
  moy.mod <- cbind(moy.mod, 
                   data2pred(ref = sems, compr = get(var), "Logmile")[,-1])
}
colnames(moy.mod) <- c("ref.rank", "jan", "jan.n", "jun", "jun.n", 
                       "jul", "jul.n", "nov", "nov.n", "dec", "dec.n")

#========================================
#--------    markdown format    ---------
#========================================
par(family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)
plot.new()
plot.window(xlim = range(sems$rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
matlines(x = moy.mod[,1], y = moy.mod[,c(2,4,6,8)])
box(which = "plot")
legend("bottomright", c("Jan", "Jun", "Jul", "Nov", "Dec"), col = 1:4, lwd = 2)
fig.des <- expression(paste("Figure 3.17: Predictive performace using Feb-May and Aug-Oct model", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)






#months of year with bin size 1
#months except for June & August as model
#-----------------------------------------
time <- "Month"
tag <- c("jan","feb","mar","apr","may","jun","jul","aug", 
         "sep","oct","nov","dec")
xlim <- c(0, ceiling(max(month$Logmile))+1)
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = month, time = time, tag = tag, brks = brks)

moy.bin <- NULL
for (var in tag[-c(6,8)])
{moy.bin <- rbind(moy.bin, get(var))}

moy.bin <- moy.bin %>% select(breaks, counts) %>% 
  group_by(breaks) %>% summarise(n = sum(counts)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

moy.mod <- data2pred(ref = moy.bin, compr = jun, "breaks")
for (var in tag[8])
{
  moy.mod <- cbind(moy.mod, 
                   data2pred(ref = moy.bin, compr = get(var), "breaks")[,-1])
}
colnames(moy.mod) <- c("ref.rank","jun", "jun.n", "aug", "aug.n")
#----------------------------------------------------


#months of year with bin size 0.5
#(Jan, Mar, Sep, Dec) (similar) as model
#-----------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = month, time = time, tag = tag, brks = brks)

moy.bin <- NULL
for (var in c("jan", "mar", "sep", "dec"))
{moy.bin <- rbind(moy.bin, get(var))}

moy.bin <- moy.bin %>% select(breaks, counts) %>% 
  group_by(breaks) %>% summarise(n = sum(counts)) %>% 
  mutate(rank = frank(-n, ties.method = "dense"))

moy.mod <- data2pred(ref = moy.bin, compr = feb, "breaks")
for (var in tag[-c(1,3,9,12)])
{
  moy.mod <- cbind(moy.mod, 
                   data2pred(ref = moy.bin, compr = get(var), "breaks")[,-1])
}
colnames(moy.mod) <- c("ref.rank","feb", "feb.n", "apr", "apr.n", "may", "may.n",
                       "jun", "jun.n", "jul", "jul.n", "aug", "aug.n", "oct", "oct.n",
                       "nov", "nov.n")
#------------------------------------------


#========================================
#--------    markdown format    ---------
#========================================
par(mfrow = c(1, 2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0), las = 1)

moy.y <- c(2,4) #bin size = 1
moy.y <- 2*seq(1, 8, by = 1) #bin size = 0.5

plot.new()
plot.window(xlim = range(moy.bin$rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(xlab="Reference rank", ylab="Match rate", cex = 0.7)
matlines(x = moy.mod[,1], y = moy.mod[,moy.y], col = 1:length(moy.y))
box(which = "plot")
legend("bottomright", tag[c(2,4)], col = 1:length(moy.y), lwd = 2) #bin size = 1
legend("bottomright", tag[-c(1,3,9,12)], col = 1:length(moy.y), lwd = 2) #bin size = 0.5

fig.des <- expression(paste("Figure 3.21: Predictive performace with bin size 1 and 0.5 for months of year", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)
#----------------------------------------