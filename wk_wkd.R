#function.path = "function_code.R"
#source(file = function.path)
load("wk_wkd.RData")
#level set setting (weekend & weekdays)
#--------------------------------------
z <- obs.rank(2015, 71, "Weekend")
tag <- c("z1","z2")
rank.time.dt(data = z, tag = tag, time = "Weekend")



#single alpha = 0.05
#-------------------
alpha = 0.05
t1 <- data.frame(lvl.set(z1, alpha))
t2 <- data.frame(lvl.set(z2, alpha))

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    mfrow = c(1, 2), oma = c(2, 0, 0, 0))

plot.new()
plot.window(xlim = c(0, max(z$Logmile)), ylim = c(0, max(z$p)))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
title(main="(a)",xlab="Logmile", ylab="Crash rate", cex = 0.7)
segments(x0 = z1$Logmile, y0 = z1$p, y1 = 0)
points(x = z1[which(z1$p>=t1$thres), ]$Logmile, y = z1[which(z1$p>=t1$thres), ]$p, cex = 0.8)
abline(h = t1$thres, col = "darkgray")
box(which = "plot")
text(x = 32, y = t1$thres, labels = round(t1$thres, 4), pos = 3)

plot.new()
plot.window(xlim = c(0, max(z$Logmile)), ylim = c(0, max(z$p)))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
title(main="(b)", xlab="Logmile", ylab="Crash rate", cex = 0.7)
segments(x0 = z2$Logmile, y0 = z2$p, y1 = 0)
points(x = z2[which(z2$p>=t2$thres), ]$Logmile, y = z2[which(z2$p>=t2$thres), ]$p, cex = 0.8)
abline(h = t2$thres, col = "darkgray")
box(which = "plot")
text(x = 32, y = t2$thres, labels = round(t2$thres, 4), pos = 3)

fig.des <- expression(paste("Figure 2.1: weekdays (a) and weekend (b) with ", alpha, " = 0.05", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)



#sequence of alphas (alpha = seq(0, 1, length = 101))
#==========================================================
#-----------fix allocation based surveillance-------------#
#----------------------------------------------------------
#surveillance plot (type 1) (size of distance vs. % events)
#----------------------------------------------------------
z3 <- z1 %>% select(diff, rank, p) %>% group_by(rank) %>% 
  mutate(diff = round(diff, 3), log.size = sum(diff), log.rate = sum(p)) %>% distinct()
z4 <- z2 %>% select(diff, rank, p) %>% group_by(rank) %>% 
  mutate(diff = round(diff, 3), log.size = sum(diff), log.rate = sum(p)) %>% distinct()

h <- 0.18*sum(z3$log.size)

p1 <- max(cumsum(z3[which(cumsum(z3$log.size)<=h),]$log.rate))
p2 <- max(cumsum(z4[which(cumsum(z4$log.size)<=h),]$log.rate))

#surveillance plot (type 2) (level alpha vs. % events in HS)
#-----------------------------------------------------------
alpha <- seq(0, 1, by = 0.01)
z1.lvl <- data.frame(lvl.set(z1, alpha))
z2.lvl <- data.frame(lvl.set(z2, alpha))
thres <- 0.18

#--------------------------#
#markdown figure format use#
#--------------------------#
par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    mfrow = c(1, 2), oma = c(2, 0, 0, 0))

plot.new()
plot.window(xlim = c(0, max(z$Logmile)), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2)
title(main="(a)",xlab="Size of Logmile", ylab="Crash rate", cex = 0.7)
lines(x = cumsum(z3$log.size), y = cumsum(z3$log.rate), type = "s")
lines(x = cumsum(z4$log.size), y = cumsum(z4$log.rate), type = "s", col = 4)
abline(v = h, col = "darkgrey")
box(which = "plot")
text(x = h, y = c(p1, p2, 0), labels = round(c(p1, p2, h), 4), pos = 4, col = c(1, 4, 1))
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);
title(main="(b)",xlab=expression(alpha), ylab="Crash rate", cex = 0.7)
lines(x = z1.lvl$alpha, y = z1.lvl$pct.events, type = "s")
lines(x = z2.lvl$alpha, y = z2.lvl$pct.events, type = "s", col = 4)
abline(v = 0.18, col = "darkgrey")
box(which = "plot")
text(x = thres, y = c(p3, p4, 0), labels = round(c(p3, p4, thres),4), pos = 4, col = c(1, 4, 1))
legend("bottomright", c("Weekdays", "Weekend"), col = c(1, 4), lty = 1, bty = "n")

fig.des <- expression(paste("Figure 2.2: surveillance plots for weekdays and weekend under distinct perspectives ", alpha, " levels", sep = ""))
mtext(fig.des, side = 1, adj = 0.5, outer = TRUE)



#-------------threshold based surveillance----------------#
#----------------------------------------------------------
u <- unique(z$p)
thr <- seq(0, round(max(u)+0.005, 2), by = 0.0001)

z1.thres <- thres.out(data = z1, var = "p", thr = thr)
z2.thres <- thres.out(data = z2, var = "p", thr = thr)

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 1, 1), oma = c(2, 0, 0, 0))

plot.new()
plot.window(xlim = rev(c(0, max(thr))), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(xlab="Threshold", ylab=expression(alpha), cex = 0.7)
lines(x = thr, y = z1.thres$size, type = "s")
lines(x = thr, y = z2.thres$size, type = "s", col = 4)

v <- 0.0019
abline(v = v, col = "darkgrey")
box(which = "plot")
p5 <- z1.thres[which(z1.thres$thr==v),]$size
p6 <- z2.thres[which(z2.thres$thr==v),]$size
text(x = v, y = c(p5+0.01, p6+0.04, 0), labels = round(c(p5, p6, v), 4), pos = 2, col = c(1, 4, 1))
legend("topleft", c("Weekdays", "Weekend"), col = c(1, 4), lty = 1, bty = "n")
mtext("Figure 2.3: threshold based surveillance for weekdays and weekend", 
      side = 1, adj = 0.5, outer = TRUE)
#----------------------------------------------------------





#modified jaccard index plot
#use "ref" as model, "compare" as data, see how many segments are matched 
#and sum up the crash rate for matched segments cumulatively
#----------------------------------------------------------------------------
#use one set as reference to predict other sets
#compared raw segments (true pattern in itself)
w1 <- pred.rank(ref = z1, compare = z2)
true1 <- z1 %>% select(rank, p) %>% group_by(rank) %>% mutate(sum.p = sum(p)) %>% distinct()
w2 <- pred.rank(ref = z2, compare = z1)
true2 <- z2 %>% select(rank, p) %>% group_by(rank) %>% mutate(sum.p = sum(p)) %>% distinct()

par(mfrow = c(1,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(w1$ref.rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekdays", xlab = "Reference rank", ylab = "Cumulative matched crash rate")
lines(x = true1$rank, y = cumsum(true1$sum.p), type = "s")
lines(x = w1$ref.rank, y = w1$cum.match.pct, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekend"), col = c(1,4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(w2$ref.rank), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekend", xlab = "Reference rank", ylab = "Cumulative matched crash rate")
lines(x = true2$rank, y = cumsum(true2$sum.p), type = "s")
lines(x = w2$ref.rank, y = w2$cum.match.pct, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekdays"), col = c(1,4), lty = 1, bty = "n")
#------------------------------------------------------------------------------

#use one set under level sets setting as reference to predict other sets
#compared true level sets segments (true pattern in itself)
l1 = NULL;l2 = NULL
for (i in 1:length(alpha))
{
  l1 = c(l1,pred.rank(ref = z1[0:z1.lvl$nseg[i],], compare = z2[0:z2.lvl$nseg[i],], rank = FALSE))
  l2 = c(l2,pred.rank(ref = z2[0:z2.lvl$nseg[i],], compare = z1[0:z1.lvl$nseg[i],], rank = FALSE))
}

par(mfrow = c(1,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekdays", xlab = expression(alpha), ylab = "Cumulative matched crash rate")
lines(x = alpha, y = z1.lvl$pct.events, type = "s")
lines(x = alpha, y = l1, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekend"), col = c(1,4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekend", xlab = expression(alpha), ylab = "Cumulative matched crash rate")
lines(x = alpha, y = z2.lvl$pct.events, type = "s")
lines(x = alpha, y = l2, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekdays"), col = c(1,4), lty = 1, bty = "n")
#---------------------------------------------------------------------------------


#ks test for surveillance plot
#-----------------------------------------------
t1 <- ks.results(s1 = z1.lvl$pct.events, s2 = l1, alpha = 0.05)
t2 <- ks.results(s1 = z2.lvl$pct.events, s2 = l2, alpha = 0.05)

max.d.pt1 <- data.frame(cbind(alpha, d = abs(t1$d))) %>% filter(d == t1$max.d)
max.d.pt2 <- data.frame(cbind(alpha, d = abs(t2$d))) %>% filter(d == t2$max.d)

par(mfrow = c(1,2), family = "serif", cex.axis = 0.7, 
    mar = c(4, 4, 2, 1), oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(alpha), ylim = c(0, max(t1$max.d,t1$bound)))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekdays", xlab = expression(alpha), ylab = "Deviance")
points(x = alpha, y = abs(t1$d))
abline(v = max.d.pt1$alpha, h = t1$bound, col = c(2,"darkgray"))
text(x = max.d.pt1$alpha, y = c(0,t1$max.d,t1$bound-0.01), 
     labels = round(c(max.d.pt1$alpha,t1$max.d,t1$bound),4), pos = 4, col = c(1,1,2))
box(which = "plot")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, max(t2$max.d,t2$bound)))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekend", xlab = expression(alpha), ylab = "Deviance")
points(x = alpha, y = abs(t2$d))
abline(v = max.d.pt2$alpha, h = t2$bound, col = c(2,"darkgray"))
text(x = max.d.pt2$alpha, y = c(0,t2$max.d,t2$bound-0.01), 
     labels = round(c(max.d.pt2$alpha,t2$max.d,t2$bound),4), pos = 4, col = c(1,1,2))
box(which = "plot")

ks.out <- data.frame(Week = c(t1$max.d,t1$p.value), Weekend = c(t2$max.d,t2$p.value),
                     row.names = c("max.d","p.value"))
#percentage of crashes in a size/alpha (%) of highest crash rate segments
#ex. alpha = 0.07, max.d = 0.1631 < bound = 0.1911
#    between true pattern and weekend using reference weekdays, for weekdays.
#
#ex. alpha = 0.08, max.d = 0.2229 > bound = 0.1911
#    between true pattern and weekdays using reference weekend, for weekend.
#--------------------------------------------------------------------------







#segments binnings with size 1 and 0.5
#-----------------------------------------------------------------
xlim <- c(0, ceiling(max(z$Logmile))+1)
time <- "Weekend"
tag <- c("wk","wkd")

#binnings using level sets with size 1
#------------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
bin2rank.time(data = z, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

l1bin1 = NULL;l2bin1 = NULL
for (i in 1:length(alpha))
{
  l1bin1 = c(l1bin1,
             pred.rank(ref = wk[0:wk.lvl$nseg[i],], compare = wkd[0:wkd.lvl$nseg[i],], 
                       var = "breaks", rank = FALSE))
  l2bin1 = c(l2bin1,
             pred.rank(ref = wkd[0:wkd.lvl$nseg[i],], compare = wk[0:wk.lvl$nseg[i],], 
                       var = "breaks", rank = FALSE))
}
ks.wkbin1 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)

#binnings using level sets with size 0.5
#-----------------------------------------------------------
brks <- seq(0, xlim[2], by = 0.5)
bin2rank.time(data = z, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

l1bin0.5 = NULL;l2bin0.5 = NULL
for (i in 1:length(alpha))
{
  l1bin0.5 = c(l1bin0.5,
             pred.rank(ref = wk[0:wk.lvl$nseg[i],], compare = wkd[0:wkd.lvl$nseg[i],], 
                       var = "breaks", rank = FALSE))
  l2bin0.5 = c(l2bin0.5,
             pred.rank(ref = wkd[0:wkd.lvl$nseg[i],], compare = wk[0:wk.lvl$nseg[i],], 
                       var = "breaks", rank = FALSE))
}
ks.wkbin0.5 <- origin.pair(ref = tag, compare = tag, var = "breaks", sig = 0.3, opt = TRUE)
#-----------------------------------------------------------

#use one set under level sets setting as reference to predict other sets
#compared true level sets segments (true pattern in itself)
#bin sizes of 1 and 0.5
#-----------------------------------------------------------------------
par(mfrow = c(2,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekdays (1)", xlab = expression(alpha), ylab = "Cumulative matched crash rate")
lines(x = alpha, y = wk.lvl$pct.events, type = "s")
lines(x = alpha, y = l1bin1, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekend"), col = c(1,4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekend (1)", xlab = expression(alpha), ylab = "Cumulative matched crash rate")
lines(x = alpha, y = wkd.lvl$pct.events, type = "s")
lines(x = alpha, y = l2bin1, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekdays"), col = c(1,4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekdays (0.5)", xlab = expression(alpha), ylab = "Cumulative matched crash rate")
lines(x = alpha, y = wk.lvl$pct.events, type = "s")
lines(x = alpha, y = l1bin0.5, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekend"), col = c(1,4), lty = 1, bty = "n")

plot.new()
plot.window(xlim = range(alpha), ylim = c(0, 1))
grid(nx = NULL, ny = NULL, col = "lightgray")
axis(1);axis(2);title(main = "Weekend (0.5)", xlab = expression(alpha), ylab = "Cumulative matched crash rate")
lines(x = alpha, y = wkd.lvl$pct.events, type = "s")
lines(x = alpha, y = l2bin0.5, type = "s", col = 4)
box(which = "plot")
legend("bottomright", c("True","Weekend"), col = c(1,4), lty = 1, bty = "n")
#----------------------------------------------------------------------------



#------------------------------------------------------------
tag <- c("wk","wkd")
rank.time.dt(data = z, time = time, tag = tag)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

wk.mod <- data2pred(ref = "wk", compare = "wkd")

par(family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 1)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste(tag[i],".mod",sep=""))$alpha, 
           y = get(paste(tag[i],".mod",sep=""))[,-1], type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], lty = 1, bty = "n") 
}
#-------------------------------------------------------------



#use data weekdays (z1) as reference to test weekend (z2)
#test % matched segments under level sets
#size 1------------------------------------------------------------
brks <- seq(0, xlim[2], by = 1)
tag <- c("wk","wkd")
bin2rank.time(data = z, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

wk.mod <- data2pred(ref = "wk", compare = "wkd", var = "breaks")
wkd.mod <- data2pred(ref = "wkd", compare = "wk", var = "breaks")

par(mfrow = c(2,2), family = "serif", cex.axis = 0.7, mar = c(4, 4, 2, 1),
    las = 1, oma = c(1, 0, 0, 0))
for (i in 1:2)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste(tag[i],".mod",sep=""))$alpha, 
           y = get(paste(tag[i],".mod",sep=""))[,-1], type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], lty = 1, bty = "n") 
}
#size 0.5------------------------------------------------------------

brks <- seq(0, xlim[2], by = 0.5)
tag <- c("wk","wkd")
bin2rank.time(data = z, time = time, tag = tag, brks = brks)
lvl.set.time(tag = tag, alpha = seq(0, 1, by = 0.01))

wk.mod <- data2pred(ref = "wk", compare = "wkd", var = "breaks")
wkd.mod <- data2pred(ref = "wkd", compare = "wk", var = "breaks")

for (i in 1:2)
{
  plot.new()
  plot.window(xlim = range(alpha), ylim = c(0, 1))
  grid(nx = NULL, ny = NULL, col = "lightgray")
  axis(1);axis(2);title(main = tag[i], xlab = expression(alpha), ylab = "Matched rate")
  matlines(x = get(paste(tag[i],".mod",sep=""))$alpha, 
           y = get(paste(tag[i],".mod",sep=""))[,-1], type = "s")
  box(which = "plot")
  legend("bottomright", tag[-i], lty = 1, bty = "n") 
}
#--------------------------------------------------------------