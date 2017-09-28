# 2017.9.27
    temp_rankplot20170927.R
    
    function(year, route, time(t)) 
    for store counts based on original logmile (0.019, 0.038, 0.057,...)
    ---------------------------------------------------------------------
    - predictions using LOESS
    - rank(original counts|t) & rank(predictions|t)
    data: year = 2015, route = 71, t = "Weekend" is primarily used
    
    plots:
     - Logmile vs. counts given t (ggplot)
     - cumsum( size = sum(diff) in individual ordered rank ) vs. cum( percentage of crashes ) given t
     - add lines of predictions into the plot
    ---------------------------------------------------------------------
