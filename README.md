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


# 2017.10.4
    all_plot.R
    
    generate results for level set setting (function from 2017.9.27 without time(t))
    simply compare observations & predictions
    
    function(alpha): for level sets, alpha can be a sequence
    --------------------------------------------------------
    - alpha
    - pct.hs = percentage of HS (close to alpha, gamma(alpha) in note)
    - times = # observations chosen to be close to alpha
    - cum.p = sum (top "times" crash rates)
    - threshold = min crash rate in pct.hs set
    - cum.pred = sum (top "times" crash rates in prediction)
    - pred.threshold = min crash rate in pct.hs set in prediction
    - pct.pred.hs = sum (crash rate above observations' threshold)
    
    plots:
    - single alpha (observations & predictions based on level alpha)
    - sequence of alpha: surveillance plot (level alpha vs. % events in HS)
                         HS above threshold (based on observations) vs. level alpha (obs. vs. pred.)
    
    test:
    - quasi KS test (unfinish)
    
