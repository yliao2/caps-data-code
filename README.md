# 2017.9.27
    temp_rankplot20170927.R (deleted)
    
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
    all_plot.R (deleted)
    
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
    
    draft:
    - define variables mentioned in the function part
    - descriptions based on the plots part
    --------------------------------------------------------------------------
    
  
# 2017.10.11
    all_plot.R (update) (deleted)
    
    make quasi-KS test, compare Obs. and Pred., used as an assessment
    make chi-squared test
    generate results for level set setting (updated)
    ------------------------------------------------
    add:
        thres.pred.p = porpotion of pred. events above threshold (threshold/alpha)
        pred.times = # segments of Pred. above threshold (threshold/alpha)
    
    plots:
    - size of hotspot vs. threshold (decreasing function, threshold-, size+ vice versa) (a)
    - HS above threshold (based on observations) vs. level alpha (obs. vs. pred.) (update)
      *It was shown wrong on 2017.10.4
    - % matched segments vs. alpha (b)
    - surveillance plot (c) (2017.10.4)
    - relative distribution (cdf & pdf/pmf)
    
    tests:
    - quasi KS test for surveilance plot (different significance level) (c)
    - quasi KS test for {size of HS vs. threshold}
    - chi-squared & quasi KS for relative distribution vs. uniform (discrete)
    
    draft:
    - add: comments on new plots and tests (temp)
    -------------------------------------------------------------------------
    
    
# 2017.10.18
    all_plot_recode.R (update)
    
    edit function: consider time (weekend, day of week, month...)
    edit level set setting (using rank fashion)
    results from level set setting (update, based on rank)
    
    plots (weekend vs. weekdays):
    - crash rate vs. logmile (single alpha)
    - sequence of alpha
        surveillance plot (level alpha vs. % events in HS)
        proportion/size of highest segements id as HS vs. threshold
        % segments id HS are matched for weekdays & weekend
        relative distribution plot (compare to unif)
        convert to uniform cdf
 
    tests:  
    - ks test for surveillance plot (not significant)


# 2017.10.25
    all_plot_recode.R (update)
    - fix level set setting (solve ties problem)
    - comments update
    dow.R (day of week)
    - data separation for day of week
    - use ks test to see magnitude of difference in different time
    
    test:
    - ks test results for surveillance plot (sun ~ sat):
        same:
        mon - tue, wed, thu, fri
        tue - wed, thu, fri
        wed - thu, fri
        diff: (the rest)
        
        may suggest weekend vs. weekdays
        
    - ks test results for level set setting (sun ~ sat):
        only wed - thu has no difference in distributions
    
    - % matched segments (sun ~ sat), only consider top 10% segements(alpha = 0.1)
    
    - ks tests results for original data (month, day of week, time of day ...)


# 2017.11.2
    all_plot_recode.R
    
    dow.R (day of week)
    - data separation for day of week (may be weekdays (1~4) & weekend (5~6)
    - use ks test to see magnitude of difference in different time
    
    moy.R (month of year)
    - data separation for month of year (break & non-break)
    - use ks test to see magnitude of difference in different time
    
    tod.R (time of day)
    - data separation for time of day (may be 4-hour period)
    - use ks test to see magnitude of difference in different time

    setting up B-spline (unfinish)


# 2017.11.8
    dow_detection.R (unfinished)
    data: mon (dow)
    model: (1) b-splines
           (2) kde (default)
           (3) kde (recode)
    - humps detection
    
    draft_new.Rmd
    descriptions for methodology (intro, ideas, why we use)
    2. evaluation methodology
        2.1 level sets
        2.2 surveillance plots
        2.3 KS tests    
    
