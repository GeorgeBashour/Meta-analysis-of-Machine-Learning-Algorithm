# Meta-analysis-of-Machine-Learning-Algorithm

This is my first attempt at doing a meta-analysis study for these types of outcomes including the Area under the curve (also known as C-statistic). 

The main guide that I built this analysis upon is Debray et al 2018 ( https://doi.org/10.1177/0962280218785504 )

The first challenge is the data ... the data extraction itself is troublesome if the team is not familiar with the statistics that go into these outcomes.


The code is a bit messy but gets the work done, it essentially has three outcomes divided into three periods: 90 days, 6 months, and 1 year (depending on what's available from the studies)

The AUC values are transformed to a logit scale for the analysis and the forest plot is given after it is back-transformed with "trasnf.ilogit" .


Mortality was a simple pooling of proportions.


The calibration was the most challenging as it was poorly reported.  With the lack of Observed and Expected data from the studies, we relied on getting the Log O:E ratio directly which was available in 4/11 studies. Alternatively, we used the slope given by 6 studies. (In short, we worked with what we had)
