---
  layout: atsa
---
  
  Project
-------


Summary of Tasks:
  
- Submit a data set;
- Choose a data set for ARIMA modeling from the approved datasets:
    - [Anomaly](https://github.com/maryclare/atsa/blob/master/content/data/Anomaly.RData)
    - [Electricity](https://github.com/maryclare/atsa/blob/master/content/data/Electricty.RData)
    - [Stocks](https://github.com/maryclare/atsa/blob/master/content/data/Stocks.RData)
    - [Yields](https://github.com/maryclare/atsa/blob/master/content/data/Yields.RData)
    - Storage (will be uploaded by Saturday, 4/13/19)

- Submit first analysis as part of Homework 7
- Choose a data set for state-space modeling from the approved datasets:
    - [Anomaly](https://github.com/maryclare/atsa/blob/master/content/data/Anomaly.RData)
    - [Electricity](https://github.com/maryclare/atsa/blob/master/content/data/Electricty.RData)
    - [Stocks](https://github.com/maryclare/atsa/blob/master/content/data/Stocks.RData)
    - [Yields](https://github.com/maryclare/atsa/blob/master/content/data/Yields.RData)
    - Storage (will be uploaded by Saturday, 4/13/19)
    - [Air](https://github.com/maryclare/atsa/blob/master/content/data/Air.RData)
    - [Beijing](https://github.com/maryclare/atsa/blob/master/content/data/Beijing.RData)

- Submit second analysis as part of Homework 8
- Submit final paper on Tuesday, 5/7/19 by 11:59pm.

  
  Before spring break, I will collect potential datasets. Over spring break, I will explore the submitted data and decide which ones seem well suited for the project. 
  
  
  
## Submitting a Potential Dataset

Submissions submitted by 12:00pm noon on Sunday, March 31 will be considered. It is ok (and encouraged) to submit as a group! For full consideration, the submission should include the data (in whatever format you have it in, e.g. Excel or .csv) and an at most one page description which includes:

- One to five sentences describing the data, specifically:
    - The names of the students in the group;
    - How the data was obtained;
    - What the data are measuring;
    - The number of observations (if the data you're interested in contains multiple time series, just describe the number of observations for a single one)
    - The amount of time between each observed value.
    
- A single time series plot (if the data you're interested in contains multiple time series, just pick one);
- A sample autocorrelation plot;
- A sample partial autocorrelation plot.

All students who submit at least one potential dataset will have a single bonus point added to their final project grade.

Here are some links to some places to look for data, which range from data repositories that contain cleaned datasets that are often used to demonstrate various concepts to places where raw (uncleaned) time series data is available for download:
  
- [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets.html?format=&task=&att=&area=&numAtt=&numIns=&type=ts&sort=nameUp&view=table)
- [US Department of the Treasury Finance Data Directory](https://www.treasury.gov/resource-center/financial-education/Pages/fdd.aspx)
- [Google Trends](https://trends.google.com/trends/?ctab=0&date=all&geo=all&q=google&sort=0)
- [Time Series Data Library](https://pkg.yangzhuoranyang.com/tsdl/)
- [City of Chicago Data Portal](https://data.cityofchicago.org)
- [US Government Open Data Portal](https://www.data.gov)
- [`babynames` Package for `R`](https://cran.r-project.org/web/packages/babynames/babynames.pdf)
- [ASA Statistics in Sports Section's Recommended Data Resources](https://community.amstat.org/sis/sportsdataresources)
- [`nbastatR` Package for `R`](http://asbcllc.com/nbastatR/)
   
  
## Rubric for Final Paper
  
The final project will be:
- 5 pages long including figures and tables, excluding `R` code and references;
- Double spaced, with font size 12 and 1 inch margins;
- Written using `R` Markdown;
- Be accompanied by `R` code that reproduces all results.
  
A template will be provided.
  
It will be graded out of 25 points as follows:
- (5 points) Accurate description of changes made according to instructor and teaching assistant feedback on previous versions;
- (4 points) Introduction and exploratory analyses;
- (6 points) First analysis, focused on ARIMA modeling;
- (6 points) Second analysis, focused on state-space modeling;
- (4 points) Conclusions.

One bonus point will be added if the student submitted a suggested dataset by 12:00pm noon on Sunday, March 31.
  