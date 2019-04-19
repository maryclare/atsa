---
  layout: atsa
---
  
  Project
-------


Summary of Tasks:
  
- Submit a data set;
- Choose a data set for ARIMA modeling from the approved datasets:
    - [Anomaly](https://github.com/maryclare/atsa/blob/master/content/data/Anomaly.RData)
        - Monthly measurements of the average deviation of temperature measurements from meterological stations and the ocean from their 1951-1980 averages. Measured from 1980 into 2018. This data was downloaded from [NASA](https://data.giss.nasa.gov/gistemp/).
    - [Stocks](https://github.com/maryclare/atsa/blob/master/content/data/Stocks.RData)
        - Daily adjusted closing prices of Apple, Adobe, Amazon, Facebook, Google, and the S&P 500 from the beginning of 2017 into 2019. This data was downloaded from [Yahoo Finance](https://finance.yahoo.com). Note that the daily measurements do not include weekends, so a "week" of data corresponds to five consecutive days. We will just treat Friday to Monday as one unit of time apart.
    - [Yields](https://github.com/maryclare/atsa/blob/master/content/data/Yields.RData)
        - Daily three month and ten year treasury yield curve rates. This data was downloaded from the [United States Treasury](https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/textview.aspx?data=yield). Note that the daily measurements do not include weekends, so a "week" of data corresponds to five consecutive days. We will just treat Friday to Monday as one unit of time apart.

- Submit first analysis as part of Homework 7
- Choose a data set for state-space modeling from the approved datasets:
    - [Anomaly](https://github.com/maryclare/atsa/blob/master/content/data/Anomaly.RData)
    - [Electricity](https://github.com/maryclare/atsa/blob/master/content/data/Electricity.RData)
    - [Stocks](https://github.com/maryclare/atsa/blob/master/content/data/Stocks.RData)
    - [Yields](https://github.com/maryclare/atsa/blob/master/content/data/Yields.RData)
    - Storage (will be uploaded soon)
    - [Air](https://github.com/maryclare/atsa/blob/master/content/data/Air.RData)
    - [Beijing](https://github.com/maryclare/atsa/blob/master/content/data/Beijing.RData)

- Submit second analysis as part of Homework 8
- Submit final paper on Thursday, 5/7/19 by 11:59pm.

  
  
  
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

Before spring break, I will collect potential datasets. Over spring break, I will explore the submitted data and decide which ones seem well suited for the project. 
   
  
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
  