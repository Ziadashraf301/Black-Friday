Black friday (Problems and data preprocessing)
================
2023-07-14

# Introduction

Black Friday is a colloquial term for the Friday after Thanksgiving in
the United States. It traditionally marks the start of the Christmas
shopping season in the United States. Many stores offer highly promoted
sales at discounted prices and often open early, sometimes as early as
midnight, or even on Thanksgiving. Some stores’ sales continue until
Monday or for a week. Black Friday has routinely been the busiest
shopping day of the year in the United States.

The term “Black Friday” has been used for centuries to describe days
marked by disastrous events. While there have been many such
occurrences, the most significant “Black Friday” in American history
took place during the Panic of 1869. This financial crisis was sparked
by Jay Gould and James Fisk, who used their connections with the Grant
Administration to attempt to monopolize the gold market. Upon learning
of the scheme, President Grant ordered the Treasury to release a large
amount of gold, which put a stop to the manipulation but also caused
prices to plummet by 18%. The fallout from this event was swift and
severe, with fortunes being made and lost in a single day.

<img
src="https://upload.wikimedia.org/wikipedia/commons/9/97/Gold_room_scene_on_Black_Friday.jpg"
width="655" />

The first recorded use of the term “Black Friday” to describe the day
after Thanksgiving dates back to November 1951 and was published in the
journal, Factory Management, and Maintenance. The term was used again in
1952 and referred to the practice of workers calling in sick on that day
to extend their holiday weekend to four days.

In Philadelphia and Rochester, the police began using the terms “Black
Friday” and “Black Saturday” in the early 1960s to describe the heavy
traffic and chaotic crowds that accompanied the start of the Christmas
shopping season. Despite efforts by retailers to rebrand the day as “Big
Friday” in the 1960s, the term “Black Friday” eventually stuck and
became widely used across the United States to describe the day of
frenzied shopping that follows Thanksgiving.

Although the term “Black Friday” had been in use for several decades, it
was not until November 29, 1975, that it was first mentioned in The New
York Times in reference to the busiest shopping and traffic day of the
year in Philadelphia. However, the use of the term gradually became more
widespread in subsequent years.

Despite its growing popularity, some retailers in other parts of the
country remained unaware of the term. In 1981, The Philadelphia Inquirer
reported that retailers in Cincinnati and Los Angeles were still
unfamiliar with the term “Black Friday” and were instead referring to it
as the day after Thanksgiving or the official beginning of the Christmas
shopping season. Nevertheless, the term “Black Friday” continued to gain
momentum and is now recognized as a common phrase used to describe the
day of post-Thanksgiving shopping in the United States.

![](https://static.dw.com/image/63854505_605.jpg)

In recent times, “Black Friday” has evolved into a massive shopping
event featuring a range of sales, promotions, and long queues outside
stores. Major retailers like Best Buy, Target, and Amazon have come to
rely on Black Friday to drive significant sales and offer door-busting
deals to attract shoppers.

The popularity of Black Friday has given rise to other shopping holidays
such as “Cyber Monday”, a day dedicated to online shopping, “Small
Business Saturday”, which encourages shoppers to support local
businesses, and “Giving Tuesday,” a day for charitable giving. These
retail holidays have become an integral part of the holiday season, and
shoppers eagerly anticipate the deals and discounts they offer. Black
Friday, in particular, has become a cultural phenomenon in the United
States, with some people camping outside stores for days to be the first
in line for deals.

**`To understand customer purchase behavior against various products of different categories, I need to collect and analyze data on customers demographics, purchasing history, and product preferences. So I will collect data have these characteristics.`**

# Problem Statement

For Black Friday, A retail company, “ABC Private Limited” wants to
understand customer purchase behavior (precisely, purchase amount) for
various products in different categories. They have shared purchase
summaries from multiple customers for selected high-volume products from
last month.

1.  `We aim to learn more about the customers through demographics, transactions, and purchases. We want to examine each variable we have and ascertain how it relates to the overall expense of the purchase. We want to build a model to predict the purchase amount of customers against various products, which will help them create a personalized offer for customers against different products through Black Friday.`

2.  `We want to segment the customers according to their demographics and purchase amounts to help the marketing team perform suitable marketing campaigns for each group of customers to attract their awareness towards the products before Black Friday.`

3.  `We also want to analyze the relationship between the products purchased to determine which products stood purchased together to recommend products to users and provide offers and discounts.`

**We will learn everything there is to know about customers, products,
and transactions using descriptive statistics, inference statistics,
Clustering, association mining, and Regression.**

# Data Collection

I used open data published on Kaggle to gain insights into consumer
behavior for the Black Friday shopping season. Kaggle is a platform that
hosts a variety of datasets, including those related to retail sales and
consumer behavior.

## About Data

The dataset provided by “ABC Private Limited” is a valuable resource for
understanding customer purchase behavior for various products within
different categories. The dataset includes purchase summaries for
selected high-volume products, as well as customer demographics, product
details, and total purchase amounts from the previous month.

Some of the key variables included in the dataset are:

1.  Customer demographics: This includes age, gender, marital status,
    city type, and other demographic information that can be used to
    segment customers and identify patterns in their purchasing
    behavior.

2.  Product details: This includes product ID and product category,
    which can be used to analyze sales by category, identify popular
    products and the relationship between products purchased.

3.  Purchase amount: This variable provides information on the amount
    that each customer spent on their purchases, which can be used to
    identify high-spending customers and for the modeling.

By analyzing this Black Friday data, a retail company can gain insights
into customer behavior and preferences, identify trends and patterns in
purchasing behavior, and develop strategies to optimize sales and
improve the customer experience.

## Load libraries and Data

The `tidyverse` library is a collection of R packages designed for data
science and data analysis. It includes several packages such as
`ggplot2`, `dplyr`, `tidyr`, `purrr`, and `readr`, which provide tools
for data manipulation, visualization, and analysis.

``` r
#load the tidyverse library
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

We have two datasets, the first is for training the regression model,
developing the clustering, association rule models and for descriptive
analysis. The test set will be used for prediction.

``` r
# Read in a CSV file called 'Train.csv'
black_friday <- read.csv('Train.csv')

# Display the first six rows of the data frame 'black_friday'
head(black_friday)
```

    ##   User_ID Product_ID Gender   Age Occupation City_Category
    ## 1 1000001  P00069042      F  0-17         10             A
    ## 2 1000001  P00248942      F  0-17         10             A
    ## 3 1000001  P00087842      F  0-17         10             A
    ## 4 1000001  P00085442      F  0-17         10             A
    ## 5 1000002  P00285442      M   55+         16             C
    ## 6 1000003  P00193542      M 26-35         15             A
    ##   Stay_In_Current_City_Years Marital_Status Product_Category_1
    ## 1                          2              0                  3
    ## 2                          2              0                  1
    ## 3                          2              0                 12
    ## 4                          2              0                 12
    ## 5                         4+              0                  8
    ## 6                          3              0                  1
    ##   Product_Category_2 Product_Category_3 Purchase
    ## 1                 NA                 NA     8370
    ## 2                  6                 14    15200
    ## 3                 NA                 NA     1422
    ## 4                 14                 NA     1057
    ## 5                 NA                 NA     7969
    ## 6                  2                 NA    15227

# Data Exploration

In this step I will discover the structure of the data, features, data
types and missing values.

## Check the structure of data

``` r
#display the structure of data
str(black_friday)
```

    ## 'data.frame':    550068 obs. of  12 variables:
    ##  $ User_ID                   : int  1000001 1000001 1000001 1000001 1000002 1000003 1000004 1000004 1000004 1000005 ...
    ##  $ Product_ID                : chr  "P00069042" "P00248942" "P00087842" "P00085442" ...
    ##  $ Gender                    : chr  "F" "F" "F" "F" ...
    ##  $ Age                       : chr  "0-17" "0-17" "0-17" "0-17" ...
    ##  $ Occupation                : int  10 10 10 10 16 15 7 7 7 20 ...
    ##  $ City_Category             : chr  "A" "A" "A" "A" ...
    ##  $ Stay_In_Current_City_Years: chr  "2" "2" "2" "2" ...
    ##  $ Marital_Status            : int  0 0 0 0 0 0 1 1 1 1 ...
    ##  $ Product_Category_1        : int  3 1 12 12 8 1 1 1 1 8 ...
    ##  $ Product_Category_2        : int  NA 6 NA 14 NA 2 8 15 16 NA ...
    ##  $ Product_Category_3        : int  NA 14 NA NA NA NA 17 NA NA NA ...
    ##  $ Purchase                  : int  8370 15200 1422 1057 7969 15227 19215 15854 15686 7871 ...

This output tells us that the train data has 550,068 observations
(transactions) and 12 variables. Here is a brief description of each
variable:

- `User_ID`: A unique identifier for each customer.

- `Product_ID`: A unique identifier for each product.

- `Gender`: The gender of the customer.

- `Age`: The age group of the customer.

- `Occupation`: The occupation of the customer.

- `City_Category`: The category of the city where the customer resides.

- `Stay_In_Current_City_Years`: The number of years the customer has
  lived in their current city.

- `Marital_Status`: The marital status of the customer.

- `Product_Category_1`: The primary category of the product.

- `Product_Category_2`: The secondary category of the product.

- `Product_Category_3`: The tertiary category of the product.

- `Purchase`: The amount spent by the customer on the product.

Based on this information, we can see that this data contains
information about customer demographics, product categories, and
purchase behavior.

## Check data types

``` r
#check the data type for each column
sapply(black_friday, class)
```

    ##                    User_ID                 Product_ID 
    ##                  "integer"                "character" 
    ##                     Gender                        Age 
    ##                "character"                "character" 
    ##                 Occupation              City_Category 
    ##                  "integer"                "character" 
    ## Stay_In_Current_City_Years             Marital_Status 
    ##                "character"                  "integer" 
    ##         Product_Category_1         Product_Category_2 
    ##                  "integer"                  "integer" 
    ##         Product_Category_3                   Purchase 
    ##                  "integer"                  "integer"

This tells us that `User_ID`, `Occupation`, `Marital_Status`,
`Product_Category_1`, `Product_Category_2`, `Product_Category_3`, and
`Purchase` are integer variables, `Product_ID`, `Gender`, `Age`,
`City_Category`, and `Stay_In_Current_City_Years` are character
variables. We need to convert all of them to factor type unless the
`purchase` column.

## Check Missing values

``` r
#count missing values in each column
colSums(is.na(black_friday))
```

    ##                    User_ID                 Product_ID 
    ##                          0                          0 
    ##                     Gender                        Age 
    ##                          0                          0 
    ##                 Occupation              City_Category 
    ##                          0                          0 
    ## Stay_In_Current_City_Years             Marital_Status 
    ##                          0                          0 
    ##         Product_Category_1         Product_Category_2 
    ##                          0                     173638 
    ##         Product_Category_3                   Purchase 
    ##                     383247                          0

This tells us that there are 173,638 missing values in the
`Product_Category_2` column and 383,247 missing values in the
`Product_Category_3` column. All the other columns have no missing
values.

# Data Cleaning

In this step, I will clean the data, change the columns type, and fill
in missing values, and errors. We will transform the data into an
organized version for analysis.

## Type Casting

We want to convert all the columns type to Factor type unless the
`purchase` column.

``` r
# Select all columns except for the "Purchase" column and convert them to factors
cols_to_convert <- colnames(black_friday)[-12]
  # Use lapply to apply the same function to all selected columns
black_friday[cols_to_convert] <- lapply(black_friday[cols_to_convert], as.factor)

#check the type now
sapply(black_friday, class)
```

    ##                    User_ID                 Product_ID 
    ##                   "factor"                   "factor" 
    ##                     Gender                        Age 
    ##                   "factor"                   "factor" 
    ##                 Occupation              City_Category 
    ##                   "factor"                   "factor" 
    ## Stay_In_Current_City_Years             Marital_Status 
    ##                   "factor"                   "factor" 
    ##         Product_Category_1         Product_Category_2 
    ##                   "factor"                   "factor" 
    ##         Product_Category_3                   Purchase 
    ##                   "factor"                  "integer"

## Missing values treatment

We have 20 unique category masked from 1 to 20. We want to fill the
missing values in the: `Product_Category_2`, and `Product_Category_3`
with 0. Because this indicates that this product is not included in
multiple categories.

``` r
# Length of unique categories.
unique_categories <- unique(black_friday$Product_Category_1)
print(unique_categories)
```

    ##  [1] 3  1  12 8  5  4  2  6  14 11 13 15 7  16 18 10 17 9  20 19
    ## Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20

``` r
length(unique_categories)
```

    ## [1] 20

``` r
set.seed(1234)
library(missForest)
library(doParallel)
```

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
registerDoParallel(cores = 3)

missing <- black_friday %>%  
  select(Product_Category_1, Product_Category_2,Product_Category_3, Purchase)

# Impute missing values using missForest
imputed_data <- missForest(missing, 
                           parallelize = 'forests',                                                         ntree = 100,
                          mtry = 2,         
                          maxiter = 15,     
                          verbose = TRUE)
```

    ##   parallelizing computation of the random forest model objects
    ##   missForest iteration 1 in progress...done!
    ##     estimated error(s): 0 0.3918082 
    ##     difference(s): 0 0.2770966 
    ##     time: 86.15 seconds
    ## 
    ##   missForest iteration 2 in progress...done!
    ##     estimated error(s): 0 0.318515 
    ##     difference(s): 0 0.06013499 
    ##     time: 110.62 seconds
    ## 
    ##   missForest iteration 3 in progress...done!
    ##     estimated error(s): 0 0.3117282 
    ##     difference(s): 0 0.03261415 
    ##     time: 101.36 seconds
    ## 
    ##   missForest iteration 4 in progress...done!
    ##     estimated error(s): 0 0.3118335 
    ##     difference(s): 0 0.03766322 
    ##     time: 87.78 seconds

``` r
imputed_data$OOBerror
```

    ##     NRMSE       PFC 
    ## 0.0000000 0.3117282

We can see that the OOB error rate is `0.3117`. This means that the
random forest algorithm was able to predict the missing values with an
accuracy of approximately `69%`.

``` r
# Extract the imputed data
imputed_pc2 <- imputed_data$ximp[, "Product_Category_2"]
imputed_pc3 <- imputed_data$ximp[, "Product_Category_3"]

# Replace missing values in the original dataset with the imputed values
black_friday$Product_Category_2[is.na(black_friday$Product_Category_2)] <- imputed_pc2[is.na(black_friday$Product_Category_2)]
black_friday$Product_Category_3[is.na(black_friday$Product_Category_3)] <- imputed_pc3[is.na(black_friday$Product_Category_3)]
```

We don’t have missing values now.

## Save data

Now I will load the preprocessed data to MySQL for later analysis and
modeling.

``` r
#Load the RMySQL package
library(RMySQL)
```

    ## Loading required package: DBI

``` r
#Create a connection to MySQL database
con <- dbConnect(MySQL(), user = "root", password = "3012001", host = "localhost",dbname = "fridayblack")
dbSendQuery(con, "SET GLOBAL local_infile = true;")
```

    ## <MySQLResult:0,0,0>

``` r
dbWriteTable(con, "black_friday_cleaned_table", black_friday, row.names = FALSE, overwrite = TRUE)
```

    ## [1] TRUE

``` r
# write query to acces the records from a particular table
result = dbSendQuery(con, "select * from black_friday_cleaned_table limit 5") 
fetch(result)
```

    ##   User_ID Product_ID Gender  Age Occupation City_Category
    ## 1 1000001  P00069042      F 0-17         10             A
    ## 2 1000001  P00248942      F 0-17         10             A
    ## 3 1000001  P00087842      F 0-17         10             A
    ## 4 1000001  P00085442      F 0-17         10             A
    ## 5 1000002  P00285442      M  55+         16             C
    ##   Stay_In_Current_City_Years Marital_Status Product_Category_1
    ## 1                          2              0                  3
    ## 2                          2              0                  1
    ## 3                          2              0                 12
    ## 4                          2              0                 12
    ## 5                         4+              0                  8
    ##   Product_Category_2 Product_Category_3 Purchase
    ## 1                  4                 12     8370
    ## 2                  6                 14    15200
    ## 3                 14                 17     1422
    ## 4                 14                 17     1057
    ## 5                 17                 16     7969

Now we loaded our data from the CSV file and then cleaned, casted, and
imputed the missing values. Then we store the preprocessed version in
the Mysql database for analysis and modeling.
