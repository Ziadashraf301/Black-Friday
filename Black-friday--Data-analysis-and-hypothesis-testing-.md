Black friday (Data analysis and hypothesis testing)
================
2023-07-14

# Load Data

First we need to retrieve the data from MySQL then check the types.

``` r
#Load the RMySQL package
library(RMySQL)
```

    ## Warning: package 'RMySQL' was built under R version 4.1.3

    ## Loading required package: DBI

    ## Warning: package 'DBI' was built under R version 4.1.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.2 --

    ## v ggplot2 3.3.6     v purrr   1.0.1
    ## v tibble  3.2.1     v dplyr   1.1.2
    ## v tidyr   1.3.0     v stringr 1.5.0
    ## v readr   2.1.2     v forcats 0.5.2

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Warning: package 'tibble' was built under R version 4.1.3

    ## Warning: package 'tidyr' was built under R version 4.1.3

    ## Warning: package 'readr' was built under R version 4.1.3

    ## Warning: package 'purrr' was built under R version 4.1.3

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## Warning: package 'stringr' was built under R version 4.1.3

    ## Warning: package 'forcats' was built under R version 4.1.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#Create a connection to MySQL database
con <- dbConnect(MySQL(), user = "root", password = "3012001", host = "localhost",dbname = "fridayblack")
dbSendQuery(con, "SET GLOBAL local_infile = true;") 
```

    ## <MySQLResult:0,0,0>

``` r
# write query to acces the records
black_friday = dbReadTable(con, "black_friday")
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
    ## 1                  4                 12     8370
    ## 2                  6                 14    15200
    ## 3                 14                 17     1422
    ## 4                 14                 17     1057
    ## 5                 17                 17     7969
    ## 6                  2                  3    15227

# Type Casting

``` r
glimpse(black_friday)
```

    ## Rows: 550,068
    ## Columns: 12
    ## $ User_ID                    <chr> "1000001", "1000001", "1000001", "1000001",~
    ## $ Product_ID                 <chr> "P00069042", "P00248942", "P00087842", "P00~
    ## $ Gender                     <chr> "F", "F", "F", "F", "M", "M", "M", "M", "M"~
    ## $ Age                        <chr> "0-17", "0-17", "0-17", "0-17", "55+", "26-~
    ## $ Occupation                 <chr> "10", "10", "10", "10", "16", "15", "7", "7~
    ## $ City_Category              <chr> "A", "A", "A", "A", "C", "A", "B", "B", "B"~
    ## $ Stay_In_Current_City_Years <chr> "2", "2", "2", "2", "4+", "3", "2", "2", "2~
    ## $ Marital_Status             <chr> "0", "0", "0", "0", "0", "0", "1", "1", "1"~
    ## $ Product_Category_1         <chr> "3", "1", "12", "12", "8", "1", "1", "1", "~
    ## $ Product_Category_2         <chr> "4", "6", "14", "14", "17", "2", "8", "15",~
    ## $ Product_Category_3         <chr> "12", "14", "17", "17", "17", "3", "17", "1~
    ## $ Purchase                   <dbl> 8370, 15200, 1422, 1057, 7969, 15227, 19215~

We want to convert all the columns type to Factor type unless the
purchase column.

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
    ##                   "factor"                  "numeric"

Now our data is ready for analysis.

# Descriptive statistics

We will get quick insights about the data using descriptive statistics.

``` r
#summary of the distribution of each variable in the data frame
summary(black_friday)
```

    ##     User_ID           Product_ID     Gender        Age           Occupation    
    ##  1001680:  1026   P00265242:  1880   F:135809   0-17 : 15102   4      : 72308  
    ##  1004277:   979   P00025442:  1615   M:414259   18-25: 99660   0      : 69638  
    ##  1001941:   898   P00110742:  1612              26-35:219587   7      : 59133  
    ##  1001181:   862   P00112142:  1562              36-45:110013   1      : 47426  
    ##  1000889:   823   P00057642:  1470              46-50: 45701   17     : 40043  
    ##  1003618:   767   P00184942:  1440              51-55: 38501   20     : 33562  
    ##  (Other):544713   (Other)  :540489              55+  : 21504   (Other):227958  
    ##  City_Category Stay_In_Current_City_Years Marital_Status Product_Category_1
    ##  A:147720      0 : 74398                  0:324731       5      :150933    
    ##  B:231173      1 :193821                  1:225337       1      :140378    
    ##  C:171175      2 :101838                                 8      :113925    
    ##                3 : 95285                                 11     : 24287    
    ##                4+: 84726                                 2      : 23864    
    ##                                                          6      : 20466    
    ##                                                          (Other): 76215    
    ##  Product_Category_2 Product_Category_3    Purchase    
    ##  17     : 74080     17     :172703     Min.   :   12  
    ##  7      : 64326     14     :117177     1st Qu.: 5823  
    ##  8      : 64088     16     : 82928     Median : 8047  
    ##  14     : 56885     12     : 35914     Mean   : 9264  
    ##  15     : 56825     15     : 32308     3rd Qu.:12054  
    ##  2      : 49217     13     : 20731     Max.   :23961  
    ##  (Other):184647     (Other): 88307

Here are some insights based on the summary of the `black_friday` data:

-   User ID 1001680 appears the most frequently in the data, with 1026
    of orders.

-   Product ID P00265242 appears the most frequently in the data, with
    1880 of orders.

-   There are 135809 female users and 414259 male users in the data.
    This indicates that there are more male customers than female
    customers in the data.

-   The most common age group is 26-35, with 219587 of the orders, while
    the least common age group is 0-17, with 15102 of the orders.

-   The most common occupation of the customers is 4, with a count of
    72308.

-   The most common city category of the customers is B, with 231173 of
    the orders, while the least common city category is A, with 147720
    of the orders.

-   The most common duration of stay in the current city for the
    customers is one year, with a count of 193821 users. There are also
    significant numbers of users who have stayed for two years (101838),
    three years (95285), four or more years (84726), and by zero years
    with a count of 74398 users

-   The majority of users in the data are single with a count of 324731,
    while the

-   remaining users are married.

-   The most commonly purchased product main categories are category 5
    (150933), category 1 (140378), and category 8 (113925).

-   For the first product subcategory, the most commonly purchased
    subcategory is 17 with a count of 74080. For the second product
    subcategory, the most commonly purchased subcategory is also 17,
    with a count of 172703.

-   The average purchase amount is 9264 dollars, while the minimum and
    maximum purchase amounts are 12 dollars and 23961 dollars ,
    respectively. This indicates that the Purchase variable is right
    skewed and have outliers.

# Data analysis

For the first part of the project we want to analyze the transactions
data for the last month to get insights into these questions before
Black Friday:

1.  `What are the top 10 users based on the number of orders?`

2.  `What are the top 10 users based on the total purchases?`

3.  `What are the top 10 items sold in the last month?`

4.  `What is the Gender that has the most orders in the last month?`

5.  `What is the Age range that has the most orders in the last month?`

6.  `What is the marital status that has the most orders in the last month?`

7.  `What is the city that has the most orders in the last month?`

8.  `What is the occupation that has the most orders in the last month?`

9.  `What is the most common duration of stay in the current city  that has the most orders in the last month?`

10. `Is the mean purchase amount statistically significantly different between the Gender, Age Groups?`

We can answers our questions now through visualization:

`What are the top 10 users based on the number of orders in the last month??`

``` r
# Group the data by user ID and count the number of orders for each user
user_counts <- black_friday %>%
  group_by(User_ID) %>%
  summarize(num_orders = n()) %>%
  arrange(desc(num_orders)) %>%
  head(10)
# Create a bar plot of the top 10 users
ggplot(data = user_counts, 
       aes(x = reorder(User_ID, desc(num_orders)), y = 
             num_orders)) +
  geom_bar(stat = "identity", fill = "#8B0000") +
  geom_text(aes(label = num_orders), vjust = 3) +
  labs(title = "Top 10 users by number of orders", 
       x = "User ID", y = "") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

-   We can see that users `1001680` and `1004277` were the most active
    customers in the last month.

-   The range of the orders of the top 10 active customers range from
    1026 to 727 order.

    `What are the top 10 users based on the total purchases in the last month?`

    ``` r
    # Group the data by user ID and calculate the sum of purchases for each user
    user_purchases <- black_friday %>%
      group_by(User_ID) %>%
      summarize(total_purchases = sum(Purchase)) %>%
      arrange(desc(total_purchases)) %>%
      head(10)

    # Create a bar plot of the top 10 users based on total purchases
    ggplot(data = user_purchases, 
           aes(x = reorder(User_ID, desc(total_purchases)), 
               y = total_purchases)) +
      geom_bar(stat = "identity", fill = "#8B0000") +
      geom_text(aes(label = paste0(
        round(total_purchases/1e6, 2), "M")), vjust = 2) +
      labs(title = "Top 10 Users by total purchases", 
           x = "User ID", y = "") +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5, hjust=1),
           axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())
    ```

    ![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

-   We can see that user `1004277` is the most richer, he bought items
    with `10.54M` in the last month.

-   We can see the most ten customers who bought items in the last
    month, the total purchase value ranges from
    `10.54 to 6.39 Millions`.

    `What are the top 10 items sold in the last month?`

    ``` r
    # Group the data by product ID and count the number of orders for each item
    product_sales <- black_friday %>%
      group_by(Product_ID) %>%
      summarize(num_orders = n()) %>%
      arrange(desc(num_orders)) %>%
      head(10)

    # Create a bar plot of the top 10 items
    ggplot(data = product_sales, 
           aes(x = reorder(Product_ID, desc(num_orders)), 
               y = num_orders)) +
      geom_bar(stat = "identity", fill = "#8B0000") +
      geom_text(aes(label = num_orders), vjust = 3) +
      labs(title = "Top 10 items by number of orders", 
           x = "Item ID", y = "") +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5, hjust=1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
    ```

    ![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

-   The top product is `P00265242`, was sold `1880` units of it, which
    is significantly higher than the rest of the products on the list.
    This suggests that this product was in high demand in the last
    month, and will likely be a popular item on Black Friday.

-   The total units sold for the top 10 products range from
    `1406 to 1880 units`, which suggests that there will be a high level
    of competition among products on Black Friday. This competition may
    be by factors such as price, quality, and availability.

    `What is the Gender that has the most orders in the last month?`

``` r
# Group the data by gender and calculate the total number of purchases made by each gender
gender_purchases <- black_friday %>%
  mutate(Gender = ifelse(Gender == "F","female","male")) %>% 
  group_by(Gender) %>%
  summarize(num_orders = n()) %>%
  arrange(desc(num_orders))

# Create a pie chart showing the proportion of purchases made by each gender
ggplot(data = gender_purchases, 
       aes(x = "", y = num_orders, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Proportion of purchases by gender", fill = "") +
  scale_fill_manual(values = c("#56B4E9","#0072B2")) +
  theme_void() +
  geom_text(aes(label = paste0(
    round(num_orders/sum(num_orders)*100), "%")), 
    position = position_stack(vjust = 0.5), size = 5) +
  guides(fill = guide_legend(title = "Gender"))
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

-   Based on this chart, it appears that males made up the majority of
    purchases on Black Friday, with `75% of all purchases`, while
    females made up `25% of all purchases`.

-   We can target males more than females in the Black Friday offers.

    `What is the Age range that has the most orders in the last month?`

``` r
# Group the data by age and calculate the total number of purchases made by each age group
age_purchases <- black_friday %>%
  group_by(Age) %>%
  summarize(num_orders = n()) %>%
  arrange(desc(num_orders))

# Create a histogram showing the distribution of purchases across different age groups with a density line
ggplot(data = age_purchases, aes(x = Age, y = num_orders)) +
  geom_bar(stat = "identity", fill = "#8B0000") +
  geom_text(aes(label = paste0(
    round(num_orders/1e3, 2), "K")), vjust = 1.4) +
  labs(title = "Distribution of purchases by age groups", x = "Age Group", y = "") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x= element_blank())
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

-   It looks like there were `219,587` purchases made by the age group
    of `26-35`, followed by `110,013` purchases made by the age group of
    `36-45`, and `99,660` purchases made by the age group of `18-25`.

-   The remaining age groups made fewer purchases, with `45,701`
    purchases made by the age group of `46-50`, `38,501` purchases made
    by the age group of 51-55, 21,504 purchases made by the age group of
    55+, and 15,102 purchases made by the age group of 0-17.

-   We can target the top 3 age groups more than others.

    `What is the marital status that has the most orders in the last month?`

``` r
# Group the data by marital status and calculate the total number of purchases made by each status
marital_status_purchases <- black_friday %>%
    mutate(Marital_Status = ifelse(Marital_Status == 1, "married", "single")) %>%
  group_by(Marital_Status) %>%
  summarize(num_orders = n()) %>%
  arrange(desc(num_orders))

# Create a pie chart showing the proportion of purchases made by each marital status
ggplot(data = marital_status_purchases, 
       aes(x = "", y = num_orders, fill = Marital_Status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Proportion of purchases by marital status", 
       fill = "Marital Status") +
  scale_fill_manual(values = c("#56B4E9","#0072B2")) +
  theme_void() +
  geom_text(aes(label = paste0(
    round(num_orders/sum(num_orders)*100), "%")), 
    position = position_stack(vjust = 0.5), size = 5)
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

-   It appears that a majority of the purchases (59%) were made by
    single customers, while the remaining 41% of the purchases were made
    by customers who are married.

    `What is the city type that has the most orders in the last month?`

    ``` r
    # Group the data by city and calculate the total number of orders made by each city
    city_orders <- black_friday %>%
      group_by(City_Category) %>%
      summarize(num_orders = n()) %>%
      arrange(desc(num_orders))

    # Define a color palette for the pie chart
    colors <- c("#BFD3E6","#0072B2","#56B4E9")

    # Create a pie chart showing the distribution of orders across different cities
    ggplot(city_orders, 
           aes(x = "", y = num_orders, fill = City_Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Distribution of orders by city") +
      scale_fill_manual(values = colors) +
      geom_text(aes(label = paste0(
        round(num_orders/sum(num_orders)*100), "%")), 
        position = position_stack(vjust = 0.5), size = 5)
    ```

    ![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

-   It looks like there were 42% of the orders made in cities
    categorized as “B”, 31% of orders made in cities categorized as “C”,
    and 27% of the orders made in cities categorized as “A”.

`What is the customers' occupation that has the most orders in the last month?`

``` r
# Group the data by occupation and calculate the total number of orders made by each occupation
occupation_orders <- black_friday %>%
  group_by(Occupation) %>%
  summarize(num_orders = n()) %>%
  arrange(desc(num_orders)) %>%  
   slice(1:5)

# Create a horizontal bar chart showing the distribution of orders across different occupations
ggplot(occupation_orders, aes(x = num_orders, y = fct_reorder(Occupation, num_orders))) +
  geom_bar(stat = "identity", fill = "#8B0000") +
  labs(title = "Top 5 Occupation by number of orders", y = "Occupation", x = "") +
  geom_text(aes(label = paste0(
    round(num_orders/1e3, 2), "K")), hjust = 1.4) +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

It looks like the top three customers’ Occupation with the most orders
in the last month are:

-   Occupation with id 4 (72,308 orders)

-   Occupation with id 0 (69,638 orders)

-   Occupation with id 7 (59,133 orders)

    We can target those customers than other.

    `What is the most common duration of stay in the current city  that has the most orders in the last month?`

``` r
# Group the data by city and calculate the total number of orders made by each city
most_common_duration <- black_friday %>%
  group_by(Stay_In_Current_City_Years) %>%
  summarize(num_orders = n()) %>%
  arrange(desc(num_orders))

# Create a histogram showing the distribution of durations of stay in the current city
ggplot(most_common_duration, 
       aes(x = Stay_In_Current_City_Years, y = num_orders)) +
  geom_bar(stat = "identity", fill = "#8B0000") +
  labs(title = "Distribution of duration of stay in current city", x = "Duration of Stay (in years)") +
  geom_text(aes(label = paste0(round(num_orders/1e3, 2), "K")), vjust = 2,size = 4) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_text( size = 10),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(), 
  )
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

It looks like the most common duration of stay in the current city is “1
year”, with 193,821 orders. The other duration of stay in descending
order based on the number of orders are:

-   “2 years”: 101,838 orders

-   “3 years”: 95,285 orders

-   “4+ years”: 84,726 orders

-   “0 years”: 74,398 orders

# Hypothesis test

`Is the mean purchase amount statistically significantly different between the Gender Groups?`

To determine if the mean purchase amount is statistically significantly
different between gender groups, we can perform a hypothesis test.

We can start by setting up our null and alternative hypotheses:

-   Null hypothesis (H0): The mean purchase amount is the same between
    gender groups.

-   Alternative hypothesis (HA): The mean purchase amount is different
    between gender groups.

We can use a two-sample t-test to test this hypothesis, where we compare
the mean purchase amount between the male and female groups.

``` r
ggplot(black_friday, aes(x = Gender, y = Purchase)) +
  geom_boxplot()
```

![](Black-friday--Data-analysis-and-hypothesis-testing-_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

We can see slightly difference between the two means. The variance of
the two groups is not equal.

``` r
# Perform a t-test to compare the mean purchase amount between male and female groups
t_test_results <- t.test(Purchase ~ Gender, data = black_friday, var.equal = FALSE)

# Print the results of the t-test
t_test_results
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Purchase by Gender
    ## t = -46.358, df = 245163, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group F and group M is not equal to 0
    ## 95 percent confidence interval:
    ##  -732.6806 -673.2399
    ## sample estimates:
    ## mean in group F mean in group M 
    ##        8734.566        9437.526

Based on the output of the Two Sample t-test, we can make the following
insights:

-   The t-value is -46.358 and the degrees of freedom is 245163, which
    indicates that there is a significant difference between the mean
    purchase amount of the male and female groups.

-   The p-value is less than 2.2e-16, which is much smaller than the
    significance level of 0.05. Therefore, we can reject the null
    hypothesis and conclude that the mean purchase amount is
    significantly different between the male and female groups.

-   The 95% confidence interval for the difference in means between the
    male and female groups is between -732.6806 and -673.2399. This
    interval does not contain the value of 0, which further supports the
    conclusion that the mean purchase amount is significantly different
    between the male and female groups.

-   The mean purchase amount for females is 8,734.566 and the mean
    purchase amount for males is 9,437.526. This indicates that, on
    average, males spend more than females in the last month.

    `Is the mean purchase amount statistically significantly different between the Age Groups?`

    To determine if the mean purchase amount is statistically
    significantly different between age groups, we can perform a
    hypothesis test.

    We can start by setting up our null and alternative hypotheses:

    -   Null hypothesis (H0): The mean purchase amount is the same
        between age groups.

    -   Alternative hypothesis (HA): The mean purchase amount is
        different between age groups.

    We can use an analysis of variance (ANOVA) test to compare the mean
    purchase amount between the different age groups.

    ``` r
    # Perform an ANOVA test to compare the mean purchase amount between age groups
    anova_results <- aov(Purchase ~ Age, data = black_friday)

    # Print the results of the ANOVA test
    summary(anova_results)
    ```

        ##                 Df    Sum Sq   Mean Sq F value Pr(>F)    
        ## Age              6 6.140e+09 1.023e+09   40.58 <2e-16 ***
        ## Residuals   550061 1.387e+13 2.522e+07                   
        ## ---
        ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Based on this output, we can make the following insights:

-   The F-statistic is 40.58, which indicates that there is a
    significant difference in the mean purchase amount between the age
    groups.

-   The p-value is less than 2e-16, which is much smaller than the
    significance level of 0.05. Therefore, we can reject the null
    hypothesis and conclude that the mean purchase amount is
    significantly different between the age groups.

-   The degrees of freedom for the age group is 6, and the degrees of
    freedom for the residuals is 550,061.

-   The sum of squares for the age group is 6.140e+09, and the sum of
    squares for the residuals is 1.387e+13.

-   The mean square for the age group is 1.023e+09, and the mean square
    for the residuals is 2.522e+07.

-   Overall, these insights suggest that there is a significant
    difference in the mean purchase amount between the age groups, and
    that the age group variable may be a good predictor of the purchase
    amount.

# Conclusion

**Based on the information that users `1001680 and 1004277` were the
most active customers in the last month, the marketing team can draw the
following insights for the Black Friday offers:**

1.  Targeted promotions: The marketing team can create targeted
    promotions for these two users to encourage them to make purchases
    on Black Friday. This can include personalized discounts, early
    access to deals, or special offers on products that they have shown
    interest in.

2.  Customer retention: The marketing team can use this opportunity to
    retain these active customers by providing them with an exceptional
    shopping experience on Black Friday. This can include offering fast
    and reliable shipping, easy returns, and excellent customer service.

**For the other top users, the marketing team can use the following
additional insights to create effective Black Friday offers:**

1.  Cross-selling: The marketing team can use the purchase history of
    these top users to identify other related products that they may be
    interested in and offer them as cross-selling opportunities on Black
    Friday.

2.  Exclusive offers: The marketing team can create exclusive offers for
    these top users to make them feel valued and encourage them to make
    purchases on Black Friday. This can include offering early access to
    deals or products, or providing them with a personal shopping
    assistant to help them find the products they are interested in.T

**The marketing team can also draw the following insights for the Black
Friday offers:**

1.  Product promotion: Since `P00265242` was the top-selling product in
    the last month, the marketing team can create promotions around this
    product to attract customers to make purchases on Black Friday. This
    can include offering discounts or bundle deals with related
    products.

2.  Targeted promotions: Based on the gender and age group insights, the
    marketing team can create targeted promotions for each group to
    increase their engagement and sales on Black Friday. For example,
    they can offer promotions on products that are popular among males
    or females, or offer discounts to customers in lower age groups.

3.  City-specific promotions: Since the majority of orders were made in
    cities categorized as “B” and “C”, the marketing team can create
    city-specific promotions to target these customers. This can include
    offering discounts on products that are popular in these cities or
    offering free shipping for orders above a certain amount.

4.  Occupation-specific promotions: The marketing team can create
    occupation-specific promotions to attract customers in the top three
    occupation categories with the most orders in the last month. This
    can include offering discounts on products related to their
    occupation or providing them with early access to deals.

5.  Personalized offers: The marketing team can use the duration of stay
    in the current city to create personalized offers for customers. For
    example, they can offer discounts on products that are popular among
    customers who have been in the city for a 1 year and offer loyalty
    rewards to customers who have been in the city for a longer period.

6.  Gender-specific pricing strategy: Based on the mean purchase amount
    for males and females, the marketing team can develop a pricing
    strategy that caters to the spending behavior of each gender. For
    example, they can offer discounts or bundle deals on products that
    are popular among females or offer premium products that are more
    likely to appeal to male customers.
