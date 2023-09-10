---
title: "Black-friday-market-basket-analysis"
output: 
  html_document:
    keep_md: true
always_allow_html: true
---



# Load Data

First we need to retrieve the data from MySQL then check the types.


```r
#Load the RMySQL package
library(RMySQL)
```

```
## Warning: package 'RMySQL' was built under R version 4.1.3
```

```
## Loading required package: DBI
```

```
## Warning: package 'DBI' was built under R version 4.1.3
```

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.1.3
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.2 --
```

```
## v ggplot2 3.3.6     v purrr   1.0.1
## v tibble  3.2.1     v dplyr   1.1.2
## v tidyr   1.3.0     v stringr 1.5.0
## v readr   2.1.2     v forcats 0.5.2
```

```
## Warning: package 'ggplot2' was built under R version 4.1.3
```

```
## Warning: package 'tibble' was built under R version 4.1.3
```

```
## Warning: package 'tidyr' was built under R version 4.1.3
```

```
## Warning: package 'readr' was built under R version 4.1.3
```

```
## Warning: package 'purrr' was built under R version 4.1.3
```

```
## Warning: package 'dplyr' was built under R version 4.1.3
```

```
## Warning: package 'stringr' was built under R version 4.1.3
```

```
## Warning: package 'forcats' was built under R version 4.1.3
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
#Create a connection to MySQL database
con <- dbConnect(MySQL(), user = "root", password = "3012001", host = "localhost",dbname = "fridayblack")
dbSendQuery(con, "SET GLOBAL local_infile = true;") 
```

```
## <MySQLResult:12,0,0>
```

```r
# write query to acces the records
black_friday = dbReadTable(con, "black_friday")
head(black_friday)
```

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
```


```r
glimpse(black_friday)
```

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
```

We want to convert all the columns type to Factor type unless the purchase column.


```r
# Select all columns except for the "Purchase" column and convert them to factors
cols_to_convert <- colnames(black_friday)[-12]
  # Use lapply to apply the same function to all selected columns
black_friday[cols_to_convert] <- lapply(black_friday[cols_to_convert], as.factor)

#check the type now
sapply(black_friday, class)
```

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
```

Now our data is ready for analysis.

# Data preprocessing

First, we need to convert the data into a transaction format in order to extract rules and identify the most frequently occurring items that are placed by the same user more frequently than other items.


```r
# Load the 'arules' library
library(arules)
```

```
## Warning: package 'arules' was built under R version 4.1.3
```

```
## Loading required package: Matrix
```

```
## Warning: package 'Matrix' was built under R version 4.1.3
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```
## 
## Attaching package: 'arules'
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```
## The following objects are masked from 'package:base':
## 
##     abbreviate, write
```

```r
# Splitting transactions
trans_data <- split(black_friday$Product_ID,
                    black_friday$User_ID)

# Transform data into a transactional dataset
trans_data <- as(trans_data, "transactions")

# Summarize the transactional dataset
summary(trans_data)
```

```
## transactions as itemMatrix in sparse format with
##  5891 rows (elements/itemsets/transactions) and
##  3631 columns (items) and a density of 0.02571586 
## 
## most frequent items:
## P00265242 P00025442 P00110742 P00112142 P00057642   (Other) 
##      1880      1615      1612      1562      1470    541929 
## 
## element (itemset/transaction) length distribution:
## sizes
##    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21 
##    1    7   10   22   33   58   77   94   98  125  107  116  125  100   99   85 
##   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37 
##   83   80   76   72   74   77   77   67   76   77   58   48   59   46   60   51 
##   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53 
##   51   41   48   64   43   49   40   37   40   48   33   42   37   36   30   45 
##   54   55   56   57   58   59   60   61   62   63   64   65   66   67   68   69 
##   41   48   38   31   42   40   26   31   31   30   27   44   25   31   31   25 
##   70   71   72   73   74   75   76   77   78   79   80   81   82   83   84   85 
##   40   25   34   23   27   25   27   26   29   18   26   22   24   22   26   30 
##   86   87   88   89   90   91   92   93   94   95   96   97   98   99  100  101 
##   20   18   24   14   23   17   20   21   18   27   21   16   13   14   19   24 
##  102  103  104  105  106  107  108  109  110  111  112  113  114  115  116  117 
##   15   15   17   12   10   11   15   15   13   13   17   15   11   25   13   11 
##  118  119  120  121  122  123  124  125  126  127  128  129  130  131  132  133 
##   18   18   25    9   14   12    8   13   10   10   11    6   12   11    8   12 
##  134  135  136  137  138  139  140  141  142  143  144  145  146  147  148  149 
##   10   12   11   19   17    5    8   10    7   12   13    5    7    7    8    9 
##  150  151  152  153  154  155  156  157  158  159  160  161  162  163  164  165 
##    8    6    7   11   11    8    4    9    5   10   10   14    8    7    9   10 
##  166  167  168  169  170  171  172  173  174  175  176  177  178  179  180  181 
##    8    6   13    9    8   13    9    9    7    9    8    8   10   13    8    7 
##  182  183  184  185  186  187  188  189  190  191  192  193  194  195  196  197 
##    5    6    9    8    3    7    7    5    8    6    4    3    3    5    3    5 
##  198  199  200  201  202  203  204  205  206  207  208  209  210  211  212  213 
##    7    6    5    7    5    7    3    5    8    6    7    6    6    2    5    1 
##  214  215  216  217  218  219  220  221  222  223  224  225  226  227  228  229 
##    4    5    7    7    8    2    6    4    5   10    5    5    1   12    6    3 
##  230  231  232  233  234  235  236  237  238  239  240  241  242  243  244  245 
##    7    7    3    4    4    4    6    5    4    4    4    2    7    6    3    3 
##  246  247  248  249  250  251  252  253  254  255  257  258  259  260  261  262 
##    4    1    3    2    2    4    2    1    6    2    5    4    2    1    5    2 
##  263  264  266  267  268  269  270  271  272  273  274  275  276  277  278  279 
##    1    5    3    6    4    5    3    6    1    3    3    5    1    1    1    2 
##  280  281  282  283  284  285  286  287  288  289  290  291  292  293  294  295 
##    5    6    5    8    1    2    6    5    2    3    3    6    2    1    1    5 
##  296  297  298  299  300  302  303  304  305  306  307  308  309  310  312  313 
##    3    1    2    6    2    1    2    3    3    4    5    2    1    3    7    2 
##  315  316  317  318  320  321  322  325  329  330  331  334  335  336  337  338 
##    4    3    1    1    1    2    4    2    2    1    2    2    1    1    2    5 
##  339  340  341  342  343  344  345  346  347  348  349  350  351  352  354  355 
##    3    3    2    3    4    2    1    4    2    2    1    1    2    3    1    5 
##  356  357  358  359  360  362  363  364  365  366  367  370  371  372  377  378 
##    1    3    1    1    1    3    2    1    1    1    2    1    2    1    2    4 
##  379  382  383  385  386  387  388  389  390  391  392  393  400  402  405  406 
##    4    2    2    2    1    1    1    2    1    4    2    3    2    1    3    1 
##  407  409  411  412  413  414  416  417  418  419  420  423  425  426  427  428 
##    3    3    2    2    1    1    1    3    1    1    1    1    1    1    1    2 
##  430  431  433  435  436  437  438  439  444  446  447  448  449  450  451  453 
##    2    1    1    3    3    1    1    2    2    1    2    1    1    1    3    2 
##  454  457  458  460  461  465  467  469  471  472  475  476  477  479  482  485 
##    1    1    2    1    1    2    3    1    2    2    2    2    1    2    1    2 
##  486  487  488  490  492  493  494  495  496  497  498  499  500  501  502  504 
##    3    1    1    1    1    2    2    1    1    2    1    2    1    1    1    1 
##  514  517  524  527  529  530  533  537  538  544  547  549  557  558  559  560 
##    1    1    1    1    1    1    2    1    1    1    1    2    1    3    1    1 
##  568  570  572  573  575  584  588  606  612  617  622  632  651  671  676  680 
##    1    1    1    1    2    1    1    1    1    1    1    1    1    1    1    1 
##  685  691  694  698  705  709  714  718  727  729  740  752  767  823  862  898 
##    1    1    1    2    1    1    1    1    1    1    1    1    1    1    1    1 
##  979 1026 
##    1    1 
## 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    6.00   26.00   54.00   93.37  117.00 1026.00 
## 
## includes extended item information - examples:
##      labels
## 1 P00000142
## 2 P00000242
## 3 P00000342
## 
## includes extended transaction information - examples:
##   transactionID
## 1       1000001
## 2       1000002
## 3       1000003
```

The summary of the transaction data:

-   The dataset has 5891 rows, representing each user's transactions.

-   It consists of 3631 columns, representing unique items.

-   The density of the dataset is 0.02571586, indicating the proportion of non-zero elements in the sparse matrix representation of the dataset.

The section "most frequent items" shows the five most frequently occurring items in the dataset, followed by "(Other)" which represents all other items not listed individually. The counts for the most frequent items are as follows:

-   P00265242: 1880 occurrences

-   P00025442: 1615 occurrences

-   P00110742: 1612 occurrences

-   P00112142: 1562 occurrences

-   P00057642: 1470 occurrences

The count 541929 represents the occurrences of all other items collectively.

A density of 0.02571586 means that approximately 2.57% of the elements in the dataset are non-zero. This indicates that the dataset is relatively sparse, with a majority of the elements being zeros. This is a strong indicator to use reasonably lower values for the parameters of the Apriori algorithm in order to capture the most important rules. Lowering the support threshold and adjusting other parameters accordingly can help discover infrequent but meaningful itemsets and association rules in sparse datasets.


```r
# Inspect the first user's transaction in the transactional dataset
inspect(head(trans_data, 1))
```

```
##     items        transactionID
## [1] {P00000142,               
##      P00004842,               
##      P00025442,               
##      P00051442,               
##      P00051842,               
##      P00058142,               
##      P00059442,               
##      P00064042,               
##      P00069042,               
##      P00074142,               
##      P00085442,               
##      P00085942,               
##      P00087842,               
##      P00102642,               
##      P00110842,               
##      P00111842,               
##      P00117942,               
##      P00142242,               
##      P00165942,               
##      P00178242,               
##      P00178342,               
##      P00183942,               
##      P00184942,               
##      P00210342,               
##      P00214842,               
##      P00220642,               
##      P00248442,               
##      P00248942,               
##      P00255842,               
##      P00258742,               
##      P00289942,               
##      P00297042,               
##      P00375436,               
##      P0096542,                
##      P0097142}         1000001
```

We can observe that these are all the unique products purchased by customer 1000001.


```r
# Inspect the last user's transaction in the transactional dataset
inspect(tail(trans_data,1))
```

```
##     items        transactionID
## [1] {P00000142,               
##      P00001742,               
##      P00002542,               
##      P00003242,               
##      P00003642,               
##      P00005042,               
##      P00010842,               
##      P00020842,               
##      P00022642,               
##      P00024042,               
##      P00024142,               
##      P00025942,               
##      P00028242,               
##      P00029842,               
##      P00031742,               
##      P00032542,               
##      P00036442,               
##      P00043342,               
##      P00043542,               
##      P00044442,               
##      P00048242,               
##      P00051442,               
##      P00052242,               
##      P00054942,               
##      P00058042,               
##      P00059442,               
##      P00062942,               
##      P00071442,               
##      P00072242,               
##      P00081142,               
##      P00085042,               
##      P00085642,               
##      P00085942,               
##      P00086342,               
##      P00086442,               
##      P00087042,               
##      P00089342,               
##      P00093242,               
##      P00100842,               
##      P00101942,               
##      P00102042,               
##      P00102342,               
##      P00103242,               
##      P00103342,               
##      P00105142,               
##      P00106042,               
##      P00108642,               
##      P00109242,               
##      P00110042,               
##      P00110142,               
##      P00110542,               
##      P00110742,               
##      P00111042,               
##      P00111142,               
##      P00111742,               
##      P00112142,               
##      P00112342,               
##      P00112442,               
##      P00112842,               
##      P00113342,               
##      P00113642,               
##      P00114142,               
##      P00114742,               
##      P00114942,               
##      P00115742,               
##      P00115842,               
##      P00116142,               
##      P00116342,               
##      P00117342,               
##      P00117442,               
##      P00118242,               
##      P00118742,               
##      P00119442,               
##      P00120042,               
##      P00120242,               
##      P00120442,               
##      P00120542,               
##      P00120842,               
##      P00120942,               
##      P00121642,               
##      P00126142,               
##      P00127642,               
##      P00129442,               
##      P00134342,               
##      P00143142,               
##      P00145742,               
##      P00146442,               
##      P00148642,               
##      P00150142,               
##      P00151342,               
##      P00152742,               
##      P00155742,               
##      P00156042,               
##      P00157342,               
##      P00158742,               
##      P00161942,               
##      P00173342,               
##      P00177542,               
##      P00178242,               
##      P00178742,               
##      P00181242,               
##      P00182442,               
##      P00183142,               
##      P00184042,               
##      P00184242,               
##      P00184342,               
##      P00184942,               
##      P00188842,               
##      P00189042,               
##      P00189642,               
##      P00192042,               
##      P00193142,               
##      P00195442,               
##      P00205242,               
##      P00209542,               
##      P00210942,               
##      P00211042,               
##      P00211242,               
##      P00213242,               
##      P00216542,               
##      P00217442,               
##      P00217742,               
##      P00218642,               
##      P00219242,               
##      P00220342,               
##      P00220442,               
##      P00221542,               
##      P00222942,               
##      P00224742,               
##      P00226342,               
##      P00226442,               
##      P00227142,               
##      P00237542,               
##      P00237942,               
##      P00238442,               
##      P00240142,               
##      P00244042,               
##      P00244742,               
##      P00246342,               
##      P00248942,               
##      P00251242,               
##      P00251342,               
##      P00252442,               
##      P00253542,               
##      P00254242,               
##      P00254642,               
##      P00255842,               
##      P00258742,               
##      P00259042,               
##      P00259842,               
##      P00265242,               
##      P00266642,               
##      P00271042,               
##      P00271242,               
##      P00276342,               
##      P00277442,               
##      P00277642,               
##      P00277842,               
##      P00280442,               
##      P00280542,               
##      P00285342,               
##      P00286842,               
##      P00287942,               
##      P00288042,               
##      P00288242,               
##      P00288642,               
##      P00289642,               
##      P00291942,               
##      P00295342,               
##      P00296642,               
##      P00300542,               
##      P00310842,               
##      P00311642,               
##      P00323942,               
##      P00329242,               
##      P00331042,               
##      P00331942,               
##      P00343042,               
##      P00346242,               
##      P00349442}        1006040
```

We can observe that these are all the unique products purchased by customer 1006040, which is a greater number of products compared to customer 1000001.

Furthermore, we observe that there are subsets of products that are common to both customers. This suggests the presence of a pattern in the ordered products, indicating potential similarities or preferences in their purchasing behavior.

# Frequent Itemsets

First, I will find the support of the two products (P00115742, P00115842) to determine how frequently they are ordered together by the same users. This will help me understand the data better and determine the appropriate parameter space for the analysis.


```r
# Determine the support of both items with support 0.01
support_P00115742_P00115842 = 
    apriori(trans_data,
            parameter = list(target = "frequent itemsets",
                             supp = 0.01),
            appearance = list(items = 
                              c("P00115742",
                                "P00115842")))
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen
##          NA    0.1    1 none FALSE            TRUE       5    0.01      1
##  maxlen            target  ext
##      10 frequent itemsets TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 58 
## 
## set item appearances ...[2 item(s)] done [0.00s].
## set transactions ...[2 item(s), 5891 transaction(s)] done [0.17s].
## sorting and recoding items ... [2 item(s)] done [0.00s].
## creating transaction tree ... done [0.00s].
## checking subsets of size 1 2 done [0.00s].
## sorting transactions ... done [0.00s].
## writing ... [3 set(s)] done [0.00s].
## creating S4 object  ... done [0.00s].
```

```r
# Inspect the itemsets 
inspect(support_P00115742_P00115842)
```

```
##     items                  support    count
## [1] {P00115742}            0.04888813 288  
## [2] {P00115842}            0.06874894 405  
## [3] {P00115742, P00115842} 0.01035478  61
```

The insights from the output are as follows:

1.  The item "P00115742" has a support of approximately 0.0489, indicating that it appears in about 4.89% of the transactions. It has been ordered 288 times.

2.  The item "P00115842" has a support of approximately 0.0687, indicating that it appears in about 6.87% of the transactions. It has been ordered 405 times.

3.  Itemset {P00115742, P00115842}: The combination of items "P00115742" and "P00115842" together has a support of approximately 0.0104, indicating that they are ordered together in about 1.04% of the transactions. This itemset has occurred 61 times.

    Based on the information provided, it seems that an absolute minimum support count is 58. This count indicates a reasonable number of occurrences for the itemset of interest. Therefore, I will use a support parameter of 0.01, as it aligns with the observed count and can be considered an appropriate threshold for capturing meaningful frequent itemsets.


```r
# Frequent itemsets for all items
support_all = 
    apriori(trans_data,
            parameter = list(target="frequent itemsets",
                             supp = 0.01))
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen
##          NA    0.1    1 none FALSE            TRUE       5    0.01      1
##  maxlen            target  ext
##      10 frequent itemsets TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 58 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[3631 item(s), 5891 transaction(s)] done [0.22s].
## sorting and recoding items ... [1971 item(s)] done [0.01s].
## creating transaction tree ... done [0.00s].
## checking subsets of size 1 2 3 4
```

```
## Warning in apriori(trans_data, parameter = list(target = "frequent itemsets", :
## Mining stopped (time limit reached). Only patterns up to a length of 4 returned!
```

```
##  done [13.63s].
## sorting transactions ... done [0.00s].
## writing ... [1351203 set(s)] done [0.29s].
## creating S4 object  ... done [0.37s].
```

```r
# Inspect the 5 most frequent items
inspect(head(sort(support_all, by="support"), 5))
```

```
##     items       support   count
## [1] {P00265242} 0.3191309 1880 
## [2] {P00025442} 0.2741470 1615 
## [3] {P00110742} 0.2736378 1612 
## [4] {P00112142} 0.2651502 1562 
## [5] {P00057642} 0.2495332 1470
```

We can observe that the largest support value corresponds to approximately 32% of appearances in the dataset. However, the itemset does not exhibit a high proportion of occurrence. Therefore, to extract strong rules with the highest conditional probability, I will utilize the confidence metric in conjunction with the support value.

# Apriori

I will start with 40% confidence to filter the strong rules.


```r
# apriori algorithm
support_all = apriori(trans_data,
                      parameter = list(supp=0.01, conf = 0.4))
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen
##         0.4    0.1    1 none FALSE            TRUE       5    0.01      1
##  maxlen target  ext
##      10  rules TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 58 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[3631 item(s), 5891 transaction(s)] done [0.23s].
## sorting and recoding items ... [1971 item(s)] done [0.01s].
## creating transaction tree ... done [0.00s].
## checking subsets of size 1 2 3 4
```

```
## Warning in apriori(trans_data, parameter = list(supp = 0.01, conf = 0.4)):
## Mining stopped (time limit reached). Only patterns up to a length of 4 returned!
```

```
##  done [13.13s].
## writing ... [1635435 rule(s)] done [0.42s].
## creating S4 object  ... done [0.49s].
```

We can observe that we obtained a total of 1,635,435 rules by applying a support threshold of 1% and a confidence threshold of 40%. This number is quite large, indicating a significant number of associations between items in the dataset.

To extract only the most important rules, it is necessary to analyze the values of support and confidence. By evaluating these values, we can determine the optimal parameters that strike a balance between the number of rules and their significance.

By adjusting the support and confidence thresholds, we can refine the rule extraction process and focus on rules that are both frequent and highly reliable. This iterative analysis will help us identify the optimal parameters that yield a more manageable set of rules while still capturing meaningful associations between items.


```r
# Load arulesViz package
library(arulesViz) 
```

```
## Warning: package 'arulesViz' was built under R version 4.1.3
```

```r
library(dplyr)
library(ggplot2)

# Take top 10K rows ordered by support 
top_rules <- head(sort(support_all, by = "support"), 10000) 

# Convert to data frame
market_data <- as(top_rules, "data.frame") 


# Create lift level column  
market_data <- market_data %>%
  mutate(lift_level = case_when(
    lift < 2 ~ "1-2",
    lift >= 2 & lift < 3 ~ "2-3",
    lift >= 3 & lift < 4 ~ "3-4", 
    lift >= 4 & lift < 5 ~ "4-5",
    lift >= 5 & lift < 6 ~ "5-6",
    lift >= 6 ~ "6+"
  ))

# Plot with lift levels mapped to color
ggplot(market_data, aes(x=confidence, y=support, color=as.factor(lift_level))) +
  geom_point(size=3, alpha=0.8, position=position_jitter(w=0.01, h=0)) + 
  scale_color_manual(values=c("blue","green","yellow","orange","red","purple")) +
  labs(title = "Association Rules (Top 10000 rules by Support)",
       x = "Confidence",
       y = "Support",
       color="Lift Level") +
  theme_classic() +
  theme(legend.position="top") 
```

![](img/unnamed-chunk-10-1.png)<!-- -->

We observe a strong association (lift \> 1) in all 10,000 top rules.

The rules with higher support tend to have lower confidence. However, the rules with higher confidence still demonstrate stronger association on average compared to other rules. Therefore, I will increase the minimum support threshold to 5% in order to filter some lower-quality rules and to include the frequentist rules.


```r
# apriori algorithm
support_all = apriori(trans_data,
                    parameter = list(supp=0.05, conf = 0.4, minlen=2))
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen
##         0.4    0.1    1 none FALSE            TRUE       5    0.05      2
##  maxlen target  ext
##      10  rules TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 294 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[3631 item(s), 5891 transaction(s)] done [0.22s].
## sorting and recoding items ... [575 item(s)] done [0.00s].
## creating transaction tree ... done [0.02s].
## checking subsets of size 1 2 3 done [0.04s].
## writing ... [508 rule(s)] done [0.00s].
## creating S4 object  ... done [0.00s].
```

```r
# Take top 10K rows ordered by support 
top_rules <- head(sort(support_all, by = "support"), 1000) 

# Convert to data frame
market_data <- as(top_rules, "data.frame") 


# Create lift level column  
market_data <- market_data %>%
  mutate(lift_level = case_when(
    lift < 2 ~ "1-2",
    lift >= 2 & lift < 3 ~ "2-3",
    lift >= 3 & lift < 4 ~ "3-4", 
    lift >= 4 & lift < 5 ~ "4-5",
    lift >= 5 & lift < 6 ~ "5-6",
    lift >= 6 ~ "6+"
  ))

# Plot with lift levels mapped to color
ggplot(market_data, aes(x=confidence, y=support, color=as.factor(lift_level))) +
  geom_point(size=3, alpha=0.6, position=position_jitter(w=0.01, h=0)) + 
  scale_color_manual(values=c("blue","green","yellow","orange","red","purple")) +
  labs(title = "Association Rules (Top rules by Support)",
       x = "Confidence",
       y = "Support",
       color="Lift Level") +
  theme_classic() +
  theme(legend.position="top") 
```

![](img/unnamed-chunk-11-1.png)<!-- -->

We can explore these rules now.


```r
# Inspect the 10 most frequent rules
inspect(head(sort(support_all, by="support"), 10))
```

```
##      lhs            rhs         support   confidence coverage  lift     count
## [1]  {P00025442} => {P00110742} 0.1200136 0.4377709  0.2741470 1.599819 707  
## [2]  {P00110742} => {P00025442} 0.1200136 0.4385856  0.2736378 1.599819 707  
## [3]  {P00112142} => {P00110742} 0.1132236 0.4270166  0.2651502 1.560518 667  
## [4]  {P00110742} => {P00112142} 0.1132236 0.4137717  0.2736378 1.560518 667  
## [5]  {P00112142} => {P00025442} 0.1071125 0.4039693  0.2651502 1.473550 631  
## [6]  {P00237542} => {P00057642} 0.1032083 0.4361549  0.2366322 1.747884 608  
## [7]  {P00057642} => {P00237542} 0.1032083 0.4136054  0.2495332 1.747884 608  
## [8]  {P00278642} => {P00265242} 0.1028688 0.4942904  0.2081141 1.548864 606  
## [9]  {P00059442} => {P00265242} 0.1028688 0.4310100  0.2386692 1.350574 606  
## [10] {P00046742} => {P00057642} 0.1021898 0.4186370  0.2441012 1.677681 602
```

From looking at the top 10 association rules sorted by support:

-   The rules {P00025442} =\> {P00110742} and {P00110742} =\> {P00025442} have the highest support at 0.12, indicating these items are commonly purchased together. The high lift (\>1.5) also shows this is a strong association.

-   P00110742,P00025442,,P00057642,P00112142 appear in multiple rules. These products seem to have strong associations with multiple other products.

-   The rule {P00278642} =\> {P00265242} has high confidence (0.49) suggesting customers who buy P00278642 have a good chance of also buying P00265242.

# Network analysis

Now we need to utilize network analysis algorithms like PageRank and HITS to identify more patterns between the products. This analysis will help us determine which products are more likely to be purchased given that a customer has already purchased certain products. It will also help us identify products that act as good hubs, indicating that if a customer purchases them, they are likely to purchase popular products as well.

The marketing team could utilize network analysis algorithms like PageRank and HITS:

-   Identify product associations and affinity - The algorithms can identify patterns of products that are frequently purchased together or are related in some way. The marketing team can use these associations to make recommendations like "Customers who bought X also bought Y".

-   Develop targeted cross-sell/up-sell campaigns - By understanding product associations, the team can specifically target customers who purchased one product with offers for the other associated products. This allows for more customized and potentially effective cross-selling.

-   Identify authority and hub products - Authority products are purchased frequently with other items, while hub products link out to many authorities. Identifying these products allows marketing to focus promotion on authority items or bundle them with hubs.

-   Inform product placement and promotions - The network patterns can inform which products are grouped together in promotions, recommended sections, store placement, and more. Related or complementary products can be bundled or placed nearby.

-   Adjust inventory and purchasing - The product networks can help forecast demand and plan inventory by understanding which items are commonly purchased together. This ensures adequate stock of affiliated products.

-   Identify opportunities for new products - Gaps in the product networks could indicate opportunities to introduce new products that complement existing items. The algorithms aid discovery of these network opportunities.


```r
# Create a HTML widget of the graph of rules
plot(support_all,
method = "graph",
engine = "htmlwidget", asEdges = FALSE, itemCol = '#CBD2FC',
max = 550,nodeCo = "#EE0F0F")
```

```
## Warning: Unknown control parameters: asEdges
```

```
## Available control parameters (with default values):
## itemCol	 =  #CBD2FC
## nodeCol	 =  c("#EE0000", "#EE0303", "#EE0606", "#EE0909", "#EE0C0C", "#EE0F0F", "#EE1212", "#EE1515", "#EE1818", "#EE1B1B", "#EE1E1E", "#EE2222", "#EE2525", "#EE2828", "#EE2B2B", "#EE2E2E", "#EE3131", "#EE3434", "#EE3737", "#EE3A3A", "#EE3D3D", "#EE4040", "#EE4444", "#EE4747", "#EE4A4A", "#EE4D4D", "#EE5050", "#EE5353", "#EE5656", "#EE5959", "#EE5C5C", "#EE5F5F", "#EE6262", "#EE6666", "#EE6969", "#EE6C6C", "#EE6F6F", "#EE7272", "#EE7575", "#EE7878", "#EE7B7B", "#EE7E7E", "#EE8181", "#EE8484", "#EE8888", "#EE8B8B",  "#EE8E8E", "#EE9191", "#EE9494", "#EE9797", "#EE9999", "#EE9B9B", "#EE9D9D", "#EE9F9F", "#EEA0A0", "#EEA2A2", "#EEA4A4", "#EEA5A5", "#EEA7A7", "#EEA9A9", "#EEABAB", "#EEACAC", "#EEAEAE", "#EEB0B0", "#EEB1B1", "#EEB3B3", "#EEB5B5", "#EEB7B7", "#EEB8B8", "#EEBABA", "#EEBCBC", "#EEBDBD", "#EEBFBF", "#EEC1C1", "#EEC3C3", "#EEC4C4", "#EEC6C6", "#EEC8C8", "#EEC9C9", "#EECBCB", "#EECDCD", "#EECFCF", "#EED0D0", "#EED2D2", "#EED4D4", "#EED5D5", "#EED7D7", "#EED9D9", "#EEDBDB", "#EEDCDC", "#EEDEDE", "#EEE0E0",  "#EEE1E1", "#EEE3E3", "#EEE5E5", "#EEE7E7", "#EEE8E8", "#EEEAEA", "#EEECEC", "#EEEEEE")
## precision	 =  3
## igraphLayout	 =  layout_nicely
## interactive	 =  TRUE
## engine	 =  visNetwork
## max	 =  100
## selection_menu	 =  TRUE
## degree_highlight	 =  1
## verbose	 =  FALSE
```

```{=html}
<div id="htmlwidget-b42c8488e516198c31ac" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-b42c8488e516198c31ac">{"x":{"nodes":{"id":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641],"label":["P00000142","P00002142","P00003242","P00003942","P00005042","P00010742","P00021742","P00025442","P00028842","P00030842","P00031042","P00034742","P00034842","P00036842","P00037142","P00044442","P00046742","P00051442","P00052842","P00057542","P00057642","P00057742","P00057942","P00058042","P00059442","P00062842","P00070042","P00071442","P00073842","P00080342","P00085942","P00086442","P00100442","P00100842","P00101842","P00102342","P00102642","P00105142","P00106042","P00110742","P00110842","P00110942","P00111142","P00111742","P00112142","P00112442","P00112542","P00113142","P00113242","P00113342","P00113642","P00114942","P00115642","P00116842","P00117442","P00117942","P00120042","P00121342","P00121642","P00125942","P00126142","P00127242","P00127642","P00127842","P00128942","P00129342","P00129542","P00129642","P00139942","P00140742","P00142142","P00144642","P00145042","P00145442","P00147942","P00148642","P00153742","P00154042","P00156442","P00161942","P00174442","P00178242","P00178942","P00182142","P00182242","P00183242","P00183342","P00184942","P00193542","P00199442","P00210042","P00212942","P00213242","P00216342","P00220342","P00220442","P00221442","P00233542","P00237542","P00240142","P00242742","P00243942","P00245642","P00248142","P00249642","P00250242","P00251242","P00255842","P00258742","P00259342","P00260042","P00265242","P00270942","P00271142","P00274942","P00277442","P00277642","P00278642","P00289942","P00293242","P00294542","P00296042","P00317842","P00318742","P00323942","P00324942","P00329542","P00334242","P00338442","P00346142","P00350942","P00355142","P0097242","rule 1","rule 2","rule 3","rule 4","rule 5","rule 6","rule 7","rule 8","rule 9","rule 10","rule 11","rule 12","rule 13","rule 14","rule 15","rule 16","rule 17","rule 18","rule 19","rule 20","rule 21","rule 22","rule 23","rule 24","rule 25","rule 26","rule 27","rule 28","rule 29","rule 30","rule 31","rule 32","rule 33","rule 34","rule 35","rule 36","rule 37","rule 38","rule 39","rule 40","rule 41","rule 42","rule 43","rule 44","rule 45","rule 46","rule 47","rule 48","rule 49","rule 50","rule 51","rule 52","rule 53","rule 54","rule 55","rule 56","rule 57","rule 58","rule 59","rule 60","rule 61","rule 62","rule 63","rule 64","rule 65","rule 66","rule 67","rule 68","rule 69","rule 70","rule 71","rule 72","rule 73","rule 74","rule 75","rule 76","rule 77","rule 78","rule 79","rule 80","rule 81","rule 82","rule 83","rule 84","rule 85","rule 86","rule 87","rule 88","rule 89","rule 90","rule 91","rule 92","rule 93","rule 94","rule 95","rule 96","rule 97","rule 98","rule 99","rule 100","rule 101","rule 102","rule 103","rule 104","rule 105","rule 106","rule 107","rule 108","rule 109","rule 110","rule 111","rule 112","rule 113","rule 114","rule 115","rule 116","rule 117","rule 118","rule 119","rule 120","rule 121","rule 122","rule 123","rule 124","rule 125","rule 126","rule 127","rule 128","rule 129","rule 130","rule 131","rule 132","rule 133","rule 134","rule 135","rule 136","rule 137","rule 138","rule 139","rule 140","rule 141","rule 142","rule 143","rule 144","rule 145","rule 146","rule 147","rule 148","rule 149","rule 150","rule 151","rule 152","rule 153","rule 154","rule 155","rule 156","rule 157","rule 158","rule 159","rule 160","rule 161","rule 162","rule 163","rule 164","rule 165","rule 166","rule 167","rule 168","rule 169","rule 170","rule 171","rule 172","rule 173","rule 174","rule 175","rule 176","rule 177","rule 178","rule 179","rule 180","rule 181","rule 182","rule 183","rule 184","rule 185","rule 186","rule 187","rule 188","rule 189","rule 190","rule 191","rule 192","rule 193","rule 194","rule 195","rule 196","rule 197","rule 198","rule 199","rule 200","rule 201","rule 202","rule 203","rule 204","rule 205","rule 206","rule 207","rule 208","rule 209","rule 210","rule 211","rule 212","rule 213","rule 214","rule 215","rule 216","rule 217","rule 218","rule 219","rule 220","rule 221","rule 222","rule 223","rule 224","rule 225","rule 226","rule 227","rule 228","rule 229","rule 230","rule 231","rule 232","rule 233","rule 234","rule 235","rule 236","rule 237","rule 238","rule 239","rule 240","rule 241","rule 242","rule 243","rule 244","rule 245","rule 246","rule 247","rule 248","rule 249","rule 250","rule 251","rule 252","rule 253","rule 254","rule 255","rule 256","rule 257","rule 258","rule 259","rule 260","rule 261","rule 262","rule 263","rule 264","rule 265","rule 266","rule 267","rule 268","rule 269","rule 270","rule 271","rule 272","rule 273","rule 274","rule 275","rule 276","rule 277","rule 278","rule 279","rule 280","rule 281","rule 282","rule 283","rule 284","rule 285","rule 286","rule 287","rule 288","rule 289","rule 290","rule 291","rule 292","rule 293","rule 294","rule 295","rule 296","rule 297","rule 298","rule 299","rule 300","rule 301","rule 302","rule 303","rule 304","rule 305","rule 306","rule 307","rule 308","rule 309","rule 310","rule 311","rule 312","rule 313","rule 314","rule 315","rule 316","rule 317","rule 318","rule 319","rule 320","rule 321","rule 322","rule 323","rule 324","rule 325","rule 326","rule 327","rule 328","rule 329","rule 330","rule 331","rule 332","rule 333","rule 334","rule 335","rule 336","rule 337","rule 338","rule 339","rule 340","rule 341","rule 342","rule 343","rule 344","rule 345","rule 346","rule 347","rule 348","rule 349","rule 350","rule 351","rule 352","rule 353","rule 354","rule 355","rule 356","rule 357","rule 358","rule 359","rule 360","rule 361","rule 362","rule 363","rule 364","rule 365","rule 366","rule 367","rule 368","rule 369","rule 370","rule 371","rule 372","rule 373","rule 374","rule 375","rule 376","rule 377","rule 378","rule 379","rule 380","rule 381","rule 382","rule 383","rule 384","rule 385","rule 386","rule 387","rule 388","rule 389","rule 390","rule 391","rule 392","rule 393","rule 394","rule 395","rule 396","rule 397","rule 398","rule 399","rule 400","rule 401","rule 402","rule 403","rule 404","rule 405","rule 406","rule 407","rule 408","rule 409","rule 410","rule 411","rule 412","rule 413","rule 414","rule 415","rule 416","rule 417","rule 418","rule 419","rule 420","rule 421","rule 422","rule 423","rule 424","rule 425","rule 426","rule 427","rule 428","rule 429","rule 430","rule 431","rule 432","rule 433","rule 434","rule 435","rule 436","rule 437","rule 438","rule 439","rule 440","rule 441","rule 442","rule 443","rule 444","rule 445","rule 446","rule 447","rule 448","rule 449","rule 450","rule 451","rule 452","rule 453","rule 454","rule 455","rule 456","rule 457","rule 458","rule 459","rule 460","rule 461","rule 462","rule 463","rule 464","rule 465","rule 466","rule 467","rule 468","rule 469","rule 470","rule 471","rule 472","rule 473","rule 474","rule 475","rule 476","rule 477","rule 478","rule 479","rule 480","rule 481","rule 482","rule 483","rule 484","rule 485","rule 486","rule 487","rule 488","rule 489","rule 490","rule 491","rule 492","rule 493","rule 494","rule 495","rule 496","rule 497","rule 498","rule 499","rule 500","rule 501","rule 502","rule 503","rule 504","rule 505","rule 506","rule 507","rule 508"],"group":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],"value":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3.40291262135923,10.6116504854369,4.60436893203884,1.72087378640777,4.36407766990291,6.04611650485438,5.56553398058253,2.44174757281554,1.24029126213592,4.12378640776699,13.7354368932039,8.92961165048544,2.92233009708739,2.44174757281554,2.92233009708739,5.80582524271845,3.88349514563107,1.24029126213592,2.44174757281554,1.24029126213592,1.48058252427185,4.60436893203884,13.254854368932,7.00728155339806,2.44174757281554,5.80582524271845,1.24029126213592,10.1310679611651,11.0922330097087,2.20145631067961,1.96116504854369,1.96116504854369,10.371359223301,1.24029126213592,1.24029126213592,1,1.96116504854369,8.68932038834952,15.1771844660194,14.4563106796117,4.84466019417476,3.88349514563107,4.12378640776699,10.8519417475728,8.44902912621359,15.6577669902913,2.20145631067961,1.48058252427185,3.64320388349515,4.36407766990291,1.24029126213592,8.20873786407768,11.0922330097087,8.68932038834952,12.7742718446602,8.68932038834952,12.5339805825243,9.41019417475728,17.5800970873786,7.4878640776699,2.20145631067961,3.88349514563107,15.8980582524272,5.56553398058253,1.48058252427185,12.5339805825243,8.68932038834952,16.378640776699,5.56553398058253,3.64320388349515,7.72815533980583,20.9441747572816,1.24029126213592,2.44174757281554,4.84466019417476,7.24757281553398,5.80582524271845,5.3252427184466,5.08495145631069,3.64320388349515,10.371359223301,3.88349514563107,4.36407766990291,2.92233009708739,3.1626213592233,4.84466019417476,3.1626213592233,14.9368932038835,7.24757281553398,5.80582524271845,2.92233009708739,3.88349514563107,1.48058252427185,10.8519417475728,8.44902912621359,13.0145631067961,4.12378640776699,8.92961165048544,1,5.56553398058253,4.60436893203884,7.96844660194175,6.28640776699029,9.6504854368932,4.84466019417476,9.41019417475728,5.08495145631069,2.68203883495146,4.36407766990291,13.0145631067961,27.6723300970874,6.04611650485438,5.56553398058253,11.0922330097087,19.7427184466019,5.56553398058253,6.28640776699029,8.68932038834952,14.6966019417476,13.495145631068,8.44902912621359,15.8980582524272,2.68203883495146,6.28640776699029,8.92961165048544,12.7742718446602,5.08495145631069,9.16990291262137,7.72815533980583,14.2160194174757,7.24757281553398,2.68203883495146,3.88349514563107,5.80582524271845,2.92233009708739,3.40291262135923,7.72815533980583,11.3325242718447,10.1310679611651,3.40291262135923,8.20873786407768,7.4878640776699,7.24757281553398,15.6577669902913,13.495145631068,15.4174757281553,9.89077669902913,16.8592233009709,6.04611650485438,2.20145631067961,4.12378640776699,1.48058252427185,1.48058252427185,8.20873786407768,10.1310679611651,3.40291262135923,10.371359223301,20.9441747572816,7.00728155339806,5.56553398058253,10.1310679611651,3.64320388349515,3.40291262135923,3.1626213592233,7.96844660194175,3.88349514563107,6.28640776699029,4.36407766990291,3.64320388349515,4.12378640776699,3.88349514563107,7.00728155339806,8.20873786407768,14.2160194174757,10.1310679611651,3.1626213592233,7.4878640776699,4.36407766990291,5.80582524271845,16.1383495145631,4.60436893203884,11.0922330097087,5.08495145631069,17.3398058252427,10.1310679611651,9.41019417475728,5.08495145631069,7.96844660194175,1.96116504854369,2.44174757281554,4.36407766990291,6.76699029126214,2.68203883495146,13.7354368932039,1,8.44902912621359,2.44174757281554,10.8519417475728,1.72087378640777,18.3009708737864,1.24029126213592,6.28640776699029,12.5339805825243,6.28640776699029,14.6966019417476,9.6504854368932,6.76699029126214,8.20873786407768,5.56553398058253,9.6504854368932,9.89077669902913,10.1310679611651,16.1383495145631,5.80582524271845,8.92961165048544,5.80582524271845,5.3252427184466,13.0145631067961,5.56553398058253,6.28640776699029,5.08495145631069,9.41019417475728,18.7815533980583,12.5339805825243,9.6504854368932,8.68932038834952,7.96844660194175,13.9757281553398,7.72815533980583,7.4878640776699,11.0922330097087,11.5728155339806,22.8665048543689,20.7038834951456,6.76699029126214,6.04611650485438,8.92961165048544,9.89077669902913,15.6577669902913,4.12378640776699,13.7354368932039,12.7742718446602,10.6116504854369,17.8203883495146,22.626213592233,10.6116504854369,13.0145631067961,4.12378640776699,4.36407766990291,8.68932038834952,11.3325242718447,9.41019417475728,5.56553398058253,16.378640776699,7.24757281553398,13.254854368932,8.68932038834952,9.41019417475728,6.52669902912622,12.2936893203884,16.618932038835,13.254854368932,19.0218446601942,26.7111650485437,30.5558252427184,19.7427184466019,21.4247572815534,31.2766990291262,10.6116504854369,11.3325242718447,10.6116504854369,12.7742718446602,15.4174757281553,13.7354368932039,12.2936893203884,11.8131067961165,23.1067961165049,23.8276699029126,8.20873786407768,22.8665048543689,15.8980582524272,23.5873786407767,12.2936893203884,28.1529126213592,18.3009708737864,1.24029126213592,3.64320388349515,8.68932038834952,1.24029126213592,11.0922330097087,6.76699029126214,9.6504854368932,6.76699029126214,10.6116504854369,11.8131067961165,12.2936893203884,35.3616504854369,24.3082524271845,20.9441747572816,13.0145631067961,17.3398058252427,18.3009708737864,15.4174757281553,16.618932038835,16.378640776699,10.1310679611651,21.1844660194175,9.41019417475728,7.72815533980583,15.6577669902913,8.44902912621359,24.7888349514563,21.4247572815534,22.3859223300971,23.1067961165049,21.6650485436893,28.3932038834951,21.4247572815534,19.2621359223301,23.8276699029126,17.8203883495146,19.0218446601942,14.4563106796117,15.8980582524272,19.7427184466019,17.5800970873786,20.2233009708738,15.8980582524272,12.0533980582524,12.7742718446602,11.5728155339806,11.8131067961165,11.5728155339806,27.4320388349515,13.495145631068,17.0995145631068,16.1383495145631,19.7427184466019,20.7038834951456,19.2621359223301,22.8665048543689,27.6723300970874,27.6723300970874,24.3082524271845,23.5873786407767,25.0291262135922,20.7038834951456,21.1844660194175,33.4393203883495,24.0679611650485,25.9902912621359,38.245145631068,30.3155339805825,34.4004854368932,25.2694174757282,8.44902912621359,22.626213592233,19.7427184466019,21.1844660194175,42.3300970873786,21.6650485436893,30.3155339805825,21.6650485436893,30.3155339805825,20.2233009708738,33.6796116504854,36.8033980582524,19.0218446601942,18.3009708737864,19.2621359223301,29.8349514563107,12.5339805825243,10.6116504854369,10.6116504854369,15.6577669902913,9.16990291262137,15.8980582524272,10.8519417475728,18.5412621359223,11.3325242718447,27.1917475728155,29.1140776699029,36.5631067961165,40.4077669902913,14.9368932038835,12.5339805825243,13.9757281553398,12.2936893203884,16.8592233009709,19.502427184466,11.8131067961165,28.6334951456311,16.1383495145631,19.7427184466019,17.3398058252427,22.8665048543689,22.626213592233,16.618932038835,17.5800970873786,29.1140776699029,26.7111650485437,30.0752427184466,23.8276699029126,26.9514563106796,23.8276699029126,31.997572815534,29.8349514563107,27.6723300970874,28.1529126213592,25.2694174757282,33.4393203883495,23.5873786407767,34.8810679611651,31.5169902912621,75.7305825242718,19.7427184466019,19.9830097087379,32.4781553398058,18.5412621359223,28.6334951456311,24.5485436893204,25.0291262135922,33.6796116504854,38.004854368932,29.5946601941748,29.3543689320388,36.3228155339806,39.4466019417476,29.8349514563107,28.3932038834951,36.3228155339806,39.9271844660194,37.7645631067961,49.7791262135922,50.7402912621359,46.1747572815534,51.2208737864078,49.5388349514563,47.6165048543689,49.0582524271845,48.5776699029126,60.8325242718447,42.5703883495146,56.0266990291262,53.623786407767,47.376213592233,51.9417475728155,52.9029126213592,46.8956310679612,41.6092233009709,37.5242718446602,45.6941747572816,38.245145631068,53.3834951456311,61.0728155339806,57.2281553398058,48.8179611650485,58.9101941747573,48.3373786407767,42.3300970873786,57.2281553398058,55.0655339805825,42.5703883495146,69.002427184466,48.0970873786408,58.6699029126214,51.2208737864078,45.9344660194175,52.1820388349515,61.3131067961165,57.2281553398058,63.7160194174757,69.9635922330097,57.7087378640777,66.3592233009709,75.7305825242718,76.2111650485437,76.2111650485437,69.002427184466,66.3592233009709,71.4053398058252,70.2038834951456,56.9878640776699,74.7694174757282,74.7694174757282,71.8859223300971,71.8859223300971,74.5291262135922,81.7378640776699,90.3883495145631,90.3883495145631,74.5291262135922,74.5291262135922,73.3276699029126,100,100,2.44174757281554,2.44174757281554,2.44174757281554,1.24029126213592,1.24029126213592,1.24029126213592,7.4878640776699,7.4878640776699,7.4878640776699,1.48058252427185,1.48058252427185,1.48058252427185],"color":["#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#CBD2FC","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F","#EE0F0F"],"title":["P00000142","P00002142","P00003242","P00003942","P00005042","P00010742","P00021742","P00025442","P00028842","P00030842","P00031042","P00034742","P00034842","P00036842","P00037142","P00044442","P00046742","P00051442","P00052842","P00057542","P00057642","P00057742","P00057942","P00058042","P00059442","P00062842","P00070042","P00071442","P00073842","P00080342","P00085942","P00086442","P00100442","P00100842","P00101842","P00102342","P00102642","P00105142","P00106042","P00110742","P00110842","P00110942","P00111142","P00111742","P00112142","P00112442","P00112542","P00113142","P00113242","P00113342","P00113642","P00114942","P00115642","P00116842","P00117442","P00117942","P00120042","P00121342","P00121642","P00125942","P00126142","P00127242","P00127642","P00127842","P00128942","P00129342","P00129542","P00129642","P00139942","P00140742","P00142142","P00144642","P00145042","P00145442","P00147942","P00148642","P00153742","P00154042","P00156442","P00161942","P00174442","P00178242","P00178942","P00182142","P00182242","P00183242","P00183342","P00184942","P00193542","P00199442","P00210042","P00212942","P00213242","P00216342","P00220342","P00220442","P00221442","P00233542","P00237542","P00240142","P00242742","P00243942","P00245642","P00248142","P00249642","P00250242","P00251242","P00255842","P00258742","P00259342","P00260042","P00265242","P00270942","P00271142","P00274942","P00277442","P00277642","P00278642","P00289942","P00293242","P00294542","P00296042","P00317842","P00318742","P00323942","P00324942","P00329542","P00334242","P00338442","P00346142","P00350942","P00355142","P0097242","<B>[1]<\/B><BR><B>{P00294542}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0518<BR>confidence = 0.5<BR>coverage = 0.104<BR>lift = 1.57<BR>count = 305<BR>order = 2<BR>id = 1","<B>[2]<\/B><BR><B>{P00317842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.443<BR>coverage = 0.128<BR>lift = 1.39<BR>count = 335<BR>order = 2<BR>id = 2","<B>[3]<\/B><BR><B>{P00346142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0526<BR>confidence = 0.523<BR>coverage = 0.101<BR>lift = 1.91<BR>count = 310<BR>order = 2<BR>id = 3","<B>[4]<\/B><BR><B>{P00350942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0506<BR>confidence = 0.405<BR>coverage = 0.125<BR>lift = 1.27<BR>count = 298<BR>order = 2<BR>id = 4","<B>[5]<\/B><BR><B>{P00293242}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.464<BR>coverage = 0.113<BR>lift = 1.45<BR>count = 309<BR>order = 2<BR>id = 5","<B>[6]<\/B><BR><B>{P00213242}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0536<BR>confidence = 0.497<BR>coverage = 0.108<BR>lift = 1.56<BR>count = 316<BR>order = 2<BR>id = 6","<B>[7]<\/B><BR><B>{P00101842}<\/B><BR>&nbsp;&nbsp; => <B>{P00028842}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.488<BR>coverage = 0.109<BR>lift = 2.4<BR>count = 314<BR>order = 2<BR>id = 7","<B>[8]<\/B><BR><B>{P00101842}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.467<BR>coverage = 0.109<BR>lift = 1.96<BR>count = 301<BR>order = 2<BR>id = 8","<B>[9]<\/B><BR><B>{P00101842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.46<BR>coverage = 0.109<BR>lift = 1.44<BR>count = 296<BR>order = 2<BR>id = 9","<B>[10]<\/B><BR><B>{P00296042}<\/B><BR>&nbsp;&nbsp; => <B>{P00255842}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.421<BR>coverage = 0.124<BR>lift = 1.79<BR>count = 308<BR>order = 2<BR>id = 10","<B>[11]<\/B><BR><B>{P00296042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0591<BR>confidence = 0.476<BR>coverage = 0.124<BR>lift = 1.49<BR>count = 348<BR>order = 2<BR>id = 11","<B>[12]<\/B><BR><B>{P00338442}<\/B><BR>&nbsp;&nbsp; => <B>{P00334242}<\/B><BR><BR>support = 0.0557<BR>confidence = 0.449<BR>coverage = 0.124<BR>lift = 2.42<BR>count = 328<BR>order = 2<BR>id = 12","<B>[13]<\/B><BR><B>{P00338442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0514<BR>confidence = 0.415<BR>coverage = 0.124<BR>lift = 1.3<BR>count = 303<BR>order = 2<BR>id = 13","<B>[14]<\/B><BR><B>{P00127842}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.494<BR>coverage = 0.103<BR>lift = 1.86<BR>count = 301<BR>order = 2<BR>id = 14","<B>[15]<\/B><BR><B>{P00127842}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0514<BR>confidence = 0.498<BR>coverage = 0.103<BR>lift = 1.81<BR>count = 303<BR>order = 2<BR>id = 15","<B>[16]<\/B><BR><B>{P00127842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.517<BR>coverage = 0.103<BR>lift = 1.89<BR>count = 315<BR>order = 2<BR>id = 16","<B>[17]<\/B><BR><B>{P00030842}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.519<BR>coverage = 0.1<BR>lift = 2.19<BR>count = 307<BR>order = 2<BR>id = 17","<B>[18]<\/B><BR><B>{P00030842}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.5<BR>coverage = 0.1<BR>lift = 2.05<BR>count = 296<BR>order = 2<BR>id = 18","<B>[19]<\/B><BR><B>{P00153742}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.485<BR>coverage = 0.105<BR>lift = 1.52<BR>count = 301<BR>order = 2<BR>id = 19","<B>[20]<\/B><BR><B>{P00126142}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.46<BR>coverage = 0.109<BR>lift = 1.93<BR>count = 296<BR>order = 2<BR>id = 20","<B>[21]<\/B><BR><B>{P00126142}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.462<BR>coverage = 0.109<BR>lift = 1.45<BR>count = 297<BR>order = 2<BR>id = 21","<B>[22]<\/B><BR><B>{P00250242}<\/B><BR>&nbsp;&nbsp; => <B>{P00248142}<\/B><BR><BR>support = 0.0526<BR>confidence = 0.404<BR>coverage = 0.13<BR>lift = 2.92<BR>count = 310<BR>order = 2<BR>id = 22","<B>[23]<\/B><BR><B>{P00250242}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0587<BR>confidence = 0.451<BR>coverage = 0.13<BR>lift = 1.41<BR>count = 346<BR>order = 2<BR>id = 23","<B>[24]<\/B><BR><B>{P00154042}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0543<BR>confidence = 0.514<BR>coverage = 0.106<BR>lift = 2.63<BR>count = 320<BR>order = 2<BR>id = 24","<B>[25]<\/B><BR><B>{P00154042}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.484<BR>coverage = 0.106<BR>lift = 1.98<BR>count = 301<BR>order = 2<BR>id = 25","<B>[26]<\/B><BR><B>{P00154042}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.506<BR>coverage = 0.106<BR>lift = 2.12<BR>count = 315<BR>order = 2<BR>id = 26","<B>[27]<\/B><BR><B>{P00129342}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.523<BR>coverage = 0.0961<BR>lift = 2.19<BR>count = 296<BR>order = 2<BR>id = 27","<B>[28]<\/B><BR><B>{P00355142}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.411<BR>coverage = 0.138<BR>lift = 1.74<BR>count = 333<BR>order = 2<BR>id = 28","<B>[29]<\/B><BR><B>{P00355142}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0572<BR>confidence = 0.416<BR>coverage = 0.138<BR>lift = 1.3<BR>count = 337<BR>order = 2<BR>id = 29","<B>[30]<\/B><BR><B>{P00127242}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0509<BR>confidence = 0.499<BR>coverage = 0.102<BR>lift = 2.65<BR>count = 300<BR>order = 2<BR>id = 30","<B>[31]<\/B><BR><B>{P00127242}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0508<BR>confidence = 0.498<BR>coverage = 0.102<BR>lift = 2.54<BR>count = 299<BR>order = 2<BR>id = 31","<B>[32]<\/B><BR><B>{P00220342}<\/B><BR>&nbsp;&nbsp; => <B>{P00278642}<\/B><BR><BR>support = 0.0508<BR>confidence = 0.428<BR>coverage = 0.118<BR>lift = 2.06<BR>count = 299<BR>order = 2<BR>id = 32","<B>[33]<\/B><BR><B>{P00220342}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0567<BR>confidence = 0.479<BR>coverage = 0.118<BR>lift = 1.5<BR>count = 334<BR>order = 2<BR>id = 33","<B>[34]<\/B><BR><B>{P00182242}<\/B><BR>&nbsp;&nbsp; => <B>{P00182142}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.511<BR>coverage = 0.0983<BR>lift = 3.31<BR>count = 296<BR>order = 2<BR>id = 34","<B>[35]<\/B><BR><B>{P00113342}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.439<BR>coverage = 0.115<BR>lift = 1.9<BR>count = 296<BR>order = 2<BR>id = 35","<B>[36]<\/B><BR><B>{P00113342}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0501<BR>confidence = 0.437<BR>coverage = 0.115<BR>lift = 1.79<BR>count = 295<BR>order = 2<BR>id = 36","<B>[37]<\/B><BR><B>{P00113342}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0508<BR>confidence = 0.443<BR>coverage = 0.115<BR>lift = 1.62<BR>count = 299<BR>order = 2<BR>id = 37","<B>[38]<\/B><BR><B>{P00113342}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.484<BR>coverage = 0.115<BR>lift = 1.77<BR>count = 327<BR>order = 2<BR>id = 38","<B>[39]<\/B><BR><B>{P00289942}<\/B><BR>&nbsp;&nbsp; => <B>{P00000142}<\/B><BR><BR>support = 0.0601<BR>confidence = 0.416<BR>coverage = 0.144<BR>lift = 2.13<BR>count = 354<BR>order = 2<BR>id = 39","<B>[40]<\/B><BR><B>{P00289942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0596<BR>confidence = 0.413<BR>coverage = 0.144<BR>lift = 1.29<BR>count = 351<BR>order = 2<BR>id = 40","<B>[41]<\/B><BR><B>{P00233542}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0528<BR>confidence = 0.489<BR>coverage = 0.108<BR>lift = 1.96<BR>count = 311<BR>order = 2<BR>id = 41","<B>[42]<\/B><BR><B>{P00233542}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.483<BR>coverage = 0.108<BR>lift = 1.76<BR>count = 307<BR>order = 2<BR>id = 42","<B>[43]<\/B><BR><B>{P00233542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.484<BR>coverage = 0.108<BR>lift = 1.77<BR>count = 308<BR>order = 2<BR>id = 43","<B>[44]<\/B><BR><B>{P00249642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.057<BR>confidence = 0.466<BR>coverage = 0.122<BR>lift = 1.46<BR>count = 336<BR>order = 2<BR>id = 44","<B>[45]<\/B><BR><B>{P00274942}<\/B><BR>&nbsp;&nbsp; => <B>{P00255842}<\/B><BR><BR>support = 0.0553<BR>confidence = 0.411<BR>coverage = 0.135<BR>lift = 1.75<BR>count = 326<BR>order = 2<BR>id = 45","<B>[46]<\/B><BR><B>{P00274942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0604<BR>confidence = 0.449<BR>coverage = 0.135<BR>lift = 1.41<BR>count = 356<BR>order = 2<BR>id = 46","<B>[47]<\/B><BR><B>{P00212942}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0509<BR>confidence = 0.477<BR>coverage = 0.107<BR>lift = 2.23<BR>count = 300<BR>order = 2<BR>id = 47","<B>[48]<\/B><BR><B>{P00212942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.472<BR>coverage = 0.107<BR>lift = 1.48<BR>count = 297<BR>order = 2<BR>id = 48","<B>[49]<\/B><BR><B>{P00115642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0519<BR>confidence = 0.427<BR>coverage = 0.122<BR>lift = 1.34<BR>count = 306<BR>order = 2<BR>id = 49","<B>[50]<\/B><BR><B>{P00121342}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.408<BR>coverage = 0.129<BR>lift = 1.49<BR>count = 309<BR>order = 2<BR>id = 50","<B>[51]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00073842}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.408<BR>coverage = 0.123<BR>lift = 2.57<BR>count = 296<BR>order = 2<BR>id = 51","<B>[52]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00242742}<\/B><BR><BR>support = 0.0552<BR>confidence = 0.448<BR>coverage = 0.123<BR>lift = 2.17<BR>count = 325<BR>order = 2<BR>id = 52","<B>[53]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0572<BR>confidence = 0.465<BR>coverage = 0.123<BR>lift = 2.38<BR>count = 337<BR>order = 2<BR>id = 53","<B>[54]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.451<BR>coverage = 0.123<BR>lift = 1.91<BR>count = 327<BR>order = 2<BR>id = 54","<B>[55]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0584<BR>confidence = 0.474<BR>coverage = 0.123<BR>lift = 1.94<BR>count = 344<BR>order = 2<BR>id = 55","<B>[56]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.451<BR>coverage = 0.123<BR>lift = 1.7<BR>count = 327<BR>order = 2<BR>id = 56","<B>[57]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0582<BR>confidence = 0.473<BR>coverage = 0.123<BR>lift = 1.9<BR>count = 343<BR>order = 2<BR>id = 57","<B>[58]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.056<BR>confidence = 0.455<BR>coverage = 0.123<BR>lift = 1.66<BR>count = 330<BR>order = 2<BR>id = 58","<B>[59]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0618<BR>confidence = 0.502<BR>coverage = 0.123<BR>lift = 2.1<BR>count = 364<BR>order = 2<BR>id = 59","<B>[60]<\/B><BR><B>{P00125942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.444<BR>coverage = 0.123<BR>lift = 1.62<BR>count = 322<BR>order = 2<BR>id = 60","<B>[61]<\/B><BR><B>{P00113642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0509<BR>confidence = 0.402<BR>coverage = 0.127<BR>lift = 1.26<BR>count = 300<BR>order = 2<BR>id = 61","<B>[62]<\/B><BR><B>{P00102342}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.449<BR>coverage = 0.116<BR>lift = 1.41<BR>count = 307<BR>order = 2<BR>id = 62","<B>[63]<\/B><BR><B>{P00248142}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0606<BR>confidence = 0.438<BR>coverage = 0.138<BR>lift = 1.37<BR>count = 357<BR>order = 2<BR>id = 63","<B>[64]<\/B><BR><B>{P00071442}<\/B><BR>&nbsp;&nbsp; => <B>{P00086442}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.426<BR>coverage = 0.125<BR>lift = 2.6<BR>count = 314<BR>order = 2<BR>id = 64","<B>[65]<\/B><BR><B>{P00071442}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.403<BR>coverage = 0.125<BR>lift = 1.52<BR>count = 297<BR>order = 2<BR>id = 65","<B>[66]<\/B><BR><B>{P00071442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0582<BR>confidence = 0.465<BR>coverage = 0.125<BR>lift = 1.7<BR>count = 343<BR>order = 2<BR>id = 66","<B>[67]<\/B><BR><B>{P00071442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.444<BR>coverage = 0.125<BR>lift = 1.62<BR>count = 327<BR>order = 2<BR>id = 67","<B>[68]<\/B><BR><B>{P00111742}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0609<BR>confidence = 0.521<BR>coverage = 0.117<BR>lift = 1.9<BR>count = 359<BR>order = 2<BR>id = 68","<B>[69]<\/B><BR><B>{P00111742}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.456<BR>coverage = 0.117<BR>lift = 1.67<BR>count = 314<BR>order = 2<BR>id = 69","<B>[70]<\/B><BR><B>{P00111742}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0519<BR>confidence = 0.444<BR>coverage = 0.117<BR>lift = 1.39<BR>count = 306<BR>order = 2<BR>id = 70","<B>[71]<\/B><BR><B>{P00021742}<\/B><BR>&nbsp;&nbsp; => <B>{P00278642}<\/B><BR><BR>support = 0.0548<BR>confidence = 0.416<BR>coverage = 0.132<BR>lift = 2<BR>count = 323<BR>order = 2<BR>id = 71","<B>[72]<\/B><BR><B>{P00021742}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0642<BR>confidence = 0.487<BR>coverage = 0.132<BR>lift = 1.53<BR>count = 378<BR>order = 2<BR>id = 72","<B>[73]<\/B><BR><B>{P00183342}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.471<BR>coverage = 0.107<BR>lift = 1.93<BR>count = 296<BR>order = 2<BR>id = 73","<B>[74]<\/B><BR><B>{P00183342}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.479<BR>coverage = 0.107<BR>lift = 2.01<BR>count = 301<BR>order = 2<BR>id = 74","<B>[75]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0528<BR>confidence = 0.473<BR>coverage = 0.112<BR>lift = 2.42<BR>count = 311<BR>order = 2<BR>id = 75","<B>[76]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0545<BR>confidence = 0.489<BR>coverage = 0.112<BR>lift = 2.44<BR>count = 321<BR>order = 2<BR>id = 76","<B>[77]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.479<BR>coverage = 0.112<BR>lift = 1.81<BR>count = 315<BR>order = 2<BR>id = 77","<B>[78]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0531<BR>confidence = 0.476<BR>coverage = 0.112<BR>lift = 1.91<BR>count = 313<BR>order = 2<BR>id = 78","<B>[79]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.053<BR>confidence = 0.475<BR>coverage = 0.112<BR>lift = 1.73<BR>count = 312<BR>order = 2<BR>id = 79","<B>[80]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0519<BR>confidence = 0.466<BR>coverage = 0.112<BR>lift = 1.95<BR>count = 306<BR>order = 2<BR>id = 80","<B>[81]<\/B><BR><B>{P00277442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0567<BR>confidence = 0.508<BR>coverage = 0.112<BR>lift = 1.86<BR>count = 334<BR>order = 2<BR>id = 81","<B>[82]<\/B><BR><B>{P00193542}<\/B><BR>&nbsp;&nbsp; => <B>{P00120042}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.499<BR>coverage = 0.104<BR>lift = 3.26<BR>count = 307<BR>order = 2<BR>id = 82","<B>[83]<\/B><BR><B>{P00193542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.502<BR>coverage = 0.104<BR>lift = 2.18<BR>count = 309<BR>order = 2<BR>id = 83","<B>[84]<\/B><BR><B>{P00193542}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0514<BR>confidence = 0.493<BR>coverage = 0.104<BR>lift = 1.86<BR>count = 303<BR>order = 2<BR>id = 84","<B>[85]<\/B><BR><B>{P00193542}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0516<BR>confidence = 0.494<BR>coverage = 0.104<BR>lift = 1.8<BR>count = 304<BR>order = 2<BR>id = 85","<B>[86]<\/B><BR><B>{P00193542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0528<BR>confidence = 0.506<BR>coverage = 0.104<BR>lift = 1.85<BR>count = 311<BR>order = 2<BR>id = 86","<B>[87]<\/B><BR><B>{P00178242}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0516<BR>confidence = 0.426<BR>coverage = 0.121<BR>lift = 1.56<BR>count = 304<BR>order = 2<BR>id = 87","<B>[88]<\/B><BR><B>{P00324942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0599<BR>confidence = 0.45<BR>coverage = 0.133<BR>lift = 1.41<BR>count = 353<BR>order = 2<BR>id = 88","<B>[89]<\/B><BR><B>{P00034842}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0545<BR>confidence = 0.471<BR>coverage = 0.116<BR>lift = 2.2<BR>count = 321<BR>order = 2<BR>id = 89","<B>[90]<\/B><BR><B>{P00034842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.462<BR>coverage = 0.116<BR>lift = 1.45<BR>count = 315<BR>order = 2<BR>id = 90","<B>[91]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00242742}<\/B><BR><BR>support = 0.0514<BR>confidence = 0.428<BR>coverage = 0.12<BR>lift = 2.08<BR>count = 303<BR>order = 2<BR>id = 91","<B>[92]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.434<BR>coverage = 0.12<BR>lift = 2.22<BR>count = 307<BR>order = 2<BR>id = 92","<B>[93]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.419<BR>coverage = 0.12<BR>lift = 1.77<BR>count = 297<BR>order = 2<BR>id = 93","<B>[94]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.057<BR>confidence = 0.475<BR>coverage = 0.12<BR>lift = 1.94<BR>count = 336<BR>order = 2<BR>id = 94","<B>[95]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0553<BR>confidence = 0.46<BR>coverage = 0.12<BR>lift = 1.85<BR>count = 326<BR>order = 2<BR>id = 95","<B>[96]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0586<BR>confidence = 0.487<BR>coverage = 0.12<BR>lift = 2.04<BR>count = 345<BR>order = 2<BR>id = 96","<B>[97]<\/B><BR><B>{P00142142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.435<BR>coverage = 0.12<BR>lift = 1.59<BR>count = 308<BR>order = 2<BR>id = 97","<B>[98]<\/B><BR><B>{P00057542}<\/B><BR>&nbsp;&nbsp; => <B>{P00000142}<\/B><BR><BR>support = 0.0557<BR>confidence = 0.449<BR>coverage = 0.124<BR>lift = 2.3<BR>count = 328<BR>order = 2<BR>id = 98","<B>[99]<\/B><BR><B>{P00057542}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0501<BR>confidence = 0.404<BR>coverage = 0.124<BR>lift = 1.89<BR>count = 295<BR>order = 2<BR>id = 99","<B>[100]<\/B><BR><B>{P00057542}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.43<BR>coverage = 0.124<BR>lift = 1.8<BR>count = 314<BR>order = 2<BR>id = 100","<B>[101]<\/B><BR><B>{P00139942}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0526<BR>confidence = 0.47<BR>coverage = 0.112<BR>lift = 2.2<BR>count = 310<BR>order = 2<BR>id = 101","<B>[102]<\/B><BR><B>{P00139942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.055<BR>confidence = 0.491<BR>coverage = 0.112<BR>lift = 2.06<BR>count = 324<BR>order = 2<BR>id = 102","<B>[103]<\/B><BR><B>{P00245642}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.459<BR>coverage = 0.117<BR>lift = 1.67<BR>count = 317<BR>order = 2<BR>id = 103","<B>[104]<\/B><BR><B>{P00174442}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0562<BR>confidence = 0.455<BR>coverage = 0.124<BR>lift = 2.13<BR>count = 331<BR>order = 2<BR>id = 104","<B>[105]<\/B><BR><B>{P00174442}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0528<BR>confidence = 0.427<BR>coverage = 0.124<BR>lift = 1.79<BR>count = 311<BR>order = 2<BR>id = 105","<B>[106]<\/B><BR><B>{P00174442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.056<BR>confidence = 0.453<BR>coverage = 0.124<BR>lift = 1.42<BR>count = 330<BR>order = 2<BR>id = 106","<B>[107]<\/B><BR><B>{P00260042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.053<BR>confidence = 0.414<BR>coverage = 0.128<BR>lift = 1.9<BR>count = 312<BR>order = 2<BR>id = 107","<B>[108]<\/B><BR><B>{P00260042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0513<BR>confidence = 0.401<BR>coverage = 0.128<BR>lift = 1.46<BR>count = 302<BR>order = 2<BR>id = 108","<B>[109]<\/B><BR><B>{P00260042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.41<BR>coverage = 0.128<BR>lift = 1.28<BR>count = 309<BR>order = 2<BR>id = 109","<B>[110]<\/B><BR><B>{P00240142}<\/B><BR>&nbsp;&nbsp; => <B>{P00278642}<\/B><BR><BR>support = 0.0586<BR>confidence = 0.423<BR>coverage = 0.139<BR>lift = 2.03<BR>count = 345<BR>order = 2<BR>id = 110","<B>[111]<\/B><BR><B>{P00240142}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0689<BR>confidence = 0.498<BR>coverage = 0.139<BR>lift = 1.56<BR>count = 406<BR>order = 2<BR>id = 111","<B>[112]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00111142}<\/B><BR><BR>support = 0.0536<BR>confidence = 0.446<BR>coverage = 0.12<BR>lift = 2.59<BR>count = 316<BR>order = 2<BR>id = 112","<B>[113]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00112542}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.444<BR>coverage = 0.12<BR>lift = 2.31<BR>count = 314<BR>order = 2<BR>id = 113","<B>[114]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0572<BR>confidence = 0.476<BR>coverage = 0.12<BR>lift = 2.43<BR>count = 337<BR>order = 2<BR>id = 114","<B>[115]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.527<BR>coverage = 0.12<BR>lift = 2.63<BR>count = 373<BR>order = 2<BR>id = 115","<B>[116]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.444<BR>coverage = 0.12<BR>lift = 1.87<BR>count = 314<BR>order = 2<BR>id = 116","<B>[117]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.448<BR>coverage = 0.12<BR>lift = 1.83<BR>count = 317<BR>order = 2<BR>id = 117","<B>[118]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.462<BR>coverage = 0.12<BR>lift = 1.74<BR>count = 327<BR>order = 2<BR>id = 118","<B>[119]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0598<BR>confidence = 0.497<BR>coverage = 0.12<BR>lift = 1.99<BR>count = 352<BR>order = 2<BR>id = 119","<B>[120]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0589<BR>confidence = 0.49<BR>coverage = 0.12<BR>lift = 1.79<BR>count = 347<BR>order = 2<BR>id = 120","<B>[121]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0553<BR>confidence = 0.46<BR>coverage = 0.12<BR>lift = 1.93<BR>count = 326<BR>order = 2<BR>id = 121","<B>[122]<\/B><BR><B>{P00329542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0606<BR>confidence = 0.504<BR>coverage = 0.12<BR>lift = 1.84<BR>count = 357<BR>order = 2<BR>id = 122","<B>[123]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00102642}<\/B><BR><BR>support = 0.0513<BR>confidence = 0.403<BR>coverage = 0.127<BR>lift = 1.9<BR>count = 302<BR>order = 2<BR>id = 123","<B>[124]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.423<BR>coverage = 0.127<BR>lift = 2.16<BR>count = 317<BR>order = 2<BR>id = 124","<B>[125]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0557<BR>confidence = 0.437<BR>coverage = 0.127<BR>lift = 1.85<BR>count = 328<BR>order = 2<BR>id = 125","<B>[126]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0584<BR>confidence = 0.459<BR>coverage = 0.127<BR>lift = 1.88<BR>count = 344<BR>order = 2<BR>id = 126","<B>[127]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.053<BR>confidence = 0.416<BR>coverage = 0.127<BR>lift = 1.57<BR>count = 312<BR>order = 2<BR>id = 127","<B>[128]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0558<BR>confidence = 0.439<BR>coverage = 0.127<BR>lift = 1.76<BR>count = 329<BR>order = 2<BR>id = 128","<B>[129]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0548<BR>confidence = 0.431<BR>coverage = 0.127<BR>lift = 1.57<BR>count = 323<BR>order = 2<BR>id = 129","<B>[130]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0594<BR>confidence = 0.467<BR>coverage = 0.127<BR>lift = 1.96<BR>count = 350<BR>order = 2<BR>id = 130","<B>[131]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0545<BR>confidence = 0.428<BR>coverage = 0.127<BR>lift = 1.56<BR>count = 321<BR>order = 2<BR>id = 131","<B>[132]<\/B><BR><B>{P00145442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0513<BR>confidence = 0.403<BR>coverage = 0.127<BR>lift = 1.26<BR>count = 302<BR>order = 2<BR>id = 132","<B>[133]<\/B><BR><B>{P00002142}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.418<BR>coverage = 0.125<BR>lift = 1.71<BR>count = 307<BR>order = 2<BR>id = 133","<B>[134]<\/B><BR><B>{P00002142}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.429<BR>coverage = 0.125<BR>lift = 1.8<BR>count = 315<BR>order = 2<BR>id = 134","<B>[135]<\/B><BR><B>{P00002142}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0514<BR>confidence = 0.412<BR>coverage = 0.125<BR>lift = 1.29<BR>count = 303<BR>order = 2<BR>id = 135","<B>[136]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00111142}<\/B><BR><BR>support = 0.0518<BR>confidence = 0.4<BR>coverage = 0.129<BR>lift = 2.32<BR>count = 305<BR>order = 2<BR>id = 136","<B>[137]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00112542}<\/B><BR><BR>support = 0.0548<BR>confidence = 0.424<BR>coverage = 0.129<BR>lift = 2.21<BR>count = 323<BR>order = 2<BR>id = 137","<B>[138]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0574<BR>confidence = 0.444<BR>coverage = 0.129<BR>lift = 2.27<BR>count = 338<BR>order = 2<BR>id = 138","<B>[139]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.437<BR>coverage = 0.129<BR>lift = 2.19<BR>count = 333<BR>order = 2<BR>id = 139","<B>[140]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0518<BR>confidence = 0.4<BR>coverage = 0.129<BR>lift = 1.73<BR>count = 305<BR>order = 2<BR>id = 140","<B>[141]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0552<BR>confidence = 0.427<BR>coverage = 0.129<BR>lift = 1.8<BR>count = 325<BR>order = 2<BR>id = 141","<B>[142]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.423<BR>coverage = 0.129<BR>lift = 1.83<BR>count = 322<BR>order = 2<BR>id = 142","<B>[143]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0545<BR>confidence = 0.421<BR>coverage = 0.129<BR>lift = 1.73<BR>count = 321<BR>order = 2<BR>id = 143","<B>[144]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0604<BR>confidence = 0.467<BR>coverage = 0.129<BR>lift = 1.76<BR>count = 356<BR>order = 2<BR>id = 144","<B>[145]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0589<BR>confidence = 0.455<BR>coverage = 0.129<BR>lift = 1.82<BR>count = 347<BR>order = 2<BR>id = 145","<B>[146]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0603<BR>confidence = 0.466<BR>coverage = 0.129<BR>lift = 1.7<BR>count = 355<BR>order = 2<BR>id = 146","<B>[147]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0564<BR>confidence = 0.436<BR>coverage = 0.129<BR>lift = 1.83<BR>count = 332<BR>order = 2<BR>id = 147","<B>[148]<\/B><BR><B>{P00127642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0613<BR>confidence = 0.474<BR>coverage = 0.129<BR>lift = 1.73<BR>count = 361<BR>order = 2<BR>id = 148","<B>[149]<\/B><BR><B>{P00183242}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0536<BR>confidence = 0.487<BR>coverage = 0.11<BR>lift = 2.1<BR>count = 316<BR>order = 2<BR>id = 149","<B>[150]<\/B><BR><B>{P00183242}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0509<BR>confidence = 0.462<BR>coverage = 0.11<BR>lift = 1.89<BR>count = 300<BR>order = 2<BR>id = 150","<B>[151]<\/B><BR><B>{P00183242}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.475<BR>coverage = 0.11<BR>lift = 1.79<BR>count = 308<BR>order = 2<BR>id = 151","<B>[152]<\/B><BR><B>{P00183242}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.458<BR>coverage = 0.11<BR>lift = 1.67<BR>count = 297<BR>order = 2<BR>id = 152","<B>[153]<\/B><BR><B>{P00183242}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.458<BR>coverage = 0.11<BR>lift = 1.92<BR>count = 297<BR>order = 2<BR>id = 153","<B>[154]<\/B><BR><B>{P00183242}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0552<BR>confidence = 0.501<BR>coverage = 0.11<BR>lift = 1.83<BR>count = 325<BR>order = 2<BR>id = 154","<B>[155]<\/B><BR><B>{P00003942}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.445<BR>coverage = 0.127<BR>lift = 2.08<BR>count = 333<BR>order = 2<BR>id = 155","<B>[156]<\/B><BR><B>{P00003942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0518<BR>confidence = 0.407<BR>coverage = 0.127<BR>lift = 1.71<BR>count = 305<BR>order = 2<BR>id = 156","<B>[157]<\/B><BR><B>{P00003942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0567<BR>confidence = 0.446<BR>coverage = 0.127<BR>lift = 1.4<BR>count = 334<BR>order = 2<BR>id = 157","<B>[158]<\/B><BR><B>{P00216342}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0642<BR>confidence = 0.401<BR>coverage = 0.16<BR>lift = 1.26<BR>count = 378<BR>order = 2<BR>id = 158","<B>[159]<\/B><BR><B>{P00210042}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0543<BR>confidence = 0.408<BR>coverage = 0.133<BR>lift = 1.69<BR>count = 320<BR>order = 2<BR>id = 159","<B>[160]<\/B><BR><B>{P00210042}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.401<BR>coverage = 0.133<BR>lift = 1.61<BR>count = 314<BR>order = 2<BR>id = 160","<B>[161]<\/B><BR><B>{P00210042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.425<BR>coverage = 0.133<BR>lift = 1.33<BR>count = 333<BR>order = 2<BR>id = 161","<B>[162]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00010742}<\/B><BR><BR>support = 0.0519<BR>confidence = 0.403<BR>coverage = 0.129<BR>lift = 1.76<BR>count = 306<BR>order = 2<BR>id = 162","<B>[163]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.0518<BR>confidence = 0.402<BR>coverage = 0.129<BR>lift = 1.68<BR>count = 305<BR>order = 2<BR>id = 163","<B>[164]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0516<BR>confidence = 0.401<BR>coverage = 0.129<BR>lift = 1.69<BR>count = 304<BR>order = 2<BR>id = 164","<B>[165]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.055<BR>confidence = 0.427<BR>coverage = 0.129<BR>lift = 1.75<BR>count = 324<BR>order = 2<BR>id = 165","<B>[166]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.404<BR>coverage = 0.129<BR>lift = 1.68<BR>count = 307<BR>order = 2<BR>id = 166","<B>[167]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.418<BR>coverage = 0.129<BR>lift = 1.53<BR>count = 317<BR>order = 2<BR>id = 167","<B>[168]<\/B><BR><B>{P00318742}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.407<BR>coverage = 0.129<BR>lift = 1.28<BR>count = 309<BR>order = 2<BR>id = 168","<B>[169]<\/B><BR><B>{P00129542}<\/B><BR>&nbsp;&nbsp; => <B>{P00034742}<\/B><BR><BR>support = 0.0519<BR>confidence = 0.407<BR>coverage = 0.128<BR>lift = 1.98<BR>count = 306<BR>order = 2<BR>id = 169","<B>[170]<\/B><BR><B>{P00129542}<\/B><BR>&nbsp;&nbsp; => <B>{P00255842}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.41<BR>coverage = 0.128<BR>lift = 1.74<BR>count = 308<BR>order = 2<BR>id = 170","<B>[171]<\/B><BR><B>{P00129542}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0521<BR>confidence = 0.408<BR>coverage = 0.128<BR>lift = 1.91<BR>count = 307<BR>order = 2<BR>id = 171","<B>[172]<\/B><BR><B>{P00129542}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0543<BR>confidence = 0.426<BR>coverage = 0.128<BR>lift = 1.76<BR>count = 320<BR>order = 2<BR>id = 172","<B>[173]<\/B><BR><B>{P00129542}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0552<BR>confidence = 0.432<BR>coverage = 0.128<BR>lift = 1.35<BR>count = 325<BR>order = 2<BR>id = 173","<B>[174]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0594<BR>confidence = 0.471<BR>coverage = 0.126<BR>lift = 2.5<BR>count = 350<BR>order = 2<BR>id = 174","<B>[175]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.448<BR>coverage = 0.126<BR>lift = 2.29<BR>count = 333<BR>order = 2<BR>id = 175","<B>[176]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00010742}<\/B><BR><BR>support = 0.0516<BR>confidence = 0.409<BR>coverage = 0.126<BR>lift = 1.79<BR>count = 304<BR>order = 2<BR>id = 176","<B>[177]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.433<BR>coverage = 0.126<BR>lift = 1.83<BR>count = 322<BR>order = 2<BR>id = 177","<B>[178]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.416<BR>coverage = 0.126<BR>lift = 1.8<BR>count = 309<BR>order = 2<BR>id = 178","<B>[179]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.424<BR>coverage = 0.126<BR>lift = 1.73<BR>count = 315<BR>order = 2<BR>id = 179","<B>[180]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0608<BR>confidence = 0.482<BR>coverage = 0.126<BR>lift = 1.97<BR>count = 358<BR>order = 2<BR>id = 180","<B>[181]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0526<BR>confidence = 0.417<BR>coverage = 0.126<BR>lift = 1.57<BR>count = 310<BR>order = 2<BR>id = 181","<B>[182]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0572<BR>confidence = 0.454<BR>coverage = 0.126<BR>lift = 1.82<BR>count = 337<BR>order = 2<BR>id = 182","<B>[183]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.053<BR>confidence = 0.42<BR>coverage = 0.126<BR>lift = 1.53<BR>count = 312<BR>order = 2<BR>id = 183","<B>[184]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0616<BR>confidence = 0.489<BR>coverage = 0.126<BR>lift = 2.05<BR>count = 363<BR>order = 2<BR>id = 184","<B>[185]<\/B><BR><B>{P00070042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.448<BR>coverage = 0.126<BR>lift = 1.64<BR>count = 333<BR>order = 2<BR>id = 185","<B>[186]<\/B><BR><B>{P00100842}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.056<BR>confidence = 0.457<BR>coverage = 0.123<BR>lift = 1.97<BR>count = 330<BR>order = 2<BR>id = 186","<B>[187]<\/B><BR><B>{P00100842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.053<BR>confidence = 0.432<BR>coverage = 0.123<BR>lift = 1.58<BR>count = 312<BR>order = 2<BR>id = 187","<B>[188]<\/B><BR><B>{P00100842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.055<BR>confidence = 0.449<BR>coverage = 0.123<BR>lift = 1.41<BR>count = 324<BR>order = 2<BR>id = 188","<B>[189]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00102642}<\/B><BR><BR>support = 0.0508<BR>confidence = 0.41<BR>coverage = 0.124<BR>lift = 1.94<BR>count = 299<BR>order = 2<BR>id = 189","<B>[190]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.413<BR>coverage = 0.124<BR>lift = 1.9<BR>count = 301<BR>order = 2<BR>id = 190","<B>[191]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.424<BR>coverage = 0.124<BR>lift = 1.83<BR>count = 309<BR>order = 2<BR>id = 191","<B>[192]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0542<BR>confidence = 0.438<BR>coverage = 0.124<BR>lift = 1.65<BR>count = 319<BR>order = 2<BR>id = 192","<B>[193]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0513<BR>confidence = 0.414<BR>coverage = 0.124<BR>lift = 1.66<BR>count = 302<BR>order = 2<BR>id = 193","<B>[194]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0591<BR>confidence = 0.477<BR>coverage = 0.124<BR>lift = 1.74<BR>count = 348<BR>order = 2<BR>id = 194","<B>[195]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0501<BR>confidence = 0.405<BR>coverage = 0.124<BR>lift = 1.7<BR>count = 295<BR>order = 2<BR>id = 195","<B>[196]<\/B><BR><B>{P00100442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0553<BR>confidence = 0.447<BR>coverage = 0.124<BR>lift = 1.63<BR>count = 326<BR>order = 2<BR>id = 196","<B>[197]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.408<BR>coverage = 0.125<BR>lift = 2.16<BR>count = 301<BR>order = 2<BR>id = 197","<B>[198]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.057<BR>confidence = 0.455<BR>coverage = 0.125<BR>lift = 2.33<BR>count = 336<BR>order = 2<BR>id = 198","<B>[199]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0506<BR>confidence = 0.404<BR>coverage = 0.125<BR>lift = 2.02<BR>count = 298<BR>order = 2<BR>id = 199","<B>[200]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0623<BR>confidence = 0.497<BR>coverage = 0.125<BR>lift = 2.1<BR>count = 367<BR>order = 2<BR>id = 200","<B>[201]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.401<BR>coverage = 0.125<BR>lift = 1.64<BR>count = 296<BR>order = 2<BR>id = 201","<B>[202]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.43<BR>coverage = 0.125<BR>lift = 1.76<BR>count = 317<BR>order = 2<BR>id = 202","<B>[203]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0582<BR>confidence = 0.465<BR>coverage = 0.125<BR>lift = 1.86<BR>count = 343<BR>order = 2<BR>id = 203","<B>[204]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.43<BR>coverage = 0.125<BR>lift = 1.57<BR>count = 317<BR>order = 2<BR>id = 204","<B>[205]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0598<BR>confidence = 0.477<BR>coverage = 0.125<BR>lift = 2<BR>count = 352<BR>order = 2<BR>id = 205","<B>[206]<\/B><BR><B>{P00144642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0562<BR>confidence = 0.449<BR>coverage = 0.125<BR>lift = 1.64<BR>count = 331<BR>order = 2<BR>id = 206","<B>[207]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0542<BR>confidence = 0.464<BR>coverage = 0.117<BR>lift = 2.37<BR>count = 319<BR>order = 2<BR>id = 207","<B>[208]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0552<BR>confidence = 0.472<BR>coverage = 0.117<BR>lift = 2.36<BR>count = 325<BR>order = 2<BR>id = 208","<B>[209]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.456<BR>coverage = 0.117<BR>lift = 1.98<BR>count = 314<BR>order = 2<BR>id = 209","<B>[210]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0562<BR>confidence = 0.481<BR>coverage = 0.117<BR>lift = 1.97<BR>count = 331<BR>order = 2<BR>id = 210","<B>[211]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0564<BR>confidence = 0.483<BR>coverage = 0.117<BR>lift = 1.82<BR>count = 332<BR>order = 2<BR>id = 211","<B>[212]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.484<BR>coverage = 0.117<BR>lift = 1.94<BR>count = 333<BR>order = 2<BR>id = 212","<B>[213]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0608<BR>confidence = 0.52<BR>coverage = 0.117<BR>lift = 1.9<BR>count = 358<BR>order = 2<BR>id = 213","<B>[214]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.458<BR>coverage = 0.117<BR>lift = 1.92<BR>count = 315<BR>order = 2<BR>id = 214","<B>[215]<\/B><BR><B>{P00243942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0557<BR>confidence = 0.477<BR>coverage = 0.117<BR>lift = 1.74<BR>count = 328<BR>order = 2<BR>id = 215","<B>[216]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00251242}<\/B><BR><BR>support = 0.0535<BR>confidence = 0.408<BR>coverage = 0.131<BR>lift = 2.02<BR>count = 315<BR>order = 2<BR>id = 216","<B>[217]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0531<BR>confidence = 0.405<BR>coverage = 0.131<BR>lift = 1.86<BR>count = 313<BR>order = 2<BR>id = 217","<B>[218]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0586<BR>confidence = 0.446<BR>coverage = 0.131<BR>lift = 1.93<BR>count = 345<BR>order = 2<BR>id = 218","<B>[219]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.406<BR>coverage = 0.131<BR>lift = 1.76<BR>count = 314<BR>order = 2<BR>id = 219","<B>[220]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0538<BR>confidence = 0.41<BR>coverage = 0.131<BR>lift = 1.92<BR>count = 317<BR>order = 2<BR>id = 220","<B>[221]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.053<BR>confidence = 0.404<BR>coverage = 0.131<BR>lift = 1.48<BR>count = 312<BR>order = 2<BR>id = 221","<B>[222]<\/B><BR><B>{P00129642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.056<BR>confidence = 0.427<BR>coverage = 0.131<BR>lift = 1.34<BR>count = 330<BR>order = 2<BR>id = 222","<B>[223]<\/B><BR><B>{P00161942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0626<BR>confidence = 0.462<BR>coverage = 0.136<BR>lift = 1.45<BR>count = 369<BR>order = 2<BR>id = 223","<B>[224]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00086442}<\/B><BR><BR>support = 0.0582<BR>confidence = 0.429<BR>coverage = 0.136<BR>lift = 2.61<BR>count = 343<BR>order = 2<BR>id = 224","<B>[225]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00052842}<\/B><BR><BR>support = 0.0562<BR>confidence = 0.414<BR>coverage = 0.136<BR>lift = 2.48<BR>count = 331<BR>order = 2<BR>id = 225","<B>[226]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112542}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.409<BR>coverage = 0.136<BR>lift = 2.13<BR>count = 327<BR>order = 2<BR>id = 226","<B>[227]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00102642}<\/B><BR><BR>support = 0.055<BR>confidence = 0.405<BR>coverage = 0.136<BR>lift = 1.91<BR>count = 324<BR>order = 2<BR>id = 227","<B>[228]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0592<BR>confidence = 0.436<BR>coverage = 0.136<BR>lift = 1.88<BR>count = 349<BR>order = 2<BR>id = 228","<B>[229]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0548<BR>confidence = 0.404<BR>coverage = 0.136<BR>lift = 1.71<BR>count = 323<BR>order = 2<BR>id = 229","<B>[230]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.402<BR>coverage = 0.136<BR>lift = 1.65<BR>count = 322<BR>order = 2<BR>id = 230","<B>[231]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0572<BR>confidence = 0.421<BR>coverage = 0.136<BR>lift = 1.59<BR>count = 337<BR>order = 2<BR>id = 231","<B>[232]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0575<BR>confidence = 0.424<BR>coverage = 0.136<BR>lift = 1.7<BR>count = 339<BR>order = 2<BR>id = 232","<B>[233]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0655<BR>confidence = 0.482<BR>coverage = 0.136<BR>lift = 1.76<BR>count = 386<BR>order = 2<BR>id = 233","<B>[234]<\/B><BR><B>{P00323942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.064<BR>confidence = 0.471<BR>coverage = 0.136<BR>lift = 1.72<BR>count = 377<BR>order = 2<BR>id = 234","<B>[235]<\/B><BR><B>{P00037142}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0542<BR>confidence = 0.424<BR>coverage = 0.128<BR>lift = 2.17<BR>count = 319<BR>order = 2<BR>id = 235","<B>[236]<\/B><BR><B>{P00037142}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0536<BR>confidence = 0.42<BR>coverage = 0.128<BR>lift = 1.96<BR>count = 316<BR>order = 2<BR>id = 236","<B>[237]<\/B><BR><B>{P00037142}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0557<BR>confidence = 0.436<BR>coverage = 0.128<BR>lift = 1.78<BR>count = 328<BR>order = 2<BR>id = 237","<B>[238]<\/B><BR><B>{P00037142}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0564<BR>confidence = 0.441<BR>coverage = 0.128<BR>lift = 1.77<BR>count = 332<BR>order = 2<BR>id = 238","<B>[239]<\/B><BR><B>{P00037142}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0604<BR>confidence = 0.473<BR>coverage = 0.128<BR>lift = 1.98<BR>count = 356<BR>order = 2<BR>id = 239","<B>[240]<\/B><BR><B>{P00037142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.409<BR>coverage = 0.128<BR>lift = 1.49<BR>count = 308<BR>order = 2<BR>id = 240","<B>[241]<\/B><BR><B>{P00156442}<\/B><BR>&nbsp;&nbsp; => <B>{P00031042}<\/B><BR><BR>support = 0.0591<BR>confidence = 0.417<BR>coverage = 0.142<BR>lift = 2.05<BR>count = 348<BR>order = 2<BR>id = 241","<B>[242]<\/B><BR><B>{P00156442}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.0584<BR>confidence = 0.412<BR>coverage = 0.142<BR>lift = 1.73<BR>count = 344<BR>order = 2<BR>id = 242","<B>[243]<\/B><BR><B>{P00156442}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.401<BR>coverage = 0.142<BR>lift = 1.64<BR>count = 335<BR>order = 2<BR>id = 243","<B>[244]<\/B><BR><B>{P00156442}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.062<BR>confidence = 0.437<BR>coverage = 0.142<BR>lift = 1.81<BR>count = 365<BR>order = 2<BR>id = 244","<B>[245]<\/B><BR><B>{P00156442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0654<BR>confidence = 0.461<BR>coverage = 0.142<BR>lift = 1.44<BR>count = 385<BR>order = 2<BR>id = 245","<B>[246]<\/B><BR><B>{P00178942}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.401<BR>coverage = 0.142<BR>lift = 1.73<BR>count = 335<BR>order = 2<BR>id = 246","<B>[247]<\/B><BR><B>{P00178942}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0586<BR>confidence = 0.413<BR>coverage = 0.142<BR>lift = 1.29<BR>count = 345<BR>order = 2<BR>id = 247","<B>[248]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0523<BR>confidence = 0.406<BR>coverage = 0.129<BR>lift = 2.03<BR>count = 308<BR>order = 2<BR>id = 248","<B>[249]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0525<BR>confidence = 0.407<BR>coverage = 0.129<BR>lift = 1.87<BR>count = 309<BR>order = 2<BR>id = 249","<B>[250]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.431<BR>coverage = 0.129<BR>lift = 1.86<BR>count = 327<BR>order = 2<BR>id = 250","<B>[251]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0574<BR>confidence = 0.445<BR>coverage = 0.129<BR>lift = 1.93<BR>count = 338<BR>order = 2<BR>id = 251","<B>[252]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.056<BR>confidence = 0.435<BR>coverage = 0.129<BR>lift = 1.64<BR>count = 330<BR>order = 2<BR>id = 252","<B>[253]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0533<BR>confidence = 0.414<BR>coverage = 0.129<BR>lift = 1.66<BR>count = 314<BR>order = 2<BR>id = 253","<B>[254]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0609<BR>confidence = 0.473<BR>coverage = 0.129<BR>lift = 1.73<BR>count = 359<BR>order = 2<BR>id = 254","<B>[255]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0545<BR>confidence = 0.423<BR>coverage = 0.129<BR>lift = 1.77<BR>count = 321<BR>order = 2<BR>id = 255","<B>[256]<\/B><BR><B>{P00113142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0587<BR>confidence = 0.456<BR>coverage = 0.129<BR>lift = 1.67<BR>count = 346<BR>order = 2<BR>id = 256","<B>[257]<\/B><BR><B>{P00199442}<\/B><BR>&nbsp;&nbsp; => <B>{P00102642}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.412<BR>coverage = 0.135<BR>lift = 1.95<BR>count = 327<BR>order = 2<BR>id = 257","<B>[258]<\/B><BR><B>{P00199442}<\/B><BR>&nbsp;&nbsp; => <B>{P00251242}<\/B><BR><BR>support = 0.056<BR>confidence = 0.416<BR>coverage = 0.135<BR>lift = 2.06<BR>count = 330<BR>order = 2<BR>id = 258","<B>[259]<\/B><BR><B>{P00199442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.054<BR>confidence = 0.401<BR>coverage = 0.135<BR>lift = 1.84<BR>count = 318<BR>order = 2<BR>id = 259","<B>[260]<\/B><BR><B>{P00199442}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0581<BR>confidence = 0.431<BR>coverage = 0.135<BR>lift = 1.86<BR>count = 342<BR>order = 2<BR>id = 260","<B>[261]<\/B><BR><B>{P00199442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0611<BR>confidence = 0.453<BR>coverage = 0.135<BR>lift = 1.65<BR>count = 360<BR>order = 2<BR>id = 261","<B>[262]<\/B><BR><B>{P00199442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0587<BR>confidence = 0.436<BR>coverage = 0.135<BR>lift = 1.59<BR>count = 346<BR>order = 2<BR>id = 262","<B>[263]<\/B><BR><B>{P00112442}<\/B><BR>&nbsp;&nbsp; => <B>{P00031042}<\/B><BR><BR>support = 0.0628<BR>confidence = 0.401<BR>coverage = 0.157<BR>lift = 1.97<BR>count = 370<BR>order = 2<BR>id = 263","<B>[264]<\/B><BR><B>{P00112442}<\/B><BR>&nbsp;&nbsp; => <B>{P00028842}<\/B><BR><BR>support = 0.0682<BR>confidence = 0.436<BR>coverage = 0.157<BR>lift = 2.14<BR>count = 402<BR>order = 2<BR>id = 264","<B>[265]<\/B><BR><B>{P00112442}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.071<BR>confidence = 0.453<BR>coverage = 0.157<BR>lift = 1.9<BR>count = 418<BR>order = 2<BR>id = 265","<B>[266]<\/B><BR><B>{P00112442}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.404<BR>coverage = 0.157<BR>lift = 1.65<BR>count = 373<BR>order = 2<BR>id = 266","<B>[267]<\/B><BR><B>{P00112442}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0645<BR>confidence = 0.412<BR>coverage = 0.157<BR>lift = 1.71<BR>count = 380<BR>order = 2<BR>id = 267","<B>[268]<\/B><BR><B>{P00112442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0715<BR>confidence = 0.456<BR>coverage = 0.157<BR>lift = 1.43<BR>count = 421<BR>order = 2<BR>id = 268","<B>[269]<\/B><BR><B>{P00057742}<\/B><BR>&nbsp;&nbsp; => <B>{P00010742}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.407<BR>coverage = 0.14<BR>lift = 1.78<BR>count = 335<BR>order = 2<BR>id = 269","<B>[270]<\/B><BR><B>{P00057742}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0574<BR>confidence = 0.411<BR>coverage = 0.14<BR>lift = 1.68<BR>count = 338<BR>order = 2<BR>id = 270","<B>[271]<\/B><BR><B>{P00057742}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.407<BR>coverage = 0.14<BR>lift = 1.69<BR>count = 335<BR>order = 2<BR>id = 271","<B>[272]<\/B><BR><B>{P00057742}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0584<BR>confidence = 0.418<BR>coverage = 0.14<BR>lift = 1.58<BR>count = 344<BR>order = 2<BR>id = 272","<B>[273]<\/B><BR><B>{P00057742}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0603<BR>confidence = 0.431<BR>coverage = 0.14<BR>lift = 1.81<BR>count = 355<BR>order = 2<BR>id = 273","<B>[274]<\/B><BR><B>{P00057742}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0591<BR>confidence = 0.423<BR>coverage = 0.14<BR>lift = 1.55<BR>count = 348<BR>order = 2<BR>id = 274","<B>[275]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00073842}<\/B><BR><BR>support = 0.0581<BR>confidence = 0.43<BR>coverage = 0.135<BR>lift = 2.7<BR>count = 342<BR>order = 2<BR>id = 275","<B>[276]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00242742}<\/B><BR><BR>support = 0.0577<BR>confidence = 0.427<BR>coverage = 0.135<BR>lift = 2.07<BR>count = 340<BR>order = 2<BR>id = 276","<B>[277]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0657<BR>confidence = 0.486<BR>coverage = 0.135<BR>lift = 2.49<BR>count = 387<BR>order = 2<BR>id = 277","<B>[278]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0662<BR>confidence = 0.49<BR>coverage = 0.135<BR>lift = 2.07<BR>count = 390<BR>order = 2<BR>id = 278","<B>[279]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0552<BR>confidence = 0.408<BR>coverage = 0.135<BR>lift = 1.67<BR>count = 325<BR>order = 2<BR>id = 279","<B>[280]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0655<BR>confidence = 0.485<BR>coverage = 0.135<BR>lift = 1.99<BR>count = 386<BR>order = 2<BR>id = 280","<B>[281]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0606<BR>confidence = 0.448<BR>coverage = 0.135<BR>lift = 1.69<BR>count = 357<BR>order = 2<BR>id = 281","<B>[282]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.066<BR>confidence = 0.489<BR>coverage = 0.135<BR>lift = 1.96<BR>count = 389<BR>order = 2<BR>id = 282","<B>[283]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0581<BR>confidence = 0.43<BR>coverage = 0.135<BR>lift = 1.57<BR>count = 342<BR>order = 2<BR>id = 283","<B>[284]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0693<BR>confidence = 0.513<BR>coverage = 0.135<BR>lift = 2.15<BR>count = 408<BR>order = 2<BR>id = 284","<B>[285]<\/B><BR><B>{P00140742}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0623<BR>confidence = 0.461<BR>coverage = 0.135<BR>lift = 1.68<BR>count = 367<BR>order = 2<BR>id = 285","<B>[286]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.402<BR>coverage = 0.125<BR>lift = 2.05<BR>count = 296<BR>order = 2<BR>id = 286","<B>[287]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0519<BR>confidence = 0.415<BR>coverage = 0.125<BR>lift = 2.08<BR>count = 306<BR>order = 2<BR>id = 287","<B>[288]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0555<BR>confidence = 0.444<BR>coverage = 0.125<BR>lift = 2.04<BR>count = 327<BR>order = 2<BR>id = 288","<B>[289]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.402<BR>coverage = 0.125<BR>lift = 1.73<BR>count = 296<BR>order = 2<BR>id = 289","<B>[290]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0572<BR>confidence = 0.457<BR>coverage = 0.125<BR>lift = 1.98<BR>count = 337<BR>order = 2<BR>id = 290","<B>[291]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0542<BR>confidence = 0.433<BR>coverage = 0.125<BR>lift = 1.77<BR>count = 319<BR>order = 2<BR>id = 291","<B>[292]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0562<BR>confidence = 0.449<BR>coverage = 0.125<BR>lift = 1.69<BR>count = 331<BR>order = 2<BR>id = 292","<B>[293]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0542<BR>confidence = 0.433<BR>coverage = 0.125<BR>lift = 1.73<BR>count = 319<BR>order = 2<BR>id = 293","<B>[294]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.455<BR>coverage = 0.125<BR>lift = 1.66<BR>count = 335<BR>order = 2<BR>id = 294","<B>[295]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0577<BR>confidence = 0.461<BR>coverage = 0.125<BR>lift = 1.93<BR>count = 340<BR>order = 2<BR>id = 295","<B>[296]<\/B><BR><B>{P00221442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0581<BR>confidence = 0.464<BR>coverage = 0.125<BR>lift = 1.7<BR>count = 342<BR>order = 2<BR>id = 296","<B>[297]<\/B><BR><B>{P00113242}<\/B><BR>&nbsp;&nbsp; => <B>{P00080342}<\/B><BR><BR>support = 0.0744<BR>confidence = 0.469<BR>coverage = 0.158<BR>lift = 2.3<BR>count = 438<BR>order = 2<BR>id = 297","<B>[298]<\/B><BR><B>{P00113242}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0665<BR>confidence = 0.42<BR>coverage = 0.158<BR>lift = 1.53<BR>count = 392<BR>order = 2<BR>id = 298","<B>[299]<\/B><BR><B>{P00113242}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0642<BR>confidence = 0.405<BR>coverage = 0.158<BR>lift = 1.48<BR>count = 378<BR>order = 2<BR>id = 299","<B>[300]<\/B><BR><B>{P00121642}<\/B><BR>&nbsp;&nbsp; => <B>{P00251242}<\/B><BR><BR>support = 0.0586<BR>confidence = 0.404<BR>coverage = 0.145<BR>lift = 2.01<BR>count = 345<BR>order = 2<BR>id = 300","<B>[301]<\/B><BR><B>{P00121642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0616<BR>confidence = 0.426<BR>coverage = 0.145<BR>lift = 1.96<BR>count = 363<BR>order = 2<BR>id = 301","<B>[302]<\/B><BR><B>{P00121642}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.0623<BR>confidence = 0.43<BR>coverage = 0.145<BR>lift = 2.01<BR>count = 367<BR>order = 2<BR>id = 302","<B>[303]<\/B><BR><B>{P00121642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0603<BR>confidence = 0.416<BR>coverage = 0.145<BR>lift = 1.52<BR>count = 355<BR>order = 2<BR>id = 303","<B>[304]<\/B><BR><B>{P00121642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0611<BR>confidence = 0.422<BR>coverage = 0.145<BR>lift = 1.32<BR>count = 360<BR>order = 2<BR>id = 304","<B>[305]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00251242}<\/B><BR><BR>support = 0.0609<BR>confidence = 0.454<BR>coverage = 0.134<BR>lift = 2.25<BR>count = 359<BR>order = 2<BR>id = 305","<B>[306]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0565<BR>confidence = 0.421<BR>coverage = 0.134<BR>lift = 1.94<BR>count = 333<BR>order = 2<BR>id = 306","<B>[307]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0643<BR>confidence = 0.479<BR>coverage = 0.134<BR>lift = 2.07<BR>count = 379<BR>order = 2<BR>id = 307","<B>[308]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00117442}<\/B><BR><BR>support = 0.056<BR>confidence = 0.417<BR>coverage = 0.134<BR>lift = 1.95<BR>count = 330<BR>order = 2<BR>id = 308","<B>[309]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0548<BR>confidence = 0.408<BR>coverage = 0.134<BR>lift = 1.49<BR>count = 323<BR>order = 2<BR>id = 309","<B>[310]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0604<BR>confidence = 0.45<BR>coverage = 0.134<BR>lift = 1.64<BR>count = 356<BR>order = 2<BR>id = 310","<B>[311]<\/B><BR><B>{P00271142}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0553<BR>confidence = 0.412<BR>coverage = 0.134<BR>lift = 1.29<BR>count = 326<BR>order = 2<BR>id = 311","<B>[312]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0669<BR>confidence = 0.432<BR>coverage = 0.155<BR>lift = 2.29<BR>count = 394<BR>order = 2<BR>id = 312","<B>[313]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0645<BR>confidence = 0.417<BR>coverage = 0.155<BR>lift = 2.13<BR>count = 380<BR>order = 2<BR>id = 313","<B>[314]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00010742}<\/B><BR><BR>support = 0.0652<BR>confidence = 0.422<BR>coverage = 0.155<BR>lift = 1.84<BR>count = 384<BR>order = 2<BR>id = 314","<B>[315]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0657<BR>confidence = 0.425<BR>coverage = 0.155<BR>lift = 1.74<BR>count = 387<BR>order = 2<BR>id = 315","<B>[316]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0647<BR>confidence = 0.418<BR>coverage = 0.155<BR>lift = 1.71<BR>count = 381<BR>order = 2<BR>id = 316","<B>[317]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0694<BR>confidence = 0.449<BR>coverage = 0.155<BR>lift = 1.8<BR>count = 409<BR>order = 2<BR>id = 317","<B>[318]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0645<BR>confidence = 0.417<BR>coverage = 0.155<BR>lift = 1.52<BR>count = 380<BR>order = 2<BR>id = 318","<B>[319]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.063<BR>confidence = 0.407<BR>coverage = 0.155<BR>lift = 1.71<BR>count = 371<BR>order = 2<BR>id = 319","<B>[320]<\/B><BR><B>{P00147942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0662<BR>confidence = 0.428<BR>coverage = 0.155<BR>lift = 1.56<BR>count = 390<BR>order = 2<BR>id = 320","<B>[321]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.062<BR>confidence = 0.432<BR>coverage = 0.143<BR>lift = 2.21<BR>count = 365<BR>order = 2<BR>id = 321","<B>[322]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0628<BR>confidence = 0.438<BR>coverage = 0.143<BR>lift = 1.85<BR>count = 370<BR>order = 2<BR>id = 322","<B>[323]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0596<BR>confidence = 0.416<BR>coverage = 0.143<BR>lift = 1.7<BR>count = 351<BR>order = 2<BR>id = 323","<B>[324]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0606<BR>confidence = 0.423<BR>coverage = 0.143<BR>lift = 1.6<BR>count = 357<BR>order = 2<BR>id = 324","<B>[325]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.442<BR>coverage = 0.143<BR>lift = 1.77<BR>count = 373<BR>order = 2<BR>id = 325","<B>[326]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0618<BR>confidence = 0.431<BR>coverage = 0.143<BR>lift = 1.57<BR>count = 364<BR>order = 2<BR>id = 326","<B>[327]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0637<BR>confidence = 0.444<BR>coverage = 0.143<BR>lift = 1.86<BR>count = 375<BR>order = 2<BR>id = 327","<B>[328]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0606<BR>confidence = 0.423<BR>coverage = 0.143<BR>lift = 1.55<BR>count = 357<BR>order = 2<BR>id = 328","<B>[329]<\/B><BR><B>{P00003242}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0579<BR>confidence = 0.404<BR>coverage = 0.143<BR>lift = 1.27<BR>count = 341<BR>order = 2<BR>id = 329","<B>[330]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0584<BR>confidence = 0.406<BR>coverage = 0.144<BR>lift = 2.15<BR>count = 344<BR>order = 2<BR>id = 330","<B>[331]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0575<BR>confidence = 0.4<BR>coverage = 0.144<BR>lift = 1.69<BR>count = 339<BR>order = 2<BR>id = 331","<B>[332]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0577<BR>confidence = 0.401<BR>coverage = 0.144<BR>lift = 1.74<BR>count = 340<BR>order = 2<BR>id = 332","<B>[333]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0575<BR>confidence = 0.4<BR>coverage = 0.144<BR>lift = 1.64<BR>count = 339<BR>order = 2<BR>id = 333","<B>[334]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0687<BR>confidence = 0.478<BR>coverage = 0.144<BR>lift = 1.96<BR>count = 405<BR>order = 2<BR>id = 334","<B>[335]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0589<BR>confidence = 0.41<BR>coverage = 0.144<BR>lift = 1.64<BR>count = 347<BR>order = 2<BR>id = 335","<B>[336]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0614<BR>confidence = 0.427<BR>coverage = 0.144<BR>lift = 1.79<BR>count = 362<BR>order = 2<BR>id = 336","<B>[337]<\/B><BR><B>{P00062842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0608<BR>confidence = 0.423<BR>coverage = 0.144<BR>lift = 1.54<BR>count = 358<BR>order = 2<BR>id = 337","<B>[338]<\/B><BR><B>{P00105142}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.407<BR>coverage = 0.156<BR>lift = 2.03<BR>count = 373<BR>order = 2<BR>id = 338","<B>[339]<\/B><BR><B>{P00105142}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.064<BR>confidence = 0.411<BR>coverage = 0.156<BR>lift = 1.55<BR>count = 377<BR>order = 2<BR>id = 339","<B>[340]<\/B><BR><B>{P00105142}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.063<BR>confidence = 0.405<BR>coverage = 0.156<BR>lift = 1.62<BR>count = 371<BR>order = 2<BR>id = 340","<B>[341]<\/B><BR><B>{P00105142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0655<BR>confidence = 0.421<BR>coverage = 0.156<BR>lift = 1.54<BR>count = 386<BR>order = 2<BR>id = 341","<B>[342]<\/B><BR><B>{P00105142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0689<BR>confidence = 0.443<BR>coverage = 0.156<BR>lift = 1.62<BR>count = 406<BR>order = 2<BR>id = 342","<B>[343]<\/B><BR><B>{P00258742}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0689<BR>confidence = 0.421<BR>coverage = 0.164<BR>lift = 1.82<BR>count = 406<BR>order = 2<BR>id = 343","<B>[344]<\/B><BR><B>{P00258742}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0665<BR>confidence = 0.407<BR>coverage = 0.164<BR>lift = 1.27<BR>count = 392<BR>order = 2<BR>id = 344","<B>[345]<\/B><BR><B>{P00106042}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.066<BR>confidence = 0.43<BR>coverage = 0.154<BR>lift = 1.57<BR>count = 389<BR>order = 2<BR>id = 345","<B>[346]<\/B><BR><B>{P00106042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0671<BR>confidence = 0.436<BR>coverage = 0.154<BR>lift = 1.6<BR>count = 395<BR>order = 2<BR>id = 346","<B>[347]<\/B><BR><B>{P00106042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.064<BR>confidence = 0.417<BR>coverage = 0.154<BR>lift = 1.31<BR>count = 377<BR>order = 2<BR>id = 347","<B>[348]<\/B><BR><B>{P00128942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0643<BR>confidence = 0.405<BR>coverage = 0.159<BR>lift = 1.75<BR>count = 379<BR>order = 2<BR>id = 348","<B>[349]<\/B><BR><B>{P00128942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.073<BR>confidence = 0.459<BR>coverage = 0.159<BR>lift = 1.68<BR>count = 430<BR>order = 2<BR>id = 349","<B>[350]<\/B><BR><B>{P00128942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0664<BR>confidence = 0.418<BR>coverage = 0.159<BR>lift = 1.53<BR>count = 391<BR>order = 2<BR>id = 350","<B>[351]<\/B><BR><B>{P00086442}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0677<BR>confidence = 0.413<BR>coverage = 0.164<BR>lift = 1.56<BR>count = 399<BR>order = 2<BR>id = 351","<B>[352]<\/B><BR><B>{P00086442}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0764<BR>confidence = 0.466<BR>coverage = 0.164<BR>lift = 1.7<BR>count = 450<BR>order = 2<BR>id = 352","<B>[353]<\/B><BR><B>{P00086442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0708<BR>confidence = 0.432<BR>coverage = 0.164<BR>lift = 1.58<BR>count = 417<BR>order = 2<BR>id = 353","<B>[354]<\/B><BR><B>{P00085942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0737<BR>confidence = 0.451<BR>coverage = 0.163<BR>lift = 1.64<BR>count = 434<BR>order = 2<BR>id = 354","<B>[355]<\/B><BR><B>{P00085942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0672<BR>confidence = 0.411<BR>coverage = 0.163<BR>lift = 1.5<BR>count = 396<BR>order = 2<BR>id = 355","<B>[356]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00120042}<\/B><BR><BR>support = 0.0553<BR>confidence = 0.415<BR>coverage = 0.133<BR>lift = 2.71<BR>count = 326<BR>order = 2<BR>id = 356","<B>[357]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0654<BR>confidence = 0.427<BR>coverage = 0.153<BR>lift = 2.13<BR>count = 385<BR>order = 2<BR>id = 357","<B>[358]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.414<BR>coverage = 0.153<BR>lift = 1.9<BR>count = 373<BR>order = 2<BR>id = 358","<B>[359]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0643<BR>confidence = 0.42<BR>coverage = 0.153<BR>lift = 1.78<BR>count = 379<BR>order = 2<BR>id = 359","<B>[360]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0793<BR>confidence = 0.518<BR>coverage = 0.153<BR>lift = 2.24<BR>count = 467<BR>order = 2<BR>id = 360","<B>[361]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0647<BR>confidence = 0.422<BR>coverage = 0.153<BR>lift = 1.73<BR>count = 381<BR>order = 2<BR>id = 361","<B>[362]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0708<BR>confidence = 0.462<BR>coverage = 0.153<BR>lift = 1.74<BR>count = 417<BR>order = 2<BR>id = 362","<B>[363]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0647<BR>confidence = 0.422<BR>coverage = 0.153<BR>lift = 1.69<BR>count = 381<BR>order = 2<BR>id = 363","<B>[364]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0708<BR>confidence = 0.462<BR>coverage = 0.153<BR>lift = 1.69<BR>count = 417<BR>order = 2<BR>id = 364","<B>[365]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0637<BR>confidence = 0.416<BR>coverage = 0.153<BR>lift = 1.74<BR>count = 375<BR>order = 2<BR>id = 365","<B>[366]<\/B><BR><B>{P00120042}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0732<BR>confidence = 0.478<BR>coverage = 0.153<BR>lift = 1.75<BR>count = 431<BR>order = 2<BR>id = 366","<B>[367]<\/B><BR><B>{P00334242}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0754<BR>confidence = 0.406<BR>coverage = 0.186<BR>lift = 1.27<BR>count = 444<BR>order = 2<BR>id = 367","<B>[368]<\/B><BR><B>{P00182142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0628<BR>confidence = 0.407<BR>coverage = 0.154<BR>lift = 1.76<BR>count = 370<BR>order = 2<BR>id = 368","<B>[369]<\/B><BR><B>{P00182142}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0623<BR>confidence = 0.404<BR>coverage = 0.154<BR>lift = 1.62<BR>count = 367<BR>order = 2<BR>id = 369","<B>[370]<\/B><BR><B>{P00182142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.063<BR>confidence = 0.408<BR>coverage = 0.154<BR>lift = 1.49<BR>count = 371<BR>order = 2<BR>id = 370","<B>[371]<\/B><BR><B>{P00182142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0704<BR>confidence = 0.457<BR>coverage = 0.154<BR>lift = 1.67<BR>count = 415<BR>order = 2<BR>id = 371","<B>[372]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0582<BR>confidence = 0.437<BR>coverage = 0.133<BR>lift = 2.19<BR>count = 343<BR>order = 2<BR>id = 372","<B>[373]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110842}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.427<BR>coverage = 0.133<BR>lift = 1.96<BR>count = 335<BR>order = 2<BR>id = 373","<B>[374]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0569<BR>confidence = 0.427<BR>coverage = 0.133<BR>lift = 1.84<BR>count = 335<BR>order = 2<BR>id = 374","<B>[375]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0604<BR>confidence = 0.454<BR>coverage = 0.133<BR>lift = 1.96<BR>count = 356<BR>order = 2<BR>id = 375","<B>[376]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0558<BR>confidence = 0.419<BR>coverage = 0.133<BR>lift = 1.72<BR>count = 329<BR>order = 2<BR>id = 376","<B>[377]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0606<BR>confidence = 0.455<BR>coverage = 0.133<BR>lift = 1.72<BR>count = 357<BR>order = 2<BR>id = 377","<B>[378]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.057<BR>confidence = 0.428<BR>coverage = 0.133<BR>lift = 1.72<BR>count = 336<BR>order = 2<BR>id = 378","<B>[379]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0625<BR>confidence = 0.469<BR>coverage = 0.133<BR>lift = 1.71<BR>count = 368<BR>order = 2<BR>id = 379","<B>[380]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0574<BR>confidence = 0.431<BR>coverage = 0.133<BR>lift = 1.8<BR>count = 338<BR>order = 2<BR>id = 380","<B>[381]<\/B><BR><B>{P00057942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0686<BR>confidence = 0.515<BR>coverage = 0.133<BR>lift = 1.88<BR>count = 404<BR>order = 2<BR>id = 381","<B>[382]<\/B><BR><B>{P00052842}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0699<BR>confidence = 0.42<BR>coverage = 0.167<BR>lift = 2.1<BR>count = 412<BR>order = 2<BR>id = 382","<B>[383]<\/B><BR><B>{P00052842}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0752<BR>confidence = 0.451<BR>coverage = 0.167<BR>lift = 1.65<BR>count = 443<BR>order = 2<BR>id = 383","<B>[384]<\/B><BR><B>{P00052842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0779<BR>confidence = 0.467<BR>coverage = 0.167<BR>lift = 1.71<BR>count = 459<BR>order = 2<BR>id = 384","<B>[385]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00102642}<\/B><BR><BR>support = 0.0599<BR>confidence = 0.422<BR>coverage = 0.142<BR>lift = 2<BR>count = 353<BR>order = 2<BR>id = 385","<B>[386]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00251242}<\/B><BR><BR>support = 0.0582<BR>confidence = 0.41<BR>coverage = 0.142<BR>lift = 2.04<BR>count = 343<BR>order = 2<BR>id = 386","<B>[387]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00117942}<\/B><BR><BR>support = 0.0592<BR>confidence = 0.417<BR>coverage = 0.142<BR>lift = 1.8<BR>count = 349<BR>order = 2<BR>id = 387","<B>[388]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0581<BR>confidence = 0.409<BR>coverage = 0.142<BR>lift = 1.54<BR>count = 342<BR>order = 2<BR>id = 388","<B>[389]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0613<BR>confidence = 0.432<BR>coverage = 0.142<BR>lift = 1.58<BR>count = 361<BR>order = 2<BR>id = 389","<B>[390]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0631<BR>confidence = 0.445<BR>coverage = 0.142<BR>lift = 1.63<BR>count = 372<BR>order = 2<BR>id = 390","<B>[391]<\/B><BR><B>{P00259342}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0577<BR>confidence = 0.407<BR>coverage = 0.142<BR>lift = 1.27<BR>count = 340<BR>order = 2<BR>id = 391","<B>[392]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0696<BR>confidence = 0.464<BR>coverage = 0.15<BR>lift = 2.46<BR>count = 410<BR>order = 2<BR>id = 392","<B>[393]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0608<BR>confidence = 0.405<BR>coverage = 0.15<BR>lift = 2.07<BR>count = 358<BR>order = 2<BR>id = 393","<B>[394]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.422<BR>coverage = 0.15<BR>lift = 1.78<BR>count = 373<BR>order = 2<BR>id = 394","<B>[395]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0616<BR>confidence = 0.411<BR>coverage = 0.15<BR>lift = 1.68<BR>count = 363<BR>order = 2<BR>id = 395","<B>[396]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0655<BR>confidence = 0.437<BR>coverage = 0.15<BR>lift = 1.79<BR>count = 386<BR>order = 2<BR>id = 396","<B>[397]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0654<BR>confidence = 0.436<BR>coverage = 0.15<BR>lift = 1.75<BR>count = 385<BR>order = 2<BR>id = 397","<B>[398]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0611<BR>confidence = 0.407<BR>coverage = 0.15<BR>lift = 1.71<BR>count = 360<BR>order = 2<BR>id = 398","<B>[399]<\/B><BR><B>{P00036842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0618<BR>confidence = 0.412<BR>coverage = 0.15<BR>lift = 1.5<BR>count = 364<BR>order = 2<BR>id = 399","<B>[400]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0699<BR>confidence = 0.44<BR>coverage = 0.159<BR>lift = 2.25<BR>count = 412<BR>order = 2<BR>id = 400","<B>[401]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0682<BR>confidence = 0.429<BR>coverage = 0.159<BR>lift = 1.81<BR>count = 402<BR>order = 2<BR>id = 401","<B>[402]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0706<BR>confidence = 0.444<BR>coverage = 0.159<BR>lift = 1.82<BR>count = 416<BR>order = 2<BR>id = 402","<B>[403]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0662<BR>confidence = 0.417<BR>coverage = 0.159<BR>lift = 1.57<BR>count = 390<BR>order = 2<BR>id = 403","<B>[404]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0684<BR>confidence = 0.431<BR>coverage = 0.159<BR>lift = 1.73<BR>count = 403<BR>order = 2<BR>id = 404","<B>[405]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0662<BR>confidence = 0.417<BR>coverage = 0.159<BR>lift = 1.52<BR>count = 390<BR>order = 2<BR>id = 405","<B>[406]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.072<BR>confidence = 0.453<BR>coverage = 0.159<BR>lift = 1.9<BR>count = 424<BR>order = 2<BR>id = 406","<B>[407]<\/B><BR><B>{P00073842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0704<BR>confidence = 0.443<BR>coverage = 0.159<BR>lift = 1.62<BR>count = 415<BR>order = 2<BR>id = 407","<B>[408]<\/B><BR><B>{P00005042}<\/B><BR>&nbsp;&nbsp; => <B>{P00148642}<\/B><BR><BR>support = 0.0689<BR>confidence = 0.419<BR>coverage = 0.164<BR>lift = 2.05<BR>count = 406<BR>order = 2<BR>id = 408","<B>[409]<\/B><BR><B>{P00005042}<\/B><BR>&nbsp;&nbsp; => <B>{P00028842}<\/B><BR><BR>support = 0.0693<BR>confidence = 0.421<BR>coverage = 0.164<BR>lift = 2.07<BR>count = 408<BR>order = 2<BR>id = 409","<B>[410]<\/B><BR><B>{P00005042}<\/B><BR>&nbsp;&nbsp; => <B>{P00255842}<\/B><BR><BR>support = 0.0672<BR>confidence = 0.409<BR>coverage = 0.164<BR>lift = 1.74<BR>count = 396<BR>order = 2<BR>id = 410","<B>[411]<\/B><BR><B>{P00005042}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.073<BR>confidence = 0.444<BR>coverage = 0.164<BR>lift = 1.86<BR>count = 430<BR>order = 2<BR>id = 411","<B>[412]<\/B><BR><B>{P00005042}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.066<BR>confidence = 0.401<BR>coverage = 0.164<BR>lift = 1.66<BR>count = 389<BR>order = 2<BR>id = 412","<B>[413]<\/B><BR><B>{P00005042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.074<BR>confidence = 0.45<BR>coverage = 0.164<BR>lift = 1.41<BR>count = 436<BR>order = 2<BR>id = 413","<B>[414]<\/B><BR><B>{P00116842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0716<BR>confidence = 0.431<BR>coverage = 0.166<BR>lift = 1.35<BR>count = 422<BR>order = 2<BR>id = 414","<B>[415]<\/B><BR><B>{P00278642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.103<BR>confidence = 0.494<BR>coverage = 0.208<BR>lift = 1.55<BR>count = 606<BR>order = 2<BR>id = 415","<B>[416]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00044442}<\/B><BR><BR>support = 0.0633<BR>confidence = 0.409<BR>coverage = 0.155<BR>lift = 2.17<BR>count = 373<BR>order = 2<BR>id = 416","<B>[417]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0635<BR>confidence = 0.41<BR>coverage = 0.155<BR>lift = 2.1<BR>count = 374<BR>order = 2<BR>id = 417","<B>[418]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0723<BR>confidence = 0.467<BR>coverage = 0.155<BR>lift = 2.34<BR>count = 426<BR>order = 2<BR>id = 418","<B>[419]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0625<BR>confidence = 0.404<BR>coverage = 0.155<BR>lift = 1.71<BR>count = 368<BR>order = 2<BR>id = 419","<B>[420]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00110942}<\/B><BR><BR>support = 0.0696<BR>confidence = 0.45<BR>coverage = 0.155<BR>lift = 1.95<BR>count = 410<BR>order = 2<BR>id = 420","<B>[421]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0667<BR>confidence = 0.431<BR>coverage = 0.155<BR>lift = 1.63<BR>count = 393<BR>order = 2<BR>id = 421","<B>[422]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0671<BR>confidence = 0.433<BR>coverage = 0.155<BR>lift = 1.74<BR>count = 395<BR>order = 2<BR>id = 422","<B>[423]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0732<BR>confidence = 0.473<BR>coverage = 0.155<BR>lift = 1.72<BR>count = 431<BR>order = 2<BR>id = 423","<B>[424]<\/B><BR><B>{P0097242}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0762<BR>confidence = 0.492<BR>coverage = 0.155<BR>lift = 1.8<BR>count = 449<BR>order = 2<BR>id = 424","<B>[425]<\/B><BR><B>{P00277642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0703<BR>confidence = 0.419<BR>coverage = 0.168<BR>lift = 1.53<BR>count = 414<BR>order = 2<BR>id = 425","<B>[426]<\/B><BR><B>{P00277642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0701<BR>confidence = 0.418<BR>coverage = 0.168<BR>lift = 1.31<BR>count = 413<BR>order = 2<BR>id = 426","<B>[427]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00112542}<\/B><BR><BR>support = 0.075<BR>confidence = 0.435<BR>coverage = 0.172<BR>lift = 2.27<BR>count = 442<BR>order = 2<BR>id = 427","<B>[428]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0772<BR>confidence = 0.448<BR>coverage = 0.172<BR>lift = 2.24<BR>count = 455<BR>order = 2<BR>id = 428","<B>[429]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0704<BR>confidence = 0.409<BR>coverage = 0.172<BR>lift = 1.73<BR>count = 415<BR>order = 2<BR>id = 429","<B>[430]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0694<BR>confidence = 0.403<BR>coverage = 0.172<BR>lift = 1.65<BR>count = 409<BR>order = 2<BR>id = 430","<B>[431]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.075<BR>confidence = 0.435<BR>coverage = 0.172<BR>lift = 1.64<BR>count = 442<BR>order = 2<BR>id = 431","<B>[432]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0776<BR>confidence = 0.45<BR>coverage = 0.172<BR>lift = 1.8<BR>count = 457<BR>order = 2<BR>id = 432","<B>[433]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.076<BR>confidence = 0.441<BR>coverage = 0.172<BR>lift = 1.61<BR>count = 448<BR>order = 2<BR>id = 433","<B>[434]<\/B><BR><B>{P00111142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0845<BR>confidence = 0.491<BR>coverage = 0.172<BR>lift = 1.79<BR>count = 498<BR>order = 2<BR>id = 434","<B>[435]<\/B><BR><B>{P00051442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0852<BR>confidence = 0.402<BR>coverage = 0.212<BR>lift = 1.26<BR>count = 502<BR>order = 2<BR>id = 435","<B>[436]<\/B><BR><B>{P00148642}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.082<BR>confidence = 0.401<BR>coverage = 0.204<BR>lift = 1.66<BR>count = 483<BR>order = 2<BR>id = 436","<B>[437]<\/B><BR><B>{P00148642}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0856<BR>confidence = 0.419<BR>coverage = 0.204<BR>lift = 1.31<BR>count = 504<BR>order = 2<BR>id = 437","<B>[438]<\/B><BR><B>{P00242742}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0844<BR>confidence = 0.409<BR>coverage = 0.206<BR>lift = 1.73<BR>count = 497<BR>order = 2<BR>id = 438","<B>[439]<\/B><BR><B>{P00242742}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.083<BR>confidence = 0.402<BR>coverage = 0.206<BR>lift = 1.65<BR>count = 489<BR>order = 2<BR>id = 439","<B>[440]<\/B><BR><B>{P00242742}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.084<BR>confidence = 0.407<BR>coverage = 0.206<BR>lift = 1.49<BR>count = 495<BR>order = 2<BR>id = 440","<B>[441]<\/B><BR><B>{P00242742}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0837<BR>confidence = 0.406<BR>coverage = 0.206<BR>lift = 1.48<BR>count = 493<BR>order = 2<BR>id = 441","<B>[442]<\/B><BR><B>{P00220442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0923<BR>confidence = 0.424<BR>coverage = 0.218<BR>lift = 1.33<BR>count = 544<BR>order = 2<BR>id = 442","<B>[443]<\/B><BR><B>{P00112542}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0794<BR>confidence = 0.414<BR>coverage = 0.192<BR>lift = 1.66<BR>count = 468<BR>order = 2<BR>id = 443","<B>[444]<\/B><BR><B>{P00112542}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0889<BR>confidence = 0.463<BR>coverage = 0.192<BR>lift = 1.69<BR>count = 524<BR>order = 2<BR>id = 444","<B>[445]<\/B><BR><B>{P00112542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0873<BR>confidence = 0.454<BR>coverage = 0.192<BR>lift = 1.66<BR>count = 514<BR>order = 2<BR>id = 445","<B>[446]<\/B><BR><B>{P00031042}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.0828<BR>confidence = 0.407<BR>coverage = 0.204<BR>lift = 1.7<BR>count = 488<BR>order = 2<BR>id = 446","<B>[447]<\/B><BR><B>{P00031042}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0861<BR>confidence = 0.422<BR>coverage = 0.204<BR>lift = 1.75<BR>count = 507<BR>order = 2<BR>id = 447","<B>[448]<\/B><BR><B>{P00031042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0867<BR>confidence = 0.426<BR>coverage = 0.204<BR>lift = 1.33<BR>count = 511<BR>order = 2<BR>id = 448","<B>[449]<\/B><BR><B>{P00044442}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0825<BR>confidence = 0.437<BR>coverage = 0.189<BR>lift = 1.85<BR>count = 486<BR>order = 2<BR>id = 449","<B>[450]<\/B><BR><B>{P00044442}<\/B><BR>&nbsp;&nbsp; => <B>{P00184942}<\/B><BR><BR>support = 0.0788<BR>confidence = 0.418<BR>coverage = 0.189<BR>lift = 1.71<BR>count = 464<BR>order = 2<BR>id = 450","<B>[451]<\/B><BR><B>{P00044442}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0759<BR>confidence = 0.402<BR>coverage = 0.189<BR>lift = 1.65<BR>count = 447<BR>order = 2<BR>id = 451","<B>[452]<\/B><BR><B>{P00044442}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0816<BR>confidence = 0.433<BR>coverage = 0.189<BR>lift = 1.74<BR>count = 481<BR>order = 2<BR>id = 452","<B>[453]<\/B><BR><B>{P00044442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0764<BR>confidence = 0.405<BR>coverage = 0.189<BR>lift = 1.48<BR>count = 450<BR>order = 2<BR>id = 453","<B>[454]<\/B><BR><B>{P00102642}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0871<BR>confidence = 0.412<BR>coverage = 0.212<BR>lift = 1.5<BR>count = 513<BR>order = 2<BR>id = 454","<B>[455]<\/B><BR><B>{P00102642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0925<BR>confidence = 0.437<BR>coverage = 0.212<BR>lift = 1.6<BR>count = 545<BR>order = 2<BR>id = 455","<B>[456]<\/B><BR><B>{P00028842}<\/B><BR>&nbsp;&nbsp; => <B>{P00059442}<\/B><BR><BR>support = 0.0898<BR>confidence = 0.442<BR>coverage = 0.203<BR>lift = 1.85<BR>count = 529<BR>order = 2<BR>id = 456","<B>[457]<\/B><BR><B>{P00028842}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0839<BR>confidence = 0.413<BR>coverage = 0.203<BR>lift = 1.71<BR>count = 494<BR>order = 2<BR>id = 457","<B>[458]<\/B><BR><B>{P00028842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.091<BR>confidence = 0.448<BR>coverage = 0.203<BR>lift = 1.4<BR>count = 536<BR>order = 2<BR>id = 458","<B>[459]<\/B><BR><B>{P00251242}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0835<BR>confidence = 0.414<BR>coverage = 0.201<BR>lift = 1.3<BR>count = 492<BR>order = 2<BR>id = 459","<B>[460]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00114942}<\/B><BR><BR>support = 0.0793<BR>confidence = 0.405<BR>coverage = 0.196<BR>lift = 2.03<BR>count = 467<BR>order = 2<BR>id = 460","<B>[461]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0898<BR>confidence = 0.459<BR>coverage = 0.196<BR>lift = 1.94<BR>count = 529<BR>order = 2<BR>id = 461","<B>[462]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.0883<BR>confidence = 0.451<BR>coverage = 0.196<BR>lift = 1.85<BR>count = 520<BR>order = 2<BR>id = 462","<B>[463]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0794<BR>confidence = 0.406<BR>coverage = 0.196<BR>lift = 1.53<BR>count = 468<BR>order = 2<BR>id = 463","<B>[464]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0981<BR>confidence = 0.502<BR>coverage = 0.196<BR>lift = 2.01<BR>count = 578<BR>order = 2<BR>id = 464","<B>[465]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0833<BR>confidence = 0.426<BR>coverage = 0.196<BR>lift = 1.55<BR>count = 491<BR>order = 2<BR>id = 465","<B>[466]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0908<BR>confidence = 0.464<BR>coverage = 0.196<BR>lift = 1.95<BR>count = 535<BR>order = 2<BR>id = 466","<B>[467]<\/B><BR><B>{P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0856<BR>confidence = 0.438<BR>coverage = 0.196<BR>lift = 1.6<BR>count = 504<BR>order = 2<BR>id = 467","<B>[468]<\/B><BR><B>{P00114942}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0818<BR>confidence = 0.409<BR>coverage = 0.2<BR>lift = 1.73<BR>count = 482<BR>order = 2<BR>id = 468","<B>[469]<\/B><BR><B>{P00114942}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0862<BR>confidence = 0.431<BR>coverage = 0.2<BR>lift = 1.63<BR>count = 508<BR>order = 2<BR>id = 469","<B>[470]<\/B><BR><B>{P00114942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0927<BR>confidence = 0.463<BR>coverage = 0.2<BR>lift = 1.86<BR>count = 546<BR>order = 2<BR>id = 470","<B>[471]<\/B><BR><B>{P00114942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0898<BR>confidence = 0.449<BR>coverage = 0.2<BR>lift = 1.64<BR>count = 529<BR>order = 2<BR>id = 471","<B>[472]<\/B><BR><B>{P00114942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0944<BR>confidence = 0.472<BR>coverage = 0.2<BR>lift = 1.72<BR>count = 556<BR>order = 2<BR>id = 472","<B>[473]<\/B><BR><B>{P00255842}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0988<BR>confidence = 0.421<BR>coverage = 0.235<BR>lift = 1.32<BR>count = 582<BR>order = 2<BR>id = 473","<B>[474]<\/B><BR><B>{P00110842}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0901<BR>confidence = 0.415<BR>coverage = 0.217<BR>lift = 1.51<BR>count = 531<BR>order = 2<BR>id = 474","<B>[475]<\/B><BR><B>{P00059442}<\/B><BR>&nbsp;&nbsp; => <B>{P00058042}<\/B><BR><BR>support = 0.0962<BR>confidence = 0.403<BR>coverage = 0.239<BR>lift = 1.67<BR>count = 567<BR>order = 2<BR>id = 475","<B>[476]<\/B><BR><B>{P00059442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.103<BR>confidence = 0.431<BR>coverage = 0.239<BR>lift = 1.35<BR>count = 606<BR>order = 2<BR>id = 476","<B>[477]<\/B><BR><B>{P00237542}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.103<BR>confidence = 0.436<BR>coverage = 0.237<BR>lift = 1.75<BR>count = 608<BR>order = 2<BR>id = 477","<B>[478]<\/B><BR><B>{P00057642}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.103<BR>confidence = 0.414<BR>coverage = 0.25<BR>lift = 1.75<BR>count = 608<BR>order = 2<BR>id = 478","<B>[479]<\/B><BR><B>{P00237542}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0981<BR>confidence = 0.415<BR>coverage = 0.237<BR>lift = 1.51<BR>count = 578<BR>order = 2<BR>id = 479","<B>[480]<\/B><BR><B>{P00237542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0962<BR>confidence = 0.407<BR>coverage = 0.237<BR>lift = 1.49<BR>count = 567<BR>order = 2<BR>id = 480","<B>[481]<\/B><BR><B>{P00110942}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0998<BR>confidence = 0.432<BR>coverage = 0.231<BR>lift = 1.58<BR>count = 588<BR>order = 2<BR>id = 481","<B>[482]<\/B><BR><B>{P00110942}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.099<BR>confidence = 0.429<BR>coverage = 0.231<BR>lift = 1.57<BR>count = 583<BR>order = 2<BR>id = 482","<B>[483]<\/B><BR><B>{P00117442}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.0896<BR>confidence = 0.419<BR>coverage = 0.214<BR>lift = 1.31<BR>count = 528<BR>order = 2<BR>id = 483","<B>[484]<\/B><BR><B>{P00046742}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.102<BR>confidence = 0.419<BR>coverage = 0.244<BR>lift = 1.68<BR>count = 602<BR>order = 2<BR>id = 484","<B>[485]<\/B><BR><B>{P00057642}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.102<BR>confidence = 0.41<BR>coverage = 0.25<BR>lift = 1.68<BR>count = 602<BR>order = 2<BR>id = 485","<B>[486]<\/B><BR><B>{P00046742}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.1<BR>confidence = 0.41<BR>coverage = 0.244<BR>lift = 1.72<BR>count = 590<BR>order = 2<BR>id = 486","<B>[487]<\/B><BR><B>{P00145042}<\/B><BR>&nbsp;&nbsp; => <B>{P00046742}<\/B><BR><BR>support = 0.1<BR>confidence = 0.42<BR>coverage = 0.239<BR>lift = 1.72<BR>count = 590<BR>order = 2<BR>id = 487","<B>[488]<\/B><BR><B>{P00058042}<\/B><BR>&nbsp;&nbsp; => <B>{P00265242}<\/B><BR><BR>support = 0.102<BR>confidence = 0.423<BR>coverage = 0.241<BR>lift = 1.32<BR>count = 601<BR>order = 2<BR>id = 488","<B>[489]<\/B><BR><B>{P00112142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.107<BR>confidence = 0.404<BR>coverage = 0.265<BR>lift = 1.47<BR>count = 631<BR>order = 2<BR>id = 489","<B>[490]<\/B><BR><B>{P00112142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.113<BR>confidence = 0.427<BR>coverage = 0.265<BR>lift = 1.56<BR>count = 667<BR>order = 2<BR>id = 490","<B>[491]<\/B><BR><B>{P00110742}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.113<BR>confidence = 0.414<BR>coverage = 0.274<BR>lift = 1.56<BR>count = 667<BR>order = 2<BR>id = 491","<B>[492]<\/B><BR><B>{P00057642}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.102<BR>confidence = 0.409<BR>coverage = 0.25<BR>lift = 1.71<BR>count = 601<BR>order = 2<BR>id = 492","<B>[493]<\/B><BR><B>{P00145042}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.102<BR>confidence = 0.427<BR>coverage = 0.239<BR>lift = 1.71<BR>count = 601<BR>order = 2<BR>id = 493","<B>[494]<\/B><BR><B>{P00057642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.101<BR>confidence = 0.405<BR>coverage = 0.25<BR>lift = 1.48<BR>count = 596<BR>order = 2<BR>id = 494","<B>[495]<\/B><BR><B>{P00025442}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.12<BR>confidence = 0.438<BR>coverage = 0.274<BR>lift = 1.6<BR>count = 707<BR>order = 2<BR>id = 495","<B>[496]<\/B><BR><B>{P00110742}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.12<BR>confidence = 0.439<BR>coverage = 0.274<BR>lift = 1.6<BR>count = 707<BR>order = 2<BR>id = 496","<B>[497]<\/B><BR><B>{P00057642,<BR>&nbsp;&nbsp;P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00145042}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.521<BR>coverage = 0.0981<BR>lift = 2.18<BR>count = 301<BR>order = 3<BR>id = 497","<B>[498]<\/B><BR><B>{P00145042,<BR>&nbsp;&nbsp;P00270942}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.563<BR>coverage = 0.0908<BR>lift = 2.25<BR>count = 301<BR>order = 3<BR>id = 498","<B>[499]<\/B><BR><B>{P00057642,<BR>&nbsp;&nbsp;P00145042}<\/B><BR>&nbsp;&nbsp; => <B>{P00270942}<\/B><BR><BR>support = 0.0511<BR>confidence = 0.501<BR>coverage = 0.102<BR>lift = 2.56<BR>count = 301<BR>order = 3<BR>id = 499","<B>[500]<\/B><BR><B>{P00025442,<BR>&nbsp;&nbsp;P00237542}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.512<BR>coverage = 0.0981<BR>lift = 1.87<BR>count = 296<BR>order = 3<BR>id = 500","<B>[501]<\/B><BR><B>{P00110742,<BR>&nbsp;&nbsp;P00237542}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.522<BR>coverage = 0.0962<BR>lift = 1.9<BR>count = 296<BR>order = 3<BR>id = 501","<B>[502]<\/B><BR><B>{P00025442,<BR>&nbsp;&nbsp;P00110742}<\/B><BR>&nbsp;&nbsp; => <B>{P00237542}<\/B><BR><BR>support = 0.0502<BR>confidence = 0.419<BR>coverage = 0.12<BR>lift = 1.77<BR>count = 296<BR>order = 3<BR>id = 502","<B>[503]<\/B><BR><B>{P00025442,<BR>&nbsp;&nbsp;P00112142}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.51<BR>coverage = 0.107<BR>lift = 1.86<BR>count = 322<BR>order = 3<BR>id = 503","<B>[504]<\/B><BR><B>{P00110742,<BR>&nbsp;&nbsp;P00112142}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.483<BR>coverage = 0.113<BR>lift = 1.76<BR>count = 322<BR>order = 3<BR>id = 504","<B>[505]<\/B><BR><B>{P00025442,<BR>&nbsp;&nbsp;P00110742}<\/B><BR>&nbsp;&nbsp; => <B>{P00112142}<\/B><BR><BR>support = 0.0547<BR>confidence = 0.455<BR>coverage = 0.12<BR>lift = 1.72<BR>count = 322<BR>order = 3<BR>id = 505","<B>[506]<\/B><BR><B>{P00025442,<BR>&nbsp;&nbsp;P00057642}<\/B><BR>&nbsp;&nbsp; => <B>{P00110742}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.509<BR>coverage = 0.0991<BR>lift = 1.86<BR>count = 297<BR>order = 3<BR>id = 506","<B>[507]<\/B><BR><B>{P00057642,<BR>&nbsp;&nbsp;P00110742}<\/B><BR>&nbsp;&nbsp; => <B>{P00025442}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.498<BR>coverage = 0.101<BR>lift = 1.82<BR>count = 297<BR>order = 3<BR>id = 507","<B>[508]<\/B><BR><B>{P00025442,<BR>&nbsp;&nbsp;P00110742}<\/B><BR>&nbsp;&nbsp; => <B>{P00057642}<\/B><BR><BR>support = 0.0504<BR>confidence = 0.42<BR>coverage = 0.12<BR>lift = 1.68<BR>count = 297<BR>order = 3<BR>id = 508"],"shape":["box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","box","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle"],"x":[-0.224722482212649,0.0285897835203113,0.339231322681394,-0.0446291107759117,-0.63678879176194,0.323442832638505,-0.88405114798837,0.473398173752064,-0.580902814568773,1,-0.4730052564654,-0.794042630460431,-0.446124360785329,0.625235415395238,0.389660752562697,0.681135761813789,0.734856697663501,-0.802471554252874,0.610511742140158,0.0643005312683569,0.668763895821342,0.277640309360958,0.455640172573411,-0.305634404800056,-0.476148304395599,0.513508081608687,0.540938490499755,0.491411828917839,0.784459980927672,0.248931220913036,0.286300738395179,0.527175131864004,0.290845978714367,-0.134560752966358,-0.671974827784079,-0.749815952610938,0.19940737436151,0.711185295684071,-0.0124373341834677,0.391732191341152,0.0816370643508657,0.409789988595594,0.831169679315525,-0.00223155921114482,0.606266045712627,-0.31367241630803,0.777623877109405,0.386691568832504,0.274063760582244,0.2490399379698,-0.764703956458339,0.755717115232009,-0.77022546432887,-0.742856205151306,-0.176074997779797,0.0979243183421805,0.551880731528943,0.467158749616347,-0.16676183021509,0.801204143223639,-0.655636707160548,0.97627868176841,0.694200857119466,0.51019370847042,0.298606291463417,0.608225984889697,-0.55303203541697,-0.0867030819306304,0.135521139191065,0.692925777891727,0.802649517984614,0.649346863734977,0.510373033307767,0.312399006407677,0.543846542073664,-0.5995087099394,-0.788515120341747,0.84741451290093,-0.258167671582125,-0.841147371023414,-0.0491433910634281,0.1599921598041,-0.31200755767855,0.629518604395882,0.802213431421806,0.395840316252424,0.78224985350648,0.277404917647524,0.497547939139513,0.0557172520063882,0.00757451375835139,-0.438082463653021,-0.774503987341103,-0.713653609360981,-0.860381824034977,-0.727656544830298,0.4389249716157,0.653337575673432,0.630567827749599,-0.870397465724599,0.825356130414258,0.746066192586861,0.426979881312128,-0.86777764174925,-0.794778303086126,-0.858162107179442,-0.27436233168706,-0.780797952159589,-0.333328826476784,0.0285248306466226,-0.130382300946796,-0.46907618924771,0.756335726281834,-0.0661408221488139,-0.804192399778739,0.732228087978271,-0.115616878842091,-0.906652874740585,-0.450908064751046,-0.756019017954366,-0.791779119857968,-0.800496924468251,-0.775387208435773,0.0226771615489736,0.563534978688631,-0.805822395985005,0.822714534379349,-0.83371024874357,-0.858786082517909,0.388179060943056,-0.783527346762312,0.0449087541209325,0.752942301768588,-0.649229076969754,-0.642720280946626,0.418301812703156,-0.646091523872615,-0.625417466606791,-0.634661252828535,-0.704213715520894,-0.618042107133856,-0.59820019817746,-0.880971694125698,-0.652494075661374,-0.949307876933096,-0.685124888809024,0.624223327814703,0.454219430390052,0.424294312564126,0.851929480100518,0.923012036998206,-0.643709448048369,-0.614727766441366,-0.587067856285887,-0.963365739624338,-0.684258660644818,0.874994051568849,0.862641780507336,0.68528867964216,0.564370462526679,0.331165327769402,-0.219086783557954,0.855463651781749,0.919058068768885,-0.977989454560718,-0.677904758225798,0.729979426977776,0.284770208015459,0.237124480624956,0.329392568358577,0.262956452071517,-0.368549376789917,-0.475953481275458,0.727783074550818,0.573720325912705,0.510115054122928,-0.650569132774299,-0.880382342481231,-0.650914246963139,-0.315249259944343,-0.502256777407859,-0.634009275214443,0.464580155358876,0.930884160721765,0.950484183998948,0.906527158574478,0.784708056948858,0.878976767001096,0.787857143710494,0.849344337724888,0.673602462948105,0.689478938941236,0.604866742305263,-0.623371292326439,-0.625638117791176,-0.688808822587633,0.523688495859238,0.591486005343717,0.458822242057734,0.412017584448446,0.211529491207437,0.173918588866687,-0.246329947988612,-1,-0.694004147574066,0.818825509442217,0.645580205993643,0.835018952120568,0.856809075187173,0.761606356979287,0.800656742256264,0.641024395627623,0.648332233265225,0.565101982279109,0.569389419626095,0.41917567810883,0.608341038359639,0.470008113963353,0.413435456450687,0.251977261391444,-0.657715409047491,-0.32762557942252,-0.515922506293749,0.943710015800114,0.907448503375191,0.776530559420906,0.878345911289859,0.833940434751328,0.6910325593718,0.597516003410555,-0.0751717572427812,-0.082440374231372,0.291413425570967,-0.0416506120574484,0.323987506230969,0.44045208053971,-0.145111646995195,0.219474213522479,-0.273353103952698,-0.0791237122688045,0.109213057608541,-0.324171356635074,-0.988722471615402,-0.68875074357997,0.95360482237795,0.908574024042216,0.904908648814517,0.908555293928416,0.791931523749445,0.879596482723843,0.794618191429536,0.855151002070783,0.686643842837718,0.69273217952485,0.619715805183512,0.227961098500824,0.536156900778251,0.459393229535055,0.51458707954849,0.444651151061705,0.486212148010241,0.3613747154497,0.373201316732267,0.2898739796452,-0.0823361790845408,0.134542147405909,0.268488825449912,-0.229896069617147,0.858258686945161,0.830881540319271,0.808586169863945,0.828484280249884,0.394527939500056,0.720226340277266,0.56639085174121,0.772191087718963,0.736313671922173,0.781130579206214,0.620577802199441,0.62717049837738,0.544364255952027,0.214044259659914,0.565282367300511,0.517830928593717,0.394528029625773,0.408183179896043,0.313112581915532,-0.142982772547094,0.225049150835406,-0.272903865702131,-0.601625665815464,-0.153455354613335,0.326700082817503,-0.241977234376378,0.155262823941788,-0.228400428426795,0.322156494883242,0.122932856705929,-0.144350354893779,0.18914820906077,-0.23316722687701,-0.693673329149175,-0.702875632926776,-0.364840376172881,-0.442569751532406,-0.527608837900092,0.616642292693093,0.700525399435972,0.443756182325882,0.6253348773116,0.456121396629426,0.413414261583152,0.681382456005831,0.597225588234815,0.658755494437675,0.502240680867251,0.526913815128958,0.431619940330635,-0.0733265208506098,0.105761840216794,-0.332236771370262,0.173145415859906,0.138439670186455,0.148843691271159,0.435077271191101,0.469252031198194,0.338368953194492,0.371375411205048,0.271419517936587,0.734320488464043,0.789930544192929,0.742335040443472,0.716274107596276,0.474611817216839,0.779214797858611,0.731375444193521,0.565890733978406,0.596348316498925,0.514714207822174,0.870807467945082,0.870421221163912,0.596105046619732,0.841848919784927,0.765222737707311,0.825605043230021,0.650878057986682,0.658432966161071,0.576664488686764,-0.222598983959633,-0.0693429943689798,-0.0476875828538122,0.137818661776692,-0.157445877546064,0.129677764181775,-0.300338128777866,-0.68241872066,0.59025601478485,0.632204198807383,0.71670628010773,0.364690075405896,0.3103949540945,0.631688140189574,0.680700354021421,0.662710364598461,0.665752954336316,0.512672811980605,0.449129430043337,0.583455421499187,0.0990014296665385,0.573734982556065,0.537224622997087,0.436012007790401,0.355698075286053,-0.376686109155201,-0.383230685126596,0.0104711961881885,-0.300315259359677,-0.385112884292839,-0.13262277027189,-0.422273558580336,0.582463859282041,0.198589214612739,0.212528866145933,0.346077150774735,0.507880979581871,0.535801778454499,0.385967805459752,0.423543016367794,0.305956956364904,0.0852038343380246,-0.146232723025897,-0.012694122451367,0.00629383810801531,0.251979489847446,0.198260504517706,-0.419032362613392,-0.466315196824717,-0.421365704717833,-0.0206278754356853,-0.331495344894288,-0.416962612033599,0.266119991474633,0.505216241121819,-0.0146420039858695,0.42898003677367,0.383010072541418,0.297169746556537,0.846833665434514,0.860972232263611,0.827247293680224,0.751009073973683,0.492379773860438,0.816946993485049,0.692482205238858,0.767647214854814,0.600629949061256,0.623688205542466,0.539214088212861,0.610459145534774,0.605862100676905,0.235400915586349,0.24955931571173,0.355085651099184,0.587515095008712,0.513530239415324,0.55532567915614,0.398253841437823,0.445269266526932,0.331009309030557,0.252631757884896,0.349209912883044,0.288363311141075,-0.285227593605789,-0.0829943860436456,-0.208565633292721,0.0926084040397299,-0.345622629242803,-0.21245937597485,-0.0526054953350036,-0.0313172514595662,-0.148219930012795,0.177180395435738,0.141894100788748,-0.286780344543268,0.636931732267738,0.712464941046281,0.452468324580365,0.422273655739809,0.698169520056517,0.631679341241772,0.498948423385536,0.533475888457808,0.439458173146635,0.552005585379236,0.477525999765513,0.531609787621164,0.465868616587263,0.506036254365013,0.37966750919706,0.390876054020767,0.313606692830221,-0.0624998647446077,0.604982609404546,0.598496441809087,0.444339592493668,0.404037294382954,0.663098854303631,0.610063190574602,0.513626862462008,0.417132219090398,0.823821423009193,0.726380936383453,0.751831261995918,0.602743795659502,0.545300899209029,-0.145913559436827,-0.440765700411265,0.204831229189629,0.163230445444264,-0.257038646182948,0.33652924865238,0.390901217869922,0.307782018293373,0.625558069427917,0.486535261441686,0.435657400934935,0.376526085821477,0.302502840805574,0.508722005214315,0.695343380475067,0.299574470274771,0.631387690978281,0.467255639937622,0.674316843926525,0.653488937733052,0.671505164525462,0.529458211126944,0.530013492580322,0.436445171399517,-0.668810907085775,0.499605097784716,0.698932542832014,0.544515697334968,0.492313481765992,0.624968359117412,0.246660538724762,0.25847365287645,0.382298676359129,0.615032449469166,0.546968685031803,0.590252776242649,0.450254568332408,0.468977488783443,0.356119452562135,0.753907676195741,0.530884711645811,0.481042491307228,0.0521304595169088,-0.160895495539716,-0.0162762092194225,0.306788948300237,0.224684522024929,0.181973121579356,-0.233296414098053,0.706102039353013,0.76562033518728,0.674695609525848,0.465465833258431,0.756699289172621,0.692665363533204,0.566877759784046,0.49119392004148,0.903715721134404,0.783827791263356,0.879673241756435,0.767861147297186,0.839458288019599,0.655968802882789,0.685552283355627,0.593928356877863,-0.695401280575949,-0.672322561963715,-0.773862015993902,-0.597629486791744,-0.483334120000539,-0.572091706529343,-0.616234854854895,-0.706597352349984,0.767642770095238,0.871564877952445,0.873261410573443,0.755963033463779,0.59632183974164,0.770374112045841,0.827029767337149,0.656855011013358,0.581520145601218,0.11888097163309,-0.31151396956294,0.909355839043766,0.900686871033903,0.785599812454113,0.871327619479961,0.781086953982635,0.84205075718204,0.684521216591605,0.626534580272619,-0.653561808828523,-0.469144364601159,-0.556518804320768,0.798043897787163,0.898771418239865,0.684585058391467,0.621576298927891,-0.60941503808733,0.789156857132735,0.641011574849622,0.587695043709704,-0.527236083455459,-0.390027111950426,-0.497341681823078,0.721701401196791,0.494830880429029,0.796390208931926,0.737942816353772,0.525389489102583,0.321492924144722,0.262109802155016,-0.572113764171235,-0.450180632628358,-0.548823080897083,-0.40190930788621,0.845520841179196,0.776164521862806,0.863694303972104,0.735041351125256,0.820444340940624,0.643728775404408,0.662203991407018,0.576106062741445,0.736137692884651,0.748854870580089,0.800205760479636,0.637945143255743,0.575870940654848,-0.641889320931861,0.206920862465165,-0.39688861190159,-0.498618072067799,0.73344135183542,0.739892905559915,0.562838091115687,0.48936673228879,0.391077986864234,0.32679306830107,-0.343959398872042,0.80782970779484,0.801908222535427,0.644705446351568,0.657199446814487,-0.406083351953777,0.579253189674477,0.494443179465339,0.4827121936006,0.601091012893545,0.614914443443333,0.526204757786804,0.365165760634428,0.370061826560557,0.657428902274057,0.649158280266133,0.642880163746808,0.506145713545683,0.500506506832449,0.515194882760842,0.479513215808658,0.494825544552781,0.479716536068962,0.521104393973165,0.514909738737898,0.534311687748968],"y":[-1,-0.453644533088225,-0.0800023866079693,-0.388215458878262,-0.724087074866136,-0.546354408063092,-0.338804259198783,0.364049300311591,-0.769762093954372,-0.260472766371972,-0.793432451166802,-0.945036950088724,-0.418473121238925,-0.332815858624534,-0.249425666174476,-0.338139314353807,-0.14062979950186,-0.311016503834355,0.649194742055919,-0.678653960921293,0.0585049801403104,-0.265013630352888,0.325049787744385,-0.658458460072862,-0.753534014229479,-0.26490173080224,-0.18446964101119,0.707192245020356,0.0315406242812408,-0.636612810108066,0.664713512556738,0.705515583600619,0.304786061077597,0.140416043893464,-0.687353972985646,-0.0204016832691111,0.481581644510616,0.47915314861512,0.082026066169109,0.215260051325469,0.350827803033632,0.316000655004252,0.319611635555675,0.0986303135999567,0.345313317599963,-0.754927114760212,0.462775305112487,0.364357165439164,-0.159207234374658,-0.0343782338758376,-0.271891200492732,0.366129091869656,-0.134236191333701,-0.112420381558528,-0.357498527426462,0.371103236833049,0.342809565283538,0.885606260960248,0.049733003224208,0.0967668714957148,-0.617140205148038,-0.459084050277414,0.288397139744299,0.648013879427787,0.619944269307927,-0.6902924276961,-0.650219431091527,0.150741029132275,-0.494164766072686,-0.119491979179201,-0.126038893388632,-0.169797737379079,-0.219690152094538,-0.0155632325245121,-0.296994977647089,-0.658600431580783,-0.237070717758824,-0.421465076108931,-0.735158959032353,-0.210802001547018,-0.357007147697751,0.62895925218133,0.175802179375901,0.596645866833869,1,0.0866593718240554,-0.522665723303033,-0.520545108496086,0.614795598343013,0.500491857675996,-0.301792377963784,-0.468637302182579,-0.336111954267912,-0.0139089099266467,-0.428496715192902,-0.0646729987218716,0.178036822186135,0.442640140238503,-0.0233948222171085,-0.38850026613266,0.0531689503480448,0.172264128060217,0.875758095689162,-0.249805251650392,-0.379216636871758,-0.275985333319315,0.238170233325088,-0.608620280208741,0.195430368031828,0.280424536258236,0.106351281215023,-0.266432435279414,-0.0754578365741684,0.132132474088845,-0.502218928594209,0.225496666077077,-0.0129696892765103,-0.391663972617815,-0.793562665602754,-0.424881373105761,-0.148060173181777,-0.536534274302612,-0.0479937903174849,-0.397505447698068,0.484228375716914,-0.204994563897539,0.213888882540816,-0.155948809480227,-0.129956388748927,0.861149748348148,-0.0877024086667823,-0.209638282987072,0.195494640915783,-0.194483381416845,-0.13608127437487,0.659550960202979,-0.160191305463473,-0.354751942519801,-0.305550492605241,-0.808739363159409,-0.801883267940778,-0.484765721662386,-0.630666394784257,-0.413941523680297,-0.117791773780999,-0.183289213469386,0.576491142478734,0.583151806725901,0.475529390084145,-0.163453660392165,-0.245877511267183,-0.248911035570491,-0.753250684083396,-0.442881895458465,-0.259014297932455,-0.275107566789429,-0.279927344124627,-0.324432715236851,-0.396826902896089,-0.500869399921201,-0.14544222189345,-0.245984427064288,-0.483520872197984,-0.286994541672022,-0.451524220572569,-0.362770208649283,0.837768497247576,0.1471995585076,-0.306379520378674,0.173918309456231,0.0916742910719672,-0.970486047068171,-0.546283928602317,0.276363527279862,0.523238881525005,0.388702564720798,-0.332945594557368,-0.5963599678951,-0.391746506744476,-0.452340019453268,-0.38759241103987,-0.190682944678314,0.676450716918803,0.0680887593378694,0.0977193278934951,-0.006974476358096,-0.00212314573717942,-0.0545243636559003,0.287887671218209,0.0768612003031899,0.273642382724823,-0.0917528978070378,0.174696912499229,-0.2682692689506,-0.118999154996093,-0.254328754250885,0.849980650226059,0.601848000146883,0.61016419051974,0.506845978980754,0.254874503550259,0.15888240419264,-0.0769779417309081,-0.373821501698338,-0.303105025074683,-0.373396061572301,-0.434917552650662,0.0832517884430986,0.356680423104981,0.373603927421586,0.160861078143058,0.377315232151155,-0.0166816866654432,0.260322140508888,0.568876410055139,0.543173075301496,0.567490530321811,0.580996762880457,0.45886660687586,0.45352840921941,-0.231825942636971,-0.415986107389733,-0.354918624930767,-0.0668218634298169,-0.161165697019646,-0.145614781252376,-0.220823123149565,-0.0391384125436606,-0.261445354651744,0.0298808228783012,-0.884718071787061,-0.564107547216812,-0.484070607440011,-0.481104758126026,-0.401648971227056,0.669333316668206,-0.426397459769381,-0.312302262064716,-0.324707817175405,0.268196118430363,0.166135359326155,-0.0654170456162464,-0.416623187653195,-0.337273096633522,0.309673573685888,0.394906005394999,0.0629846841632058,0.35303530910164,0.0728185556274801,0.0134813867063284,0.366212967289509,0.132063247642477,0.361492110814392,-0.0196969132396793,0.236509859908331,0.227137878181875,-0.0752062452610754,-0.0787487151822186,-0.127999365596868,0.167943848477976,-0.0146086449704685,0.18601052811151,-0.182758122528432,0.10034796569906,-0.145507003949833,-0.580080760191821,-0.3717162281558,-0.384983871242367,0.394880362583686,0.452352962591754,0.126998879721813,0.419231798188193,0.361025336331154,0.130086447740739,0.379388756783958,0.0766677493069376,0.424501773864654,0.19206174073647,0.418602513207095,0.0196887882717518,0.298076309365827,0.211272437959411,-0.0584289493533013,0.201569142531292,0.245875332245378,-0.116903237940932,0.139459015316014,-0.453863004720186,-0.337102715068221,-0.353123607341097,-0.116227956095734,-0.504895717050993,-0.133675746239493,-0.295764404103912,-0.542273344876664,-0.599122703566826,-0.231695691811393,-0.535699012817899,-0.565033939994873,-0.0955027613151512,-0.349471713490872,-0.826063285324366,-0.679553768492516,-0.525910948977422,-0.709815172770352,-0.460881899220329,-0.37851508769435,-0.223080481388603,-0.421535672312561,-0.19674405056152,0.0538012511480157,-0.404301627276147,-0.281370903932687,0.0705452490614353,-0.103768514060783,0.0839271979445315,-0.335189989058191,-0.0101897468443569,0.305364401584731,0.193120559492653,-0.0402767389880059,0.460928565544361,0.394787034350277,0.418073206560456,0.386005571820467,0.188821814111389,0.427316242183071,0.0251519715783042,0.298360200641667,-0.363366950081638,-0.220294725851616,0.0922247359204567,-0.173080141966103,-0.403898425445448,-0.263830312591812,-0.0932361116072964,0.092646886317205,-0.321657400145018,0.0016487537666896,0.0432566287200329,0.30254764307153,0.281082136847929,0.00217448259772812,0.328434682682704,0.114647991912029,0.33218479611554,-0.0493767897942226,0.209383446675767,0.237366791877893,0.323193745658371,0.323606260219653,0.256146121730149,-0.100274307308148,0.204262451859984,-0.0392841082411198,-0.231432671188332,0.707526521244861,0.694240621258691,0.597916365860573,0.576058084044392,0.49671686748227,0.249797253275744,0.1837559802281,0.535288604535195,0.306911035565413,0.552194586264445,0.41095898792917,-0.22631133695849,-0.330745943307195,-0.271063255863587,-0.136712639533348,-0.349056743221746,-0.0321995572336307,-0.868820244655379,-0.837764924724996,-0.667745707131374,-0.806064713368502,-0.523849045028642,0.31497922549439,-0.029707251268385,0.439054453366034,0.431715629587139,0.453005354227465,0.459250842529154,0.452044543541264,0.238008816853195,0.495666860924645,0.0663553206755012,0.349299877149267,0.602705093070393,0.42655108852907,0.500142856340441,0.525804117079768,0.499516860694192,0.38228792727301,-0.888085125921757,-0.844277011132797,-0.855897386926017,-0.67371231776869,-0.811163407708926,-0.531982664460433,-0.465317740303058,-0.25249814116469,-0.485261144199099,0.0344364288641139,-0.328785234943876,-0.034715736850857,-0.11611964009741,-0.0924854506570426,-0.188467275009807,-0.151553641752977,-0.362162806200175,-0.233571052968104,0.115361463335229,-0.0516716431943818,0.120947638704198,-0.275607265824045,0.0308682279358898,0.0437493398910129,0.31663718801574,0.275791540033799,0.282883539282633,0.265093468692108,-0.0039984129032814,0.301552662821224,0.113662146892897,0.301280586859156,-0.0462257671656678,0.214344419177437,-0.438448325466582,0.0991592568259423,0.0219187578493227,0.150760020650104,0.228731921687491,-0.158931058386124,0.135122406230694,-0.0950008985008151,0.204716795411096,0.284711861396369,0.295006615545012,-0.115459140540871,0.279614145182334,0.181553699968789,-0.0555787352158044,-0.459679142374739,-0.277279734846184,-0.511294973723156,-0.490935329017606,-0.3265211585374,-0.162364916684112,0.0229371428678062,-0.395536780279756,-0.0703091327560044,-0.12227572408893,-0.121913518150941,-0.169629672400104,0.130795281724447,-0.0480642186752778,0.146001493824931,-0.228660280307707,0.0584047416901761,-0.182110806805027,-0.43288699858671,-0.211915313676603,0.00601030680213044,-0.467333257081581,-0.314242525449593,-0.148585628161121,-0.377786293254152,-0.0537027938472531,0.534728817630773,0.522473919979314,0.288649854104685,0.525206125962226,0.398249224192167,0.332236223944883,-0.0160294333511943,0.236540143687513,0.146263281942132,-0.0875944524544321,0.534248697131482,0.578193603655599,0.44953104333825,0.607742057737749,0.605896209004469,0.505428338290193,0.595172047426968,0.476356967500793,0.469972466469328,0.469953071347066,0.396631707762066,0.178650623172257,0.438410515181297,0.106001180294462,0.471476450424525,0.232606128217395,0.494594190065653,0.056032205868249,0.347282394305191,-0.202260134142267,0.529333476115443,0.351571481628336,0.575262944320418,0.451392188085529,0.435735742615469,0.391698979924336,0.410760804228848,0.428758618640898,0.094299527129186,0.44889456177726,0.220193713028117,0.480384820146428,0.0438523763441006,0.328769919261413,0.59830040414085,0.595714062048405,0.482319061111409,0.454499589663822,0.307012563203273,0.411564535567828,0.336042415805682,0.365541242494795,0.263391262045518,0.0194271820161946,-0.477392008405382,-0.285341688595076,-0.23193220459825,-0.503014015973165,-0.337949853820983,-0.181163857644784,-0.400137451341441,-0.0828216220599778,-0.0524533703004616,-0.062066246509522,-0.110404538927292,0.234384586501157,0.0317752256860506,0.222351515973538,-0.150037580128467,0.131642951300379,-0.761082724346575,-0.847402168579654,-0.733645471211766,-0.834265869331627,-0.729041526989677,-0.504276427367714,-0.178724082126962,-0.33815116690862,-0.0933194591010565,0.0657485987842161,0.331942053165855,0.0596568911431239,0.301591226860453,0.351050685517382,0.143733482232491,0.355792129299706,0.240568473408677,0.0984896140111444,-0.134807669199753,0.479291885303998,0.438148028447597,0.141353947573816,0.0885977879827913,0.431015995594408,0.191721299669643,0.415678942057351,0.30019938810445,-0.292228045714957,-0.700057441421877,-0.468127980627491,-0.0281035323617663,-0.0943754022364218,0.245626165615649,0.142197287470663,-0.149373884206089,0.270814135550816,0.506031621670057,0.384063859031732,-0.884707632922614,-0.797834498874227,-0.548638292650522,-0.226311156500532,-0.510573682208948,-0.328133700669286,-0.180747088572396,-0.0852770398505308,0.519543906706806,0.372228645199369,-0.867821302719014,-0.766028578649935,-0.531059690257594,0.00621996633261102,0.163833285267023,-0.120382541913854,-0.185718302848668,0.149723714376668,-0.0119063564813766,0.158410214083022,-0.226958662720403,0.0583511571191611,0.177933503154635,0.47646368913016,0.22809704360373,0.46684591122665,0.336213131290976,-0.446970128198616,0.314360866775311,-0.77271161254175,-0.522015924353679,-0.0287884254850728,-0.00787767757770907,0.176363785004685,0.0830110679135081,0.45745253208463,0.307991049530296,-0.334202868726333,-0.0721157039677899,-0.0552596398196329,-0.282497403610945,-0.284936094875113,-0.480014947737071,0.478534559358025,0.318279260981248,0.335807405265487,-0.117597213044449,-0.120382450707827,0.128894506798842,0.372855446872119,0.351472936670024,-0.132673185527219,-0.121619700172132,-0.109159629840544,0.175507766494626,0.163194101961914,0.168138288502808,0.352867845014876,0.350743898792759,0.320309647941706,0.220028647798238,0.207405826139551,0.213205289186014]},"edges":{"from":[121,123,130,131,120,93,35,35,35,122,122,129,129,64,64,64,10,10,77,61,61,106,106,78,78,78,66,132,132,62,62,95,95,85,50,50,50,50,119,119,98,98,98,105,115,115,92,92,53,58,60,60,60,60,60,60,60,60,60,60,51,36,104,28,28,28,28,44,44,44,7,7,87,87,116,116,116,116,116,116,116,89,89,89,89,89,82,126,13,13,71,71,71,71,71,71,71,20,20,20,69,69,103,81,81,81,111,111,111,100,100,127,127,127,127,127,127,127,127,127,127,127,74,74,74,74,74,74,74,74,74,74,2,2,2,63,63,63,63,63,63,63,63,63,63,63,63,63,86,86,86,86,86,86,4,4,4,94,91,91,91,124,124,124,124,124,124,124,67,67,67,67,67,27,27,27,27,27,27,27,27,27,27,27,27,34,34,34,33,33,33,33,33,33,33,33,72,72,72,72,72,72,72,72,72,72,102,102,102,102,102,102,102,102,102,68,68,68,68,68,68,68,80,125,125,125,125,125,125,125,125,125,125,125,15,15,15,15,15,15,79,79,79,79,79,83,83,48,48,48,48,48,48,48,48,48,90,90,90,90,90,90,46,46,46,46,46,46,22,22,22,22,22,22,70,70,70,70,70,70,70,70,70,70,70,97,97,97,97,97,97,97,97,97,97,97,49,49,49,59,59,59,59,59,114,114,114,114,114,114,114,75,75,75,75,75,75,75,75,75,3,3,3,3,3,3,3,3,3,26,26,26,26,26,26,26,26,38,38,38,38,38,109,109,39,39,39,65,65,65,32,32,32,31,31,23,57,57,57,57,57,57,57,57,57,57,128,84,84,84,84,23,23,23,23,23,23,23,23,23,23,19,19,19,110,110,110,110,110,110,110,14,14,14,14,14,14,14,14,29,29,29,29,29,29,29,29,5,5,5,5,5,5,54,118,133,133,133,133,133,133,133,133,133,117,117,43,43,43,43,43,43,43,43,18,76,76,101,101,101,101,96,47,47,47,11,11,11,16,16,16,16,16,37,37,9,9,9,107,113,113,113,113,113,113,113,113,52,52,52,52,52,108,41,25,25,99,21,99,99,42,42,55,17,21,17,73,24,45,45,40,21,73,21,8,40,21,113,73,113,21,73,8,99,40,99,8,40,8,45,40,45,8,40,8,21,21,40,8,40,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641],"to":[134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,630,631,631,632,632,633,633,634,634,635,635,636,636,637,637,638,638,639,639,640,640,641,641,112,112,8,112,112,112,9,25,112,108,112,128,112,45,8,40,99,17,112,25,112,104,112,113,17,73,73,99,112,16,113,118,112,84,42,88,8,40,1,112,21,8,40,112,108,112,55,112,112,8,29,101,113,99,17,45,21,8,73,40,112,112,112,32,45,8,40,8,40,112,118,112,17,73,113,52,45,21,8,73,40,57,42,45,8,40,40,112,55,112,101,113,99,17,21,73,40,1,55,73,55,73,8,55,73,112,41,40,112,118,112,43,47,113,52,99,17,45,21,8,73,40,37,113,99,17,45,21,8,73,40,112,88,73,112,43,47,113,52,56,99,42,17,45,21,8,73,40,56,17,45,8,73,40,55,73,112,112,24,21,112,6,25,99,88,24,40,112,12,108,55,24,112,16,113,6,99,42,88,17,45,21,8,73,40,56,40,112,37,41,56,45,21,8,73,40,16,113,52,99,88,17,21,8,73,40,113,52,42,17,45,21,8,73,40,107,41,56,42,55,40,112,112,32,19,47,37,56,99,17,45,21,8,40,113,55,17,21,73,40,11,25,88,24,112,56,112,52,41,56,42,45,21,8,73,40,37,107,41,56,8,40,11,9,25,88,24,112,6,17,24,45,73,40,29,101,113,99,88,17,45,21,8,73,40,113,52,41,56,42,17,45,21,8,73,40,30,8,40,107,41,55,40,112,107,41,56,55,8,40,112,16,113,6,88,17,21,8,73,40,113,99,17,45,21,8,73,40,112,16,99,42,88,17,21,73,40,52,45,21,8,40,56,112,8,40,112,42,8,40,45,8,40,8,40,57,52,41,99,42,17,45,21,8,73,40,112,42,21,8,40,52,41,56,42,17,45,21,8,73,40,52,8,40,37,107,56,45,8,40,112,16,113,99,88,17,21,73,40,113,99,17,45,21,8,73,40,76,9,108,25,24,112,112,112,16,113,52,99,42,45,21,8,40,40,112,47,52,99,17,45,21,8,40,112,24,112,99,17,8,40,112,21,8,40,25,24,112,99,88,17,21,40,8,40,25,24,112,112,52,99,17,45,21,8,73,40,99,45,21,8,40,112,40,24,112,21,99,8,40,8,40,112,21,17,73,17,112,8,40,45,73,21,40,40,8,73,21,113,40,8,99,40,8,45,40,8,21],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"smooth":false},"physics":{"stabilization":false},"interaction":{"hover":true,"zoomSpeed":1}},"groups":["1","2"],"width":null,"height":null,"idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);","highlight":{"enabled":true,"hoverNearest":true,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script>
```

We observe clear clusters, at least two, in the association network. This reveals distinct groups of products that exhibit strong relationships or associations with each other.

Additionally, there is clear visibility into products that receive a substantial number of incoming links or connections from other products, indicating their popularity or influence within the network.

Furthermore, certain parts of the network exhibit higher density. This identifies specific areas within the network where there is a higher concentration of connections or associations between products, suggesting stronger interconnections in those regions.


```r
# convert rules into a graph with products as nodes and rules as edges
library("igraph")
```

```
## Warning: package 'igraph' was built under R version 4.1.3
```

```
## 
## Attaching package: 'igraph'
```

```
## The following object is masked from 'package:arules':
## 
##     union
```

```
## The following objects are masked from 'package:dplyr':
## 
##     as_data_frame, groups, union
```

```
## The following objects are masked from 'package:purrr':
## 
##     compose, simplify
```

```
## The following object is masked from 'package:tidyr':
## 
##     crossing
```

```
## The following object is masked from 'package:tibble':
## 
##     as_data_frame
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
g <- associations2igraph(support_all,associationsAsNodes = FALSE)

plot(g)
```

![](img/unnamed-chunk-14-1.png)<!-- -->


```r
# Load the tidygraph library
library(tidygraph)
```

```
## Warning: package 'tidygraph' was built under R version 4.1.3
```

```
## 
## Attaching package: 'tidygraph'
```

```
## The following object is masked from 'package:igraph':
## 
##     groups
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```r
# Convert the graph object 'g' into a tidygraph object
g <- as_tbl_graph(g)

# Convert the tidygraph object 'g' into a data frame
indeices <- data.frame(g)
```


```r
# Calculate the PageRank scores for the graph 'g'
page <- page.rank(g)

# Add a new column named 'page_score' to the 'indices' data frame with rounded PageRank scores
indeices['page_score'] <- round(page$vector, 3)

# Arrange the rows in the 'indices' data frame in descending order based on the 'page_score' column
# Select the top 10 rows using the head() function
indeices %>% arrange(by = desc(page_score)) %>% head(10)
```

```
##    name     label index page_score
## 1  1017 P00110742  1017      0.175
## 2   250 P00025442   250      0.158
## 3   566 P00057642   566      0.097
## 4  2537 P00265242  2537      0.073
## 5  1031 P00112142  1031      0.067
## 6  2264 P00237542  2264      0.058
## 7  1356 P00145042  1356      0.049
## 8   459 P00046742   459      0.032
## 9  2592 P00270942  2592      0.027
## 10  570 P00058042   570      0.008
```

![](img/paste-E3279812.png)

![](img/paste-32A03B2A.png)

We can observe that:

-   Products with higher PageRank scores are more central/influential within the network structure. This indicates relatively stronger relationships and affinity with other products.

-   With a PageRank score of 17.5%, product P00110742 is identified as a highly central/influential node within the product association network inferred from transaction data.

-   Product P00025442 also has a high PageRank score of 15.8%, suggesting it is also an important node in the network.

-   Their central positions imply they have relatively stronger relationships and affinities to other products compared to less connected nodes.

-   The network topology revealed by PageRank analysis suggests there is a higher relative likelihood that customers will buy P00110742 or P00025442 if they purchase something else connected to them in the network.

-   Inspecting the network graph and the above figures, we can see P00110742 and P00025442 have many incoming edges from other nodes, providing quantitative evidence of thier strategic importance as a compelling products within the analyzed customer purchase patterns.


```r
# Calculate hub scores
hub <- hub_score(g)  

# Add hub scores to indeices dataframe
indeices['hub_score'] <- round(hub$vector,3)

# View top 10 by hub score
indeices %>% arrange(by = desc(hub_score)) %>% head(10)
```

```
##    name     label index page_score hub_score
## 1  2592 P00270942  2592      0.027     1.000
## 2  1185 P00127642  1185      0.002     0.947
## 3  1017 P00110742  1017      0.175     0.928
## 4   250 P00025442   250      0.158     0.924
## 5   683 P00070042   683      0.002     0.904
## 6  3158 P00329542  3158      0.002     0.880
## 7  2107 P00221442  2107      0.002     0.873
## 8   566 P00057642   566      0.097     0.865
## 9  1110 P00120042  1110      0.002     0.858
## 10 1314 P00140742  1314      0.002     0.855
```

We can see that:

-   Product P00270942 has the highest hub score, indicating it is strongly connected to other highly connected nodes.

-   ![](img/paste-196028D6.png)

-   P00110742 and P00025442, which had among the highest PageRank scores, also feature in the top hub rankings. This affirms their influential positions.

-   While P00270942 has a much lower PageRank than the others, its high hub score shows it plays an important connecting role.

-   The top hub products can be leveraged strategically as anchor points to influence related sections of the network.


```r
# Calculate the authority scores for the graph 'g'
authority <- authority.score(g)

# Add a new column named 'authority_score' to the 'indices' data frame with rounded authority scores
indeices['authority_score'] <- round(authority$vector, 3)

# Arrange the rows in the 'indices' data frame in descending order based on the 'authority_score' column
# Select the top 10 rows using the head() function
indeices %>% arrange(by = desc(authority_score)) %>% head(10)
```

```
##    name     label index page_score hub_score authority_score
## 1  1017 P00110742  1017      0.175     0.928           1.000
## 2   250 P00025442   250      0.158     0.924           0.862
## 3   566 P00057642   566      0.097     0.865           0.684
## 4  1031 P00112142  1031      0.067     0.601           0.575
## 5  1356 P00145042  1356      0.049     0.365           0.567
## 6   459 P00046742   459      0.032     0.202           0.517
## 7  2264 P00237542  2264      0.058     0.711           0.467
## 8  2592 P00270942  2592      0.027     1.000           0.377
## 9  1059 P00114942  1059      0.007     0.579           0.274
## 10 1019 P00110942  1019      0.005     0.301           0.233
```

We can see from the top 5 nodes by authority score:

-   Product P00110742 has the highest authority score, indicating it is strongly pointed to by influential hub products.

-   P00025442 maintains its position among top influential products based on PageRank, hub and authority scores.

-   P00057642 and P00112142 emerge as important authorities.

-   Authority scores identify products of interest specifically as targets of influential hub nodes.

-   Top authorities present opportunities for marketing based on positive associations transferred from hubs.

# Conclusions

-   Analyzing centrality, hubness and authority scores provided a more comprehensive understanding of influential products beyond any single measure.

-   P00110742 emerged as a consistently top-ranked product across different analytical methods, highlighting its strategic importance.

-   Strong mutually supportive relationships were indicated between P00110742 and P00025442 based on their high co-occurrence in mined association rules.

-   Products like P00110742, P00025442, P00057642, P00112142 showed evidence of broad associations through their involvement in multiple rules and connectivity patterns.

# Recommendations

-   Focus bundled offers and targeted ads around P00110742 as a compelling anchor product.

-   Promote P00110742-P00025442 bundles to leverage their reinforced link. Offer discounts for add-ons.

-   Leverage positive endorsements from major hub nodes to boost authority products.

-   Test sensitivity of demand and baskets to availability changes of top items.

-   Refine assortments and placements considering affiliation patterns revealed.
