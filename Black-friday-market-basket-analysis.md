Black-friday-market-basket-analysis
================




# Load Data

First, we need to retrieve the data from MySQL and then check the types.


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

First, I will find the support of the two products (P00115742, and P00115842) to determine how frequently they are ordered together by the same users. This will help me understand the data better and determine the appropriate parameter space for the analysis.


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

    Based on the information provided, it seems that the absolute minimum support count is 58. This count indicates a reasonable number of occurrences for the itemset of interest. Therefore, I will use a support parameter of 0.01, as it aligns with the observed count and can be considered an appropriate threshold for capturing meaningful frequent itemsets.


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

# Take the top 10K rows ordered by support 
top_rules <- head(sort(support_all, by = "support"), 10000) 

# Convert to data frame
market_data <- as(top_rules, "data.frame") 


# Create a lift-level column  
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

![](img/3unnamed-chunk-10-1.png)<!-- -->

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
# Take the top 10K rows ordered by support 
top_rules <- head(sort(support_all, by = "support"), 1000) 

# Convert to data frame
market_data <- as(top_rules, "data.frame") 


# Create a lift-level column  
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

![](img/3unnamed-chunk-11-1.png)<!-- -->

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

-   Adjust inventory and purchasing - The product networks can help forecast demand and plan inventory by understanding which items are commonly purchased together. This ensures an adequate stock of affiliated products.

-   Identify opportunities for new products - Gaps in the product networks could indicate opportunities to introduce new products that complement existing items. The algorithms aid discovery of these network opportunities.


```r
# Create HTML widget of the graph of rules
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

![](img/paste-1A9DE0ED.png)

We observe clear clusters, at least two, in the association network. This reveals distinct groups of products that exhibit strong relationships or associations with each other.

Additionally, there is clear visibility into products that receive a substantial number of incoming links or connections from other products, indicating their popularity or influence within the network.

Furthermore, certain parts of the network exhibit higher density. This identifies specific areas within the network where there is a higher concentration of connections or associations between products, suggesting stronger interconnections in those regions.

```r
# Convert rules into a graph with products as nodes and rules as edges
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

![](img/3unnamed-chunk-14-1.png)<!-- -->


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

![](img/paste-196028D6.png)

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

-   P00025442 maintains its position among the top influential products based on PageRank, hub, and authority scores.

-   P00057642 and P00112142 emerge as important authorities.

-   Authority scores identify products of interest specifically as targets of influential hub nodes.

-   Top authorities present opportunities for marketing based on positive associations transferred from hubs.

# Conclusions

-   Analyzing centrality, hubness, and authority scores provided a more comprehensive understanding of influential products beyond any single measure.

-   P00110742 emerged as a consistently top-ranked product across different analytical methods, highlighting its strategic importance.

-   Strong mutually supportive relationships were indicated between P00110742 and P00025442 based on their high co-occurrence in mined association rules.

-   Products like P00110742, P00025442, P00057642, and P00112142 showed evidence of broad associations through their involvement in multiple rules and connectivity patterns.

# Recommendations

-   Focus bundled offers and targeted ads around P00110742 as a compelling anchor product.

-   Promote P00110742-P00025442 bundles to leverage their reinforced link. Offer discounts for add-ons.

-   Leverage positive endorsements from major hub nodes to boost authority products.

-   Test sensitivity of demand and baskets to availability changes of top items.

-   Refine assortments and placements considering affiliation patterns revealed.
