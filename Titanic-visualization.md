Data visualization using the Titanic Data
================

We will attempt to make visualizations of the famous titanic data. We
will be using the `tidyverse` for data manupulation and visualization.
An almost similar version of the data used here can be found at
[Download](https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/problem12.html)

``` r
# Setting the working directory
setwd("C:/Users/Admin/Documents/Dell E6320/R  Datasets")
# Loading the libraries
library(tidyverse)
library(extrafont)
library(extrafontdb)


# Loading the data 
titanic_data <- read_csv("titanic_data.csv")
head(titanic_data)
```

``` 
# A tibble: 6 x 7
  PassengerId Survived Pclass Sex      Age  Fare Embarked
        <dbl>    <dbl>  <dbl> <chr>  <dbl> <dbl> <chr>   
1           1        0      3 male      22  7.25 S       
2           2        1      1 female    38 71.3  C       
3           3        1      3 female    26  7.92 S       
4           4        1      1 female    35 53.1  S       
5           5        0      3 male      35  8.05 S       
6           6        0      3 male      NA  8.46 Q       
```

The column `PassengerId` can be removed. We also need to convert the
`Survived`,`Sex` and `Pclass` to factors. `Survived` is coded in 0s and
1s. Os for died and 1s for survived. We can recode `Survived`
appropritely.

``` r
titanic_data <- titanic_data %>% 
  select(-1) %>% 
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("Died", "Survived")),
         Sex = factor(Sex),
         Pclass = factor(Pclass))

head(titanic_data)
```

``` 
# A tibble: 6 x 6
  Survived Pclass Sex      Age  Fare Embarked
  <fct>    <fct>  <fct>  <dbl> <dbl> <chr>   
1 Died     3      male      22  7.25 S       
2 Survived 1      female    38 71.3  C       
3 Survived 3      female    26  7.92 S       
4 Survived 1      female    35 53.1  S       
5 Died     3      male      35  8.05 S       
6 Died     3      male      NA  8.46 Q       
```

We may want to visualize the overall survival rates. We can do this
using tables or graphs.

``` r
# Table
table(titanic_data$Survived)
```

``` 

    Died Survived 
     549      342 
```

``` r
# Graph
titanic_data %>% 
  ggplot(aes(x = Survived)) +
  geom_bar(width = 0.4) +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold")
  ) +
  labs(title = "Overall Survival Rates", x = NULL, y = "Passenger count")
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

A graph has a good visual impression compared to the table. Surival
chances were low based on the graph.

We might want to visualize how the survival rates varied by sex. Did
more males or females survive?

``` r
titanic_data %>% 
  ggplot(aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.4) +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Sex", x = NULL, y = "Passenger count")
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Based on the above plot, females had about 70% survival chance while
males had about 30% survival chance.

Next, we investigate the survival rate by the passenger class.

``` r
titanic_data %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(width = 0.4) +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Passenger Class", x = NULL, y = "Passenger count")
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

We see that passengers in class 1, 2, 3 had about 70%, 45% and 30%
survival chance respectively.

We might also be interested in visualizing how survival rates varied by
age. We will use a `histogram` and a `boxplot`.

``` r
# Histogram
titanic_data %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_histogram() +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Age")
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Children less than 5 years had higher survival chances. Passengers aged
20-40 were more likely to die. Passengers aged about 65 - 75 had an
almost 0 survival chance. One passenger aged 80 years survived.

``` r
# Boxplot
titanic_data %>% 
  ggplot(aes(x = Survived, y = Age)) +
  geom_boxplot() +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Age", x = NULL)
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Passengers who survived seems to have a lower median age.

Finally, we will investigate how different variables intercted with each
other and with the survival rate.

#### Survival rates by sex and Pclass

``` r
titanic_data %>% 
  ggplot(aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.4) +
  facet_wrap(~ Pclass) +
  theme_test() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates Sex and Passenger class", x = NULL, y = "Passenger Count")
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Females in 1,2 and 3 class had 90%,90% and 50% survival chances
respectively comapred to 45%, 20% and 20% respectively for their males
counterparts.

#### Survival rates by Age. Sex and Passenger Class

``` r
titanic_data %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_histogram() +
  facet_wrap(~Sex + Pclass) +
  theme_test() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman", face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates Age, Sex and Passenger class")
```

![](Titanic-visualization_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### Conclusion

Graphs are powerful visualization tools. In the titanic data, age, sex
and passenger class were important predictors of survival. Identifying
such predictors is important for predictive modelling.
