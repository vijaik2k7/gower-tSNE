---
title: "Gower_tSNE"
output: html_document
author: "Vijai Kasthuri Rangan" 

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
```

### Introduction ###

The following is an illustrative example of using 'gower' distance to cluster mixed type data and using tSNE for visualizing the clusters. There are several distance measures that one can use for determing the (dis) similarity of one observation to another. The euclidean distance measure is used when we have continous variables, while 'gower' and manhattan distances are common for categorical and high dimensional variables respectively.

In the following example we use the 'college' dataset available from ISLR library to segement the 777 colleges using an unsupervised learning approach. 

Load the required libraries

```{r}

library(dplyr)
library(ISLR) #college dataset
library(cluster) #cluster analysis (PAM)
library(Rtsne) #tSNE
library(ggplot2) 

set.seed(1)
```

### Dataset ###

Using the college dataset, create buckets based on top10perc to categorize the colleges as "Elite" or "Not Elite". Any college with more than 50 top 10 percentile students in its enrollees have be considered as elite. Subset the data to only the required columns - here only name, acceptance rate, outstate, enroll, graduate rate, private indicator and Elite flags are included

```{r}
college_clean <- College %>% mutate(name=row.names(.), Acc_rate = Accept/Apps, 
                                    isElite = cut(Top10perc, breaks=c(0,50,100), labels = c("Not Elite", "Elite"), include.lowest = TRUE)) %>%
  mutate(isElite=factor(isElite)) %>% select(name, Acc_rate, Outstate, Enroll, Grad.Rate, Private, isElite)

glimpse(college_clean)


```

On exploring the data we see that the number of enrollees are right skewed and it might make sense to log transform this variable

```{r}
hist((college_clean$Enroll), 100, main="Distribution of Number Enrolled", xlab = 'Number Enrolled', ylab='Freq')
hist(log(college_clean$Enroll), 100, main="Distribution of log(Number Enrolled)", xlab = 'log(Enroll)', ylab='Freq')
```

### Distance Matrix ###

Compute the distance matrix using the 'daisy' function with the metric as 'gower'. The Gower distances are calculated for nominal, ordinal, and (a)symmetric data. All variables are standardized and scaled to a value between [0,1] and the dissimilarity between two observations with categorical variables is the weighted mean of the contributions of each variable. .

```{r}

gower_dist <- daisy(college_clean[,-1], metric = 'gower', type = list(logratio = 3))
gower_mat <- as.matrix(gower_dist)

# Output most similar pair

college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# dis similar pair

college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

```

### Clusters ###

To determine the number of clusters, we use the silhoutte width which peaks at 3. We cluster the data using PAM (partitioning around mediods). This algorithm is a k-mediods version of the well know k-means algorithm.

```{r}
# silhoutte width to determine number of clusters

sil_width <- c(NA)
for(i in 2:20){
  pam_fit <- pam(gower_dist, diss=TRUE, k=i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}


plot(1:20, sil_width, xlab = "Number of Clusters", ylab="silhoutte distance")
lines(1:20, sil_width)


pam_fit <- pam(gower_dist, diss=TRUE, k=3)
college_clean$cluster <- pam_fit$clustering

```

What do the different clusters look like? 

```{r}
pam_summary <- college_clean %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_summary$the_summary
```

We see that the first cluster is a grouping of Non elite private colleges, while the second is Elite private and the third is majorly public non elite colleges

### t-SNE ###

Plotting a tSNE (t-distributed stochastic neighbor embedding - a dimensionality reduction method (n -> 2) using probability distribution over pairs of high dimensional objects) visual of this dataset shows the three groups along with an interesting small mix of groups 2 and 3

```{r}
tsne_obj <- Rtsne(gower_dist, distance=TRUE)
tnse_data <- tsne_obj$Y %>% data.frame() %>% setNames(c("X","Y")) %>% mutate(cluster = factor(pam_fit$clustering), name = college_clean$name)

ggplot(aes(x=X, y=Y,), data=tnse_data)+geom_point(aes(color=cluster, label=name))

```


#####Inspired from http://dpmartin42.github.io/blogposts/r/cluster-mixed-types
