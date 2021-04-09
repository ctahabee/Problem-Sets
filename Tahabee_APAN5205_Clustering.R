---
title: "R Notebook"
output: html_notebook
---

```{r}
setwd('~/Documents/Applied Analytics Frameworks & Methods II/Assignments/')
data = read.csv("survey.csv")
```

## SECTION 1
1. How many variables are included in this dataset?
```{r}
length(data)
```

2. We will cluster the data based on the eleven fast-food restaurant characteristics that respondents rated the importance of. These are the first eleven variables in the dataset. So, subset the data to only include the first eleven columns of data. Call this data_cluster.

For the first two sections of this assignment, we will only work with this reduced dataset. 
How many variables does data_cluster have?
```{r}
data_cluster = data[1:11]
length(data_cluster)
```

3. Cluster analysis is particularly sensitive to missing values. Let us see if this dataset has any missing values. How many missing values are there for the variable cleanliness?
```{r}
sum(is.na(data_cluster$cleanliness))
```

4. How many rows of data would be left if rows corresponding to missing values on any of the eleven variables were removed? (Hint: You can use na.omit() but don't overwrite the original data)
```{r}
nrow(na.omit(data_cluster))
```

5. Let us impute the missing values. There are many packages and functions in R to use for imputation. We are going to make use of the mice package with the default method, predictive mean matching. To do this, run the following code. (Setting the seed is critical to getting consistent results).

install.packages('mice') # install mice package if you don't have it
```{r}
library(mice)
set.seed(1706)
data_cluster = complete(mice(data_cluster, use.matcher=T))
```

What is the imputed value of observation 10 for the variable cleanliness?
```{r}
data_cluster$cleanliness[10]
```

6. Cluster analysis is sensitive to the scale of the variables, so we are going to standardize the variables. Use the scale() function as illustrated here:
```{r}
my_data = scale(data_cluster$cleanliness)
data_cluster = scale(data_cluster)
```
Note that scale() returns a matrix.

What is the value of observation 10 for the variable cleanliness?
```{r}
my_data[10]
```

## SECTION 2
1. Compute the Euclidean distance between all observations in data_cluster. How many elements are in the distance matrix?
```{r}
d = dist(x = scale(data_cluster),method = 'euclidean') 
length(d)
```

2. Conduct a Hierarchical cluster analysis using the method, 'ward.D2'. Plot the dendrogram from this process. Let us see how well the dendrogram matches true distances. What is the Cophenetic correlation coefficient?
```{r}
clusters = hclust(d,method = "ward.D2")
cor(cophenetic(clusters),d)
```

3. Based on the distances shown in the dendrogram alone, which is the best cluster solution?
```{r}
plot(clusters)
# 2 cluster solution
```

4. If one decided to go with a two-cluster solution, how many observations would be in the smaller of the two clusters? (Save the cluster memberships in an object as you will need it in a later question)
```{r}
h_segments_2 = cutree(tree = clusters,k=2)
table(h_segments_2)
```

5. If one decided to go with a three-cluster solution, how many observations would be in the smallest of the three clusters? (Save the cluster memberships in an object as you will need it in a later question)
```{r}
h_segments_3 = cutree(tree = clusters,k=3)
table(h_segments_3)
```

6. Next, we will cluster the data using k-means. Conduct k-means clustering to generate a two-cluster solution.  Since the choice of initial solution in k-means has a random element, use a seed of 1706. Set max iterations to 100. Do not set nstart. (Save the cluster memberships in an object as you will need it in a later question). How many observations are in the smaller cluster?
```{r}
set.seed(1706)
km2 = kmeans(x = data_cluster,centers = 2,iter.max=100)
table(km2$cluster)
```

7. Run another k-means clustering, but this time to generate a three-cluster solution. As above, use a seed of 1706 and set max iterations to 100. Do not set nstart. (Save the cluster memberships in an object as you will need it in a later question)
How many observations are in the smallest cluster?
```{r}
set.seed(1706)
km3 = kmeans(x = data_cluster,centers = 3,iter.max=100)
table(km3$cluster)
```

8. In the above k-means analyses, we set the number of clusters expected. Now, let us examine a data driven approach to determining the number of clusters. Compute the total within cluster sum of squares for cluster solutions from 2 to 10. Use a seed of 1706. Do not set nstart. What is the total within cluster sum of squares for a three-cluster solution?
```{r}
within_ss = sapply(1:10,FUN = function(x){
  set.seed(1706)
  kmeans(x = data_cluster,centers = x,iter.max = 100)$tot.withinss})

within_ss[3]
```

9. For the three-cluster solution, what is the ratio of between sum of squares and total sum of squares? (Express as a decimal.)
```{r}
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(1706)
  km = kmeans(x = data_cluster,centers = x,iter.max = 100)
  km$betweenss/km$totss} )

ratio_ss[3]
```

10. Construct a line graph of clusters (on x-axis) against total within cluster sum of squares (on y-axis). Based on this chart, which of the following are good cluster solutions?
```{r}
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#2 and 3 cause the most drastic change in steepness
```

11. Next, let us examine the Silhouette method, another data driven approach to choosing number of clusters. What is the average silhouette width for a 2 cluster solution? Use pam() from library(cluster) to compute silhouette width.
```{r}
library(cluster)
pam(data_cluster,k = 2)$silinfo$avg.width
```

12. What is the average silhouette width for a 3 cluster solution?
```{r}
pam(data_cluster,k = 3)$silinfo$avg.width
```

13. Examine average silhouette width for other cluster solutions. Based on this criterion, which is the best cluster solution?
```{r}
silhoette_width = sapply(2:10,
                         FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

#2 cluster solution cause highest average silouhette width
```


Now, we will make use of a Model-based clustering technique. Use Mclust() from library(mclust) to cluster the data. How many clusters has the model decided to group the data into?
```{r}
library(mclust)
clusters_mclust = Mclust(data_cluster)
summary(clusters_mclust)
```

Now, use model-based clustering to force a two-cluster solution. (Save the cluster memberships in an object as you will need it in a later question). How many observations are in the smaller cluster?
```{r}
clusters_mclust_2 = Mclust(data_cluster,G=2)
summary(clusters_mclust_2)
```

Now, let us compare the two-cluster solutions obtained from hierarchical cluster to k-means. Specifically, compare the cluster assignments for hierarchical cluster analysis to k-means for the two-cluster solution. For how many observations do the cluster assignments differ?

Note: each clustering method will assign group labels (1 or 2) to each data point.  Which label is applied is somewhat arbitrary.  Imagine that k-means assigned 90% of the data points to group 1 and 10% to group 2, and that the hierarchical cluster flipped the labels.  In this case, it would be reasonable to say that the group 1 for k-means exactly matched the group 2 for the hierarchical cluster, and likewise the group 2 for k-means exactly matched the group 1 for the hierarchical cluster.  Since we don't know whether the labeling will be consistent, use the following rule: 

The number of cases that differ will be the minimum of a) the number of cases that match with the original labels and b) the number of cases that do not match with the original labels.
```{r}
tab_m_segments_2 = table(clusters_mclust_2$classification)
tab_k_segments_2 = table(km2$cluster)
tab_h_segments_2 = table(h_segments_2)
tab_h_segments_2
tab_k_segments_2
```

Now, compare the two-cluster solutions for k-means to Model-based clustering. Specifically, compare the cluster assignments for k-means to Model-based clustering. For how many observations do the cluster assignments differ?

Note:  As in the previous question, use the following rule:

The number of cases that differ will be the minimum of a) the number of cases that match with the original labels and b) the number of cases that do not match with the original labels.
```{r}
tab_k_segments_2
tab_m_segments_2
```
```{r}
171-43
579-451
```

## Section 3 
Based on the previous section, it must be obvious that with cluster analysis, multiple solutions are possible. Choice of the optimal number of clusters and clustering technique should be based not only on data-driven metrics but also understanding of the domain and the problem at hand.

Having looked at the sizes of the clusters, the marketer felt the smaller segment in the two-cluster solution was not actionable because there too few customers. On the other hand, a three-cluster solution seemed more promising because it had more balanced segment sizes. Since the cluster selection methods also offered some support for a three-cluster solution, we will use three clusters. We will use k-means clustering with three clusters. Use a seed of 1706 and set maximum iterations to 100. Do not set nstart.

Next, we will examine the profile of the resulting clusters or segments in terms of clustering variables and demographics. To do so, combine the cluster memberships from three-cluster k-means with the original dataset, the one with not only clustering variables but also demographic variables. You can use cbind() to combine.
```{r}
clusters_mclust_3 = Mclust(data_cluster,G=3)
k_segments_3 = km3$cluster
m_segments_3 = clusters_mclust_3$classification
data_k_3 = cbind(data,k_segments_3)
```

1. Compared to other clusters, Cluster 1 has the lowest value for:
```{r}
library(dplyr); library(ggplot2); library(tidyr)
data_k_3 %>%
  select(speed_of_service:taste_burgers,k_segments_3)%>%
  group_by(k_segments_3)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,speed_of_service:taste_burgers)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments_3)))+
  geom_col(position='dodge')+
  coord_flip()
```

2. Compared to other clusters, Cluster 2 has the highest value for:
(Refer to graph in 2.)

3. Compared to other clusters, Cluster 3 has the highest value for:
(Refer to graph in 2.)

4. Compared to other clusters, Cluster 3 has the lowest value for:
(Refer to graph in 2.)

5. Now, let us understand the demographic makeup of the customers that belong to each cluster or market segment. To examine distributions of factor demographic variables, you could use the table function. E.g.,
```{r}
table(data_k_3$k_segments_3,data_k_3$dollars_avg_meal)
```

Since segment sizes are different, you could examine percentage of each group in the segment by using prop.table as illustrated here:
```{r}
round(prop.table(table(data_k_3$k_segments_3,data_k_3$dollars_avg_meal),1),2)*100
```

7-8
```{r}
library(RColorBrewer)
lapply(12:21,function(x) {
  dat = round(prop.table(table(data_k_3$k_segments_3,data_k_3[,x]),1),2)*100
dat = data.frame(dat)
ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
})
```

