---
title: "R Notebook"
output: html_notebook
---
# Assignment 3 


## Packages & datasets
```{r}
setwd("~/Documents/Applied Analytics Frameworks & Methods II/Assignment Data Sets/")
library(arules)
library(dplyr)
library(ggplot2)
library(recommenderlab)
ratings = read.csv("product_ratings.csv")
data(Groceries)
```


## Section 1


### 1. Load the dataset called Groceries that accompanies the arules package by executing data(Groceries). How many transactions does the transactions dataset Groceries contain?

```{r}
dim(Groceries)[1]
```

### 2. Which of the following are among the top 5 (five) frequently occurring items in the dataset?

```{r}
summary(Groceries)
```

### 3. Run a market basket analysis to generate a set of rules with the following parameters: support=0.01 and confidence=0.01. How many rules were generated?

```{r}
rules_1 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01))
summary(rules_1)
```

### 4. Now, repeat the above market basket analysis but with support=0.001 and confidence=0.001. How many rules were generated?

```{r}
rules_2 = apriori(Groceries,parameter=list(support=0.001,confidence=0.001))
summary(rules_2)
```

### 5.Let us go back to the first market basket analysis with support of 0.01 and confidence of 0.01. Among the rules generated, what is the value of the largest lift? Type in the value of the largest lift.

```{r}
x1 = inspect(rules_1)
x1 = x1[x1$count!=0,]
x1[order(x1$lift,x1$support, decreasing = T),]
```

### 6. In a grocery store setting, rules with just two items are more actionable than ones that have many items. For instance, cross-promotions (e.g., when you buy a carton of Brand A milk, take 50 cents of Brand X bread) or retail merchandising decisions (e.g., place Brand A milk close to Brand X bread) are easier to implement for two-item sets. Therefore, now you will generate a list of rules for only two-items. Keep support and confidence at 0.01. How many two-item rules are created? Note, a rule with only one-item should not be included.

```{r}
rules_3 = apriori(Groceries,parameter=list(support=0.01,confidence=0.01,minlen=2,maxlen=2))
summary(rules_3)
x2 = inspect(rules_3)
x2 = x2[x2$count!=0,]
```

### 7. Which of the following rules created from the analysis in the previous question have the highest confidence?

```{r}
x2[order(x2$confidence, decreasing = T),]
```

### 8. What is support for the rule, soda => whole milk?

```{r}
x2_sorted = x2[order(x2$confidence, decreasing = T),]
```


### 9. The store manager contends that the support between soda and whole milk is not of much value, because most people buy whole milk.  She goes on to say the association between soda and whole milk is weak.  Is she correct?

## YES

### 10. A shopper just picked up yogurt while shopping for groceries. What is the most likely item the shopper will also buy from the grocery store? Assume the shopper only buys two items on this shopping trip.

## WHOLE MILK

## Section 2 


### 1. Read in the data, product_ratings_data.csv   download using read.csv(). What format is the data in?

```{r}
ratings
```

### Long

### 2. From the imported data, create a realRatingMatrix object called, ratings_matrix. How many ratings does the realRatingMatrix contain?

```{r}
ratings_matrix = as(ratings,Class = 'realRatingMatrix')
as(ratings_matrix,'matrix')
ratings_matrix
```

### 3. What rating did u10023 give to prod_14?

```{r}
as(ratings_matrix,'matrix')['u10023','prod_14']
```

### 4. Now, let us split the data into a train sample and a test sample. We will use the sample() function with a seed of 1031 to create a train sample with 90% of the data. How many rows are in the train sample? Answer the remaining questions in this section using the train sample.

```{r}
set.seed(1031)
split = sample(nrow(ratings_matrix),size = 0.9*nrow(ratings_matrix))
train = ratings_matrix[split,]
test = ratings_matrix[-split,]
```

### 5. How many products (prod) did user 20150 (u20150) rate?

```{r}
nratings(ratings_matrix['u20150',])
```

### 6. How many user ratings did product 25 (prod_25) receive?

```{r}
nratings(train[,'prod_25'])
```

### 7. What is the most common rating in the train sample?

```{r}
getRatings(train)
ggplot(data=data.frame(ratings = rowMeans(train)),aes(x=ratings))+
  geom_histogram(fill='sienna')
```

### 8. What is the average rating for product 100 (prod_100) in the train sample?

```{r}
mean(getRatings(train[,'prod_100']))
```

### 9. Now, normalize user ratings using the normalize() function from recommenderLab. Use the defaults of method='center' and row=TRUE. What is the average rating for product 100 (prod_100) in the normalized train sample?

```{r}
mean(getRatings(normalize(train, method='center')[,'prod_100']))
```

### 10. Using the normalized user ratings generated above, assess the cosine similarity between the first five users in the train dataset (u395, u21174, u9881, u18449, u8926). Which of the following pairs is most similar?

```{r}
similarity(normalize(train)[1:5,],method = 'euclidean')
```


## Section 3

### 1. Construct a user-based collaborative filtering recommender using the train data. Use defaults for the parameters in the Recommender function. Based on this recommender, which of the following are in the list of top 5 recommended products for u10139? (Note: u10139 is in the test data).

```{r}
recom_ubcf = Recommender(train, method='UBCF')
pred_ubcf_topN = predict(recom_ubcf,newdata=test,method='topNList',n=5)
getList(pred_ubcf_topN)['u10139']
```

### 2. Based on the recommender created above, what is the predicted rating of product 1 (prod_1) by user 10139 (u10139)?

```{r}
pred_ubcf = predict(recom_ubcf,newdata=test,type='ratings')
as(pred_ubcf,'matrix')['u10139','prod_1'] 
```

### 3. Construct an item-based collaborative filtering recommender using train data. Use defaults for the parameters in the Recommender function. Based on this recommender, which of the following are in the list of top 5 recommended products for u10139?

```{r}
recom_ibcf = Recommender(train, method='IBCF')
recom_ibcf
pred_ibcf_topN = predict(recom_ibcf,newdata=test,method='topNList',n=5)
getList(pred_ibcf_topN)['u10139']
```

### 4. Based on the recommender created in the previous question, what is the predicted rating of product 1 (prod_1) by user 10139 (u10139)?

```{r}
pred_ibcf = predict(recom_ibcf,newdata=test,type='ratings')
as(pred_ibcf,'matrix')['u10139','prod_1']
```

### 5. The recommenderlab library offers a useful framework to cross-validate and evaluate recommenders. Here, we are going to create an evaluation scheme using ratings_matrix, the realRatingMatrix that we had before splitting into a train and test dataset. The evaluationScheme() function below will do a 80:20 split on the data, placing 80% of the data in the train sample. And, we will give the recommender algorithm 30 items from the test set and hold out the other items for computing the error. As with any random sampling operation, it is important to set the seed right (1031) before creating the evaluationScheme. Now, evaluate accuracy of an item-based collaborative filtering recommender using defaults.

```{r}
set.seed(1031)
es = evaluationScheme(ratings_matrix,method='split',train = 0.8, given=30)
recom = Recommender(getData(es,'train'),method='IBCF')
pred_ibcf = predict(recom,newdata = getData(es,'known'),type='ratings')
accuracy_ibcf = calcPredictionAccuracy(x = pred_ibcf,data = getData(es,'unknown')); accuracy_ibcf
```

### 6. Now, evaluate the accuracy of the user-based collaborative filtering recommender using defaults. To do so, modify the code in the previous question. Note, there is no need to recreate the evaluation scheme. What is the RMSE for the user-based collaborative filtering recommender?

```{r}
set.seed(1031)
recom2 = Recommender(getData(es,'train'),method='UBCF')
pred_ubcf = predict(recom2,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf = calcPredictionAccuracy(x = pred_ubcf,data = getData(es,'unknown')); accuracy_ubcf
```

### 7. Next, evaluate the accuracy of another user-based collaborative filtering recommender, with just one change from the previous question. Set the parameter nn to 100. To learn more about the default nn, run:

##recommenderRegistry$get_entries()$UBCF_realRatingMatrix.

##What is the RMSE for this modified user-based collaborative filtering recommender?

```{r}
set.seed(1031)
recom3 = Recommender(getData(es,'train'),method='UBCF', parameter = list(nn=100))
pred_ubcf2 = predict(recom3,newdata = getData(es,'known'),type='ratings')
accuracy_ubcf2 = calcPredictionAccuracy(x = pred_ubcf2,data = getData(es,'unknown')); accuracy_ubcf2
```

### 8. RMSE for popularity-based filtering recommender?

```{r}
set.seed(1031)
recom4 = Recommender(getData(es,'train'),method='POPULAR')
pred_popular = predict(recom4,newdata = getData(es,'known'),type='ratings')
accuracy_popular = calcPredictionAccuracy(x = pred_popular,data = getData(es,'unknown')); accuracy_popular
```

