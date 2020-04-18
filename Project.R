require(readr)
require(arules)
require(plyr)

#Fetching Data from Github
priorOrderedItemset <- read.csv("https://raw.githubusercontent.com/psk285/AR-Project/master/order_products__train.csv",nrows = 2000)
ProdDetails <- read.csv("https://raw.githubusercontent.com/psk285/AR-Project/master/products.csv")


#1.Frequent itemsets for products in orders dataset. You have to output product names and not just product id

#Getting required first and second column in ProdDetails df and then joining both tables on column 'product_id' and manipulating the data in ascending order and then dropping unwanted columns
priorOrderedItemset <- priorOrderedItemset[,1:2]
ProdDetails <- merge(priorOrderedItemset, ProdDetails, by="product_id")
ProdDetails <- arrange(ProdDetails, order_id)
Final<-ProdDetails[,c(1,2,3)]

#Grouping Data according order_id and then converting the data to transaction class
MergedFinal <- split(Final$product_name, Final$order_id)
MergedFinal <- as(MergedFinal,"transactions")

#Visualization of frequency of items from MergedFinalData
itemFrequencyPlot(MergedFinal, topN=25,type="absolute", main="Freq. of Items")

#Mining frequent itemsets.
ItemSets <- eclat(MergedFinal, parameter = list(supp = 0.01, maxlen = 8))
inspect(ItemSets)



#2.Association rules for products in orders dataset. You have to output 
#  product names and not just product id

# Generating rules using apriori function and sorting the data in order of confidence.
MergedFinalRules <- apriori (MergedFinal, parameter = list(supp = 0.01, conf = 0.8))
MergedFinalRules <- sort (MergedFinalRules, by="confidence", decreasing=TRUE)
inspect(MergedFinalRules)



#3.Frequent itemsets for departments in orders dataset (i.e which
#departments have highest number of orders). You have to output
#department names and not just department id

Deps <- read.csv("https://raw.githubusercontent.com/psk285/AR-Project/master/departments.csv")
DepsDetails <- ProdDetails[,c(2,3,5)]

#Merging Product Details and Department Details by department_id
DepsDetails <- merge(DepsDetails, Deps, by="department_id")
DepsDetails <- arrange(DepsDetails, order_id)
DepsDetails <- DepsDetails[,c(1,2,4)]

#Grouping department by order_id and then converting the object in to transaction class
DepsDetails <- split(DepsDetails$department, DepsDetails$order_id)
DepsDetails <- as(DepsDetails,"transactions")

itemFrequencyPlot(DepsDetails,topN=20,type="absolute", main="Frequency of Items")

FrequentDepsItems <- eclat(DepsDetails, parameter = list(supp = 0.01, maxlen = 8))
inspect(head(FrequentDepsItems))



#4.Association rules for departments in orders dataset (e.g. frozen ->groceries). You have to output department names and not just department id

# Generating rules using apriori function and sorting the data in order of confidence.
MergedDeptRules <- apriori (DepsDetails, parameter = list(supp = 0.01, conf = 0.8))
MergedDeptRules <- sort (MergedDeptRules, by="confidence", decreasing=TRUE)
inspect(head(MergedDeptRules))
