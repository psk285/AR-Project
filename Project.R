require(readr)
require(arules)
require(plyr)

#Fetching Data from online link
priorOrderedItems <- read.csv("https://raw.githubusercontent.com/psk285/AR-Project/master/order_products__train.csv",nrows = 2000)
ProductDetails <- read.csv("https://raw.githubusercontent.com/psk285/AR-Project/master/products.csv")


#1.Frequent itemsets for products in orders dataset. You have to output
#  product names and not just product id

#Getting required first and second column and then joining both tables on column 'product_id'
#and arranging the data in ascending order and then dropping unwanted columns
priorOrderedItems <- priorOrderedItems[,1:2]
ProductDetails <- merge(priorOrderedItems, ProductDetails, by="product_id")
ProductDetails <- arrange(ProductDetails, order_id)
FinalData<-ProductDetails[,c(1,2,3)]

#Grouping Data according order_id and then converting the data to transaction class
MergedFinalData <- split(FinalData$product_name, FinalData$order_id)
MergedFinalData <- as(MergedFinalData,"transactions")

#Visualization of frequency of items from MergedFinalData
itemFrequencyPlot(MergedFinalData, topN=20,type="absolute", main="Frequency of Items")

#Mining frequent itemsets.
ItemSets <- eclat(MergedFinalData, parameter = list(supp = 0.01, maxlen = 8))
inspect(ItemSets)



#2.Association rules for products in orders dataset. You have to output 
#  product names and not just product id

# Generating rules using apriori function and sorting the data in order of confidence.
MergedFinaRules <- apriori (MergedFinalData, parameter = list(supp = 0.01, conf = 0.7))
MergedFinaRules <- sort (MergedFinaRules, by="confidence", decreasing=TRUE)
inspect(MergedFinaRules)



#3.Frequent itemsets for departments in orders dataset (i.e which
#departments have highest number of orders). You have to output
#department names and not just department id

Departments <- read.csv("https://raw.githubusercontent.com/psk285/AR-Project/master/departments.csv")
DepartmentsDetails <- ProductDetails[,c(2,3,5)]

#Merging Product Details and Department Details by department_id
DepartmentsDetails <- merge(DepartmentsDetails, Departments, by="department_id")
DepartmentsDetails <- arrange(DepartmentsDetails, order_id)
DepartmentsDetails <- DepartmentsDetails[,c(1,2,4)]

#Grouping department by order_id and then converting the object in to transaction class
DepartmentsDetails <- split(DepartmentsDetails$department, DepartmentsDetails$order_id)
DepartmentsDetails <- as(DepartmentsDetails,"transactions")

itemFrequencyPlot(DepartmentsDetails,topN=20,type="absolute", main="Frequency of Items")

FrequentDepartmentsItems <- eclat(DepartmentsDetails, parameter = list(supp = 0.001, maxlen = 8))
inspect(head(FrequentDepartmentsItems))



#4.Association rules for departments in orders dataset (e.g. frozen ->
#groceries). You have to output department names and not just
#department id

# Generating rules using apriori function and sorting the data in order of confidence.
MergedDepartmentRules <- apriori (DepartmentsDetails, parameter = list(supp = 0.01, conf = 0.7))
MergedDepartmentRules <- sort (MergedDepartmentRules, by="confidence", decreasing=TRUE)
inspect(head(MergedDepartmentRules))

