#set working directory
setwd("D:/GitHub/Market_Basket_Analysis_Grocery")

#read the excel data
library(readxl)
grocery_data=read.csv2("input/Groceries_dataset.csv",header = TRUE, sep = ',')

#removes NAs or missing values from a data frame
#df_groceries <- df_groceries[complete.cases(retail), ]


df_sorted <- df_groceries[order(df_groceries$Member_number),]
df_sorted$Member_number <- as.numeric(df_sorted$Member_number)

# baket Analysis Stated
#colnames(retail)
# "Member_number"   "Date"            "itemDescription"


library(plyr)
#library(dplyr)
df_itemList <- ddply(df_groceries,c("Member_number","Date"), function(df1)paste(df1$itemDescription,collapse = ","))
df_itemList$Member_number <- NULL
df_itemList$Date <- NULL


colnames(df_itemList) <- c("itemList")

write.csv2(df_itemList,"output/market_basket.csv", quote = FALSE, row.names = TRUE)


txn = read.transactions('output/market_basket.csv',rm.duplicates = TRUE, format='basket',sep=',')
inspect(txn);

library(arules)
basket_rules <- apriori(txn,parameter = list(sup = 0.001, conf = 0.01,maxlen=10));

rules <- sort(basket_rules, by='lift', decreasing = TRUE)


#summary(rules)
inspect(rules)
topRules <- rules[1:50]
library(grid)
library(arulesViz)
#plot(topRules)
plot(topRules,method="graph",interactive=TRUE,shading=NA)

write.csv(rules,"rules_generated.csv", quote = FALSE, row.names = TRUE)

plot(topRules,method="graph",interactive=FALSE,shading=NA)

arulesViz::plotly_arules(rules)

plot(rules[1:20],method = "matrix",control = list(reorder = TRUE))

p <- plotly_arules(rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html")

inspectDT(rules)



