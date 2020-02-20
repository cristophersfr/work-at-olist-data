library(ggplot2)

olist_products_dataset <- read.csv("datasets/olist_products_dataset.csv")

##### Question 1 #####
# Será que nossos diferentes lojistas associados conseguem manter 
# o preço do mesmo produto sem grandes discrepâncias?

olist_order_items_dataset <- read.csv("datasets/olist_order_items_dataset.csv")

outliers <- c()

for(product.id in levels(olist_order_items_dataset$product_id)){
  # Filter the product data
  dt.per.product <- subset(olist_order_items_dataset, olist_order_items_dataset$product_id == product.id)
  
  # If it exists more than one price for this product, check it
  if(nrow(dt.per.product) > 1){
    # get the average value for this product
    avg.price <- mean(dt.per.product$price)
    # get the standard deviation for this product
    sd.price <- sd(dt.per.product$price)
    
    # check if some product has a value out of `bounds`
    product.overprices <- subset(dt.per.product, 
                                 dt.per.product$price > (avg.price + 3*sd.price) | 
                                   dt.per.product$price < (avg.price - 3*sd.price)
                                 )
    if(nrow(product.overprices) > 0){
      print(paste("Outlier identified: ", product.id))
      outliers <- append(outliers, product.id)
    }
  }
}

##### Question 2 #####
# Podemos dar os mesmos benefícios para todos os lojistas (sellers)? 
# Ou existe algum que merece destaque?

olist_sellers_dataset <- read.csv("datasets/olist_sellers_dataset.csv")

library(dplyr)

by_seller_id <- olist_order_items_dataset %>% group_by(seller_id)
sales_by_seller <- by_seller_id %>% summarise(sales = sum(price))
# Approach #1 : If the distribution is Gaussian-like
top_sellers <- sales_by_seller %>% filter(sales > mean(sales, na.rm = TRUE) + 3*sd(sales))
# Approach #2 : Approach for a more general approach
top_sellers <- sales_by_seller %>% filter(sales > quantile(sales_by_seller$sales)[4])

# This appears to have a fit-gets-rich behavior with power-law-like distribution

##### Question 3 #####
# Existe diferença no valor do frete praticado em regiões/cidades diferentes? 
# Ou podemos aplicar as mesmas regras de subsídio de frete para qualquer localidade?
olist_geolocation_dataset <- read.csv(file = "datasets/olist_geolocation_dataset.csv")
olist_customers_dataset <- read.csv(file="datasets/olist_customers_dataset.csv")
olist_orders_dataset <- read.csv(file="datasets/olist_orders_dataset.csv")

sellers.with.location <- ( olist_sellers_dataset
                           %>% inner_join(olist_geolocation_dataset, by= c("seller_zip_code_prefix"="geolocation_zip_code_prefix"))
                           ) %>% inner_join(olist_order_items_dataset) %>% select(seller_state, order_id, freight_value)

sellers.freight <- sellers.with.location %>% group_by(seller_state) %>% summarise(freight_mean = mean(freight_value), freight_sd = sd(freight_value))

customers.with.location <- ( olist_customers_dataset
                           %>% inner_join(olist_geolocation_dataset, by= c("customer_zip_code_prefix"="geolocation_zip_code_prefix"))
                           ) %>% inner_join(olist_orders_dataset) %>% inner_join(olist_order_items_dataset) %>% select(customer_city, order_id, freight_value)

customers.freight <- customers.with.location %>% group_by(customer_city) %>% summarise(freight_mean = mean(freight_value), freight_sd = sd(freight_value))

g <- ggplot(data=customer.with.location)
g <- g + geom_boxplot(mapping=aes(x=customer_state, y=freight_value))