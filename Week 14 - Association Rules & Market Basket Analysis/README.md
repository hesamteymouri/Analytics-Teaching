# README — Association Rules and Market Basket Analysis

## Overview

This week introduces association rule mining and market basket analysis. These techniques help firms understand which products are frequently purchased together.

Using retail transaction data, students will learn how to identify relationships between products and generate association rules that support cross-selling and recommendation strategies.

Students will learn how to:

- Understand support, confidence, and lift
- Apply the Apriori algorithm
- Discover product associations in retail transactions
- Interpret results for marketing strategy


## Dataset

This exercise uses the Online Retail dataset, which contains transactional data from a UK-based online retailer between December 2010 and December 2011.

Each invoice represents a basket of purchased products. By analyzing these baskets, we can identify products that tend to be purchased together.


## Key Variables

InvoiceNo: Invoice number representing a transaction  
Description: Product description  
Quantity: Quantity purchased  
UnitPrice: Price per unit  
CustomerID: Customer identifier  


## Dataset Source

Online Retail Dataset  
UCI Machine Learning Repository

https://archive.ics.uci.edu/dataset/352/online+retail



## Note for Students

The script demonstrates how to convert transaction data into basket format and apply the Apriori algorithm to discover association rules.
