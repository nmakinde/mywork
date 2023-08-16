# -*- coding: utf-8 -*-
"""
Created on Sun Aug 13 07:42:45 2023

@author: personal
"""

import pandas as pd

file_path = 'C:/Users/personal/Desktop/Personal/customers.csv'  
customers_data = pd.read_csv(file_path)

file_path1 = 'C:/Users/personal/Desktop/Personal/products.csv'  
products_data = pd.read_csv(file_path1)

file_path2 = 'C:/Users/personal/Desktop/Personal/purchases.csv'  
purchases_data = pd.read_csv(file_path2)

# Display the first few rows of the DataFrame
print(customers_data.head())

print(products_data.head())

print(purchases_data.head())

# Display basic statistics about numerical columns using describe()
customers_data.describe()

products_data.describe()

purchases_data.describe()

# Display summary information using the .info() method
customers_data.info() 

products_data.info()

purchases_data.info()

import re

# Extract the numeric value within [] and convert to int64 for product_id in purchase data
purchases_data['product_id'] = purchases_data['product_id'].apply(lambda x: int(re.search(r'\[(\d+)\]', x).group(1)))

purchases_data.info()

#To create a table that for transaction date
#sales = Join the product table with the purchase table using the inner join method.

# Perform inner join based on the 'ID' column
sales = pd.merge(purchases_data, products_data, on='product_id', how='inner')

print(sales)

#To sort the sales table in ascending order
# Sort the DataFrame by the 'customer_id' column in ascending order

sales_sorted = sales.sort_values(by='customer_id', ascending=True)

print(sales_sorted)

#To have a column for total transaction(Total transaction by product)

# Calculate the total transaction column
sales_sorted['total_transaction'] = sales_sorted['quantity'] * sales_sorted['amount']

# Display the updated DataFrame
print(sales_sorted)


#To have daily count of products sold by product_name

# Convert 'purchase_date' column to datetime
sales_sorted['purchase_date'] = pd.to_datetime(sales_sorted['purchase_date'])

# Extract date from 'purchase_date' column
sales_sorted['purchase_day'] = sales_sorted['purchase_date'].dt.date

# Group by 'purchase_day' and 'product_name', then calculate the sum of 'quantity'
daily_product_count = sales_sorted.groupby(['purchase_day', 'product_name'])['quantity'].sum().reset_index()

# Merge the daily_product_count DataFrame back into the sales_sorted DataFrame
sales_daily = pd.merge(sales_sorted, daily_product_count, on=['purchase_day', 'product_name'], how='left', suffixes=('', '_daily_count'))

# Display the updated sales_sorted DataFrame
print(sales_daily)

#To show customer activities based on the products

# Merge sales_daily data with customer data based on 'customer_id'
sales_customer = pd.merge(customers_data, sales_daily, on='customer_id', how='left')

# Display the merged data
print(sales_customer)

#To display the final table for Zobiatics,the index and the purchase date column will be remove to avoid duplication since there is a new column for purchase day.

# Columns to remove
columns_to_remove = [ 'purchase_date']  

# Drop the specified columns from the DataFrame
cleaned_sales = sales_customer.drop(columns_to_remove, axis=1)

# Display the DataFrame after dropping the columns
print(cleaned_sales)


#To transfer data into Duckdb datawarehouse


import duckdb

# Connect to DuckDB
con = duckdb.connect(database='sales_database')


# Load DataFrame into DuckDB as a table
cleaned_sales = pd.DataFrame(cleaned_sales)  # Define or load your DataFrame
con.register('cleaned_sales_table', cleaned_sales)

# Query the registered table in DuckDB
result = con.execute('SELECT * FROM cleaned_sales_table')


# Fetch and iterate through the result set
while True:
    row = result.fetchone()
    if row is None:
        break
    print(row)

# Close connection
con.close()


