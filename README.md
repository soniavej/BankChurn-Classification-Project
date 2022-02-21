### Bank Churn Classification Project

Background:

“ABC” bank recently saw a steep decline in the number of users of their credit card. Credit cards are a good source of income for banks because of different kinds of fees charged  like annual fees, balance transfer fees, and cash advance fees, late payment fees, foreign transaction fees, and others.Customers leaving credit card services would lead bank to loss. Hence, the bank wants to analyze the data of customers and identify the customers who will leave their credit card services so that bank could improve upon those areas.As a Data scientist  I need to develop a classification model that will help the bank improve its services so that customers do not renounce their credit cards


Objective:

1.Explore and visualize the dataset.
2.To predict and forecast if the customer would stay or leave the current credit card he is having. 
3.To create a best fit model using Logistic Regression, Decision Tree and Random Forest.
4.To calculate the Accuracy, Precision and Recall for the models to decide which classification technique gives the best result.
5.As per this classification problem, focus is to build a model with maximum recall.

Some of the Exploratory Findings are:
  Attrited customers have :
      Lesser number of products   with the bank
      Higher number of inactive months in last 12 months
      Higher number of times contacted by the bank
      Lower Revolving Balance amount
      Lower total transaction amount
      Lower total transaction count
      Lower transaction count change Q4 to Q1
      Lower utilization ratio
      
      
<img src="https://user-images.githubusercontent.com/99994988/154917047-311f2d63-943a-4bbe-8b37-31a47171e3b5.png" width="400" height="400" align="centre">

<img src="https://user-images.githubusercontent.com/99994988/154917274-c2df74e6-8bf3-46de-b8cc-31c56718f938.png" width="500" height="500" align="centre">

<img src="https://user-images.githubusercontent.com/99994988/154917487-4e4822d8-69ce-478c-8ae6-68f2efa43ed9.png" width="700" height="500" align="centre">

Models used for Prediction:

Logistic Regression Model with AUC 73.4%
Decision Tree AUC 91.2%
Random Forest AUC 98.1%

<img src="https://user-images.githubusercontent.com/99994988/154925297-0562a04c-916b-46c2-af27-5f7ba5f9d580.png" width="700" height="500" align="centre">


Comapring the Accuracy, Precision, Recall, AUC and F1 Score

<img src="https://user-images.githubusercontent.com/99994988/154926061-f301b02d-9c99-4536-b285-024f25fe1f72.png" >

Predicting that customer will not leave, who actually  leaves the bank, would result in loss for the bank .If predicted correctly, bank could give some promotional offers to the customer to retain them. So, the false negatives should be minimized. So, the objective of our prediction is to maximize the Recall. Greater the Recall, lesser the chances of false negatives.Therefore, Random Forest with 99% of Recall, is the best fit model in predicting the attrition of the customers, of this dataset.



<img src="https://user-images.githubusercontent.com/99994988/154926927-b3185b5a-5413-4ee1-be44-2b037046a121.png" >

Final Predictions and steps to minimise the churn:

The Top  important features to understand customer credit card churn, are
Total Transaction Amount
Total Transaction Count
Total Count Change Q4 to Q1
Total Revolving Balance
Total Amount Change Q4 to Q1
Total Relationship Count

Steps to minimize Churn:

1.Increase Credit Limit: This will increase in Cr card spend/transaction amount
2.Cash Back Schemes:This will lead to increase Cr card usage and transaction count
3.0% interest EMI: This will encourage customers to easily  purchase higher cost products and convert the expenditure to EMI.As a result, this will increase transaction count/transaction amount and the balance will also revolve nicely
4.Increase Relationship Count: By providing various offers and schemes as stated above.







