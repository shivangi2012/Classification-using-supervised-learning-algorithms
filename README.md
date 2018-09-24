# Classification-using-supervised-learning-algorithms

Data Source:

The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution.The marketing campaigns were based 
on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would
be ('yes') or not ('no') subscribed. 

Goal:

To predict if the client will subscribe a term deposit (variable y).

Technical details:

I have tested following different supervised learning algorithms on this data to predict if the client will subscribe a term deposit or not.
I have got similar accuracy on all the algorithms. Firstly I trained the data on 70% of data and then tested on 30% of data.

   Algorithm             Accuracy on test data

1. Decision Trees               89.3%
2. Random Forests               89.8%
3. K nearest neighbours         88.6%
4. Logistic Regression          89.8%
5. SVM Linear                   89.89%
   SVM Radial                   89.15%
6. Neural Networks              88.7%
7. Naive Bayes                  88.12%
8. Bagged CART(Bagging)         88.93%
9. Boosted CART(Boosting)       89.1%

We conclude that Random Forests, Logistic Regression and SVMs models give better accuracy on this data.

Language used: R
