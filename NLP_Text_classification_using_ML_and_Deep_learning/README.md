We have taken 200k news items from HuffPost. The dataset is taken from a Kaggle competition. The news articles in the dataset are each tagged with one of several categories (such as Politics, Technology,Entertainment, etc.). Each entry in the dataset typically includes the headline and a short description of the article, along
with its assigned category. We have applied and compare various supervised machine learning models and deep learning to categorize news articles into predefined categories based on their headlines and short descriptions. This project enables understanding text classification nuances using traditional machine learning such as deep learning approaches.

Major Packages used: sklearn, nltk

We have applied following Machine Learning models and analysed them:
a) Logistic Regression: Use as a baseline model to understand the linear separability of text categories.
b) Random Forest (RF):  ensemble learning method for classification by constructing a multitude of decision trees at training time for handling high-dimensional text data.
c) XGBoost: (eXtreme Gradient Boosting) trendy ensemble machine learning method for classification, it is the implementation of gradient boosted decision trees designed for speed and performance 

We have also used Artificial Neural Network (ANN): Design a simple feedforward neural network with at least one hidden layer to
classify news categories and Convolutional Neural Network (CNN)

We have evaluate their performance on the test set using metrics such as accuracy,
precision, recall, and F1-score. Additionally we have also constructed confusion matrices
to observe the results.

Comparative Analysis:

Conventional ML Techniques vs Deep learning

Logistic regression is often used for linear relationships and binary classification tasks. It might grapple with scenarios of non-linear relationaships and patterns.It has significantly low computational complexity and model interpretability is very high compared to DL. We observed in our case that Deep learning and conventional ML techniques perform very similarly, but LSTM (RNN) had a very good improvement in accuracy but is very computationally intensive.So we can conclude that Deep Learning out performs conventional ML techniques with a trade-off of high computational complexity and lower model interpretability.In our case, conventional ML techniques(Tree ensemble methods) consumed more time for execution and DL techniques consumed more computational resources.

Comprehending the reasoning behind specific predictions made by complex deep learning models, especially those with convoluted and tangled structures is challenging, furnishing them as "black boxes." Tree-ensemble models such as the XGBoost and RandomForest are generally well suited for structured and tabular data, several studies have shown that Tree-ensemble methods such as XGBoost performs similar to and in certain cases outperforms deep learning methods, learns quickly than DL methods for structured/tabular data. https://medium.com/latinxinai/do-you-really-need-deep-learning-84d1a8629ed6. Tree ensemble methods are definitely more interpretable than DL techniques.When it comes to high dimensional and unstructured complex data formats such as images etc, Deep learning is the way to go.
