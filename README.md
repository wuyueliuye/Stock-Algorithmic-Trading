# Financial-Machine-Learning 
#### Stock Algorithmic Trading
[Stock Algorithmic Trading](https://github.com/wuyueliuye/Financial-Machine-Learning/blob/master/stock_md.md)

In this project, I selected 3 months’ tick data for Nivdia to go for a
predictive analysis. By detecting the special patterns from the tick
data, I intended to make forcast for the trend of the stock. The primary
idea was to abstract features from setting up bars and monitors, then by
adding extra indicators to find close relevant indexes for a reference.

Technically, from fitting the Random Forest Model and analyzing the
feature importance, I intended to mock the impaction from factors or
components so as to find out the relatively important indicators for the
tick price.

Besides, to optimize performance, parameter tuning was necessary. Here
I’ve been focusing on two parameters, the detection boundary and the
target barrier. By training the model under purged k-fold cross
validation and measuring with multiple metrics, we can eventually find
some efficient settings.

The techniques include: • CUSUM filters • Meta labeling • SMOTE for
imbalanced classes • Randomforest • Feature importance analysis •
Parameter tuning •purged k-fold CV • Performance measures
