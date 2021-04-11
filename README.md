# Master_Thesis
The marginal cost estimation of power generators is a crucial element to create a sustainable
electricity market because it is one of the key metrics used for market surveillance.
The current techniques for cost estimation are usually based on the first-order approach
to profit function of power generators. However, the evidence of existing market power in
the energy market changes the market's dynamics, assumptions, and expected behaviours
of generators. Therefore, this technique may be misleading in the case of existing market
power. Another common method is reference bids that use mean and median values of
past supplier bids as marginal cost indicator. However, using only median and mean
values from past bids for the particular period to determine a reasonable cost level is a
controversial topic for energy markets, one of the highest dynamics markets.
In this work, we applied a machine learning regression model of XGboost that overcomes
the weaknesses of both techniques to predict the marginal cost of fossil fuel power plants
from Spain energy markets for two years (2017-2018). The developed machine learning
model uses past 90 days matched and unmatched bids with 90 days sliding window.
First, the model trained on only feature sets of bids that consisted of 2160 feature columns
after that the model extended with additional explanatory features. On the other hand,
we see that the created machine learning models resulted in much fewer errors in mean
absolute error and root means squared error compared to reference bids.


Datafile: 
(Link > ML_Reference_bids >data>ifnra20172018)

https://drive.google.com/drive/folders/1rW6twG8YL9zeZKDJo3SEiZPIbINkn6uT






