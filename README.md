# Rainfall_Viz_Pred
# About
The cornerstone of this project is data visualisation. Most of the data out there is in raw form <br />
and data is most essential for any decision support system. Proper analysis, graphs and more can help make <br />
decision.<br />
Another aspect to this project is prediction of rainfall using a LSTM based CART model. The output of LSTM <br />
is provided as an output to CART and CART is used in the regression mode. For every LSTM output class there is a <br />
CART model associated with it. <br />

# About Files Included
1. FinalSet.csv contains values of rainfall, averge wind speed, dew point temperature <br />
   Maximum temperature, Minimum temperature, Sea level pressure, Mean sea level pressure,<br />
   relative humidity and date. Date values range from 1996 to 2015.
2. StdData.csv contains processed data to make it standard with mean=0 and standard deviation=1.
3. App.R contains shiny app for the data visualization.
4. Yearlyrain.csv contains aggregate of rainfall of each year.
