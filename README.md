# Group-Uplift-Prediction

### Function file conatins the small required function in the modelling
- [Preprocessing Stage]: Function to (1) do the bootstrap and (2) transform the training/validation data in to required form based on different modelling approach and algorithms
- [Modelling Stage]: (1) Generate the required formula (interaction terms or not) for training models and (2) collecting the classification result and probabilities for KNN using FNN package
- [Performance Evaluation Stage]: Collecting the result from two group (Ypre = 1/0) to measure the performance

### Module final contain different method and approach
- [Method] Including Random Forest, KNN, Lasso, Logistic Regression
- [Module] Including One model approahc, Two model approach and Modelling separately approach

### Final Uplift Prediction_Modelling separately version & Final Uplift Prediction_Group version
Two file are the final function we are going to use, which will shows the progression of the analysis including number of bootstrap have been completed and which algorithm is running

## Plot
A customed plot functin allow people to visualize the result of bootstrap in a plot
