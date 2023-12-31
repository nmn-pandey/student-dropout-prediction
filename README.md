# Student Dropout Prediction
This project aims to predict student dropout and academic success using demographic, socioeconomic, and academic data. The project is implemented in Python and R.

## Data
The data is sourced from Kaggle and contains information on 4424 students across several attributes like gender, age, nationality, qualifications, academic performance etc. More details on the data dictionary can be found in the link below.
- Dataset Link: _https://www.kaggle.com/datasets/thedevastator/higher-education-predictors-of-student-retention_

## Notebooks
The Python and R implementations are in the following notebooks in the _src_ folder:

- _student_predictions.ipynb_: Interactive Python notebook implementing including data preprocessing, EDA, machine learning models, and deep learning models.
- _student_predictions.py_: Complete Python implementation including data preprocessing, EDA, machine learning models, and deep learning models.
- _student_predictions.R_: Complete R implementation including data preprocessing, EDA, machine learning models, and deep learning models.

## Models
The following machine learning and deep learning models are implemented:

### Machine Learning

- Logistic Regression
- K-Nearest Neighbors
- Random Forest (primary model)
- Decision Tree
- Support Vector Machine

### Deep Learning

- 2-layer Neural Network
- 3-layer Neural Network
- 5-layer Neural Network with Dropout
- Convolutional Neural Network (best performer)

## Usage
The notebooks can be run end-to-end to reproduce the analysis and modeling pipeline. Models are evaluated using 5-fold stratified cross validation. Class imbalance is handled via oversampling and undersampling techniques.

### The Python notebook requires the following key libraries:

- Pandas, NumPy, Matplotlib, Seaborn
- Scikit-learn, Keras, Tensorflow

### The R notebook requires the following key libraries:

- dplyr, tidyverse, corrplot, skimr, keras
- cluster, factoextra, moments, caret, randomForest, gridExtra

# Authors
- Naman Pandey - _https://www.linkedin.com/in/nmn-pandey/_
- Drishti Doshi - _https://www.linkedin.com/in/drishti-doshi-45060221a/_
- Sujeet Sharma - _https://www.linkedin.com/in/sujeet-sharma-644247109/_

Let me know if you have any questions.
