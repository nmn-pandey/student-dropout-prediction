# Student Dropout Prediction
This project aims to predict student dropout and academic success using demographic, socioeconomic, and academic data. The project is implemented in Python and R.

## Data
The data is sourced from Kaggle and contains information on 4424 students across several attributes like gender, age, nationality, qualifications, academic performance etc. More details on the data dictionary can be found in the project report.

## Notebooks
The Python and R implementations are in the following notebooks:
- student_predictions.py: Complete Python implementation including data preprocessing, EDA, machine learning models, and deep learning models.
- student_predictions.R: Complete R implementation including data preprocessing, EDA, machine learning models, and deep learning models.

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

- dplyr, tidyverse, corrplot, keras

# Authors
Naman Pandey - linkedin.com/in/nmn-pandey/
Drishti Doshi - linkedin.com/in/drishti-doshi-45060221a/
Sujeet Sharma - linkedin.com/in/sujeet-sharma-644247109/

Let me know if you have any questions.
