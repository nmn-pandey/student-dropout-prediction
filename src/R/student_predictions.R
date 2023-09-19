######################################################################
#
#   Predictive Data Analysis 
#   Naman Pandey
#   Revision: 1.00
#
#
#   Reference:
#   Dataset Link: https://www.kaggle.com/datasets/thedevastator/higher-education-predictors-of-student-retention
#
######################################################################

######################################################################
#                                                                    #
#     Dataset Description and Research Question Formulation          #
#                                                                    #
######################################################################

### DESCRIBE THE DATASET HERE ### 

### Research Questions ### 
# 
# Q1. How does the student's demographic, socioeconomic and academic data available at different points during the course
#     affect the likelihood of student dropout, enrollment, or graduation at the end of the course? 
#     (Can we predict which students are at risk of dropping out based on their demographic, socioeconomic, and academic data?)
#     (Can we predict which students are most likely to excel in certain academic disciplines or fields, 
#     and how can we use this information to guide career counseling and educational planning?)
# Q2. Can we predict which students are most likely to receive scholarships or financial aid, 
#     and how can we use this information to optimize allocation of resources?


######################################################################
#                                                                    #
#         Importing and loading the required libraries               #
#                                                                    #
######################################################################

# Load the viridis package to get colour blindness 
# Load reshape2 package to reshape the corr_matrix to a format understood by ggplot
# gridExtra for multiple plots on same screen

required_libraries <- c("ggrepel", "dplyr", "tidyverse", "skimr", "corrplot", "cluster", "factoextra", "moments", "viridis", "caret","reshape2", "keras", "randomForest", "gridExtra")

for (lib in required_libraries) {
  # Install and load the libraries if not already installed
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
  # Load the libraries if already installed
  else {
    library(lib, character.only = TRUE)
  }
}
# Remove required_libraries and lib from memory
rm("required_libraries","lib")

#################################################################
#                                                               #
#             Data Preparation and Quality Check                #
#                                                               #
#################################################################

### Loading the dataset ### 

setwd("~/Desktop/Coursework/Coursework Assignments/Assignments/CS5812 - Predictive Data Analysis/R Code")

students.df <- read.csv(file = "../Dataset/student_dropout.csv",
                        header = T, sep = ",")

### Data Quality Check ### 

# Checking dimensions of the data
dim(students.df)

# Getting the variable names
names(students.df)

# Getting the structure and checking the data type
str(students.df)

# getting a summary of the data
#   skim() is same as summary(), but more cleaner and in tabular form
skim(students.df)

# Eyeballing the data
head(students.df)
tail(students.df)

# Get no of Unique values in every column
for(col in names(students.df)) {
  # Get the number of unique values in the column
  unique_values <- unique(students.df[[col]])
  length_unique <- length(unique_values)
  
  # If no of unique values is less, e.g. 10 supposedly, print those values
  if(length_unique < 10) {
    print(paste(col, "has", length_unique,"unique values; Repeated values are ", toString(sort(unique_values))))
  }
  else{
    print(paste(col, "has", length_unique,"values"))
  }
}

# Counting NA values
setNames(as.data.frame(colSums(is.na(students.df))), "Count of NA")

# Count the number of duplicate rows in the dataset
sum(duplicated(students.df))

# defining categorical and numerical columns based on the above checks and metadata
categorical_cols <- c("Marital.status", "Application.mode", "Application.order", "Course", "Daytime.evening.attendance", "Previous.qualification", "Nacionality", "Mother.s.qualification", "Father.s.qualification", "Mother.s.occupation", "Father.s.occupation", "Displaced", "Educational.special.needs", "Debtor", "Tuition.fees.up.to.date", "Gender", "Scholarship.holder", "International", "Target")
numerical_cols <- setdiff(names(students.df), categorical_cols)


# defining custom function for categorical variables
# to show frequency table, barplots and pie charts
categorical_fn <- function(col_name){
  # Frequency Table sorted in descending order
  freq_table_df <- as.data.frame(table(students.df[,col_name]))
  colnames(freq_table_df) <- c(col_name, "Frequency")
  freq_table_df <- freq_table_df %>% arrange(desc(Frequency))
  
  # Bar Plot
  bar_plot <- ggplot(freq_table_df, 
                     aes(x = !!sym(col_name), y = Frequency, fill = !!sym(col_name))) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100*Frequency/sum(Frequency),2), "%"),
                  y = Frequency + 50),
              size = 3, color = "black") +  # Add percentages as text
    scale_fill_viridis(discrete = TRUE) +
    labs(title = paste("Bar Plot for", col_name, "distribution"), 
         x = col_name, y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(color = "black")) 
  
  # Combine plots and table in a grid
  table_grob <- tableGrob(freq_table_df, 
                          rows = NULL,
                          theme = ttheme_minimal())
  
  grid.arrange(table_grob,bar_plot,
               ncol = 2,
               widths = c(1, 5))
  
  # Ask the user to press a key to view the next variable
  readline(prompt = "Press any key to continue")
}




# apply custom function to categorical columns
#   Saving to a variable suppresses duplicate results being displayed by lapply()
categorical_results <- lapply(categorical_cols, categorical_fn)


# defining custom function for numerical variables
# to show numerical summary, box plot and histogram
numerical_fn <- function(col_name){ 
  # Numerical Summary
  summary_table <- data.frame(t(summary(students.df[,col_name])))[,2:3]
  names(summary_table) <- c("Statistics", col_name)
  
  # Box Plot
  box_plot <- ggplot(students.df, aes(x = "", y = !!sym(col_name))) +
    geom_boxplot(aes(fill = "viridis")) +
    labs(title = paste("Box Plot for", col_name), y = col_name) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  # Histogram
  hist_plot <- ggplot(students.df, aes(x = !!sym(col_name), fill = ..count..)) +
    geom_histogram(binwidth = diff(range(students.df[, col_name]))/30, alpha = 0.7) +
    labs(title = paste("Histogram for", col_name, "distribution"), 
         x = col_name, y = "Frequency") +
    scale_fill_viridis() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the visualizations
  print(paste("Numerical Summary for", col_name))
  print(summary_table)
  print(paste("Skewness for",col_name,"is",round(skewness(students.df[,col_name]),2)))
  print(hist_plot)
  print(box_plot)
  grid.arrange(hist_plot, box_plot, ncol = 2)
  
  # Ask the user to press a key to view the next variable
  readline(prompt = "Press any key to continue")
}

# apply custom function to numerical columns
#   Saving to a variable suppresses duplicate results being displayed by lapply
numerical_results <- lapply(numerical_cols, numerical_fn)


### ERRORS FOUND ### 
# Application order 0 has a frequency of 1, which does not match the metadata
####### Many of the variables are defined as categorical, which might not suit our model.

#################################################################
#                                                               #
#                       Data Cleaning                           #
#                                                               #
#################################################################

## Copy the dataset
students.df.cleaned <- students.df

## Correcting Application order
#   Find the mode of the Application.order column
#   and replace 0 values with the mode
mode_value <- as.numeric(names(sort(-table(students.df.cleaned$Application.order)))[1])
students.df.cleaned$Application.order[students.df.cleaned$Application.order == 0] <- mode_value
skim(students.df.cleaned$Application.order)

# Define the minimum frequency for combining categories
min_freq <- 150

# Combine categories with low frequency
combined_cats_dict <- list()
for (col in categorical_cols) {
  
  #Check if the variable is not binary
  if (length(unique(students.df.cleaned[, col])) > 2) {
    # Count the frequency of each category
    counts <- table(students.df.cleaned[, col])
    # Identify the categories with low frequency
    low_freq_cats <- names(counts[counts < min_freq])
    # Only combine categories if at least 2 categories have frequency less than min_freq
    if (length(low_freq_cats) >= 2) {
      # Combine the low frequency categories into a single "Other" category represented as -1
      students.df.cleaned[, col] <- ifelse(students.df.cleaned[, col] %in% low_freq_cats, -1, students.df.cleaned[, col])
      # Identify the categories that were combined
      combined_cats_dict[[col]] <- paste(low_freq_cats, collapse = ", ")
      other_percent <- length(students.df.cleaned[students.df.cleaned[, col] == -1, col]) / nrow(students.df) * 100
      cat_msg <- paste("For", col, "variable, -1 (Other) constitutes", other_percent, "% & combines categories", combined_cats_dict[[col]])
      print(cat_msg)
    }
  }
}

# loop through categorical columns and create frequency table for each
for (col in categorical_cols) {
  freq_table <- as.data.frame(table(students.df.cleaned[, col]))
  freq_table$Percent <- freq_table$Freq / nrow(students.df) * 100
  freq_table$Percent <- sprintf("%.2f%%", freq_table$Percent)
  freq_table <- data.frame(col = col, freq_table)
  freq_table <- freq_table[, c("col", "Var1", "Freq", "Percent")]
  colnames(freq_table) <- c("Variable", "Category", "Frequency", "Percent")
  print(as.data.frame(t(freq_table), stringsAsFactors = FALSE))
}

#Change Column Name in dataset
colnames(students.df.cleaned)[colnames(students.df.cleaned) == 'Nacionality'] <- 'Nationality'

#Update the column name in the categorical_cols list
categorical_cols[categorical_cols == 'Nacionality'] <- 'Nationality'

# ## Converting columns that should be categorical into factors
# for (col in categorical_cols) {
#   students.df.cleaned[[col]] <- factor(students.df.cleaned[[col]])
# }

## Check the structure of the dataset
str(students.df.cleaned)


#################################################################
#                                                               #
#                   Exploratory Data Analysis                   #
#                                                               #
#################################################################

### For Question 1 ### 

## Analysing independent variables with Target Variable

# Target variable - categorical
table(students.df.cleaned$Target)

# Visualize the distribution of the target variable
ggplot(data = students.df, aes(x = Target)) +
  geom_bar(aes(fill = stat(count)), color = "black") +
  scale_fill_viridis() +
  labs(title = "Distribution of Target Variable",
       x = "Target", y = "Count")

# Define a custom function to create
#   2-way bar plots and frequency tables for categorical variables versus Target Variable
#   and 2-way boxplots and numerical summaries for numerical variables
#     USING AES_STRING INSTEAD OF AES TO PASS COLUMN NAMES AS ARGUMENT
create_plot_summary <- function(data, x, y = NULL) {
  
  # If y is not provided, create a bar plot
  if(is.null(y)) {
    # Create a stacked bar plot of x and Target variables
    plot1 <- ggplot(data, aes_string(x = x, fill = "Target")) +
      geom_bar(position = "dodge") +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Relationship between Target Variable and", x))
    
    # Creating a stacked bar plot to visualise proportions of Target variable in x
    plot2 <- ggplot(data = data, aes_string(x = x, fill="Target")) +
      geom_bar(position="fill") +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Distribution of Target variable by", x), y="Proportion")
    
    # Create a frequency table of x and Target variables
    freq_table <- table(data[, x], data$Target)
    colnames(freq_table) <- paste0("Count.", colnames(freq_table))
    rownames(freq_table) <- paste0(x, ".", rownames(freq_table))
    cat(paste0("Frequency table for '", x, "' variable:\n"))
    print(freq_table)
    
    # Display all plots in one page
    grid.arrange(plot1, plot2, ncol = 2)
    
  } else {
    
    # Create a boxplot of y and Target variables
    plot1 <- ggplot(data, aes_string(x = "Target", y = y, fill = "Target")) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Relationship between Target Variable and", y))
    
    # Print summary statistics of y by Target variable
    summary_table <- aggregate(as.formula(paste(y," ~ Target")), data = data, FUN = summary)
    colnames(summary_table) <- c("Target", sub("\\..*", "", names(summary_table[2:length(summary_table)])))
    cat(paste0("Summary table for '", y, "' variable:\n"))
    print(summary_table)
    
    # Display both plot and table in one page
    grid.arrange(plot1)
    
  }
  readline(prompt = "Press any key to continue")
}

# Selecting all independent variables (all variables except Target)
vars <- names(students.df[, -which(names(students.df) == "Target")])

# Use lapply() or loops to call the function for different variables
results <- lapply(vars, function(x){
  if(is.element(x, categorical_cols)){
    #create_plot_summary(students.df.cleaned, x)
  }else{
    create_plot_summary(students.df.cleaned, "Target", x)
  }
})

### For numerical variables

# Select only the numerical columns
num_vars <- students.df.cleaned[, numerical_cols]

# Scaling numerical variables
num_vars_scaled <- scale(num_vars)

# Calculate the correlation matrix
corr_matrix <- cor(num_vars_scaled)

# Plot basic heatmap
# heatmap(corr_matrix)
# 
# # Plot the full heatmap using ggplot2
# ggplot(data = melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) + 
#   geom_tile() +
#   scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", na.value = "white") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(title = "Correlation Heatmap")

# Create a function to extract the lower triangle of a matrix
lower_triangle <- function(mat) { mat[upper.tri(mat)] <- NA; diag(mat) <- NA; mat }

# Extract the lower triangle of the correlation matrix and convert it to long format
corr_lower_melt <- corr_matrix %>% 
  lower_triangle() %>% as.data.frame() %>% rownames_to_column("Var1") %>% 
  pivot_longer(-Var1, names_to = "variable", values_to = "value") %>% 
  mutate(across(c("variable", "Var1"), factor, levels = unique(Var1)), value = ifelse(is.na(value) | abs(value) < 0.2, NA, value))

# Plot the heatmap using ggplot2
ggplot(corr_lower_melt, aes(Var1, variable, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + 
  scale_fill_viridis(name = "Correlation", option = "plasma", na.value = "white", direction = 1, limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Lower TriangularCorrelation Heatmap")



## Cluster Analysis

# hierarchical clustering

# scaling numerical variables
num_vars_scaled <- scale(num_vars)

#   first generate the distance matrix with euclidian distance
#     note: exclude the Target attribute
dist_students <- dist(num_vars, method = 'euclidian')
#   then apply complete linkage
hc_students <- hclust(dist_students, method = 'ward.D2')
hc_students

# plot the associated dendrogram
plot(hc_students, hang = -0.1, labels = students.df.cleaned$Target)
#abline(h = 100, col = "orange", lty = 1)
abline(h=450, col= "orange", lty = 1)

# 'cut' the dendrogram to select one partition with 5 groups
#   note: the cutree command requires a distance cutoff h
#      or the number k of desired groups
hc_id_students <- cutree(hc_students, k = 3)

# k-means

# Using elbow method to find optimal number of cluster
set.seed(123)
wcss <- c()
for(i in 1:10) wcss[i] <- sum(kmeans(num_vars, i)$withinss)
plot(1:10, wcss, type = "b", main = "Cluster of clients", xlab = "No of clusters", ylab = "WCSS")

#   note: select k = 5 groups
k <- 3 # OR 3 yet to be confirmed
kmeans_students <- kmeans(num_vars, centers=k, nstart=25)
kmeans_students

# get the cluster id from the kmeans model object
k_id_students <- kmeans_students$cluster

# visualizing clusters
fviz_cluster(kmeans_students, data=num_vars, ellipse.type="t", geom="point") 

######################################################################
# 3. Evaluation of cluster results

# silhoutte plot

# then calculate the silhoutte score for the two cluster solutions
#   note: look at the help for silhoutte to understand the required input
sil_hc_students <- cluster::silhouette(hc_id_students, dist_students)
sil_k_students <- cluster::silhouette(k_id_students, dist_students)

# plot the results of the silhoutte analysis for the two cluster solutions
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,1))
plot(sil_hc_students)
plot(sil_k_students)
par(opar)


# Calculate distribution 
# Create a factor variable for the clusters
cluster_labels_k <- factor(k_id_students)

# Create the contingency table
table(cluster_labels_k, students.df$Target)
# 49% accuracy when vars are scaled
# 


# Create a factor variable for the clusters
cluster_labels_hc <- factor(hc_id_students)

# Create the contingency table
table(cluster_labels_hc, students.df$Target)
# 20% when vars are scaled

## Principle Component Analysis

# Perform PCA
pc_students <- prcomp(students.df.cleaned[,numerical_cols], scale = T, center = T)

# Print the summary of the PCA results
summary(pc_students)

# Plot the variance explained by each variable
fviz_eig(pc_students, addlabels = TRUE, ylim = c(0, 60)) 

# calculate the proportion of explained variance (PEV) from the std values
pc_students_var <- pc_students$sdev^2
pc_students_var
pc_students_PEV <- pc_students_var / sum(pc_students_var)
pc_students_PEV


# plot the cumulative value of PEV for increasing number of additional PCs
#   note: add an 80% threshold line to inform the feature extraction
#     according to the plot the first 3 PCs should be selected
opar <- par(no.readonly = TRUE)
plot(
  cumsum(pc_students_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
par(opar)

# get and inspect the loadings for each PC
#   note: loadings are reported as a rotation matrix (see lecture)
pc_students_loadings <- pc_students$rotation
pc_students_loadings

# plot the loadings for the first three PCs as a barplot
#   note: two vectors for colours and labels are created for convenience
#     for details on the other parameters see the help for barplot and legend
opar <- par(no.readonly = TRUE)
colvector = c('red', 'orange', 'yellow', 'green', 'cyan', 'blue')
labvector = c('PC1', 'PC2', 'PC3')
barplot(
  pc_students_loadings[,c(1:3)],
  beside = T,
  yaxt = 'n',
  names.arg = labvector,
  col = colvector,
  ylim = c(-1,1),
  border = 'white',
  ylab = 'loadings'
)
axis(2, seq(-1,1,0.1))
legend(
  'topright',
  bty = 'n',
  col = colvector,
  pch = 15,
  row.names(pc_students_loadings)
)
par(opar)



#################################################################
#                                                               #
#                       Machine Learning                        #
#                                                               #
#################################################################

### NOT TO BE CONSIDERED ### 
### DEEP LEARNING AND MACHINE LEARNING CODE IN PYTHON ###

### Class Imbalance Problem ###

# Copy the dataset and convert the Target variable into factor
data <- students.df.cleaned
data$Target <- as.factor(data$Target)

# Split the data into training and testing sets
train_idx <- createDataPartition(data$Target, p = 0.7, list = FALSE, times = 1)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Train the random forest model using simple sampling
classifier_random <- randomForest(x = train_data[, -which(names(train_data) == "Target")],
                                  y = as.factor(train_data$Target),
                                  ntree = 500)

# Train the random forest model using stratified sampling
##  Create new test data for the stratified sampling
train_idx_strat <- caret::createDataPartition(data$Target, p = 0.7, list = FALSE, times = 1)
train_data_strat <- data[train_idx_strat, ]
classifier_strat <- randomForest(x = train_data_strat[, -which(names(train_data_strat) == "Target")],
                                 y = as.factor(train_data_strat$Target),
                                 ntree = 500)

# Train the random forest model using resampling
classifier_resample <- caret::train(Target ~ ., data = train_data, method = "rf", 
                                    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))

# generate confusion matrices
pred_random <- predict(classifier_random, newdata = test_data)
cm_random <- confusionMatrix(data = pred_random, test_data$Target)
cm_random

pred_stratified <- predict(classifier_strat, newdata = test_data)
cm_stratified <- confusionMatrix(data = pred_stratified, test_data$Target)
cm_stratified

pred_resample <- predict(classifier_resample, newdata = test_data)
cm_resample <- confusionMatrix(data = pred_resample, test_data$Target)
cm_resample

# Extract overall accuracy from confusion matrices
acc_random <- cm_random$overall[1]
acc_stratified <- cm_stratified$overall[1]
acc_resample <- cm_resample$overall[1]

# Create a data frame to compare accuracy
accuracy_df <- data.frame(Sampling_Method = c("Random", "Stratified", "Resample"),
                          Overall_Accuracy = c(acc_random, acc_stratified, acc_resample))

# Print the accuracy comparison
print(accuracy_df)

#################################################################
#                                                               #
#                         Deep Learning                         #
#                                                               #
#################################################################

### NOT TO BE CONSIDERED ### 
### DEEP LEARNING AND MACHINE LEARNING CODE IN PYTHON ###

# Installing tensorflow
#install_tensorflow()

# Set seed for reproducibility
set.seed(123)

# Copy students.df.cleaned for further modification
data <- students.df.cleaned

# Convert 'Target' variable to numerical
data <- data %>%
  mutate(Target = case_when(
    Target == "Graduate" ~ 0,
    Target == "Dropout" ~ 1,
    Target == "Enrolled" ~ 2
  ))

# Split data into training and test sets
train_data <- sample_frac(data, 0.8)
test_data <- setdiff(data, train_data)

# Separate features and target variable
x_train <- train_data %>% select(-Scholarship.holder) %>% as.matrix()
y_train <- train_data$Scholarship.holder

x_test <- test_data %>% select(-Scholarship.holder) %>% as.matrix()
y_test <- test_data$Scholarship.holder

# Normalize data
x_train <- scale(x_train)
x_test <- scale(x_test)

## MODEL 1 : SIMPLE FULLY CONNECTED LINEAR MODEL 

# Define model architecture
model_linear <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = dim(x_train)[2]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile first model
model_linear %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Train first model
history_linear <- model_linear %>% fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 512,
  validation_split = 0.2
)

# Evaluate first model on test data
results_linear <- model_linear %>% evaluate(x_test, y_test)

## MODEL 2 : FULLY CONNECTED NEURAL NETWORK WITH DROPOUT LAYERS

# Define model architecture
model_dropout <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = dim(x_train)[2]) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile second model
model_dropout %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# Train second model
history_dropout <- model_dropout %>% fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 512,
  validation_split = 0.2
)

# Evaluate second model on test data
results_dropout <- model_dropout %>% evaluate(x_test, y_test)
