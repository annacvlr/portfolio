"""script that wrangles datasets and creates a new csv file with the columns to focus on"""
#import libraries
import pandas as pd


dataset = pd.read_csv("../data/LogisticGrowthData.csv")
metadata = pd.read_csv("../data/LogisticGrowthMetaData.csv")

#unique ID combining Species, Temp, Medium, Rep, and Citation
dataset["ID"] = dataset[["Species", "Temp", "Medium", "Rep", "Citation"]].astype(str).agg("_".join, axis=1)

#check column names
print(dataset.columns)

#ID into numerical ID
dataset["ID"] = dataset["ID"].astype("category")  # Convert to categorical
dataset["num_ID"] = dataset["ID"].cat.codes + 1  # Convert to numerical ID (starting from 1)

#reorder num_ID to be the first column
dataset = dataset[["num_ID"] + [col for col in dataset.columns if col != "num_ID"]]

#dataFrame with ID metadata 
IDsubset_dataset = dataset[["num_ID", "ID"]]
IDsubset_dataset.to_csv("../data/ID_metadata.csv", index=False)

#dataFrame with relevant plotting information
subset_dataset = dataset[["num_ID", "Time", "PopBio", "PopBio_units"]]
subset_dataset.to_csv("../results/subset_data.csv", index=False)
