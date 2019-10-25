import pandas as pd 
import seaborn as sns

data = pd.read_csv("Data/AmesHousing.csv") 

print(data.head())

tips = sns.load_dataset("tips")
sns.boxplot(x=tips["total_bill"])