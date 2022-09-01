#Imports
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os, glob

#Data collection
dataframes = []
path = os.getcwd()

rt = pd.read_csv("Rt.csv", sep=',', header=0)
print("Read dataset_covid_italia.csv")
print(rt.head())

#Plotting
# for dataset in dataframes:
#     colums = headerNames[dataset.name.split('_')[0]]
#     ycolums = colums[1:]
#     for c in ycolums:
#         graph = sns.lineplot(x=colums[0], y=c, data=dataset)
#         graph.axes.set_title(dataset.name + " class: " + c,fontsize=12)
#         print("Plotting:", dataset.name)
#         fig = graph.get_figure()
#         fig.savefig(dataset.name + c + ".png")
#         graph.clear()

plt.style.use("seaborn-dark")
sns.lineplot(x="Date", y="Rt_medio", data=rt)
plt.show()