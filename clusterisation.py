import pandas as pd
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import numpy as np


data = pd.read_csv('dermatology.csv')
data_var = data.drop(['age', 'class'], axis=1)

scaler = StandardScaler()
data_scaled = scaler.fit_transform(data_var)

nb_clus=len(np.unique(data['class']))

#%%

kmeans = KMeans(n_clusters=nb_clus, init='k-means++', random_state=42)
kmeans.fit(data_scaled)
wcss=kmeans.inertia_

data['cluster'] = kmeans.labels_

# Afficher la distribution des clusters
print("\nDistribution des clusters :")
print(data['cluster'].value_counts())

# Afficher des statistiques sur chaque cluster
print("\nStatistiques sur chaque cluster :")
print(data.groupby('cluster').describe())

# Visualisation des clusters (uniquement pour les deux premières colonnes)
plt.scatter(data.iloc[:, 0], data.iloc[:, 1], c=data['cluster'], cmap='viridis')
plt.xlabel(data.columns[0])
plt.ylabel(data.columns[1])
plt.title('Clustering des données dermatology.csv avec K-Means')
plt.show()

#%%

correspondance = {0: 'A', 1: 'B', 2: 'C', 3: 'D', 4: 'E', 5: 'F'}

data['cluster'] = [correspondance[nombre] for nombre in data['cluster']]

#%%

import numpy as np

n=366
k=6

cost_matrix = np.zeros((k, k), dtype=int)

lettre_to_nombre = {lettre: i for i, lettre in enumerate(['A', 'B', 'C', 'D', 'E', 'F'])}

# Calculer la matrice de coût
for i in range(n):
    cluster = data['cluster'][i]
    classe = data['class'][i]
    cost_matrix[lettre_to_nombre[cluster],classe-1]+=1
    
cost_matrix=1-cost_matrix/n

print("Matrice de coût :\n", cost_matrix)

#%%
selected_columns = pd.DataFrame(cost_matrix)

selected_columns.to_csv('cluster-classe.csv', index=False)  

