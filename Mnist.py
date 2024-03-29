from sklearn.datasets import fetch_openml
import numpy as np
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.manifold import TSNE

# Load MNIST dataset
mnist = fetch_openml('mnist_784')
X = mnist.data.astype('float32') / 255.0
y = mnist.target.astype('int')

# Subsample for faster execution (optional)
np.random.seed(42)
subset_indices = np.random.choice(len(X), 10000, replace=False)
X_array = X.to_numpy()
X_subset = X_array[subset_indices]
y_subset = y[subset_indices]

# Apply PCA for initial dimensionality reduction (optional)
pca = PCA(n_components=50)
X_pca = pca.fit_transform(X_subset)

# Apply t-SNE for further dimensionality reduction
tsne = TSNE(n_components=2, random_state=42)
X_tsne = tsne.fit_transform(X_subset)

# Visualize the results
plt.figure(figsize=(20, 7))

plt.subplot(131)
for i in range(len(np.unique(y_subset))):
    plt.scatter(X_tsne[y_subset == i, 0], X_tsne[y_subset == i, 1], label=f'Cluster {i}', s=10)
plt.title('t-SNE Visualization')
plt.legend()

plt.show()