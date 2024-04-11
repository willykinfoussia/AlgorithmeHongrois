
associationListToAdjacencyMatrix <- function(association) {
  n <- length(association)  # Taille de la liste d'associations
  # Initialiser une matrice n x n avec tous les éléments à zéro
  adjacencyMatrix <- matrix(0, nrow = n, ncol = n)
  # Parcourir la liste d'associations
  for (i in 1:n) {
    # Pour chaque élément i dans la liste d'associations, définir l'élément correspondant dans la matrice d'adjacence à 1
    adjacencyMatrix[i, association[i]] <- 1
  }
  return(adjacencyMatrix)  # Retourner la matrice d'adjacence générée
}

# Fonction pour calculer le coût total d'une affectation donnée
calculateCost <- function(matrix, assignment) {
  totalCost <- 0  # Initialisation du coût total à zéro
  # Parcourir chaque ligne de la matrice
  for (i in 1:nrow(matrix)) {
    # Ajouter le coût de l'affectation actuelle de l'élément i (selon la liste assignment) au coût total
    totalCost <- totalCost + matrix[i, assignment[i]]
  }
  return(totalCost)  # Retourner le coût total calculé
}

# Fonction pour générer la prochaine permutation
next_permutation <- function(arr) {
  n <- length(arr)
  i <- n - 1
  while (i >= 1 && arr[i] >= arr[i + 1]) {
    i <- i - 1
  }
  if (i == 0) {
    return(arr)
  }
  j <- n
  while (arr[j] <= arr[i]) {
    j <- j - 1
  }
  temp <- arr[i]
  arr[i] <- arr[j]
  arr[j] <- temp
  arr[(i + 1):n] <- rev(arr[(i + 1):n])
  return(arr)
}


NaiveAlgorithme_R <- function(matrix, verbose = FALSE) {
  if (verbose) {
    print("Original Matrix : ")
    print(matrix)
  }
  
  n <- nrow(matrix)  # Taille de la matrice
  
  # Vecteur pour stocker l'assignation optimale avec le coût minimal
  minAssignment <- numeric(n)
  
  # Initialisation du coût minimal avec une valeur maximale
  minCost <- Inf
  
  # Vecteur pour stocker l'assignation courante
  currentAssignment <- numeric(n)
  
  # Vecteur pour stocker les indices de permutation
  indices <- 1:n  # Remplir les indices de 1 à n
  
  # Génération de toutes les permutations possibles
  repeat {
    # Construction de l'assignation courante en utilisant les indices de permutation
    for (i in 1:n) {
      currentAssignment[i] <- indices[i]
    }
    
    # Calcul du coût de l'assignation courante
    currentCost <- calculateCost(matrix, currentAssignment)
    
    # Mise à jour de l'assignation optimale et du coût minimal si nécessaire
    if (currentCost < minCost) {
      minCost <- currentCost
      minAssignment <- currentAssignment
    }
    
    indices <- next_permutation(indices)
    egal <- all.equal(currentAssignment, indices)
    # Générer la prochaine permutation
    if (is.logical(egal) && egal) break
  }
  
  adjacencyMatrix <- associationListToAdjacencyMatrix(minAssignment)
  
  if (verbose) {
    print("Affectation optimal :")
    print(minAssignment)
    print("Matrix:")
    print(adjacencyMatrix)
    print("Minimal Cost : ", minCost)
  }
  
  # Retourner une paire contenant l'assignation avec le coût minimal et le coût minimal lui-même
  return(adjacencyMatrix)
}