

copy_matrix <- function(src, dst) {
  dst <- list()  # Effacer le contenu de la matrice de destination
  
  # Copier les lignes de la matrice source vers la matrice de destination
  for (row in src) {
    # Créer une nouvelle ligne dans la matrice de destination avec les éléments de la ligne source
    copied_row <- as.vector(row)  # Copie des éléments de la ligne source
    dst <- append(dst, list(copied_row))  # Ajouter la ligne copiée à la matrice de destination
  }
  return(dst)
}

adjust_matrix <- function(matrix) {
  rows <- nrow(matrix)  # Nombre de lignes dans la matrice
  cols <- ncol(matrix)  # Nombre de colonnes dans la matrice (supposée non vide)
  
  # Si le nombre de lignes est supérieur au nombre de colonnes
  if (rows > cols) {
    # Ajouter des éléments fictifs à chaque ligne jusqu'à ce que le nombre de colonnes soit égal au nombre de lignes
    for (i in 1:rows) {
      matrix[[i]] <- c(matrix[[i]], rep(Inf, rows - cols))  # Ajoute des éléments fictifs (définis par Inf)
    }
  }
  # Si le nombre de colonnes est supérieur au nombre de lignes
  else if (rows < cols) {
    # Ajouter des lignes fictives jusqu'à ce que le nombre de lignes soit égal au nombre de colonnes
    while (length(matrix) < cols) {
      matrix <- c(matrix, list(rep(Inf, cols)))  # Ajoute une ligne fictive remplie de valeurs maximales de type T
    }
  }
  return(matrix)
}

step1 <- function(matrix, step) {
  # Ajustement des lignes
  for (i in 1:nrow(matrix)) {
    smallest_in_row <- min(matrix[i,])  # Trouver le plus petit élément dans la ligne
    if (smallest_in_row > 0) {  # Si le plus petit élément est supérieur à zéro
      matrix[i, ] <- matrix[i,] - smallest_in_row  # Soustraire le plus petit élément à chaque élément de la ligne
    }
  }
  
  # Ajustement des colonnes
  size <- ncol(matrix)  # La taille de la matrice est garantie d'être carrée
  for (column in 1:size) {
    min_in_column <- Inf  # Initialiser la valeur minimale à Inf
    for (row in 1:size) {
      min_in_column <- min(min_in_column, matrix[row, column])  # Trouver le plus petit élément dans la colonne
    }
    
    if (min_in_column > 0) {  # Si le plus petit élément est supérieur à zéro
      for (row in 1:size) {
        matrix[row, column] <- matrix[row, column] - min_in_column  # Soustraire le plus petit élément à chaque élément de la colonne
      }
    }
  }
  
  step <- 2  # Passer à l'étape 2 de l'algorithme
  return(list(step = step, matrix = matrix))
}

reset_temporary_vectors <- function(vector_to_reset) {
  for (i in 1:length(vector_to_reset)) {
    vector_to_reset[i] <- 0  # Pour chaque élément du vecteur, assigner zéro
  }
}

step2 <- function(matrix, M, RowCover, ColCover, step) {
  size <- nrow(matrix)  # Taille de la matrice
  
  # Parcourir la matrice
  for (row in 1:size) {
    for (col in 1:size) {
      if (matrix[row, col] == 0) {  # Si un zéro est trouvé dans la matrice
        if (RowCover[row] == 0 && ColCover[col] == 0) {  # Vérifier si la ligne et la colonne ne sont pas déjà couvertes
          M[row, col] <- 1  # Étoiler le zéro
          RowCover[row] <- 1  # Couvrir la ligne
          ColCover[col] <- 1  # Couvrir la colonne
        }
      }
    }
  }
  
  reset_temporary_vectors(RowCover)  # Réinitialiser le vecteur de couverture des lignes pour une utilisation ultérieure
  reset_temporary_vectors(ColCover)  # Réinitialiser le vecteur de couverture des colonnes pour une utilisation ultérieure
  
  step <- 3  # Passer à l'étape 3 de l'algorithme
  return(list(step = step, matrix = matrix))
}

step3 <- function(M, ColCover, step) {
  size <- nrow(M)  # Taille de la matrice
  covered_columns <- 0  # Nombre de colonnes couvertes
  
  # Parcourir la matrice de masquage
  for (row in 1:size) {
    for (col in 1:size) {
      if (M[row, col] == 1) {  # Si un zéro étoilé est trouvé dans la matrice de masquage
        ColCover[col] <- 1  # Couvrir la colonne correspondante
      }
    }
  }
  
  # Compter le nombre de colonnes couvertes
  for (cover_status in ColCover) {
    if (cover_status == 1) {
      covered_columns <- covered_columns + 1
    }
  }
  
  # Si toutes les colonnes sont couvertes
  if (covered_columns >= size) {
    step <- 7  # solution trouvée
    return(list(step = step))
  } else {
    step <- 4  # passer à l'étape 4 de l'algorithme
    return(list(step = step))
  }
}

find_uncovered_zero <- function(matrix, RowCover, ColCover) {
  size <- nrow(matrix)  # Taille de la matrice
  current_row <- 1  # Indice de ligne initialisé à 1
  current_col <- 1  # Indice de colonne initialisé à 1
  found <- FALSE  # Indicateur pour indiquer si un zéro a été trouvé
  row <- -1  # Initialisation de l'indice de ligne du zéro trouvé à -1
  col <- -1  # Initialisation de l'indice de colonne du zéro trouvé à -1
  
  # Boucle pour parcourir la matrice
  while (!found && current_row <= size) {
    current_col <- 1  # Réinitialiser l'indice de colonne à 1
    while (current_col <= size) {
      # Vérifier si le zéro actuel n'est pas couvert
      if (matrix[current_row, current_col] == 0 && 
          RowCover[current_row] == 0 && 
          ColCover[current_col] == 0) {
        # Mettre à jour les indices du zéro trouvé
        row <- current_row
        col <- current_col
        found <- TRUE  # Indiquer que le zéro a été trouvé
      }
      current_col <- current_col + 1  # Passer à la colonne suivante
    }
    current_row <- current_row + 1  # Passer à la ligne suivante
  }
  
  return(list(row, col))
}

has_starred_zero_in_row <- function(row, M) {
  has_starred_zero <- FALSE  # Variable indiquant si un zéro étoilé a été trouvé, initialisée à FALSE
  
  # Parcourir les colonnes de la ligne spécifiée
  for (col in 1:ncol(M)) {
    # Vérifier si un zéro étoilé est présent dans la ligne
    if (M[row, col] == 1) {
      has_starred_zero <- TRUE  # Mettre à jour la variable indiquant la présence d'un zéro étoilé
      break  # Sortir de la boucle car un zéro étoilé a été trouvé
    }
  }
  
  return(has_starred_zero)  # Retourner TRUE si un zéro étoilé a été trouvé, sinon FALSE
}

find_starred_zero_in_row <- function(row, M) {
  col <- -1  # Initialiser la colonne à -1 pour indiquer qu'aucun zéro étoilé n'a été trouvé
  
  # Parcourir les colonnes de la ligne spécifiée
  for (column in 1:ncol(M)) {
    # Si un zéro étoilé est trouvé dans la ligne
    if (M[row, column] == 1) {
      col <- column  # Mettre à jour la colonne où le zéro étoilé est trouvé
      break  # Sortir de la boucle car un zéro étoilé a été trouvé
    }
  }
  
  return(col)
}

step4 <- function(matrix, M, RowCover, ColCover, path_row_0, path_col_0, step) {
  row <- -1  # L'indice de ligne du zéro non couvert initialisé à -1
  col <- -1  # L'indice de colonne du zéro non couvert initialisé à -1
  done <- FALSE  # Variable de terminaison de la boucle
  
  while (!done) {  # Boucle jusqu'à ce qu'un zéro non couvert soit trouvé ou que toute la matrice soit parcourue
    result <- find_uncovered_zero(matrix, RowCover, ColCover)  # Trouver un zéro non couvert dans la matrice
    row <- result[[1]]
    col <- result[[2]]
    
    if (row == -1) {  # Si aucun zéro non couvert n'est trouvé dans la matrice
      done <- TRUE  # Indiquer que la recherche est terminée
      step <- 6  # Passer à l'étape 6 de l'algorithme
      return(list(step = step, row = NULL, col = NULL))
    } else {  # Si un zéro non couvert est trouvé dans la matrice
      M[row, col] <- 2  # Primariser ce zéro
      if (has_starred_zero_in_row(row, M)) {  # Si un zéro étoilé existe dans la ligne contenant ce zéro primarisé
        col <- find_starred_zero_in_row(row, M)  # Trouver le zéro étoilé dans la même ligne
        RowCover[row] <- 1  # Couvrir cette ligne
        ColCover[col] <- 0  # Découvrir la colonne contenant le zéro étoilé
      } else {  # Si aucun zéro étoilé n'existe dans la ligne contenant ce zéro primarisé
        done <- TRUE  # Indiquer que la recherche est terminée
        step <- 5  # Passer à l'étape 5 de l'algorithme
        path_row_0 <- row  # Sauvegarder l'indice de la ligne du zéro primarisé
        path_col_0 <- col  # Sauvegarder l'indice de la colonne du zéro primarisé
        return(list(step = step, row = row, col = col))
      }
    }
  }
}

find_starred_zero_in_column <- function(col, M) {
  row <- -1  # Initialiser la ligne où le zéro étoilé est trouvé à -1 pour indiquer qu'aucun zéro étoilé n'a été trouvé
  
  print("M:")
  print(M)
  print("col:")
  print(col)
  
  for (i in 1:nrow(M)) {  # Parcourir les lignes de la colonne spécifiée
    if (M[i, col] == 1) {  # Si un zéro étoilé est trouvé dans la colonne
      row <- i  # Mettre à jour la ligne où le zéro étoilé est trouvé
      break  # Sortir de la boucle car un zéro étoilé a été trouvé
    }
  }
  
  return(row)
}

find_primed_zero_in_row <- function(row, M) {
  col <- -1  # Initialiser la colonne où le zéro primarisé est trouvé à -1 pour indiquer qu'aucun zéro primarisé n'a été trouvé
  
  for (j in 1:ncol(M)) {  # Parcourir les colonnes de la ligne spécifiée
    if (M[row, j] == 2) {  # Si un zéro primarisé est trouvé dans la ligne
      col <- j  # Mettre à jour la colonne où le zéro primarisé est trouvé
      return(col)  # Sortir de la fonction car un zéro primarisé a été trouvé
    }
  }
  
  return(col)  # Si aucun zéro primarisé n'est trouvé dans la ligne, renvoyer -1
}

augment_path <- function(path, path_count, M) {
  for (p in 1:path_count) {  # Parcourir les zéros dans le chemin
    if (M[path[p, 1], path[p, 2]] == 1) {  # Si le zéro est marqué (zéro étoilé)
      M[path[p, 1], path[p, 2]] <- 0  # Inverser son état (devient zéro non marqué)
    } else {  # Sinon (si le zéro n'est pas marqué)
      M[path[p, 1], path[p, 2]] <- 1  # Inverser son état (devient zéro marqué)
    }
  }
}

erase_primes <- function(M) {
  for (i in 1:nrow(M)) {  # Parcourir chaque ligne de la matrice
    for (j in 1:ncol(M)) {  # Parcourir chaque élément de la ligne
      if (M[i, j] == 2) {  # Si l'élément est un zéro primarisé
        M[i, j] <- 0  # Remplacer le zéro primarisé par un zéro non marqué
      }
    }
  }
}

step5 <- function(path, path_row_0, path_col_0, M, RowCover, ColCover, step) {
  r <- -1  # Initialiser l'indice de ligne à -1
  c <- -1  # Initialiser l'indice de colonne à -1
  path_count <- 1  # Initialiser le nombre de zéros dans la série à 1
  
  print("path")
  print(path)
  
  print("path_row_0")
  print(path_row_0)
  print("path_col_0")
  print(path_col_0)
  
  # Ajouter le zéro primarisé initial à la série
  path[path_count, 1] <- path_row_0
  path[path_count, 2] <- path_col_0
  
  done <- FALSE  # Variable de terminaison de la boucle
  while (!done) {  # Tant que la série n'est pas terminée
    # Trouver un zéro étoilé dans la même colonne que le dernier zéro ajouté à la série
    r <- find_starred_zero_in_column(path[path_count, 2], M)
    if (r > -1) {  # Si un zéro étoilé est trouvé dans la colonne
      # Ajouter ce zéro étoilé à la série
      path_count <- path_count + 1
      path[path_count, 1] <- r
      path[path_count, 2] <- path[(path_count - 1), 2]
    } else {
      done <- TRUE  # Si aucun zéro étoilé n'est trouvé, terminer la série
    }
    
    if (!done) {  # Si la série n'est pas terminée
      # Trouver un zéro primarisé dans la même ligne que le dernier zéro ajouté à la série
      c <- find_primed_zero_in_row(path[path_count, 1], M)
      # Ajouter ce zéro primarisé à la série
      path_count <- path_count + 1
      path[path_count, 1] <- path[(path_count - 1), 1]
      path[path_count, 2] <- c
    }
  }
  
  # Mettre à jour la matrice de masquage en fonction de la série
  augment_path(path, path_count, M)
  # Réinitialiser les couvertures des lignes et colonnes
  reset_temporary_vectors(RowCover)
  reset_temporary_vectors(ColCover)
  # Effacer tous les zéros primarisés dans la matrice de masquage
  erase_primes(M)
  
  # Revenir à l'étape 3 de l'algorithme
  step <- 3
  return(list(step = step))
}

find_smallest <- function(minval, matrix, RowCover, ColCover) {
  for (r in 1:length(matrix)) {  # Parcourir chaque ligne de la matrice
    for (c in 1:length(matrix)) {  # Parcourir chaque colonne de la matrice
      # Vérifier si la valeur à la position [r][c] est non couverte par les vecteurs de couverture
      if (RowCover[r] == 0 && ColCover[c] == 0) {
        # Si la valeur est plus petite que la valeur minimale actuelle, mettre à jour la valeur minimale
        if (minval > matrix[[r]][[c]]) {
          minval <- matrix[[r]][[c]]
        }
      }
    }
  }
}

step6 <- function(matrix, row_cover, col_cover, step) {
  # Trouver la plus petite valeur non couverte dans la matrice
  min_value <- min(matrix[which(row_cover == 0), which(col_cover == 0)], na.rm = TRUE)
  
  size <- nrow(matrix)
  # Parcourir chaque élément de la matrice
  for (r in 1:size) {
    for (c in 1:size) {
      # Si la ligne est couverte, ajouter la plus petite valeur non couverte à chaque élément de la ligne
      if (row_cover[r] == 1) {
        matrix[r, c] <- matrix[r, c] + min_value
      }
      # Si la colonne n'est pas couverte, soustraire la plus petite valeur non couverte à chaque élément de la colonne
      if (col_cover[c] == 0) {
        matrix[r, c] <- matrix[r, c] - min_value
      }
    }
  }
  
  # Revenir à l'étape 4 de l'algorithme
  step <- 4
  return(list(step = step, matrix = matrix))
}

# Fonction pour l'algorithme hongrois
Hungarian_R <- function(matrix, verbose = FALSE) {
  # Copie de la matrice originale
  original_matrix <- copy_matrix(matrix)
  
  # Conversion de la matrice en une matrice carrée
  matrix <- adjust_matrix(matrix)
  sz <- nrow(matrix)
  
  # Matrice masquée M : 1 pour les zéros étoilés, 2 pour les zéros primés
  M <- matrix(0, nrow = sz, ncol = sz)
  
  # Vecteurs de couverture des lignes et des colonnes
  RowCover <- rep(0, sz)
  ColCover <- rep(0, sz)
  
  # Variables temporaires pour stocker la plus petite valeur non couverte
  path_row_0 <- 1
  path_col_0 <- 1
  
  # Tableau pour l'algorithme du chemin augmentant
  path <- matrix(1, nrow = sz + 1, ncol = 2)
  
  # Affichage de la matrice d'entrée si verbose est activé
  if (verbose) {
    print(matrix)
    print("----------")
  }
  
  done <- FALSE
  step <- 1
  while (!done) {
    if(step == 1){
      result <- step1(matrix, step)
      step <- result$step
      matrix <- result$matrix
      if (verbose) {
        print(matrix)
        print("----------Step : 1")
      }
    }else if(step == 2){
      result <- step2(matrix, M, RowCover, ColCover, step)
      step <- result$step
      matrix <- result$matrix
      if (verbose) {
        print(matrix)
        print("----------Step : 2")
      }
    }else if(step == 3){
      result <- step3(M, ColCover, step)
      step <- result$step
      if (verbose) {
        print(matrix)
        print("----------Step : 3")
      }
    }else if(step == 4){
      result <- step4(matrix, M, RowCover, ColCover, path_row_0, path_col_0, step)
      step <- result$step
      if(is.null(result$row)){
        path_row_0 <- result$row
        path_col_0 <- result$col
      }
      if (verbose) {
        print(matrix)
        print("----------Step : 4")
      }
    }else if(step == 5){
      result <- step5(path, path_row_0, path_col_0, M, RowCover, ColCover, step)
      step <- result$step
      if (verbose) {
        print(matrix)
        print("----------Step : 5")
      }
    }else if(step == 6){
      result <- step6(matrix, RowCover, ColCover, step)
      step <- result$step
      matrix <- result$matrix
      if (verbose) {
        print(matrix)
        print("----------Step : 6")
      }
    }else{
      # Redimensionner M pour qu'elle ait la même taille que la matrice originale
      M <- M[1:nrow(original_matrix), 1:ncol(original_matrix)]
      if (verbose) {
        print("Original Matrix:")
        print_matrix(original_matrix)
        print("Assignments Matrix:")
        print_matrix(M)
        print("----------Final Step : 7")
      }
      done <- TRUE
    }
  }
  
  # Calcul de la valeur de la solution
  return(M)
}