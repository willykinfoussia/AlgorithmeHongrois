#include <algorithm>
#include <cmath>
#include <iostream>
#include <iterator>
#include <limits>
#include <list>
#include <string>
#include <type_traits>
#include <vector>
#include <chrono> // Pour mesurer le temps
#include <numeric>
#include <random>

using namespace std;
using namespace std::chrono;

#include <Rcpp.h>
using namespace Rcpp;


// Fonction pour imprimer une chaîne de caractères
void print(const string& message) {
    cout << message << endl; // Affiche la chaîne de caractères suivie d'un saut de ligne
}

// Fonction pour imprimer un entier
void print(int number) {
    cout << number << endl; // Affiche l'entier suivi d'un saut de ligne
}

// Modèle de fonction pour imprimer un message et une valeur de types différents
template <typename T, typename U>
void print(const T& message, const U& value) {
    cout << message << value << endl; // Affiche le message, la valeur et un saut de ligne
}

// Modèle de fonction pour imprimer un vecteur de n'importe quel type
template <typename T>
void print(const vector<T>& my_vector){
    for (const T& elem : my_vector) { // Parcourt chaque élément du vecteur
        cout << elem << " "; // Affiche l'élément suivi d'un espace
    }
    cout << endl; // Saut de ligne après avoir imprimé tous les éléments
}

// Modèle de fonction pour imprimer une matrice (vecteur de vecteurs) de n'importe quel type
template <typename T>
void print(vector<vector<T>> matrix){
    for (const auto& row : matrix) { // Parcourt chaque ligne de la matrice
        print(row); // Appelle la fonction print pour imprimer chaque ligne
    }
}

// Modèle de fonction pour imprimer un vecteur de paires de n'importe quel type
template <typename T, typename U>
void print(vector<pair<T, U>> pairs){
    for (const auto& p : pairs) { // Parcourt chaque paire dans le vecteur
        cout << "(" << p.first << ", " << p.second << ")" << endl; // Affiche la paire suivie d'un saut de ligne
    }
}

void copy_matrix(const vector<vector<int>>& src, vector<vector<int>>& dst) {
    dst.clear(); // Effacer le contenu de la matrice de destination

    // Copier les lignes de la matrice source vers la matrice de destination
    for (const auto& row : src) {
        // Créer une nouvelle ligne dans la matrice de destination avec les éléments de la ligne source
        vector<int> copied_row(row.begin(), row.end()); // Utilisation du constructeur de plage pour copier les éléments
        dst.push_back(copied_row); // Ajouter la ligne copiée à la matrice de destination
    }
}

/**
 * @brief Remplir la matrice pour la rendre carrée si nécessaire en ajoutant des lignes ou des colonnes fictives.
 * 
 * Cette fonction ajuste une matrice pour s'assurer qu'elle est carrée en ajoutant des lignes ou des colonnes fictives si nécessaire.
 * Si le nombre de lignes est supérieur au nombre de colonnes, des éléments fictifs sont ajoutés à chaque ligne
 * jusqu'à ce que le nombre de colonnes soit égal au nombre de lignes. Si le nombre de colonnes est supérieur au
 * nombre de lignes, des lignes fictives sont ajoutées jusqu'à ce que le nombre de lignes soit égal au nombre de colonnes.
 * 
 * @tparam T Le type des éléments dans la matrice.
 * @param matrix La matrice à remplir.
 */
template<typename T>
void adjust_matrix(vector<vector<T>>& matrix)
{
    size_t rows = matrix.size(); // Nombre de lignes dans la matrice
    size_t cols = matrix[0].size(); // Nombre de colonnes dans la matrice (supposée non vide)

    // Si le nombre de lignes est supérieur au nombre de colonnes
    if (rows > cols) {
        // Ajouter des éléments fictifs à chaque ligne jusqu'à ce que le nombre de colonnes soit égal au nombre de lignes
        for (auto& row: matrix)
            row.resize(rows, numeric_limits<T>::max()); // Ajoute des éléments fictifs (définis par la valeur maximale de type T)
    }
    // Si le nombre de colonnes est supérieur au nombre de lignes
    else if (rows < cols) {
        // Ajouter des lignes fictives jusqu'à ce que le nombre de lignes soit égal au nombre de colonnes
        while (matrix.size() < cols)
            matrix.push_back(vector<T>(cols, numeric_limits<T>::max())); // Ajoute une ligne fictive remplie de valeurs maximales de type T
    }
}

/**
 * @brief Pour chaque ligne de la matrice, trouver le plus petit élément et le soustraire à chaque élément de sa ligne.
 * Pour chaque colonne de la matrice, trouver le plus petit élément et le soustraire à chaque élément de sa colonne. Passer à l'étape 2.
 * 
 * @tparam T Le type des éléments dans la matrice.
 * @param matrix La matrice à traiter.
 * @param step Un compteur de l'étape actuelle de l'algorithme (modifié par référence).
 */
template<typename T>
void step1(vector<vector<T>>& matrix, int& step)
{
    // Ajustement des lignes
    for (auto& row: matrix) { // Pour chaque ligne de la matrice
        auto smallest_in_row = *min_element(begin(row), end(row)); // Trouver le plus petit élément dans la ligne
        if (smallest_in_row > 0) { // Si le plus petit élément est supérieur à zéro
            for (auto& element: row) // Pour chaque élément de la ligne
                element -= smallest_in_row; // Soustraire le plus petit élément à l'élément actuel
        }
    }
    
    // Ajustement des colonnes
    int size = matrix.size(); // La taille de la matrice est garantie d'être carrée
    for (int column = 0; column < size; ++column) { // Pour chaque colonne de la matrice
        T min_in_column = numeric_limits<T>::max(); // Initialiser la valeur minimale à la valeur maximale du type T
        for (int row = 0; row < size; ++row) { // Parcourir chaque élément de la colonne
            min_in_column = min(min_in_column, matrix[row][column]); // Trouver le plus petit élément dans la colonne
        }
        
        if (min_in_column > 0) { // Si le plus petit élément est supérieur à zéro
            for (int row = 0; row < size; ++row) { // Pour chaque élément de la colonne
                matrix[row][column] -= min_in_column; // Soustraire le plus petit élément à l'élément actuel
            }
        }
    }
   
    step = 2; // Passer à l'étape 2 de l'algorithme
}


/**
 * @brief Réinitialiser les vecteurs temporaires en les remplissant de zéros.
 * 
 * Cette fonction parcourt le vecteur donné en entrée et initialise chaque élément à zéro.
 * 
 * @param cover Le vecteur à réinitialiser.
 */
inline void reset_temporary_vectors(vector<int>& vector_to_reset) 
{
    for (auto& element: vector_to_reset) 
        element = 0; // Pour chaque élément du vecteur, assigner zéro
}

/**
 * @brief Trouver un zéro (Z) dans la matrice résultante. Si aucun zéro étoilé n'est présent dans sa ligne ou sa colonne,
 * étoiler Z. Répéter pour chaque élément de la matrice. Passer à l'étape 3. Dans cette étape, nous introduisons la matrice
 * de masquage M, qui a les mêmes dimensions que la matrice de coût et est utilisée pour étoiler et primariser les zéros de
 * la matrice de coût. Si M(i,j)=1, alors C(i,j) est un zéro étoilé. Si M(i,j)=2, alors C(i,j) est un zéro primarisé. Nous définissons
 * également deux vecteurs RowCover et ColCover qui sont utilisés pour "couvrir" les lignes et les colonnes de la matrice de coût.
 * Dans la boucle imbriquée (sur les indices i et j), nous vérifions si C(i,j) est une valeur zéro et si sa colonne ou sa ligne n'est
 * pas déjà couverte. Si ce n'est pas le cas, nous étoilons ce zéro (c'est-à-dire que nous définissons M(i,j)=1) et couvrons sa ligne
 * et sa colonne (c'est-à-dire que nous définissons R_cov(i)=1 et C_cov(j)=1). Avant de passer à l'étape 3, nous découvrons toutes
 * les lignes et colonnes afin de pouvoir utiliser les vecteurs de couverture pour nous aider à compter le nombre de zéros étoilés.
 * 
 * @tparam T Le type des éléments dans la matrice.
 * @param matrix La matrice d'entrée.
 * @param M La matrice de masquage (modifiée par référence).
 * @param RowCover Vecteur de couverture des lignes (modifié par référence).
 * @param ColCover Vecteur de couverture des colonnes (modifié par référence).
 * @param step Un compteur de l'étape actuelle de l'algorithme (modifié par référence).
 */
template<typename T>
void step2(const vector<vector<T>>& matrix, vector<vector<int>>& M, vector<int>& RowCover,vector<int>& ColCover, int& step)
{
    int size = matrix.size(); // Taille de la matrice

    // Parcourir la matrice
    for (int row = 0; row < size; ++row) {
        for (int col = 0; col < size; ++col) {
            if (matrix[row][col] == 0) { // Si un zéro est trouvé dans la matrice
                if (RowCover[row] == 0 && ColCover[col] == 0) { // Vérifier si la ligne et la colonne ne sont pas déjà couvertes
                    M[row][col] = 1; // Étoiler le zéro
                    RowCover[row] = 1; // Couvrir la ligne
                    ColCover[col] = 1; // Couvrir la colonne
                }
            }
        }
    }

    reset_temporary_vectors(RowCover); // Réinitialiser le vecteur de couverture des lignes pour une utilisation ultérieure
    reset_temporary_vectors(ColCover); // Réinitialiser le vecteur de couverture des colonnes pour une utilisation ultérieure
    
    step = 3; // Passer à l'étape 3 de l'algorithme
}


/**
 * @brief Couvrir chaque colonne contenant un zéro étoilé. Si K colonnes sont couvertes, les zéros étoilés décrivent un ensemble complet
 * d'associations uniques. Dans ce cas, aller à TERMINÉ, sinon, aller à l'étape 4. Une fois que nous avons parcouru l'ensemble de la matrice
 * de coût, nous comptons le nombre de zéros indépendants trouvés. Si nous avons trouvé (et étoilé) K zéros indépendants, alors nous avons terminé.
 * Sinon, nous passons à l'étape 4.
 * 
 * @param M La matrice de masquage.
 * @param ColCover Le vecteur de couverture des colonnes (modifié par référence).
 * @param step Un compteur de l'étape actuelle de l'algorithme (modifié par référence).
 */
void step3(const vector<vector<int>>& M, vector<int>& ColCover, int& step)
{
    int size = M.size(); // Taille de la matrice
    int covered_columns = 0; // Nombre de colonnes couvertes

    // Parcourir la matrice de masquage
    for (int row = 0; row < size; ++row) {
        for (int col = 0; col < size; ++col) {
            if (M[row][col] == 1) { // Si un zéro étoilé est trouvé dans la matrice de masquage
                ColCover[col] = 1; // Couvrir la colonne correspondante
            }
        }
    }

    // Compter le nombre de colonnes couvertes
    for (auto& cover_status: ColCover) {
        if (cover_status == 1) {
            covered_columns++;
        }
    }

    // Si toutes les colonnes sont couvertes
    if (covered_columns >= size) {
        step = 7; // solution trouvée
    }
    else {
        step = 4; // passer à l'étape 4 de l'algorithme
    }
}


/**
 * @brief Trouver un zéro dans la matrice qui n'est pas couvert par RowCover ou ColCover.
 * 
 * Cette fonction parcourt la matrice donnée ainsi que les vecteurs de couverture des lignes
 * et des colonnes pour trouver un zéro non couvert. Elle met à jour les variables row et col
 * avec les coordonnées du zéro trouvé.
 * 
 * @tparam T Le type des éléments dans la matrice.
 * @param row La ligne où le zéro est trouvé (mis à jour par référence).
 * @param col La colonne où le zéro est trouvé (mis à jour par référence).
 * @param matrix La matrice à rechercher.
 * @param RowCover Le vecteur de couverture des lignes.
 * @param ColCover Le vecteur de couverture des colonnes.
 */
template<typename T>
void find_uncovered_zero(int& row, int& col, const vector<vector<T>>& matrix, const vector<int>& RowCover, const vector<int>& ColCover)
{
    int current_row = 0; // Indice de ligne initialisé à 0
    int current_col = 0; // Indice de colonne initialisé à 0
    int size = matrix.size(); // Taille de la matrice
    bool found = false; // Indicateur pour indiquer si un zéro a été trouvé
    row = -1; // Initialisation de l'indice de ligne du zéro trouvé à -1
    col = -1; // Initialisation de l'indice de colonne du zéro trouvé à -1

    // Boucle pour parcourir la matrice
    while (!found) {
        current_col = 0; // Réinitialiser l'indice de colonne à 0
        while (true) {
            // Vérifier si le zéro actuel n'est pas couvert
            if (matrix[current_row][current_col] == 0 && 
                RowCover[current_row] == 0 && 
                ColCover[current_col] == 0) {
                // Mettre à jour les indices du zéro trouvé
                row = current_row;
                col = current_col;
                found = true; // Indiquer que le zéro a été trouvé
            }
            current_col += 1; // Passer à la colonne suivante
            // Vérifier si toute la ligne a été parcourue ou si un zéro a été trouvé
            if (current_col >= size || found) 
                break; // Sortir de la boucle interne
        }
        current_row += 1; // Passer à la ligne suivante
        // Vérifier si toute la matrice a été parcourue
        if (current_row >= size) 
            found = true; // Indiquer que la recherche est terminée
    }
}


/**
 * @brief Vérifie si une ligne donnée de la matrice de masquage contient au moins un zéro étoilé.
 * 
 * Cette fonction parcourt la ligne spécifiée de la matrice de masquage et vérifie si au moins un zéro
 * étoilé est présent dans cette ligne.
 * 
 * @param row La ligne à vérifier.
 * @param M La matrice de masquage.
 * @return true si la ligne contient un zéro étoilé, sinon false.
 */
bool has_starred_zero_in_row(int row, const vector<vector<int>>& M)
{
    bool has_starred_zero = false; // Variable indiquant si un zéro étoilé a été trouvé, initialisée à false
    
    // Parcourir les colonnes de la ligne spécifiée
    for (unsigned col = 0; col < M.size(); col++) {
        // Vérifier si un zéro étoilé est présent dans la ligne
        if (M[row][col] == 1) {
            has_starred_zero = true; // Mettre à jour la variable indiquant la présence d'un zéro étoilé
            break; // Sortir de la boucle car un zéro étoilé a été trouvé
        }
    }
    
    return has_starred_zero; // Retourner true si un zéro étoilé a été trouvé, sinon false
}


/**
 * @brief Recherche un zéro étoilé dans une ligne spécifique de la matrice de masquage et renvoie la colonne où ce zéro est trouvé.
 * 
 * Cette fonction parcourt la ligne spécifiée de la matrice de masquage et recherche un zéro étoilé.
 * Si un zéro étoilé est trouvé dans la ligne, la fonction met à jour la variable col avec la colonne où ce zéro étoilé est trouvé.
 * 
 * @param row La ligne à rechercher.
 * @param col La colonne où le zéro étoilé est trouvé (mis à jour par référence).
 * @param M La matrice de masquage.
 */
void find_starred_zero_in_row(int row, int& col, const vector<vector<int>>& M)
{
    col = -1; // Initialiser la colonne à -1 pour indiquer qu'aucun zéro étoilé n'a été trouvé
    
    // Parcourir les colonnes de la ligne spécifiée
    for (unsigned column = 0; column < M.size(); column++) {
        // Si un zéro étoilé est trouvé dans la ligne
        if (M[row][column] == 1) {
            col = column; // Mettre à jour la colonne où le zéro étoilé est trouvé
            break; // Sortir de la boucle car un zéro étoilé a été trouvé
        }
    }
}

/**
 * @brief Trouver un zéro non couvert et le primariser. Si aucun zéro étoilé n'existe dans la ligne contenant ce zéro primarisé, aller à l'étape 5.
 * Sinon, couvrir cette ligne et découvrir la colonne contenant le zéro étoilé. Continuer de cette manière jusqu'à ce qu'il n'y ait plus de zéros non couverts
 * restants. Sauvegarder la plus petite valeur non couverte et aller à l'étape 6.
 * 
 * @tparam T Le type des éléments dans la matrice.
 * @param matrix La matrice d'entrée.
 * @param M La matrice de masquage (modifiée par référence).
 * @param RowCover Le vecteur de couverture des lignes (modifié par référence).
 * @param ColCover Le vecteur de couverture des colonnes (modifié par référence).
 * @param path_row_0 L'indice de la ligne du zéro primarisé (modifié par référence).
 * @param path_col_0 L'indice de la colonne du zéro primarisé (modifié par référence).
 * @param step Un compteur de l'étape actuelle de l'algorithme (modifié par référence).
 */
template<typename T>
void step4(const vector<vector<T>>& matrix, vector<vector<int>>& M, vector<int>& RowCover, vector<int>& ColCover, int& path_row_0, int& path_col_0, int& step)
{
    int row = -1; // L'indice de ligne du zéro non couvert initialisé à -1
    int col = -1; // L'indice de colonne du zéro non couvert initialisé à -1
    bool done = false; // Variable de terminaison de la boucle

    while (!done) { // Boucle jusqu'à ce qu'un zéro non couvert soit trouvé ou que toute la matrice soit parcourue

        find_uncovered_zero(row, col, matrix, RowCover, ColCover); // Trouver un zéro non couvert dans la matrice

        if (row == -1) { // Si aucun zéro non couvert n'est trouvé dans la matrice
            done = true; // Indiquer que la recherche est terminée
            step = 6; // Passer à l'étape 6 de l'algorithme
        }
        else { // Si un zéro non couvert est trouvé dans la matrice
            M[row][col] = 2; // Primariser ce zéro
            if (has_starred_zero_in_row(row, M)) { // Si un zéro étoilé existe dans la ligne contenant ce zéro primarisé
                find_starred_zero_in_row(row, col, M); // Trouver le zéro étoilé dans la même ligne
                RowCover[row] = 1; // Couvrir cette ligne
                ColCover[col] = 0; // Découvrir la colonne contenant le zéro étoilé
            }
            else { // Si aucun zéro étoilé n'existe dans la ligne contenant ce zéro primarisé
                done = true; // Indiquer que la recherche est terminée
                step = 5; // Passer à l'étape 5 de l'algorithme
                path_row_0 = row; // Sauvegarder l'indice de la ligne du zéro primarisé
                path_col_0 = col; // Sauvegarder l'indice de la colonne du zéro primarisé
            }
        }
    }
}


/**
 * @brief Trouver un zéro étoilé dans une colonne spécifique de la matrice de masquage et renvoyer la ligne où ce zéro est trouvé.
 * 
 * Cette fonction parcourt la colonne spécifiée de la matrice de masquage et recherche un zéro étoilé.
 * Si un zéro étoilé est trouvé dans la colonne, la fonction met à jour la variable r avec la ligne où ce zéro étoilé est trouvé.
 * 
 * @param c La colonne à rechercher.
 * @param r La ligne où le zéro étoilé est trouvé (mis à jour par référence).
 * @param M La matrice de masquage.
 */
void find_starred_zero_in_column(int col, int& row, const vector<vector<int>>& M)
{
    row = -1; // Initialiser la ligne où le zéro étoilé est trouvé à -1 pour indiquer qu'aucun zéro étoilé n'a été trouvé
    for (unsigned i = 0; i < M.size(); i++) { // Parcourir les lignes de la colonne spécifiée
        if (M[i][col] == 1) { // Si un zéro étoilé est trouvé dans la colonne
            row = i; // Mettre à jour la ligne où le zéro étoilé est trouvé
            break; // Sortir de la boucle car un zéro étoilé a été trouvé
        }
    }
}


/**
 * @brief Recherche un zéro primarisé dans une ligne spécifique de la matrice de masquage et renvoie la colonne où ce zéro est trouvé.
 * 
 * Cette fonction parcourt la ligne spécifiée de la matrice de masquage et recherche un zéro primarisé.
 * Si un zéro primarisé est trouvé dans la ligne, la fonction met à jour la variable c avec la colonne où ce zéro primarisé est trouvé.
 * 
 * @param r La ligne à rechercher.
 * @param c La colonne où le zéro primarisé est trouvé (mis à jour par référence).
 * @param M La matrice de masquage.
 */
void find_primed_zero_in_row(int row, int& col, vector<vector<int>>& M)
{
    for (unsigned j = 0; j < M.size(); j++) { // Parcourir les colonnes de la ligne spécifiée
        if (M[row][j] == 2) { // Si un zéro primarisé est trouvé dans la ligne
            col = j; // Mettre à jour la colonne où le zéro primarisé est trouvé
            return; // Sortir de la fonction car un zéro primarisé a été trouvé
        }
    }
    col = -1; // Si aucun zéro primarisé n'est trouvé dans la ligne, mettre à jour col à -1
}


/**
 * @brief Augmente un chemin donné dans la matrice de masquage M.
 * 
 * Cette fonction parcourt le chemin donné et inverse l'état des zéros sur ce chemin dans la matrice de masquage.
 * Si un zéro était marqué (était un zéro étoilé), il devient non marqué (zéro non étoilé), et vice versa.
 * 
 * @param path Le chemin à augmenter.
 * @param path_count Le nombre de zéros dans le chemin.
 * @param M La matrice de masquage (modifiée par référence).
 */
void augment_path(vector<vector<int>>& path, int path_count, vector<vector<int>>& M)
{
    for (int p = 0; p < path_count; p++) { // Parcourir les zéros dans le chemin
        if (M[path[p][0]][path[p][1]] == 1) { // Si le zéro est marqué (zéro étoilé)
            M[path[p][0]][path[p][1]] = 0; // Inverser son état (devient zéro non marqué)
        } else { // Sinon (si le zéro n'est pas marqué)
            M[path[p][0]][path[p][1]] = 1; // Inverser son état (devient zéro marqué)
        }
    }
}


/**
 * @brief Efface tous les zéros primarisés dans la matrice de masquage M.
 * 
 * Cette fonction parcourt chaque élément de la matrice de masquage et remplace chaque zéro primarisé (avec une valeur de 2) par un zéro non marqué (avec une valeur de 0).
 * 
 * @param M La matrice de masquage (modifiée par référence).
 */
void erase_primes(vector<vector<int>>& M)
{
    for (auto& row: M) { // Parcourir chaque ligne de la matrice
        for (auto& val: row) { // Parcourir chaque élément de la ligne
            if (val == 2) { // Si l'élément est un zéro primarisé
                val = 0; // Remplacer le zéro primarisé par un zéro non marqué
            }
        }
    }
}


/**
 * @brief Construit une série de zéros primarisés et étoilés alternés et effectue différentes actions sur la matrice et les couvertures des lignes et colonnes en fonction de cette série.
 * 
 * Cette fonction suit un processus qui commence par un zéro primarisé découvert dans l'étape 4. Elle trouve ensuite un zéro étoilé dans la même colonne, puis un autre zéro primarisé dans la même ligne, et continue ainsi jusqu'à ce qu'elle atteigne un zéro primarisé sans zéro étoilé dans sa colonne. Ensuite, elle met à jour la matrice de masquage et les couvertures des lignes et colonnes selon les règles spécifiées et revient à l'étape 3.
 * 
 * @param path La série de zéros primarisés et étoilés alternés (modifiée par référence).
 * @param path_row_0 L'indice de ligne du zéro primarisé initial.
 * @param path_col_0 L'indice de colonne du zéro primarisé initial.
 * @param M La matrice de masquage (modifiée par référence).
 * @param RowCover Le vecteur de couverture des lignes (modifié par référence).
 * @param ColCover Le vecteur de couverture des colonnes (modifié par référence).
 * @param step Le compteur de l'étape actuelle de l'algorithme (modifié par référence).
 */
void step5(vector<vector<int>>& path, int path_row_0, int path_col_0, vector<vector<int>>& M, vector<int>& RowCover, vector<int>& ColCover, int& step)
{
    int r = -1; // Initialiser l'indice de ligne à -1
    int c = -1; // Initialiser l'indice de colonne à -1
    int path_count = 1; // Initialiser le nombre de zéros dans la série à 1
    
    // Ajouter le zéro primarisé initial à la série
    path[path_count - 1][0] = path_row_0;
    path[path_count - 1][1] = path_col_0;

    
    bool done = false; // Variable de terminaison de la boucle
    while (!done) { // Tant que la série n'est pas terminée
        // Trouver un zéro étoilé dans la même colonne que le dernier zéro ajouté à la série
        find_starred_zero_in_column(path[path_count - 1][1], r, M);
        if (r > -1) { // Si un zéro étoilé est trouvé dans la colonne
            // Ajouter ce zéro étoilé à la série
            path_count += 1;
            path[path_count - 1][0] = r;
            path[path_count - 1][1] = path[path_count - 2][1];
        } else {
            done = true; // Si aucun zéro étoilé n'est trouvé, terminer la série
        }
        
        if (!done) { // Si la série n'est pas terminée
            // Trouver un zéro primarisé dans la même ligne que le dernier zéro ajouté à la série
            find_primed_zero_in_row(path[path_count - 1][0], c, M);
            // Ajouter ce zéro primarisé à la série
            path_count += 1;
            path[path_count - 1][0] = path[path_count - 2][0];
            path[path_count - 1][1] = c;
        }
    }
    
    // Mettre à jour la matrice de masquage en fonction de la série
    augment_path(path, path_count, M);
    // Réinitialiser les couvertures des lignes et colonnes
    reset_temporary_vectors(RowCover);
    reset_temporary_vectors(ColCover);
    // Effacer tous les zéros primarisés dans la matrice de masquage
    erase_primes(M);
    
    // Revenir à l'étape 3 de l'algorithme
    step = 3;
}


/**
 * @brief Trouve la plus petite valeur non couverte dans une matrice donnée.
 * 
 * Cette fonction parcourt chaque élément de la matrice et recherche la plus petite valeur qui n'est pas couverte par les vecteurs de couverture des lignes et des colonnes.
 * 
 * @tparam T Le type des éléments de la matrice.
 * @param minval La plus petite valeur non couverte (mise à jour par référence).
 * @param matrix La matrice dans laquelle chercher la plus petite valeur.
 * @param RowCover Le vecteur de couverture des lignes.
 * @param ColCover Le vecteur de couverture des colonnes.
 */
template<typename T>
void find_smallest(T& minval, const vector<vector<T>>& matrix, const vector<int>& RowCover, const vector<int>& ColCover)
{
    for (unsigned r = 0; r < matrix.size(); r++) { // Parcourir chaque ligne de la matrice
        for (unsigned c = 0; c < matrix.size(); c++) { // Parcourir chaque colonne de la matrice
            // Vérifier si la valeur à la position [r][c] est non couverte par les vecteurs de couverture
            if (RowCover[r] == 0 && ColCover[c] == 0) {
                // Si la valeur est plus petite que la valeur minimale actuelle, mettre à jour la valeur minimale
                if (minval > matrix[r][c]) {
                    minval = matrix[r][c];
                }
            }
        }
    }
}


/**
 * @brief Effectue des opérations sur la matrice en ajoutant la plus petite valeur non couverte trouvée dans l'étape précédente à chaque élément des lignes couvertes, et en soustrayant cette valeur à chaque élément des colonnes non couvertes.
 * 
 * Cette fonction ajoute la plus petite valeur non couverte trouvée dans l'étape précédente à chaque élément des lignes couvertes, et soustrait cette valeur à chaque élément des colonnes non couvertes de la matrice. Elle ne modifie pas les étoiles, les primés ou les lignes couvertes. Cette étape utilise la plus petite valeur non couverte dans la matrice de coût pour modifier la matrice. Elle attend pour trouver cette valeur jusqu'à l'étape 6 plutôt que de la chercher à l'étape 4. Cela garantit que la valeur utilisée est correcte, car certaines modifications peuvent avoir été apportées à la matrice dans les étapes précédentes.
 * 
 * @tparam T Le type des éléments de la matrice.
 * @param matrix La matrice à modifier.
 * @param RowCover Le vecteur de couverture des lignes.
 * @param ColCover Le vecteur de couverture des colonnes.
 * @param step Le compteur de l'étape actuelle de l'algorithme (modifié par référence).
 */
template<typename T>
void step6(vector<vector<T>>& matrix, const vector<int>& row_cover, const vector<int>& col_cover, int& step)
{
    // Trouver la plus petite valeur non couverte dans la matrice
    T min_value = numeric_limits<T>::max();
    find_smallest(min_value, matrix, row_cover, col_cover);
    
    int size = matrix.size();
    // Parcourir chaque élément de la matrice
    for (int r = 0; r < size; r++) {
        for (int c = 0; c < size; c++) {
            // Si la ligne est couverte, ajouter la plus petite valeur non couverte à chaque élément de la ligne
            if (row_cover[r] == 1)
                matrix[r][c] += min_value;
            // Si la colonne n'est pas couverte, soustraire la plus petite valeur non couverte à chaque élément de la colonne
            if (col_cover[c] == 0)
                matrix[r][c] -= min_value;
        }
    }
    
    // Revenir à l'étape 4 de l'algorithme
    step = 4;
}


/**
 * @brief Implémente l'algorithme de l'Algorithme hongrois pour résoudre le problème d'association.
 * 
 * @param matrix La matrice d'entrée du problème.
 * @param verbose Indique si les étapes intermédiaires doivent être affichées (par défaut false).
 * @return La valeur de la solution trouvée.
 */
// [[Rcpp::export]]
vector<vector<int>> Hungarian(vector<vector<int>> matrix, bool verbose = false){

    // Copie de la matrice originale
    vector<vector<int>> original_matrix;
    copy_matrix(matrix, original_matrix);

    // Conversion de la matrice en une matrice carrée
    adjust_matrix(matrix);
    size_t sz = matrix.size();

    // Matrice masquée M : 1 pour les zéros étoilés, 2 pour les zéros primés
    vector<vector<int>> M(sz, vector<int>(sz, 0));

    // Vecteurs de couverture des lignes et des colonnes
    vector<int> RowCover(sz, 0);
    vector<int> ColCover(sz, 0);

    // Variables temporaires pour stocker la plus petite valeur non couverte
    int path_row_0, path_col_0;

    // Tableau pour l'algorithme du chemin augmentant
    vector<vector<int>> path(sz + 1, vector<int>(2, 0));

    // Affichage de la matrice d'entrée si verbose est activé
    if (verbose) {
        print(matrix);
        print("----------");
    }

    bool done = false;
    int step = 1;
    while (!done) {
        switch (step) {
            case 1:
                step1(matrix, step);
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 1);
                }
                break;
            case 2:
                step2(matrix, M, RowCover, ColCover, step);
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 2);
                }
                break;
            case 3:
                step3(M, ColCover, step);
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 3);
                }
                break;
            case 4:
                step4(matrix, M, RowCover, ColCover, path_row_0, path_col_0, step);
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 4);
                }
                break;
            case 5:
                step5(path, path_row_0, path_col_0, M, RowCover, ColCover, step);
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 5);
                }
                break;
            case 6:
                step6(matrix, RowCover, ColCover, step);
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 6);
                }
                break;
            case 7:
                // Redimensionner M pour qu'elle ait la même taille que la matrice originale
                for (auto& vec: M) {
                    vec.resize(matrix.begin()->size());
                }
                M.resize(matrix.size());
                if (verbose) {
                    print("Original Matrix:");
                    print(matrix);
                    print("Assignments Matrix:");
                    print(M);
                    print("----------Final Step : ", 7);
                }
                done = true;
                break;
            default:
                if (verbose) {
                    print(matrix);
                    print("----------Step : ", 8);
                }
                done = true;
                break;
        }
    }

    // Calcul de la valeur de la solution
    return M;
}


// Fonction pour générer une matrice de taille n x n en fonction de k
vector<vector<int>> generateMatrix(int n, int k) {
    // Initialisation de la matrice avec des valeurs nulles
    vector<vector<int>> matrix(n, vector<int>(n, 0));

    // Parcours de chaque élément de la matrice
    for (int i = 0; i < n * n; ++i) {
        // Calcul du numéro de ligne et de colonne en fonction de l'indice i
        int row = i / n;
        int col = i % n;

        // Calcul de la valeur de la cellule en fonction de i, row, col et k
        matrix[row][col] = i + (row + col) % k;
    }

    // Retourne la matrice générée
    return matrix;
}

int main() {
    vector<vector<int>> matrix_test = generateMatrix(6, 4);
    print(matrix_test);

    // Définir la matrice de coût
    vector<vector<int>> matrix = generateMatrix(1000, 100);

    // Nombre de fois à exécuter l'algorithme
    int num_executions = 100;
    
    // Vecteur pour stocker les temps d'exécution
    vector<double> execution_times(num_executions);

    // Exécuter l'algorithme plusieurs fois et mesurer le temps d'exécution à chaque fois
    for (int i = 0; i < num_executions; ++i) {
        auto start = chrono::steady_clock::now();
        Hungarian(matrix);
        auto end = chrono::steady_clock::now();
        chrono::duration<double, milli> duration = end - start;
        execution_times[i] = duration.count();
    }

    // Calculer la moyenne des temps d'exécution
    double mean_execution_time = accumulate(execution_times.begin(), execution_times.end(), 0.0) / num_executions;

    // Calculer l'écart type
    double variance = 0.0;
    for (double time : execution_times) {
        variance += pow(time - mean_execution_time, 2);
    }
    variance /= num_executions;
    double standard_deviation = sqrt(variance);

    // Calculer l'intervalle de confiance à 95%
    double confidence_interval = 1.96 * (standard_deviation / sqrt(num_executions));

    // Afficher les résultats
    cout << "Moyenne du temps d'execution : " << mean_execution_time << " millisecondes" << endl;
    cout << "Intervalle de confiance (95%) : [" << mean_execution_time - confidence_interval
              << ", " << mean_execution_time + confidence_interval << "] millisecondes" << endl;

    return 0;
}