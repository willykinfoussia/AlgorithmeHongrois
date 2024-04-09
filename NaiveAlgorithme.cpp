#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric> // Inclure l'en-tête pour la fonction iota
#include <limits>
#include <utility>

using namespace std;

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

// Fonction pour convertir une liste d'associations en une matrice d'adjacence
vector<vector<int>> associationListToAdjacencyMatrix(const vector<int>& association) {
    int n = association.size(); // Taille de la liste d'associations
    // Initialiser une matrice n x n avec tous les éléments à zéro
    vector<vector<int>> adjacencyMatrix(n, vector<int>(n, 0));
    // Parcourir la liste d'associations
    for (int i = 0; i < n; ++i) {
        // Pour chaque élément i dans la liste d'associations, définir l'élément correspondant dans la matrice d'adjacence à 1
        adjacencyMatrix[i][association[i]] = 1;
    }
    return adjacencyMatrix; // Retourner la matrice d'adjacence générée
}

// Fonction pour calculer le coût total d'une affectation donnée
int calculateCost(const vector<vector<int>>& matrix, const vector<int>& assignment) {
    int totalCost = 0; // Initialisation du coût total à zéro
    // Parcourir chaque ligne de la matrice
    for (size_t i = 0; i < matrix.size(); ++i) {
        // Ajouter le coût de l'affectation actuelle de l'élément i (selon la liste assignment) au coût total
        totalCost += matrix[i][assignment[i]];
    }
    return totalCost; // Retourner le coût total calculé
}

/**
 * @brief Résout le problème d'affectation en générant toutes les permutations possibles de l'affectation et en trouvant celle avec le coût minimal.
 * 
 * @param matrix La matrice d'association.
 * @return Une paire contenant le vecteur d'assignation avec le coût minimal et le coût minimal lui-même.
 */
// [[Rcpp::export]]
vector<vector<int>> NaiveAlgorithme(const vector<vector<int>>& matrix, bool verbose = false) {
    if(verbose){
        print("Original Matrix : ");
        print(matrix);
    }

    int n = matrix.size(); // Taille de la matrice
    
    // Vecteur pour stocker l'assignation optimale avec le coût minimal
    vector<int> minAssignment(n);
    
    // Initialisation du coût minimal avec une valeur maximale
    int minCost = numeric_limits<int>::max();
    
    // Vecteur pour stocker l'assignation courante
    vector<int> currentAssignment(n);
    
    // Vecteur pour stocker les indices de permutation
    vector<int> indices(n);
    iota(indices.begin(), indices.end(), 0); // Remplir les indices de 0 à n-1
    
    // Génération de toutes les permutations possibles
    do {
        // Construction de l'assignation courante en utilisant les indices de permutation
        for (int i = 0; i < n; ++i) {
            currentAssignment[i] = indices[i];
        }
        
        // Calcul du coût de l'assignation courante
        int currentCost = calculateCost(matrix, currentAssignment);
        
        // Mise à jour de l'assignation optimale et du coût minimal si nécessaire
        if (currentCost < minCost) {
            minCost = currentCost;
            minAssignment = currentAssignment;
        }
    } while (next_permutation(indices.begin(), indices.end())); // Générer la prochaine permutation

    vector<vector<int>> adjacencyMatrix = associationListToAdjacencyMatrix(minAssignment);
    if(verbose){
        print("Affectation optimal :");
        print(minAssignment);
        print("Matrix:");
        print(adjacencyMatrix);
        print("Minimal Cost : ", minCost);
    }

    // Retourner une paire contenant l'assignation avec le coût minimal et le coût minimal lui-même
    return adjacencyMatrix;
}

int main(){
    // Exemple d'utilisation
    vector<vector<int>> matrix = {{2, 1, 0}, {0, 0, 3}, {3, 0, 2}};
    NaiveAlgorithme(matrix);
    return 0;
}