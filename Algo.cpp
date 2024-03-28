#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <limits>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerMatrix hungarian_algorithm(NumericMatrix cost_matrix) {
  int n = cost_matrix.nrow();
  
  // Création des tableaux de support
  vector<int> u(n);
  vector<int> v(n);
  vector<bool> p(n);
  vector<int> way(n);
  
  // Initialisation des tableaux de support
  for (int i = 0; i < n; ++i) {
    u[i] = 0;
    v[i] = numeric_limits<int>::max();
    p[i] = false;
    way[i] = -1;
  }
  
  // Calcul des tableaux de support
  for (int i = 0; i < n; ++i) {
    p[0] = true;
    int j0 = 0;
    vector<int> minv(n, numeric_limits<int>::max());
    vector<bool> used(n, false);
    
    do {
      used[j0] = true;
      int i0 = -1;
      int delta = numeric_limits<int>::max();
      for (int j = 0; j < n; ++j) {
        if (!used[j]) {
          int cur = cost_matrix(i, j) - u[i] - v[j];
          if (cur < minv[j]) {
            minv[j] = cur;
            way[j] = i;
          }
          if (minv[j] < delta) {
            delta = minv[j];
            j0 = j;
          }
        }
      }
      
      for (int j = 0; j < n; ++j) {
        if (used[j]) {
          u[way[j]] += delta;
          v[j] -= delta;
        } else {
          minv[j] -= delta;
        }
      }
      
      int j1 = j0;
      do {
        p[way[j1]] = true;
        j1 = way[j1];
      } while (j1 != 0);
      j0 = j1;
    } while (way[j0] != -1);
    
    do {
      int j1 = way[j0];
      way[j0] = way[j1];
      j0 = j1;
    } while (j0 != 0);
  }
  
  // Récupération de l'affectation optimale
  IntegerMatrix assignment(n, 2);
  for (int j = 0, count = 0; j < n; ++j) {
    if (p[j]) {
      assignment(count, 0) = way[j];
      assignment(count, 1) = j;
      count++;
    }
  }
  
  return assignment;
}

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}
