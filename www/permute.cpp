// www/permute.cpp - Version corrigée
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// ---------- Structures et constantes ----------------------------------------

struct Genotype {
  int a1, a2;
  bool valid;
  
  Genotype(int g, int missing_code) : a1(0), a2(0), valid(false) {
    if (g == NA_INTEGER || g == missing_code || g <= 0 || g > 999999) return;
    a1 = g / 1000;
    a2 = g % 1000;
    valid = (a1 > 0 && a2 > 0);
  }
};

// ---------- Fonctions optimisées --------------------------------------------

// Fisher-Yates shuffle optimisé
inline void fy_shuffle(std::vector<int>& x) {
  const int n = x.size();
  for (int i = n - 1; i > 0; --i) {
    int j = static_cast<int>(R::unif_rand() * (i + 1));
    std::swap(x[i], x[j]);
  }
}

// Décodage rapide des génotypes
static void extract_alleles(const IntegerMatrix& m, int j, 
                           const std::vector<int>& rows,
                           int missing_code,
                           std::vector<int>& alleles,
                           std::vector<int>& valid_indices) {
  alleles.clear();
  valid_indices.clear();
  
  for (int idx : rows) {
    int gt = m(idx, j);
    if (gt == NA_INTEGER || gt == missing_code) continue;
    
    int a1 = gt / 1000;
    int a2 = gt % 1000;
    if (a1 <= 0 || a2 <= 0) continue;
    
    alleles.push_back(a1);
    alleles.push_back(a2);
    valid_indices.push_back(idx);
  }
}

// Permutation unique optimisée
static void permute_within_pops_once(IntegerMatrix& m, int pop_col, int missing_code) {
  const int n = m.nrow();
  const int p = m.ncol();
  
  // Construction du cache des populations
  std::unordered_map<int, std::vector<int>> pop_rows;
  for (int i = 0; i < n; ++i) {
    int popv = m(i, pop_col);
    pop_rows[popv].push_back(i);
  }
  
  // Buffers réutilisables
  std::vector<int> alleles, valid_indices;
  alleles.reserve(n * 2);
  valid_indices.reserve(n);
  
  // Parcours des loci (sauf colonne population)
  for (int j = 0; j < p; ++j) {
    if (j == pop_col) continue;
    
    for (auto& kv : pop_rows) {
      const auto& rows = kv.second;
      if (rows.size() < 2) continue;
      
      extract_alleles(m, j, rows, missing_code, alleles, valid_indices);
      
      const int n_alleles = alleles.size();
      if (n_alleles < 2 || valid_indices.empty()) continue;
      
      // Permutation des allèles
      fy_shuffle(alleles);
      
      // Réassignation des génotypes
      int k = 0;
      for (int idx : valid_indices) {
        if (k + 1 >= n_alleles) break;
        m(idx, j) = alleles[k] * 1000 + alleles[k + 1];
        k += 2;
      }
    }
  }
}

// ---------- Fonction principale corrigée -----------------------------------

// [[Rcpp::export]]
List batch_permute_wc_fis(IntegerMatrix dat,
                         int pop_col_1based,
                         int missing_code,
                         int B) {  // 'B' est maintenant correctement déclaré comme paramètre
  RNGScope scope;
  
  // Validation des paramètres
  const int n = dat.nrow();
  const int p = dat.ncol();
  if (p < 2) stop("Need at least 2 columns: pop + >=1 locus.");
  
  const int pop_col = pop_col_1based - 1;
  if (pop_col < 0 || pop_col >= p) stop("pop_col out of range.");
  
  const int L = p - 1; // number of loci
  
  if (B <= 0) stop("Number of permutations must be positive.");
  
  // Output: B x L (permutation-by-locus FIS)
  NumericMatrix out(B, L);
  
  // Try to propagate locus names to output colnames
  CharacterVector cn = colnames(dat);
  CharacterVector loc_names(L);
  int k = 0;
  for (int j = 0; j < p; ++j) {
    if (j == pop_col) continue;
    if (cn.size() == p) {
      loc_names[k] = cn[j];
    } else {
      loc_names[k] = "L" + std::to_string(k + 1);
    }
    k++;
  }
  colnames(out) = loc_names;
  
  // hierfstat::wc from its namespace
  Environment ns = Environment::namespace_env("hierfstat");
  Function wc = ns["wc"];
  
  // allocate once to reduce churn
  IntegerMatrix pm = clone(dat);
  
  for (int b = 0; b < B; ++b) {  // 'B' utilisé correctement ici
    if (b % 100 == 0 && b > 0) {
      Rcpp::checkUserInterrupt();
    }
    
    // reset pm = dat
    std::copy(dat.begin(), dat.end(), pm.begin());
    
    // permute in-place
    permute_within_pops_once(pm, pop_col, missing_code);
    
    // Build a data.frame for wc(pm) with same column names/order
    List df_list(p);
    
    // column 0: pop
    IntegerVector pop_vec(n);
    for (int i = 0; i < n; ++i) pop_vec[i] = pm(i, pop_col);
    df_list[0] = pop_vec;
    
    // other columns (loci) in original order
    int col_idx = 1;
    for (int j = 0; j < p; ++j) {
      if (j == pop_col) continue;
      IntegerVector colv(n);
      for (int i = 0; i < n; ++i) colv[i] = pm(i, j);
      df_list[col_idx++] = colv;
    }
    
    // names for the data.frame
    CharacterVector names(p);
    if (cn.size() == p) {
      names[0] = cn[pop_col];
      col_idx = 1;
      for (int j = 0; j < p; ++j) {
        if (j == pop_col) continue;
        names[col_idx++] = cn[j];
      }
    } else {
      names[0] = "pop";
      for (int k2 = 1; k2 < p; ++k2) {
        names[k2] = "L" + std::to_string(k2);
      }
    }
    df_list.attr("names") = names;
    df_list.attr("class") = "data.frame";
    df_list.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
    
    DataFrame df(df_list);
    
    // call wc and extract per.loc[,"FIS"] from DataFrame
    try {
      List res = wc(df);
      DataFrame perloc = res["per.loc"];
      
      if (perloc.nrows() != L) {
        stop("wc()$per.loc row count != L.");
      }
      
      // find FIS column (case-sensitive "FIS")
      CharacterVector pc = perloc.names();
      int fis_col = -1;
      for (int c = 0; c < pc.size(); ++c) {
        if (as<std::string>(pc[c]) == "FIS") {
          fis_col = c;
          break;
        }
      }
      if (fis_col < 0) stop("wc()$per.loc has no 'FIS' column.");
      
      NumericVector fis = perloc[fis_col];
      if (fis.size() != L) stop("FIS length mismatch.");
      
      for (int j = 0; j < L; ++j) out(b, j) = fis[j];
      
    } catch (std::exception& e) {
      // Fill with NA if wc() fails
      for (int j = 0; j < L; ++j) out(b, j) = NA_REAL;
    }
  }
  
  return List::create(
    _["permutations"] = out,
    _["n_perm"] = B,
    _["n_loci"] = L,
    _["locus_names"] = loc_names
  );
}

// ---------- Fonctions utilitaires supplémentaires --------------------------

// [[Rcpp::export]]
IntegerMatrix simulate_permutation(IntegerMatrix dat, int pop_col_1based, int missing_code) {
  // Simule une seule permutation pour test
  const int pop_col = pop_col_1based - 1;
  IntegerMatrix pm = clone(dat);
  permute_within_pops_once(pm, pop_col, missing_code);
  return pm;
}

// [[Rcpp::export]]
NumericVector quick_fis_test(IntegerMatrix dat, int pop_col_1based, int missing_code) {
  // Test rapide du calcul FIS sur les données originales
  const int n = dat.nrow();
  const int p = dat.ncol();
  const int pop_col = pop_col_1based - 1;
  const int L = p - 1;
  
  Environment ns = Environment::namespace_env("hierfstat");
  Function wc = ns["wc"];
  
  // Construction du data.frame
  List df_list(p);
  IntegerVector pop_vec(n);
  for (int i = 0; i < n; ++i) pop_vec[i] = dat(i, pop_col);
  df_list[0] = pop_vec;
  
  int col_idx = 1;
  for (int j = 0; j < p; ++j) {
    if (j == pop_col) continue;
    IntegerVector colv(n);
    for (int i = 0; i < n; ++i) colv[i] = dat(i, j);
    df_list[col_idx++] = colv;
  }
  
  CharacterVector names(p);
  names[0] = "pop";
  for (int k = 1; k < p; ++k) names[k] = "L" + std::to_string(k);
  df_list.attr("names") = names;
  df_list.attr("class") = "data.frame";
  df_list.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);
  
  DataFrame df(df_list);
  List res = wc(df);
  DataFrame perloc = res["per.loc"];
  
  // Extraction FIS
  CharacterVector pc = perloc.names();
  int fis_col = -1;
  for (int c = 0; c < pc.size(); ++c) {
    if (as<std::string>(pc[c]) == "FIS") {
      fis_col = c;
      break;
    }
  }
  
  if (fis_col < 0) return NumericVector(L, NA_REAL);
  return perloc[fis_col];
}