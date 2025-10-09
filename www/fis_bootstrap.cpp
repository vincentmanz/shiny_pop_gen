// www/fis_bootstrap.cpp - Version corrigée
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <numeric>
using namespace Rcpp;

// [[Rcpp::depends(Rcpp)]]

// ---------- helpers ---------------------------------------------------------

// Decode 6-digit genotype -> allele1, allele2 (returns false if missing)
inline bool decode_geno(const int g, int& a1, int& a2, const int missing_code) {
  if (g == missing_code || g <= 0 || g > 999999) return false;
  a1 = g / 1000;
  a2 = g % 1000;
  return (a1 > 0 && a2 > 0);
}

// Build row indices per population (0-based row ids)
static std::vector< std::vector<int> >
build_pop_rows(const IntegerMatrix& mat, const int pop_col) {
  const int n = mat.nrow();
  std::unordered_map<int, std::vector<int>> pop_map;
  
  for (int i = 0; i < n; ++i) {
    const int lab = mat(i, pop_col);
    pop_map[lab].push_back(i);
  }
  
  std::vector< std::vector<int> > rows;
  rows.reserve(pop_map.size());
  for (const auto& kv : pop_map) {
    rows.push_back(kv.second);
  }
  return rows;
}

// Compute per-locus FIS using H_O / H_S
static NumericVector
fis_by_locus_for_strata(const IntegerMatrix& mat,
                        const std::vector< std::vector<int> >& strata,
                        const int missing_code) {
  
  const int L = mat.ncol() - 1;
  NumericVector out(L, NA_REAL);
  
  std::unordered_map<int, int> allele_counts;
  allele_counts.reserve(64);
  
  for (int ell = 0; ell < L; ++ell) {
    const int j = 1 + ell;
    
    double HS_wsum = 0.0, HO_wsum = 0.0;
    double wsum = 0.0;
    int valid_pops = 0;
    
    for (const auto& rows : strata) {
      if (rows.empty()) continue;
      
      int n_i = 0;
      int het_i = 0;
      allele_counts.clear();
      
      for (int rid : rows) {
        int a1, a2;
        const int g = mat(rid, j);
        if (!decode_geno(g, a1, a2, missing_code)) continue;
        
        ++n_i;
        if (a1 != a2) ++het_i;
        ++allele_counts[a1];
        ++allele_counts[a2];
      }
      
      if (n_i < 2) continue;
      
      const double m = 2.0 * n_i;
      double sum_p2 = 0.0;
      for (const auto& kv : allele_counts) {
        const double p = kv.second / m;
        sum_p2 += p * p;
      }
      
      const double HS_i = (2.0 * n_i) / (2.0 * n_i - 1.0) * (1.0 - sum_p2);
      const double HO_i = static_cast<double>(het_i) / n_i;
      const double w_i = static_cast<double>(n_i);
      
      HS_wsum += HS_i * w_i;
      HO_wsum += HO_i * w_i;
      wsum += w_i;
      ++valid_pops;
    }
    
    if (wsum > 0.0 && valid_pops > 0) {
      const double HS_bar = HS_wsum / wsum;
      const double HO_bar = HO_wsum / wsum;
      out[ell] = (HS_bar > 1e-10) ? (1.0 - HO_bar / HS_bar) : NA_REAL;
    }
  }
  return out;
}

// Bootstrap rows within populations
static std::vector< std::vector<int> >
bootstrap_rows_within_pops(const std::vector< std::vector<int> >& base_rows) {
  std::vector< std::vector<int> > out = base_rows;
  
  for (size_t p = 0; p < base_rows.size(); ++p) {
    const auto& src = base_rows[p];
    const int n = static_cast<int>(src.size());
    
    if (n <= 0) {
      out[p].clear();
      continue;
    }
    
    for (int i = 0; i < n; ++i) {
      out[p][i] = src[static_cast<int>(R::unif_rand() * n)];
    }
  }
  return out;
}

// Bootstrap population blocks
static std::vector< std::vector<int> >
bootstrap_pop_blocks(const std::vector< std::vector<int> >& base_rows) {
  const int P = static_cast<int>(base_rows.size());
  if (P == 0) return {};
  
  std::vector< std::vector<int> > out;
  out.reserve(P);
  
  for (int k = 0; k < P; ++k) {
    out.push_back(base_rows[static_cast<int>(R::unif_rand() * P)]);
  }
  return out;
}

// ---------- exports principales ---------------------------------------------------------

// [[Rcpp::export]]
NumericMatrix boot_indiv_wc_fis(const IntegerMatrix& mat,
                                const int pop_col = 0,
                                const int missing_code = 0,
                                const int B = 1000) {
  RNGScope rngs;
  
  if (mat.ncol() < 2) stop("Matrix must have pop + >=1 locus columns.");
  if (pop_col < 0 || pop_col >= mat.ncol()) stop("pop_col out of range.");
  
  const int L = mat.ncol() - 1;
  NumericMatrix out(B, L);
  
  auto base_rows = build_pop_rows(mat, pop_col);
  
  for (int b = 0; b < B; ++b) {
    if (b % 100 == 0) Rcpp::checkUserInterrupt();
    
    auto boot_rows = bootstrap_rows_within_pops(base_rows);
    NumericVector fis = fis_by_locus_for_strata(mat, boot_rows, missing_code);
    for (int ell = 0; ell < L; ++ell) out(b, ell) = fis[ell];
  }
  return out;
}

// [[Rcpp::export]]
List boot_popblock_wc_fis(const IntegerMatrix& mat,
                          const int pop_col = 0,
                          const int missing_code = 0,
                          const int B = 1000,
                          const bool return_locus_matrix = false) {
  RNGScope rngs;
  
  if (mat.ncol() < 2) stop("Matrix must have pop + >=1 locus columns.");
  if (pop_col < 0 || pop_col >= mat.ncol()) stop("pop_col out of range.");
  
  const int L = mat.ncol() - 1;
  NumericVector overall(B);
  NumericMatrix perloc(return_locus_matrix ? B : 0, return_locus_matrix ? L : 0);
  
  auto base_rows = build_pop_rows(mat, pop_col);
  
  for (int b = 0; b < B; ++b) {
    if (b % 100 == 0) Rcpp::checkUserInterrupt();
    
    auto boots = bootstrap_pop_blocks(base_rows);
    NumericVector fis = fis_by_locus_for_strata(mat, boots, missing_code);
    
    double sum = 0.0;
    int cnt = 0;
    for (int ell = 0; ell < L; ++ell) {
      const double v = fis[ell];
      if (!Rcpp::NumericVector::is_na(v)) {
        sum += v;
        ++cnt;
      }
      if (return_locus_matrix && B > 0) perloc(b, ell) = v;
    }
    overall[b] = (cnt > 0) ? (sum / cnt) : NA_REAL;
  }
  
  return List::create(
    _["overall"] = overall,
    _["per_locus"] = perloc
  );
}

// [[Rcpp::export]]
NumericVector boot_loci_mean(const NumericVector& obs_fis_by_locus,
                             const int B = 1000) {
  RNGScope rngs;
  const int L = obs_fis_by_locus.size();
  if (L == 0) stop("obs_fis_by_locus is empty.");
  
  NumericVector out(B);
  std::vector<int> idx;
  idx.reserve(L);
  
  for (int i = 0; i < L; ++i) {
    if (!Rcpp::NumericVector::is_na(obs_fis_by_locus[i])) {
      idx.push_back(i);
    }
  }
  
  const int K = static_cast<int>(idx.size());
  if (K == 0) {
    std::fill(out.begin(), out.end(), NA_REAL);
    return out;
  }
  
  for (int b = 0; b < B; ++b) {
    double sum = 0.0;
    for (int t = 0; t < L; ++t) {
      const int pick = idx[static_cast<int>(R::unif_rand() * K)];
      sum += obs_fis_by_locus[pick];
    }
    out[b] = sum / static_cast<double>(L);
  }
  return out;
}

// ---------- fonctions utilitaires supplémentaires (CORRIGÉES) ----------

// [[Rcpp::export]]
NumericVector calculate_observed_fis(const IntegerMatrix& mat,
                                     const int pop_col = 0,
                                     const int missing_code = 0) {
  if (mat.ncol() < 2) stop("Matrix must have pop + >=1 locus columns.");
  
  auto base_rows = build_pop_rows(mat, pop_col);
  return fis_by_locus_for_strata(mat, base_rows, missing_code);
}

// CORRECTION: Fonction summarize_fis_results simplifiée et corrigée
// [[Rcpp::export]]
List summarize_fis_results(const NumericMatrix& boot_results,
                          const double confidence = 0.95) {
  const int L = boot_results.ncol();
  const int B = boot_results.nrow();
  
  if (B == 0 || L == 0) stop("Empty results matrix.");
  if (confidence <= 0 || confidence >= 1) stop("Confidence level must be between 0 and 1.");
  
  const double alpha = (1.0 - confidence) / 2.0;
  NumericVector mean_fis(L), lower_ci(L), upper_ci(L);
  NumericVector n_valid(L);
  
  for (int ell = 0; ell < L; ++ell) {
    // Compter les valeurs valides et les stocker dans un vecteur temporaire
    std::vector<double> valid_vals;
    valid_vals.reserve(B);
    
    for (int b = 0; b < B; ++b) {
      double val = boot_results(b, ell);
      if (!NumericVector::is_na(val)) {
        valid_vals.push_back(val);
      }
    }
    
    n_valid[ell] = valid_vals.size();
    
    if (valid_vals.size() > 0) {
      // Calcul de la moyenne
      double sum = 0.0;
      for (double v : valid_vals) sum += v;
      mean_fis[ell] = sum / valid_vals.size();
      
      // Calcul des quantiles si suffisamment de données
      if (valid_vals.size() >= 2) {
        std::sort(valid_vals.begin(), valid_vals.end());
        int lower_idx = static_cast<int>(valid_vals.size() * alpha);
        int upper_idx = static_cast<int>(valid_vals.size() * (1 - alpha));
        
        // S'assurer que les indices sont dans les limites
        lower_idx = std::max(0, std::min(lower_idx, static_cast<int>(valid_vals.size()) - 1));
        upper_idx = std::max(0, std::min(upper_idx, static_cast<int>(valid_vals.size()) - 1));
        
        lower_ci[ell] = valid_vals[lower_idx];
        upper_ci[ell] = valid_vals[upper_idx];
      } else {
        lower_ci[ell] = upper_ci[ell] = NA_REAL;
      }
    } else {
      mean_fis[ell] = lower_ci[ell] = upper_ci[ell] = NA_REAL;
    }
  }
  
  // CORRECTION: Utilisation de seq_len
  IntegerVector locus_index = seq_len(L);
  
  return List::create(
    _["locus_index"] = locus_index,
    _["mean_fis"] = mean_fis,
    _["lower_ci"] = lower_ci,
    _["upper_ci"] = upper_ci,
    _["n_valid"] = n_valid,
    _["n_bootstrap"] = B,
    _["confidence_level"] = confidence
  );
}

// CORRECTION: Fonction create_results_dataframe simplifiée
// [[Rcpp::export]]
DataFrame create_results_dataframe(const NumericMatrix& boot_results,
                                   const CharacterVector& locus_names,
                                   const double confidence = 0.95) {
  const int L = boot_results.ncol();
  const int B = boot_results.nrow();
  
  if (L != locus_names.size()) {
    stop("Number of loci doesn't match number of locus names.");
  }
  
  const double alpha = (1.0 - confidence) / 2.0;
  NumericVector mean_fis(L), lower_ci(L), upper_ci(L);
  
  for (int ell = 0; ell < L; ++ell) {
    // Utilisation de std::vector pour stocker les valeurs valides
    std::vector<double> valid_vals;
    
    for (int b = 0; b < B; ++b) {
      double val = boot_results(b, ell);
      if (!NumericVector::is_na(val)) {
        valid_vals.push_back(val);
      }
    }
    
    if (valid_vals.size() > 0) {
      // Calcul de la moyenne
      double sum = 0.0;
      for (double v : valid_vals) sum += v;
      mean_fis[ell] = sum / valid_vals.size();
      
      if (valid_vals.size() >= 2) {
        std::sort(valid_vals.begin(), valid_vals.end());
        int lower_idx = static_cast<int>(valid_vals.size() * alpha);
        int upper_idx = static_cast<int>(valid_vals.size() * (1 - alpha));
        
        lower_idx = std::max(0, std::min(lower_idx, static_cast<int>(valid_vals.size()) - 1));
        upper_idx = std::max(0, std::min(upper_idx, static_cast<int>(valid_vals.size()) - 1));
        
        lower_ci[ell] = valid_vals[lower_idx];
        upper_ci[ell] = valid_vals[upper_idx];
      }
    }
  }
  
  // Construction du DataFrame
  return DataFrame::create(
    _["Locus"] = locus_names,
    _["Mean_FIS"] = mean_fis,
    _["CI_Lower"] = lower_ci,
    _["CI_Upper"] = upper_ci,
    _["stringsAsFactors"] = false
  );
}

// CORRECTION: Fonction calculate_fis_confidence_width simplifiée
// [[Rcpp::export]]
double calculate_fis_confidence_width(const NumericMatrix& boot_results,
                                     int locus_index,
                                     double confidence = 0.95) {
  const int B = boot_results.nrow();
  if (locus_index < 0 || locus_index >= boot_results.ncol()) {
    stop("Locus index out of range.");
  }
  
  // Utilisation de std::vector pour stocker les valeurs valides
  std::vector<double> valid_vals;
  
  for (int b = 0; b < B; ++b) {
    double val = boot_results(b, locus_index);
    if (!NumericVector::is_na(val)) {
      valid_vals.push_back(val);
    }
  }
  
  if (valid_vals.size() < 2) return NA_REAL;
  
  const double alpha = (1.0 - confidence) / 2.0;
  std::sort(valid_vals.begin(), valid_vals.end());
  
  int lower_idx = static_cast<int>(valid_vals.size() * alpha);
  int upper_idx = static_cast<int>(valid_vals.size() * (1 - alpha));
  
  lower_idx = std::max(0, std::min(lower_idx, static_cast<int>(valid_vals.size()) - 1));
  upper_idx = std::max(0, std::min(upper_idx, static_cast<int>(valid_vals.size()) - 1));
  
  double lower = valid_vals[lower_idx];
  double upper = valid_vals[upper_idx];
  
  return upper - lower;
}

// ---------- nouvelle fonction utilitaire sans problème de reserve ----------
// [[Rcpp::export]]
NumericVector calculate_locus_quality(const NumericMatrix& boot_results) {
  const int L = boot_results.ncol();
  const int B = boot_results.nrow();
  
  NumericVector quality_scores(L);
  
  for (int ell = 0; ell < L; ++ell) {
    int valid_count = 0;
    double sum = 0.0;
    double sum_sq = 0.0;
    
    for (int b = 0; b < B; ++b) {
      double val = boot_results(b, ell);
      if (!NumericVector::is_na(val)) {
        valid_count++;
        sum += val;
        sum_sq += val * val;
      }
    }
    
    if (valid_count > 1) {
      double mean_val = sum / valid_count;
      double variance = (sum_sq - sum * sum / valid_count) / (valid_count - 1);
      double cv = sqrt(variance) / mean_val;  // coefficient of variation
      quality_scores[ell] = 1.0 / (1.0 + cv);  // score de qualité (0-1)
    } else {
      quality_scores[ell] = NA_REAL;
    }
  }
  
  return quality_scores;
}