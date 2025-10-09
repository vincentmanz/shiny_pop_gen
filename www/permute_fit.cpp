// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <numeric>
using namespace Rcpp;

/*
 Optimized streaming global-permutation test for FIT
 ---------------------------------------------------
 Key optimizations:
 - Precomputation and memory reuse
 - Reduced allocations in inner loops
 - Optimized data structures
 - Cache-friendly access patterns
 */

// ---------- optimized genotype helpers ----------

inline bool is_missing(const int g, const int missing_code) {
  return (g == missing_code) || (g <= 0);
}

// Optimized: inlined and branch-prediction friendly
inline bool decode_geno_fast(const int g, int& a1, int& a2, const int missing_code) {
  if ((g == missing_code) || (g <= 0)) return false;
  a1 = g / 1000;
  a2 = g % 1000;
  return true;
}

// Optimized genotype creation with conditional swap
inline int make_geno_fast(int a, int b) {
  if (a > b) {
    return b * 1000 + a;
  }
  return a * 1000 + b;
}

// ---------- optimized FIT computation ----------

// Reusable allele counter to avoid allocations
struct AlleleCounter {
  std::unordered_map<int, int> counts;
  int n_dip = 0;
  int n_het = 0;
  
  void reset() {
    counts.clear();
    n_dip = 0;
    n_het = 0;
  }
  
  void add_genotype(int a1, int a2) {
    ++n_dip;
    if (a1 != a2) ++n_het;
    ++counts[a1];
    ++counts[a2];
  }
};

// Optimized FIT computation with pre-allocated structures
static NumericVector
fit_by_locus_optimized(const IntegerMatrix& mat,
                       const int pop_col,
                       const int missing_code,
                       const std::vector<int>& locus_cols) {
  
  const int N = mat.nrow();
  const int K = static_cast<int>(locus_cols.size());
  
  NumericVector out(K, NA_REAL);
  AlleleCounter counter;
  
  for (int ell = 0; ell < K; ++ell) {
    const int j = locus_cols[ell];
    counter.reset();
    
    // Process all individuals for this locus
    for (int i = 0; i < N; ++i) {
      const int g = mat(i, j);
      int a1, a2;
      if (decode_geno_fast(g, a1, a2, missing_code)) {
        counter.add_genotype(a1, a2);
      }
    }
    
    if (counter.n_dip <= 0) {
      out[ell] = NA_REAL;
      continue;
    }
    
    const double HI = static_cast<double>(counter.n_het) / counter.n_dip;
    
    // Compute HT efficiently
    double total_genes = 0.0;
    double sum_p2 = 0.0;
    
    for (const auto& kv : counter.counts) {
      total_genes += kv.second;
    }
    
    if (total_genes <= 0.0) {
      out[ell] = NA_REAL;
      continue;
    }
    
    for (const auto& kv : counter.counts) {
      const double p = kv.second / total_genes;
      sum_p2 += p * p;
    }
    
    const double HT = 1.0 - sum_p2;
    out[ell] = (HT > 1e-12) ? (1.0 - (HI / HT)) : NA_REAL;
  }
  
  return out;
}

// ---------- optimized shuffle ----------

// Optimized Fisher-Yates shuffle with better cache performance
inline void fy_shuffle_optimized(std::vector<int>& v) {
  const int n = static_cast<int>(v.size());
  for (int i = n - 1; i > 0; --i) {
    // Single RNG call per iteration
    const int j = static_cast<int>(R::unif_rand() * (i + 1));
    // Conditional swap to avoid unnecessary operations
    if (j != i) {
      std::swap(v[i], v[j]);
    }
  }
}

// Pre-allocated workspace for permutations
struct PermutationWorkspace {
  std::vector<int> pool_buffer;
  std::vector<int> indices_buffer;
  
  void resize(size_t pool_size, size_t indices_size) {
    pool_buffer.resize(pool_size);
    indices_buffer.resize(indices_size);
  }
};

// ---------- main optimized function ----------

// [[Rcpp::export]]
List stream_permute_wc_fit_global_pvals_optimized(const IntegerMatrix mat_int,
                                                  const int pop_col,
                                                  const int missing_code,
                                                  const int B,
                                                  const NumericVector obs_fit_by_locus,
                                                  const double obs_fit_overall) {
  
  const int N = mat_int.nrow();
  const int P = mat_int.ncol();
  if (P < 2) stop("mat_int must have at least pop + 1 locus columns.");
  if (pop_col < 0 || pop_col >= P) stop("pop_col is out of range.");
  if (B <= 0) stop("B must be > 0.");
  
  // Precompute locus columns once
  std::vector<int> locus_cols;
  locus_cols.reserve(P - 1);
  for (int j = 0; j < P; ++j) {
    if (j != pop_col) locus_cols.push_back(j);
  }
  const int K = static_cast<int>(locus_cols.size());
  
  if (obs_fit_by_locus.size() != K) {
    stop("obs_fit_by_locus length must equal number of loci.");
  }
  
  // Precompute locus data structures
  std::vector<std::vector<int>> ind_idx_by_locus(K);
  std::vector<std::vector<int>> allele_pool_by_locus(K);
  std::vector<int> pool_sizes(K, 0);
  
  // First pass: compute sizes and reserve memory
  for (int ell = 0; ell < K; ++ell) {
    const int j = locus_cols[ell];
    int count = 0;
    
    for (int i = 0; i < N; ++i) {
      const int g = mat_int(i, j);
      int a1, a2;
      if (decode_geno_fast(g, a1, a2, missing_code)) {
        ++count;
      }
    }
    
    ind_idx_by_locus[ell].reserve(count);
    allele_pool_by_locus[ell].reserve(2 * count);
    pool_sizes[ell] = 2 * count;
  }
  
  // Second pass: fill data structures
  for (int ell = 0; ell < K; ++ell) {
    const int j = locus_cols[ell];
    auto& inds = ind_idx_by_locus[ell];
    auto& pool = allele_pool_by_locus[ell];
    
    for (int i = 0; i < N; ++i) {
      const int g = mat_int(i, j);
      int a1, a2;
      if (decode_geno_fast(g, a1, a2, missing_code)) {
        inds.push_back(i);
        pool.push_back(a1);
        pool.push_back(a2);
      }
    }
  }
  
  // Initialize counters
  std::vector<double> ge(K, 0.0), le(K, 0.0);
  double ge_overall = 0.0, le_overall = 0.0;
  
  // Pre-allocate workspace for permutations
  PermutationWorkspace workspace;
  size_t max_pool_size = 0;
  size_t max_indices_size = 0;
  
  for (int ell = 0; ell < K; ++ell) {
    max_pool_size = std::max(max_pool_size, allele_pool_by_locus[ell].size());
    max_indices_size = std::max(max_indices_size, ind_idx_by_locus[ell].size());
  }
  
  workspace.resize(max_pool_size, max_indices_size);
  
  // Pre-allocate permutation matrix (avoid copying entire matrix each time)
  IntegerMatrix perm_mat(N, P);
  
  // Only copy non-locus columns once (population data)
  for (int j = 0; j < P; ++j) {
    if (j == pop_col) {
      for (int i = 0; i < N; ++i) {
        perm_mat(i, j) = mat_int(i, j);
      }
    }
  }
  
  // Copy initial locus data
  for (int ell = 0; ell < K; ++ell) {
    const int j = locus_cols[ell];
    const auto& inds = ind_idx_by_locus[ell];
    const auto& pool = allele_pool_by_locus[ell];
    
    const int m = static_cast<int>(inds.size());
    for (int t = 0; t < m; ++t) {
      const int i = inds[t];
      perm_mat(i, j) = make_geno_fast(pool[2*t], pool[2*t + 1]);
    }
  }
  
  // Main permutation loop
  for (int b = 0; b < B; ++b) {
    // Global shuffle per locus
    for (int ell = 0; ell < K; ++ell) {
      const int j = locus_cols[ell];
      const auto& inds = ind_idx_by_locus[ell];
      const auto& base_pool = allele_pool_by_locus[ell];
      
      // Use workspace to avoid allocations
      auto& pool = workspace.pool_buffer;
      std::copy(base_pool.begin(), base_pool.end(), pool.begin());
      fy_shuffle_optimized(pool);
      
      const int m = static_cast<int>(inds.size());
      for (int t = 0; t < m; ++t) {
        const int i = inds[t];
        perm_mat(i, j) = make_geno_fast(pool[2*t], pool[2*t + 1]);
      }
    }
    
    // Compute FIT on permuted data
    NumericVector fit_locus = fit_by_locus_optimized(perm_mat, pop_col, 
                                                    missing_code, locus_cols);
    
    // Update counters efficiently
    double sum_fit = 0.0;
    int n_valid = 0;
    
    for (int ell = 0; ell < K; ++ell) {
      const double obs = obs_fit_by_locus[ell];
      const double nul = fit_locus[ell];
      
      if (!NumericVector::is_na(nul) && !NumericVector::is_na(obs)) {
        // Branchless increment for better performance
        ge[ell] += (nul >= obs) ? 1.0 : 0.0;
        le[ell] += (nul <= obs) ? 1.0 : 0.0;
        
        sum_fit += nul;
        ++n_valid;
      }
    }
    
    if (n_valid > 0 && !NumericVector::is_na(obs_fit_overall)) {
      const double mean_fit = sum_fit / n_valid;
      ge_overall += (mean_fit >= obs_fit_overall) ? 1.0 : 0.0;
      le_overall += (mean_fit <= obs_fit_overall) ? 1.0 : 0.0;
    }
  }
  
  // Compute p-values (optimized)
  NumericVector pvals(K, NA_REAL);
  const double B_inv = 1.0 / B;
  
  for (int ell = 0; ell < K; ++ell) {
    const double p1 = ge[ell] * B_inv;
    const double p2 = le[ell] * B_inv;
    const double p = 2.0 * std::min(p1, p2);
    pvals[ell] = (p > 1.0) ? 1.0 : p;
  }
  
  double p_overall = NA_REAL;
  if (!NumericVector::is_na(obs_fit_overall)) {
    const double p1 = ge_overall * B_inv;
    const double p2 = le_overall * B_inv;
    const double p = 2.0 * std::min(p1, p2);
    p_overall = (p > 1.0) ? 1.0 : p;
  }
  
  return List::create(
    _["pvals_by_locus"] = pvals,
    _["pval_overall"]   = p_overall,
    _["ge_by_locus"]    = wrap(ge),
    _["le_by_locus"]    = wrap(le)
  );
}

// Optimized FIT calculator for external use
// [[Rcpp::export]]
NumericVector fit_by_locus_total_optimized_cpp(const IntegerMatrix mat_int,
                                               const int pop_col,
                                               const int missing_code) {
  std::vector<int> locus_cols;
  const int P = mat_int.ncol();
  
  for (int j = 0; j < P; ++j) {
    if (j != pop_col) locus_cols.push_back(j);
  }
  
  return fit_by_locus_optimized(mat_int, pop_col, missing_code, locus_cols);
}