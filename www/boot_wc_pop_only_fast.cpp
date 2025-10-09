// ============================================================================
// boot_wc_pop_only_fast_optimized.cpp
// Optimized version with better memory management and performance
// ============================================================================

// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <memory>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// ---------------------- OPTIMIZED RNG & UTILITIES --------------------------

// Optimized PRNG - faster than original
struct FastRng {
  uint64_t x;
  FastRng(uint64_t seed = 1) : x(seed) {}
  
  inline uint64_t next() {
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    return x * 0x2545F4914F6CDD1DULL;
  }
  
  inline double unif01() { 
    return (next() >> 11) * (1.0/9007199254740992.0);
  }
  
  inline int unif_int(int n) {
    return static_cast<int>(unif01() * n);
  }
};

// Inlined genotype decoder
inline bool decode_geno_fast(int g, int missing_code, int &a1, int &a2) {
  if (g == missing_code || g <= 0) return false;
  a1 = g / 1000;
  a2 = g % 1000;
  return true;
}

// Optimized percentile using partial sort for large vectors
static inline double qtile_optimized(std::vector<double> &v, double p) {
  if (v.empty()) return NA_REAL;
  if (p <= 0.0) return *std::min_element(v.begin(), v.end());
  if (p >= 1.0) return *std::max_element(v.begin(), v.end());
  
  const size_t n = v.size();
  const size_t k = static_cast<size_t>(p * (n - 1));
  
  if (n > 1000) {
    std::nth_element(v.begin(), v.begin() + k, v.end());
    return v[k];
  } else {
    std::sort(v.begin(), v.end());
    return v[k];
  }
}

// Reusable allele counting structure to avoid allocations
struct AlleleCounter {
  std::vector<int> n_dip;
  std::vector<int> n_het;
  std::vector<std::unordered_map<int, int>> acnt;
  std::unordered_map<int, int> acnt_total;
  int n_dip_total = 0;
  int n_het_total = 0;
  
  void reset(size_t num_groups) {
    n_dip.assign(num_groups, 0);
    n_het.assign(num_groups, 0);
    acnt.assign(num_groups, std::unordered_map<int, int>());
    acnt_total.clear();
    n_dip_total = 0;
    n_het_total = 0;
  }
};

// --------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List boot_wc_pop_only_fast_optimized_cpp(const Rcpp::IntegerMatrix mat_int,
                                     const int pop_col,
                                     const int missing_code,
                                     const int B,
                                     const Rcpp::NumericVector probs,
                                     const int seed = 1,
                                     const int n_threads = 0) {
  
  const int N = mat_int.nrow();
  const int P = mat_int.ncol();
  if (P < 2) stop("Need at least pop + 1 locus columns.");
  if (pop_col < 0 || pop_col >= P) stop("pop_col out of range.");
  if (B <= 0) stop("B must be > 0.");
  if (probs.size() < 1) stop("Provide at least one quantile.");
  
  // Optimized column access
  const int *base = &mat_int[0];
  auto col_ptr = [&](int j) -> const int* { 
    return base + static_cast<size_t>(j) * N; 
  };
  
  // Precompute population structure
  const int *popc = col_ptr(pop_col);
  std::unordered_map<int, int> pop2idx;
  std::vector<int> uniq_pops;
  uniq_pops.reserve(N);
  
  for (int i = 0; i < N; ++i) {
    auto result = pop2idx.emplace(popc[i], pop2idx.size());
    if (result.second) {
      uniq_pops.push_back(popc[i]);
    }
  }
  
  const int R = static_cast<int>(pop2idx.size());
  if (R < 1) stop("No populations found.");
  
  std::vector<std::vector<int>> rows_by_pop(R);
  for (int i = 0; i < N; ++i) {
    rows_by_pop[pop2idx[popc[i]]].push_back(i);
  }
  
  // Precompute locus columns and population sizes
  std::vector<int> locus_cols;
  locus_cols.reserve(P - 1);
  for (int j = 0; j < P; ++j) {
    if (j != pop_col) locus_cols.push_back(j);
  }
  const int L = static_cast<int>(locus_cols.size());
  
  std::vector<int> pop_sizes(R);
  for (int k = 0; k < R; ++k) {
    pop_sizes[k] = static_cast<int>(rows_by_pop[k].size());
  }
  
  // Pre-allocate bootstrap storage
  std::vector<std::vector<double>> boot_FST(L, std::vector<double>(B, NA_REAL));
  std::vector<std::vector<double>> boot_FIT(L, std::vector<double>(B, NA_REAL));
  std::vector<std::vector<double>> boot_HI(L, std::vector<double>(B, NA_REAL));
  std::vector<std::vector<double>> boot_HS(L, std::vector<double>(B, NA_REAL));
  
  // Thread configuration
  int threads = n_threads;
#ifdef _OPENMP
  if (threads <= 0) threads = omp_get_max_threads();
  threads = std::max(1, threads);
#endif
  
#ifdef _OPENMP
#pragma omp parallel num_threads(threads)
{
  // Thread-local storage to avoid allocations in inner loops
  AlleleCounter counter;
  std::vector<int> times;
  std::vector<const std::vector<int>*> groups;
  
#pragma omp for schedule(dynamic)
#endif
  for (int b = 0; b < B; ++b) {
    // Thread-local RNG with better seeding
#ifdef _OPENMP
    FastRng rng(seed + omp_get_thread_num() * B + b);
#else
    FastRng rng(seed + b);
#endif
    
    // Stage 1: Resample populations
    times.assign(R, 0);
    for (int t = 0; t < R; ++t) {
      ++times[rng.unif_int(R)];
    }
    
    // Build group clones without copying population data
    groups.clear();
    groups.reserve(R); // Maximum possible
    
    for (int k = 0; k < R; ++k) {
      int mult = times[k];
      if (mult <= 0 || rows_by_pop[k].empty()) continue;
      
      for (int m = 0; m < mult; ++m) {
        groups.push_back(&rows_by_pop[k]);
      }
    }
    
    const int num_groups = static_cast<int>(groups.size());
    if (num_groups <= 1) {
      for (int ell = 0; ell < L; ++ell) {
        boot_FST[ell][b] = boot_FIT[ell][b] = 
        boot_HI[ell][b] = boot_HS[ell][b] = NA_REAL;
      }
      continue;
    }
    
    // Reuse counter for all loci
    counter.reset(num_groups);
    
    // Process each locus
    for (int ell = 0; ell < L; ++ell) {
      const int j = locus_cols[ell];
      const int *col = col_ptr(j);
      
      // Reset counter for new locus
      counter.reset(num_groups);
      
      // Count alleles and heterozygotes
      for (int gidx = 0; gidx < num_groups; ++gidx) {
        const std::vector<int> &rows = *groups[gidx];
        int nd = 0, nh = 0;
        
        for (int idx : rows) {
          int G = col[idx], a1, a2;
          if (!decode_geno_fast(G, missing_code, a1, a2)) continue;
          
          ++nd;
          if (a1 != a2) ++nh;
          
          ++counter.acnt[gidx][a1];
          ++counter.acnt[gidx][a2];
          ++counter.acnt_total[a1];
          ++counter.acnt_total[a2];
        }
        
        counter.n_dip[gidx] = nd;
        counter.n_het[gidx] = nh;
        counter.n_dip_total += nd;
        counter.n_het_total += nh;
      }
      
      // Find active groups (non-empty)
      std::vector<int> active;
      active.reserve(num_groups);
      for (int gidx = 0; gidx < num_groups; ++gidx) {
        if (counter.n_dip[gidx] > 0) active.push_back(gidx);
      }
      const int r = static_cast<int>(active.size());
      
      if (counter.n_dip_total == 0 || r <= 1) {
        boot_FST[ell][b] = boot_FIT[ell][b] = NA_REAL;
        boot_HI[ell][b] = boot_HS[ell][b] = NA_REAL;
        continue;
      }
      
      // Compute HI (observed heterozygosity)
      const double HI = static_cast<double>(counter.n_het_total) / counter.n_dip_total;
      
      // Compute HS (within-pop gene diversity)
      double HS = 0.0;
      const double denom_glob = 2.0 * counter.n_dip_total;
      
      for (int gidx : active) {
        const double nd = counter.n_dip[gidx];
        const double denom_g = 2.0 * nd;
        double sum_p2 = 0.0;
        
        for (const auto &kv : counter.acnt[gidx]) {
          const double p = kv.second / denom_g;
          sum_p2 += p * p;
        }
        
        const double Hk = 1.0 - sum_p2;
        HS += (denom_g / denom_glob) * Hk;
      }
      
      // W&C size terms
      double n_total = 0.0, sum_n2 = 0.0;
      for (int gidx : active) {
        n_total += counter.n_dip[gidx];
        sum_n2 += static_cast<double>(counter.n_dip[gidx]) * counter.n_dip[gidx];
      }
      
      const double n_bar = n_total / r;
      const double n_C = (n_total - (sum_n2 / n_total)) / (r - 1.0);
      
      if (n_bar <= 1.0 || n_C <= 0.0) {
        boot_FST[ell][b] = boot_FIT[ell][b] = NA_REAL;
        boot_HI[ell][b] = HI;
        boot_HS[ell][b] = HS;
        continue;
      }
      
      // Process alleles for W&C components
      double A = 0.0, Bc = 0.0, C = 0.0;
      const double denom_total_gene = 2.0 * n_total;
      
      for (const auto &allele_pair : counter.acnt_total) {
        const int al = allele_pair.first;
        
        // Global allele frequency
        double ac_glob = 0.0;
        for (int gidx : active) {
          auto it = counter.acnt[gidx].find(al);
          if (it != counter.acnt[gidx].end()) {
            ac_glob += it->second;
          }
        }
        const double p_bar = ac_glob / denom_total_gene;
        
        // Variance across groups
        double s_num = 0.0;
        for (int gidx : active) {
          const double nd = counter.n_dip[gidx];
          const double denom_g = 2.0 * nd;
          double p_i = 0.0;
          
          auto it = counter.acnt[gidx].find(al);
          if (it != counter.acnt[gidx].end()) {
            p_i = it->second / denom_g;
          }
          
          const double diff = p_i - p_bar;
          s_num += nd * diff * diff;
        }
        
        const double s2 = s_num / (n_bar * (r - 1));
        
        // Mean heterozygosity for this allele
        double hbar_j = 0.0;
        int r_eff = 0;
        
        for (int gidx : active) {
          const double nd = counter.n_dip[gidx];
          if (nd == 0) continue;
          
          const double denom_g = 2.0 * nd;
          auto it = counter.acnt[gidx].find(al);
          const double ac_g = (it == counter.acnt[gidx].end()) ? 0.0 : it->second;
          const double p_i = ac_g / denom_g;
          
          hbar_j += 2.0 * p_i * (1.0 - p_i);
          ++r_eff;
        }
        
        if (r_eff > 0) hbar_j /= r_eff;
        
        // W&C components
        const double term_common = (p_bar * (1.0 - p_bar)) - 
                                  (((r - 1.0) / r) * s2);
        
        const double a_j = (n_bar / n_C) * 
                          (s2 - ((term_common - (hbar_j / 4.0)) / (n_bar - 1.0)));
        const double b_j = (n_bar / (n_bar - 1.0)) * 
                          (term_common - (((2.0 * n_bar) - 1.0) / (4.0 * n_bar)) * hbar_j);
        const double c_j = hbar_j / 2.0;
        
        A += a_j;
        Bc += b_j;
        C += c_j;
      }
      
      // Final F-statistics
      const double denom_tot = A + Bc + C;
      double Fst = NA_REAL, Fit = NA_REAL;
      
      if (std::isfinite(denom_tot) && denom_tot != 0.0) {
        Fst = A / denom_tot;
        Fit = (A + Bc) / denom_tot;
      }
      
      boot_FST[ell][b] = Fst;
      boot_FIT[ell][b] = Fit;
      boot_HI[ell][b] = HI;
      boot_HS[ell][b] = HS;
    } // loci
  } // replicates
#ifdef _OPENMP
}
#endif

  // Build output matrices with optimized quantile computation
  const int Q = probs.size();
  CharacterVector rn(Q);
  for (int q = 0; q < Q; ++q) {
    std::ostringstream os;
    os << static_cast<double>(probs[q]);
    rn[q] = os.str();
  }
  
  // Optimized output builder with pre-allocation
  auto make_QxL_optimized = [&](std::vector<std::vector<double>> &src) -> NumericMatrix {
    const int Lloc = static_cast<int>(src.size());
    NumericMatrix M(Q, Lloc);
    M.attr("dimnames") = List::create(rn, R_NilValue);
    
    std::vector<double> v;
    v.reserve(B); // Pre-allocate for bootstrap samples
    
    for (int ell = 0; ell < Lloc; ++ell) {
      v.clear();
      for (double x : src[ell]) {
        if (!NumericVector::is_na(x)) v.push_back(x);
      }
      
      for (int q = 0; q < Q; ++q) {
        const double p = std::max(0.0, std::min(1.0, static_cast<double>(probs[q])));
        M(q, ell) = qtile_optimized(v, p);
      }
    }
    return M;
  };
  
  return List::create(
    _["FST"] = make_QxL_optimized(boot_FST),
    _["FIT"] = make_QxL_optimized(boot_FIT),
    _["HI"]  = make_QxL_optimized(boot_HI),
    _["HS"]  = make_QxL_optimized(boot_HS)
  );
}