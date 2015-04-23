#include <vector>
#include <algorithm>
#include <R.h>
#include <Rinternals.h>

void open_RNG() {
  GetRNGstate();
}

void close_RNG() {
  PutRNGstate();
}

inline double gen_unif() {
  return unif_rand();
}

template<class T>
inline T get_rand_int(const T max) {
  return static_cast<T>(gen_unif() * static_cast<double>(max));
}

template<class T>
void sample(std::vector<T>& x, const typename std::vector<T>::size_type to_draw) {
  for (typename std::vector<T>::size_type i = 0; i < to_draw; ++i) {
    std::swap(x[i], x[i + get_rand_int(x.size() - i)]);
  }
  x.resize(to_draw);
}

template<class T>
void shuffle(std::vector<T>& x) {
  for (typename std::vector<T>::size_type i = 0; i < x.size(); ++i) {
    std::swap(x[i], x[i + get_rand_int(x.size() - i)]);
  }
}

template<class T>
inline void append(std::vector<T>& x, const std::vector<T>& y) {
  x.insert(x.end(), y.begin(), y.end());
}

extern "C" {

  SEXP cpp_randomize(const SEXP blocking_R, const SEXP treat_ind_R) {

    const int* const blocking = INTEGER(blocking_R);
    const R_xlen_t n_units = xlength(blocking_R);
    const std::vector<int> treat_ind(INTEGER(treat_ind_R), INTEGER(treat_ind_R) + xlength(treat_ind_R));

    std::vector<int>::size_type n_blocks = 0;
    for (R_xlen_t i = 0; i < n_units; ++i) {
      if (static_cast<std::vector<int>::size_type>(blocking[i]) >= n_blocks) {
        n_blocks = blocking[i] + 1;
      }
    }

    std::vector<std::vector<int>::size_type> block_sizes(n_blocks, 0);
    for (R_xlen_t i = 0; i < n_units; ++i) {
      ++block_sizes[static_cast<std::vector<int>::size_type>(blocking[i])];
    }

    open_RNG();

    std::vector<std::vector<int> > rand_treat(n_blocks);
    for (std::vector<int>::size_type b = 0; b < n_blocks; ++b) {

      rand_treat[b].reserve(block_sizes[b]);

      if (block_sizes[b] % treat_ind.size() != 0) {
        append(rand_treat[b], treat_ind);
        sample(rand_treat[b], block_sizes[b] % treat_ind.size());
      }

      while (rand_treat[b].size() < block_sizes[b]) {
        append(rand_treat[b], treat_ind);
      }

      shuffle(rand_treat[b]);
    }

    close_RNG();

    const SEXP treatment_R = PROTECT(allocVector(INTSXP, n_units));
    int* const treatment = INTEGER(treatment_R);

    for (R_xlen_t i = 0; i < n_units; ++i) {
      treatment[i] = rand_treat[static_cast<std::vector<int>::size_type>(blocking[i])].back();
      rand_treat[static_cast<std::vector<int>::size_type>(blocking[i])].pop_back();
    }

    UNPROTECT(1);
    return treatment_R;
  }

}
