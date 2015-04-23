#include <vector>
#include <R.h>
#include <Rinternals.h>

extern "C" {

  SEXP cpp_estimate(const SEXP outcomes_R, const SEXP blocking_R, const SEXP treatments_R, const SEXP contrast_R) {

    const R_xlen_t n_units = xlength(outcomes_R);

    if (xlength(blocking_R) != n_units || xlength(treatments_R) != n_units) {
      error("Non-matching sample vectors.");
    }
    if (xlength(contrast_R) != 2) {
      error("Contrast must be vector of length two.");
    }

    const double* const outcomes = REAL(outcomes_R);
    const int* const blocking = INTEGER(blocking_R);
    const int* const treatments = INTEGER(treatments_R);
    const int contrast0 = INTEGER(contrast_R)[0];
    const int contrast1 = INTEGER(contrast_R)[1];

    std::vector<double>::size_type n_blocks = 0;
    for (R_xlen_t i = 0; i < n_units; ++i) {
      if (static_cast<std::vector<double>::size_type>(blocking[i]) >= n_blocks) {
        n_blocks = blocking[i] + 1;
      }
    }

    std::vector<double> block_size(n_blocks, 0.0);
    std::vector<double> sum_out_treat0(n_blocks, 0.0);
    std::vector<double> sum_n_treat0(n_blocks, 0.0);
    std::vector<double> sum_out_treat1(n_blocks, 0.0);
    std::vector<double> sum_n_treat1(n_blocks, 0.0);

    for (R_xlen_t i = 0; i < n_units; ++i) {
      std::vector<double>::size_type i_block = static_cast<std::vector<double>::size_type>(blocking[i]);
      ++block_size[i_block];
      if (treatments[i] == contrast0) {
        sum_out_treat0[i_block] += outcomes[i];
        ++sum_n_treat0[i_block];
      } else if (treatments[i] == contrast1) {
        sum_out_treat1[i_block] += outcomes[i];
        ++sum_n_treat1[i_block];
      }
    }

    double estimate = 0.0;
    for (std::vector<double>::size_type b = 0; b < n_blocks; ++b) {
      if (sum_n_treat0[b] == 0.0 || sum_n_treat1[b] == 0.0) {
        error("Some treatments are missing in a block.");
      }
      estimate += block_size[b] * ((sum_out_treat0[b] / sum_n_treat0[b]) - (sum_out_treat1[b] / sum_n_treat1[b]));
    }
    estimate /= static_cast<double>(n_units);

    return ScalarReal(estimate);
  }

}
