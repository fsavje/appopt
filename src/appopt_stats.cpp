#include <cmath>
#include <R.h>
#include <Rinternals.h>

inline static double get_dist(const int unit1, const int unit2, const double* const data_matrix, const int dimensions) {
  double out_dist = 0;
  for (int d = 0; d < dimensions; ++d) {
    double diff = data_matrix[unit1 * dimensions + d] - data_matrix[unit2 * dimensions + d];
    out_dist += diff * diff;
  }
  return std::sqrt(out_dist);
}

extern "C" {

  SEXP cpp_get_block_stats(const SEXP data_matrix_R,
                           const SEXP blocking_R,
                           const SEXP n_blocks_R,
                           const SEXP block_size_R) {

      const int dimensions = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[0];
      const int n_data_points = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[1];

      if (n_data_points != length(blocking_R)) {
        error("Blocks and data do not match.");
      }

      const double* const data_matrix = REAL(data_matrix_R);
      const int* const blocking = INTEGER(blocking_R);
      const int n_blocks = asInteger(n_blocks_R);
      const double block_size = asReal(block_size_R);

      double max_dist = 0;
      double avg_dist = 0;

      double* const within_block_sum = new double[n_blocks];
      double* const within_block_n = new double[n_blocks];
      for (int i = 0; i < n_blocks; ++i) {
        within_block_sum[i] = 0.0;
        within_block_n[i] = 0.0;
      }

      for (int i = 0; i < n_data_points; ++i) {
        ++within_block_n[blocking[i]];
        for (int j = i + 1; j < n_data_points; ++j) {
          if (blocking[i] == blocking[j]) {
            double this_dist = get_dist(i, j, data_matrix, dimensions);
            if (this_dist > max_dist) {
              max_dist = this_dist;
            }
            within_block_sum[blocking[i]] += this_dist;
          }
        }
      }

      for (int i = 0; i < n_blocks; ++i) {
        if (within_block_n[i] >= block_size) {
          avg_dist += within_block_sum[i] / within_block_n[i];
        } else {
          error("Not valid blocking.");
        }
      }

      delete[] within_block_sum;
      delete[] within_block_n;

      avg_dist = 2 * avg_dist / static_cast<double>(n_data_points);

      const SEXP outlist_R = PROTECT(allocVector(VECSXP, 2));
      SET_VECTOR_ELT(outlist_R, 0, ScalarReal(max_dist));
      SET_VECTOR_ELT(outlist_R, 1, ScalarReal(avg_dist));

      const SEXP listnames = PROTECT(allocVector(STRSXP, 2));
      SET_STRING_ELT(listnames, 0, mkChar("max"));
      SET_STRING_ELT(listnames, 1, mkChar("mean"));
      setAttrib(outlist_R, R_NamesSymbol, listnames);

      UNPROTECT(2);
      return outlist_R;
  }

}
