#include <R.h>
#include <Rinternals.h>
#include <ANN/ANN.h>

#define ANN_BRUTE  1
#define ANN_KDTREE 2
#define ANN_BDTREE 3

namespace ANN {

  // ANN package initializes a global pointer that may not be
  // deleted midway -- only when no ANNpointSet exists. The following
  // (ugly) global variable is needed to keep track of outstanding
  // AnnData objects and deletes the ANN pointer when the last object
  // is destroyed. (With `annClose()' in `ann_data_finalizer()').
  //
  // Without this hack, this psuedo code breaks:
  // AnnData* a1 = cpp_ann_init(data, 3, 0.0); // must be kd_tree or bd_tree
  // AnnData* a2 = cpp_ann_init(data, 3, 0.0); // must be bd_tree
  // cpp_ann_query(a2, ..., common_search = true);
  // delete a1;
  // cpp_ann_query(a2, ..., common_search = true); // Pointer to saved search has been deleted, breaks
  int total_AnnData_objects = 0;

  struct SearchTree {
    const int n_search_points;
    const int* const indices;
    ANNpointSet* const search_tree;

    SearchTree(int _n_search_points, int* _indices, ANNpointSet* _search_tree) :
      n_search_points(_n_search_points), indices(_indices), search_tree(_search_tree) {}

    ~SearchTree() {
      delete[] indices;
      // points to R data matrix, no need to delete the points themselves
      delete[] search_tree->thePoints();
      delete search_tree;
    }
  };

  class AnnData {
    private:
      const int n_data_points;
      const int dimensions;
      double* const data_matrix; // allocated by R
      const int treetype;
      const double eps;
      SearchTree* saved_search;

      SearchTree* get_search(SEXP, bool);
      void get_indices(const SearchTree*, SEXP, int, bool, int*);
      void get_distances(const SearchTree*, SEXP, int, bool, int*, double*);

    public:
      AnnData(int _n_data_points, int _dimensions,
              double* _data_matrix, int _treetype, double _eps) :
        n_data_points(_n_data_points), dimensions(_dimensions),
        data_matrix(_data_matrix), treetype(_treetype), eps(_eps), saved_search(NULL) {
          ++ANN::total_AnnData_objects;
        }

      ~AnnData() {
        delete saved_search;
        --ANN::total_AnnData_objects;
      }

      // Parameters:
      //   (1) search indices, (2) query indices, (3) k,
      //   (4) get distances?, (5) allow selfmatch?, (6) load/save search?
      // Returns R list
      SEXP get_knn(SEXP, SEXP, int, bool, bool, bool);
  };

  void ann_data_finalizer(SEXP ann_data_ptr_R) {
    if (!R_ExternalPtrAddr(ann_data_ptr_R)) return;
    AnnData* ann_data = static_cast<AnnData*>(R_ExternalPtrAddr(ann_data_ptr_R));
    delete ann_data;
    R_ClearExternalPtr(ann_data_ptr_R);
    if (ANN::total_AnnData_objects == 0) {
      annClose(); // Delete ANN's hanging pointer
    }
  }
}

extern "C" {

  SEXP cpp_ann_init(const SEXP data_matrix_R,
                    const SEXP treetype_R,
                    const SEXP eps_R) {

    const int dimensions = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[0];
    const int n_data_points = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[1];
    ANN::AnnData* ann_data = new ANN::AnnData(n_data_points,
                                              dimensions,
                                              REAL(data_matrix_R),
                                              asInteger(treetype_R),
                                              asReal(eps_R));

    SEXP ann_data_ptr_R = PROTECT(R_MakeExternalPtr(ann_data, R_NilValue, data_matrix_R));
    R_RegisterCFinalizerEx(ann_data_ptr_R, R_CFinalizer_t(ANN::ann_data_finalizer), TRUE);

    UNPROTECT(1);
    return ann_data_ptr_R;
  }

  SEXP cpp_ann_query(const SEXP ann_data_ptr_R,
                     const SEXP sindices_R,
                     const SEXP qindices_R,
                     const SEXP k_R,
                     const SEXP return_distances_R,
                     const SEXP selfmatch_R,
                     const SEXP common_search_R) {

    if (TYPEOF(ann_data_ptr_R) != EXTPTRSXP || !R_ExternalPtrAddr(ann_data_ptr_R)) {
      error("invalid ANN data pointer");
    }
    ANN::AnnData* ann_data = static_cast<ANN::AnnData*>(R_ExternalPtrAddr(ann_data_ptr_R));

    const int k = asInteger(k_R);
    const bool return_distances = (asLogical(return_distances_R) == 1);
    const bool selfmatch = (asLogical(selfmatch_R) == 1);
    const bool common_search = (asLogical(common_search_R) == 1);

    return ann_data->get_knn(sindices_R, qindices_R, k, return_distances, selfmatch, common_search);
  }
}

namespace ANN {

  SEXP AnnData::get_knn(const SEXP sindices_R,
                        const SEXP qindices_R,
                        const int k,
                        const bool return_distances,
                        const bool selfmatch,
                        const bool save_if_new) {

    const int n_queries = xlength(qindices_R);
    const SearchTree* const search = get_search(sindices_R, save_if_new);
    SEXP outlist_R;

    if (return_distances) {
      outlist_R = PROTECT(allocVector(VECSXP, 2));

      const SEXP nn_indices_R = PROTECT(allocMatrix(INTSXP, k, n_queries));
      const SEXP nn_dists_R = PROTECT(allocMatrix(REALSXP, k, n_queries));
      SET_VECTOR_ELT(outlist_R, 0, nn_indices_R);
      SET_VECTOR_ELT(outlist_R, 1, nn_dists_R);

      const SEXP listnames = PROTECT(allocVector(STRSXP, 2));
      SET_STRING_ELT(listnames, 0, mkChar("nn_indices"));
      SET_STRING_ELT(listnames, 1, mkChar("nn_dists"));
      setAttrib(outlist_R, R_NamesSymbol, listnames);

      get_distances(search, qindices_R, k, selfmatch, INTEGER(nn_indices_R), REAL(nn_dists_R));

      UNPROTECT(4);

    } else {
      outlist_R = PROTECT(allocVector(VECSXP, 1));

      const SEXP nn_indices_R = PROTECT(allocMatrix(INTSXP, k, n_queries));
      SET_VECTOR_ELT(outlist_R, 0, nn_indices_R);

      const SEXP listnames = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(listnames, 0, mkChar("nn_indices"));
      setAttrib(outlist_R, R_NamesSymbol, listnames);

      get_indices(search, qindices_R, k, selfmatch, INTEGER(nn_indices_R));

      UNPROTECT(3);
    }

    if (!save_if_new) {
      // temporary search, delete search tree
      delete search;
    }

    return outlist_R;
  }

  void AnnData::get_indices(const SearchTree* const search,
                            const SEXP qindices_R,
                            const int k,
                            const bool selfmatch,
                            int* write_indices) { // length n_queries * k

    const int n_queries = xlength(qindices_R);
    const int* const query_indices = INTEGER(qindices_R);
    const int run_k = k + !selfmatch;
    int tmp_indices[run_k];
    double tmp_dists[run_k];

    for (int i = 0; i < n_queries; ++i) {
      if (query_indices[i] < 0 || query_indices[i] > n_data_points) {
        error("Query index out of bounds.");
      }
      search->search_tree->annkSearch(data_matrix + query_indices[i] * dimensions, // pointer to query point
                                      run_k,       // number of neighbors
                                      tmp_indices, // pointer to start of index result
                                      tmp_dists,   // pointer to start of distance result
                                      eps);        // error margin

      // Translate to R indices
      const int* const stop_index = write_indices + k;
      int tmp_i = 0;

      if (!selfmatch) {
        // Translate from the k + 1 closest (incl. self) to k closest (excl. self)
        // First nn is nearly always self but with duplicate points it need not be
        for (; query_indices[i] != search->indices[tmp_indices[tmp_i]] && write_indices != stop_index;
             ++write_indices, ++tmp_i) {
          *write_indices = search->indices[tmp_indices[tmp_i]];
        }
        ++tmp_i; // Found self, skip it
      }

      // Remaining nns are OK
      for (; write_indices != stop_index; ++write_indices, ++tmp_i) {
        *write_indices = search->indices[tmp_indices[tmp_i]];
      }
    }
  }

  void AnnData::get_distances(const SearchTree* const search,
                              const SEXP qindices_R,
                              const int k,
                              const bool selfmatch,
                              int* write_indices,    // length n_queries * k
                              double* write_dists) { // length n_queries * k

    const int n_queries = xlength(qindices_R);
    const int* const query_indices = INTEGER(qindices_R);
    const int run_k = k + !selfmatch;
    int tmp_indices[run_k];
    double tmp_dists[run_k];

    for (int i = 0; i < n_queries; ++i) {
      if (query_indices[i] < 0 || query_indices[i] > n_data_points) {
        error("Query index out of bounds.");
      }
      search->search_tree->annkSearch(data_matrix + query_indices[i] * dimensions, // pointer to query point
                                      run_k,       // number of neighbors
                                      tmp_indices, // pointer to start of index result
                                      tmp_dists,   // pointer to start of distance result
                                      eps);        // error margin

      // Translate to R indices and unsquare distances
      const int* const stop_index = write_indices + k;
      int tmp_i = 0;

      if (!selfmatch) {
        for (; query_indices[i] != search->indices[tmp_indices[tmp_i]] && write_indices != stop_index;
             ++write_indices, ++write_dists, ++tmp_i) {
          *write_indices = search->indices[tmp_indices[tmp_i]];
          *write_dists = sqrt(tmp_dists[tmp_i]);
        }
        ++tmp_i;
      }
      for (; write_indices != stop_index; ++write_indices, ++write_dists, ++tmp_i) {
        *write_indices = search->indices[tmp_indices[tmp_i]];
        *write_dists = sqrt(tmp_dists[tmp_i]);
      }
    }
  }

  SearchTree* AnnData::get_search(const SEXP sindices_R,
                                  const bool save_if_new) {

    const int n_search_points = xlength(sindices_R);
    const int* const search_indices = INTEGER(sindices_R);

    // try to load?
    bool load = false;
    if (save_if_new && saved_search) { // saved search exists (member object)
      // check if saved search is identical to requested search
      if (n_search_points == saved_search->n_search_points) {
        load = true;
        for (int i = 0; i < n_search_points; ++i) {
          if (search_indices[i] != saved_search->indices[i]) {
            load = false; // not identical with saved search, construct new
            break;
          }
        }
      }
    }

    SearchTree* out_search;

    if (load) {
      out_search = saved_search;
    } else {
      // Need to construct new search tree
      int* const copy_indices = new int[n_search_points];
      ANNpointArray search_data_points = new ANNpoint[n_search_points];
      ANNpointSet* search_tree;

      for (int i = 0; i < n_search_points; ++i) {
        if (search_indices[i] < 0 || search_indices[i] > n_data_points) {
          error("Search index out of bounds.");
        }
        // pointer to data points of search
        search_data_points[i] = data_matrix + search_indices[i] * dimensions;
        // copy indices to make presistent (could be skipped if
        // save_if_new==FALSE but complicates SearchTree destruction)
        copy_indices[i] = search_indices[i];
      }

      switch (treetype) {
        case ANN_BRUTE:
          search_tree = new ANNbruteForce(search_data_points, n_search_points, dimensions);
          break;
        case ANN_KDTREE:
          search_tree = new ANNkd_tree(search_data_points, n_search_points, dimensions);
          break;
        case ANN_BDTREE:
          search_tree = new ANNbd_tree(search_data_points, n_search_points, dimensions);
          break;
        default:
          error("Unknown tree structure.");
      }

      out_search = new SearchTree(n_search_points, copy_indices, search_tree);

      if (save_if_new) {
        delete saved_search;
        saved_search = out_search;
      }
    }

    return out_search;
  }

}
