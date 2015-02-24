#include <R.h>
#include <Rinternals.h>
#include <ANN/ANN.h>

#define ANN_BRUTE  1
#define ANN_KDTREE 2
#define ANN_BDTREE 3

namespace ANN {

  struct SearchTree {
    const size_t n_search_points;
    const int* const indices;
    ANNpointSet* const search_tree;

    SearchTree(size_t _n_search_points, int* _indices, ANNpointSet* _search_tree) :
      n_search_points(_n_search_points), indices(_indices), search_tree(_search_tree) {}

    ~SearchTree() {
      delete[] indices;
      // points to R data matrix, no need to delete the points themselves
      ANNpointArray temp_data = search_tree->thePoints();
      delete search_tree;
      delete[] temp_data;
    }
  };

  class AnnData {
    private:
      const size_t n_data_points;
      const size_t dimensions;
      double* const data_matrix; // allocated by R
      int treetype;
      double eps;
      SearchTree* saved_search;

      SearchTree* get_search(SEXP, bool);
      void get_indices(SearchTree*, SEXP, int, bool, int*);
      void get_distances(SearchTree*, SEXP, int, bool, int*, double*);

    public:
      AnnData(size_t _n_data_points, size_t _dimensions,
              double* _data_matrix, int _treetype, double _eps) :
        n_data_points(_n_data_points), dimensions(_dimensions),
        data_matrix(_data_matrix), treetype(_treetype), eps(_eps), saved_search(NULL) {}

      ~AnnData() {
        delete saved_search;
      }

      // Parameters:
      //   (1) search indices, (2) query indices, (3) k,
      //   (4) get distances?, (5) allow selfmatch?, (6) load/save search?
      // Returns R list
      SEXP get_knn(SEXP, SEXP, int, bool, bool, bool);
  };

  static void ann_data_finalizer(SEXP ann_data_ptr_R) {
      if (!R_ExternalPtrAddr(ann_data_ptr_R)) return;
      AnnData* ann_data = static_cast<AnnData*>(R_ExternalPtrAddr(ann_data_ptr_R));
      delete ann_data;
      R_ClearExternalPtr(ann_data_ptr_R);
  }
}

extern "C" {

  SEXP c_ann_init(SEXP data_matrix_R, SEXP treetype_R, SEXP eps_R) {

    size_t dimensions = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[0];
    size_t n_data_points = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[1];
    ANN::AnnData* ann_data = new ANN::AnnData(n_data_points,
                                              dimensions,
                                              REAL(data_matrix_R),
                                              asInteger(treetype_R),
                                              asReal(eps_R));

    SEXP ann_data_ptr_R = PROTECT(R_MakeExternalPtr(ann_data, R_NilValue, data_matrix_R));
    R_RegisterCFinalizerEx(ann_data_ptr_R, (R_CFinalizer_t) ANN::ann_data_finalizer, TRUE);

    UNPROTECT(1);
    return ann_data_ptr_R;
  }

  SEXP c_ann_query(SEXP ann_data_ptr_R,
                   SEXP sindices_R,
                   SEXP qindices_R,
                   SEXP k_R,
                   SEXP return_distances_R,
                   SEXP selfmatch_R,
                   SEXP common_search_R) {

    if (!R_ExternalPtrAddr(ann_data_ptr_R)) {
      error("invalid ANN data pointer");
    }
    ANN::AnnData* ann_data = static_cast<ANN::AnnData*>(R_ExternalPtrAddr(ann_data_ptr_R));

    int k = asInteger(k_R);
    bool return_distances = (asLogical(return_distances_R) == 1);
    bool selfmatch = (asLogical(selfmatch_R) == 1);
    bool common_search = (asLogical(common_search_R) == 1);

    return ann_data->get_knn(sindices_R, qindices_R, k, return_distances, selfmatch, common_search);
  }
}

namespace ANN {

  SEXP AnnData::get_knn(SEXP sindices_R,
                        SEXP qindices_R,
                        int k,
                        bool return_distances,
                        bool selfmatch,
                        bool save_if_new) {

    size_t n_queries = xlength(qindices_R);
    SearchTree* search = get_search(sindices_R, save_if_new);
    SEXP outlist_R;

    if (return_distances) {
      outlist_R = PROTECT(allocVector(VECSXP, 2));

      SEXP nn_indices_R = PROTECT(allocMatrix(INTSXP, k, n_queries));
      SEXP nn_dists_R = PROTECT(allocMatrix(REALSXP, k, n_queries));
      SET_VECTOR_ELT(outlist_R, 0, nn_indices_R);
      SET_VECTOR_ELT(outlist_R, 1, nn_dists_R);

      SEXP listnames = PROTECT(allocVector(STRSXP, 2));
      SET_STRING_ELT(listnames, 0, mkChar("nn_indices"));
      SET_STRING_ELT(listnames, 1, mkChar("nn_dists"));
      setAttrib(outlist_R, R_NamesSymbol, listnames);

      get_distances(search, qindices_R, k, selfmatch, INTEGER(nn_indices_R), REAL(nn_dists_R));

      UNPROTECT(4);

    } else {
      outlist_R = PROTECT(allocVector(VECSXP, 1));

      SEXP nn_indices_R = PROTECT(allocMatrix(INTSXP, k, n_queries));
      SET_VECTOR_ELT(outlist_R, 0, nn_indices_R);

      SEXP listnames = PROTECT(allocVector(STRSXP, 1));
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

  void AnnData::get_indices(SearchTree* search,
                            SEXP qindices_R,
                            int k,
                            bool selfmatch,
                            int* write_indices) { // length n_queries * k

    size_t n_queries = xlength(qindices_R);
    int* query_indices = INTEGER(qindices_R);
    int run_k = k + !selfmatch;
    int tmp_indices[run_k];
    double tmp_dists[run_k];

    for (size_t i = 0; i < n_queries; ++i) {
      if (query_indices[i] < 1 || static_cast<unsigned int>(query_indices[i]) > n_data_points) {
        error("query index out of bounds");
      }
      search->search_tree->annkSearch(data_matrix + query_indices[i] * dimensions, // pointer to query point
                                      run_k,       // number of neighbors
                                      tmp_indices, // pointer to start of index result
                                      tmp_dists,   // pointer to start of distance result
                                      eps);        // error margin

      // Translate to R indices
      int* stop_index = write_indices + k;
      size_t tmp_i = 0;

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

  void AnnData::get_distances(SearchTree* search,
                              SEXP qindices_R,
                              int k,
                              bool selfmatch,
                              int* write_indices,    // length n_queries * k
                              double* write_dists) { // length n_queries * k

    size_t n_queries = xlength(qindices_R);
    int* query_indices = INTEGER(qindices_R);
    int run_k = k + !selfmatch;
    int tmp_indices[run_k];
    double tmp_dists[run_k];

    for (size_t i = 0; i < n_queries; ++i) {
      if (query_indices[i] < 1 || static_cast<unsigned int>(query_indices[i]) > n_data_points) {
        error("query index out of bounds");
      }
      search->search_tree->annkSearch(data_matrix + query_indices[i] * dimensions, // pointer to query point
                                      run_k,       // number of neighbors
                                      tmp_indices, // pointer to start of index result
                                      tmp_dists,   // pointer to start of distance result
                                      eps);        // error margin

      // Translate to R indices and unsquare distances
      int* stop_index = write_indices + k;
      size_t tmp_i = 0;

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

  SearchTree* AnnData::get_search(SEXP sindices_R, bool save_if_new) {

    size_t n_search_points = xlength(sindices_R);
    int* search_indices = INTEGER(sindices_R);

    // try to load?
    bool load = false;
    if (save_if_new && saved_search) { // saved search exists
      // check if saved search is identical to requested search
      if (n_search_points == saved_search->n_search_points) {
        load = true;
        for (size_t i = 0; i < n_search_points; ++i) {
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

      int* copy_indices = new int[n_search_points];
      ANNpointArray search_data_points = new ANNpoint[n_search_points];
      ANNpointSet* search_tree;

      for (size_t i = 0; i < n_search_points; ++i) {
        if (search_indices[i] < 1 || static_cast<unsigned int>(search_indices[i]) > n_data_points) {
          error("search index out of bounds");
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
          annClose(); // Delete hanging pointer used at tree construction
          break;
        case ANN_BDTREE:
          search_tree = new ANNbd_tree(search_data_points, n_search_points, dimensions);
          annClose(); // Delete hanging pointer used at tree construction
          break;
        default:
          error("unknown tree structure");
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
