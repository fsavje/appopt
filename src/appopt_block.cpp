#include <cmath>
#include <list>
#include <algorithm>
#include <utility>
#include <R.h>
#include <Rinternals.h>
#include <CHOLMOD/cholmod.h>

#define LEXICAL      1
#define FSTPOWORDER  2
#define SNDPOWORDER  3
#define HEURISTIC    4
#define MAXIS        5

#define DIRECTED    1
#define UNDIRECTED  2
#define PAPER       3

//SEXP get_R_mat(cholmod_sparse*);
int get_blocking_internal(int, int, SEXP, int, int, std::list<int>&, int*);
cholmod_sparse* get_cholmod_NNE(int, int, SEXP, cholmod_common*);
cholmod_sparse* get_second_power(cholmod_sparse*, int, cholmod_common*);

void get_ordering(cholmod_sparse*, int, int, std::list<int>&, cholmod_common*);
void heuristic_search(const cholmod_sparse*, std::list<int>&, std::list<int>&);
void findMIS_in_sp_lex(const cholmod_sparse*, std::list<int>&);
void findMIS_in_sp_order(const cholmod_sparse*, const std::list<int>&, std::list<int>&);
void findMaxIS_in_sp(cholmod_sparse*, int, std::list<int>&, cholmod_common*);
bool recur_MaxIS(bool*, int, int*, const int*, const int*, const int*, int, int*, std::list<int>&);

extern "C" {

  SEXP cpp_get_blocking(const SEXP n_vertices_R,
                        const SEXP block_size_R,
                        const SEXP NNE_R,
                        const SEXP algorithm_R,
                        const SEXP MIS_method_R) {

    const int n_vertices = asInteger(n_vertices_R);
    const int n_edges = asInteger(block_size_R) - 1;
    const int algorithm = asInteger(algorithm_R);
    const int MIS_method = asInteger(MIS_method_R);

    const SEXP out_blocks_R = PROTECT(allocVector(INTSXP, n_vertices));
    int* const out_blocks = INTEGER(out_blocks_R);
    std::fill_n(out_blocks, n_vertices, 0);
    // For some reason there's a memory leak when `seeds'
    // is allocated on the stack, it works on the heap.
    std::list<int>* const seeds_ptr = new std::list<int>();
    std::list<int>& seeds = *seeds_ptr;

    int n_unassigned = get_blocking_internal(n_vertices, n_edges, NNE_R,
                                             algorithm, MIS_method,
                                             seeds, out_blocks);

    // List of seeds
    const SEXP out_seeds_R = PROTECT(allocVector(INTSXP, seeds.size()));
    int* out_seeds = INTEGER(out_seeds_R);
    for (std::list<int>::const_iterator it = seeds.begin();
         it != seeds.end(); ++it, ++out_seeds) {
      *out_seeds = *it;
    }
    delete seeds_ptr;

    // Returns unassigned and checks solution, will break
    // ungracefully if something went wrong with blocking.
    // The theorems in the paper ensures that this will never happen.
    const SEXP out_unassigned_R = PROTECT(allocVector(INTSXP, n_unassigned));
    int* out_unassigned_ptr = INTEGER(out_unassigned_R);
    for (int i = 0; i < n_vertices; ++i) {
      if (out_blocks[i] == 0) {
        // If overflow = some vertices have been assigned to multiple blocks.
        *out_unassigned_ptr = i;
        ++out_unassigned_ptr;
      }
    }

    const SEXP outlist_R = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(outlist_R, 0, out_seeds_R);
    SET_VECTOR_ELT(outlist_R, 1, out_blocks_R);
    SET_VECTOR_ELT(outlist_R, 2, out_unassigned_R);

    const SEXP listnames = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(listnames, 0, mkChar("seeds"));
    SET_STRING_ELT(listnames, 1, mkChar("blocks"));
    SET_STRING_ELT(listnames, 2, mkChar("unassigned"));
    setAttrib(outlist_R, R_NamesSymbol, listnames);

    UNPROTECT(5);
    return outlist_R;
  }

}

int get_blocking_internal(const int n_vertices,
                          const int n_edges,
                          const SEXP NNE_R,
                          const int algorithm,
                          const int MIS_method,
                          std::list<int>& seeds,
                          int* const blocks) {

  cholmod_common cholmod_c;
  cholmod_start(&cholmod_c);

  cholmod_sparse* NNE = get_cholmod_NNE(n_vertices, n_edges, NNE_R, &cholmod_c);

  if (algorithm == UNDIRECTED || algorithm == PAPER) {
    // NNE = NNE | t(NNE)
    cholmod_sparse* NNEt = cholmod_transpose(NNE, CHOLMOD_PATTERN, &cholmod_c);
    cholmod_sparse* NNE_tmp = cholmod_add(NNE, NNEt, NULL, NULL, false, false, &cholmod_c);
    cholmod_free_sparse(&NNEt, &cholmod_c);
    NNE->i = NULL; // Remove pointer to R object before freeing memory
    cholmod_free_sparse(&NNE, &cholmod_c);
    NNE = NNE_tmp;
  }

  switch(MIS_method) {
    case LEXICAL:
      findMIS_in_sp_lex(NNE, seeds);
      break;

    case FSTPOWORDER:
    case SNDPOWORDER:
    case HEURISTIC:
      {
        std::list<int> ordering;
        get_ordering(NNE, MIS_method, algorithm, ordering, &cholmod_c);
        if (MIS_method == HEURISTIC) {
          heuristic_search(NNE, ordering, seeds);
        } else {
          findMIS_in_sp_order(NNE, ordering, seeds);
        }
      }
      break;

    case MAXIS:
      findMaxIS_in_sp(NNE, algorithm, seeds, &cholmod_c);
      break;

    default:
      error("Unknown MIS method.");
  }

  const int* const NNE_p = static_cast<const int*>(NNE->p);
  const int* const NNE_i = static_cast<const int*>(NNE->i);
  int n_unassigned = n_vertices;

  int block_label = 1;
  for (std::list<int>::const_iterator it = seeds.begin();
       it != seeds.end(); ++it, ++block_label) {
    // Set block for seed
    blocks[*it] = block_label;
    --n_unassigned;

    // Set block for adjacent to seed
    const int* const a_stop = NNE_i + NNE_p[*it + 1];
    for (const int* a = NNE_i + NNE_p[*it]; a != a_stop; ++a) {
      blocks[*a] = block_label;
      --n_unassigned;
    }
  }

  if (algorithm == PAPER) {
    // Assign unassigned to the block that contains
    // any of its nearest neighbors. When ties pick
    // first adjacent lexicographically by index.
    // Write negative block label as temp.
    for (int i = 0; i < n_vertices; ++i) {
      if (blocks[i] == 0) {
        --n_unassigned;
        int lowest_adjacent = n_vertices;
        const int* const a_stop = NNE_i + NNE_p[i + 1];
        for (const int* a = NNE_i + NNE_p[i]; a != a_stop; ++a) {
          if (*a < lowest_adjacent && blocks[*a] > 0) {
            blocks[i] = -blocks[*a];
            lowest_adjacent = *a;
          }
        }
      }
    }
    for (int i = 0; i < n_vertices; ++i) {
      if (blocks[i] < 0) {
        blocks[i] = -blocks[i];
      }
    }
  }

  if (algorithm == DIRECTED) { // This is already done for undirected case
    NNE->i = NULL; // Remove pointer to R object before freeing memory
  }
  cholmod_free_sparse(&NNE, &cholmod_c);
  cholmod_finish(&cholmod_c);

  return n_unassigned;
}

void findMaxIS_in_sp(cholmod_sparse* const NNE,
                     const int algorithm,
                     std::list<int>& MaxIs,
                     cholmod_common* const cholmod_c) {

  cholmod_sparse* second_power = get_second_power(NNE, algorithm, cholmod_c);
  const int* const sp_p = static_cast<const int*>(second_power->p);
  const int* const sp_i = static_cast<const int*>(second_power->i);
  bool inSetI[NNE->ncol];
  std::fill_n(inSetI, NNE->ncol, true);
  int removed_indices[NNE->ncol];
  int sizeMaxIS = 0;

  recur_MaxIS(inSetI, 0, removed_indices,
              removed_indices + NNE->ncol,
              sp_p, sp_i, 0, &sizeMaxIS, MaxIs);

  cholmod_free_sparse(&second_power, cholmod_c);
}

bool recur_MaxIS(bool* const inSetI,
                 int i,
                 int* removed_indices,
                 const int* const ri_end,
                 const int* const adja_p,
                 const int* const adja_i,
                 const int sizeSetS,
                 int* const sizeMaxIS,
                 std::list<int>& MaxIS) {

  if (removed_indices == ri_end) {
    if (sizeSetS > *sizeMaxIS) {
      MaxIS.clear();
      *sizeMaxIS = sizeSetS;
      return true;
    } else {
      return false;
    }
  }

  bool updated = false;
  const int* const ri_undo_i = removed_indices;

  for (; (ri_end - removed_indices) + sizeSetS > *sizeMaxIS; ++i) {
    if (inSetI[i]) {
      inSetI[i] = false;
      *removed_indices = i;
      ++removed_indices;
      const int* const ri_undo_a = removed_indices;

      const int* const a_stop = adja_i + adja_p[i + 1];
      for (const int* a = adja_i + adja_p[i]; a != a_stop; ++a) {
        if (inSetI[*a]) {
          inSetI[*a] = false;
          *removed_indices = *a;
          ++removed_indices;
        }
      }

      if (recur_MaxIS(inSetI, i + 1, removed_indices, ri_end, adja_p, adja_i, sizeSetS + 1, sizeMaxIS, MaxIS)) {
        MaxIS.push_back(i);
        updated = true;
      }

      while (removed_indices > ri_undo_a) {
        --removed_indices;
        inSetI[*removed_indices] = true;
      }
    }
  }

  while (removed_indices > ri_undo_i) {
    --removed_indices;
    inSetI[*removed_indices] = true;
  }

  return updated;
}

void heuristic_search(const cholmod_sparse* const NNE,
                      std::list<int>& ordering,
                      std::list<int>& MIS) {

  std::list<int> tempMIS;

  size_t to_check = 100 * static_cast<size_t>(std::sqrt(static_cast<long double>(NNE->ncol))) + 100;
  if (to_check > NNE->ncol) {
    to_check = NNE->ncol;
  }

  // 1,2,3,4,5
  // 2,1,3,4,5
  // 1,3,2,4,5
  // 3,2,4,1,5
  // 2,4,1,5,3
  // etc...

  std::list<int>::iterator insert_position = ordering.begin();
  for (size_t i = 0; i < to_check; ++i) {
    ++insert_position;
    ordering.insert(insert_position, ordering.front());
    ordering.pop_front();

    findMIS_in_sp_order(NNE, ordering, tempMIS);

    if (tempMIS.size() > MIS.size()) {
      MIS.swap(tempMIS);
    }
    tempMIS.clear();
  }
}

inline void findMIS_check_v(const int i,
                            bool* const inSetA,
                            const int* const NNE_p,
                            const int* const NNE_i,
                            std::list<int>& MIS) {
  // Is not removed?
  if (!inSetA[i]) {
    // Check all adjacent
    bool adjacent_toA = false;
    const int* const a_stop = NNE_i + NNE_p[i + 1];

    for (const int* a = NNE_i + NNE_p[i];
         !adjacent_toA && a != a_stop; ++a) {
      // breaks when adjacent_toA == true, or all neighbors are checked
      adjacent_toA = inSetA[*a];
    }

    if (!adjacent_toA) {
      // Not adjacent, add to S
      MIS.push_back(i);
      inSetA[i] = true;
      for (const int* a = NNE_i + NNE_p[i];
           a != a_stop; ++a) {
        inSetA[*a] = true;
      }
    }
  }
}

void findMIS_in_sp_lex(const cholmod_sparse* const NNE,
                       std::list<int>& MIS) {

  bool* const inSetA = new bool[NNE->ncol];
  std::fill_n(inSetA, NNE->ncol, false);
  const int* const NNE_p = static_cast<const int*>(NNE->p);
  const int* const NNE_i = static_cast<const int*>(NNE->i);

  int n_vertices = static_cast<int>(NNE->ncol);
  for (int i = 0; i < n_vertices; ++i) {
    findMIS_check_v(i, inSetA, NNE_p, NNE_i, MIS);
  }

  delete[] inSetA;
}

void findMIS_in_sp_order(const cholmod_sparse* const NNE,
                         const std::list<int>& ordering,
                         std::list<int>& MIS) {

  bool* const inSetA = new bool[NNE->ncol];
  std::fill_n(inSetA, NNE->ncol, false);
  const int* const NNE_p = static_cast<const int*>(NNE->p);
  const int* const NNE_i = static_cast<const int*>(NNE->i);

  for (std::list<int>::const_iterator it = ordering.begin();
       it != ordering.end(); ++it) {
    findMIS_check_v(*it, inSetA, NNE_p, NNE_i, MIS);
  }

  delete[] inSetA;
}

void get_ordering(cholmod_sparse* const NNE,
                  const int MIS_method,
                  const int algorithm,
                  std::list<int>& ordering,
                  cholmod_common* const cholmod_c) {

  cholmod_sparse* adja_mat;
  switch(MIS_method) {
    case FSTPOWORDER:
      if (algorithm == DIRECTED) {
        // adja_mat = NNE | t(NNE)
        cholmod_sparse* NNEt = cholmod_transpose(NNE, CHOLMOD_PATTERN, cholmod_c);
        adja_mat = cholmod_add(NNE, NNEt, NULL, NULL, false, false, cholmod_c);
        cholmod_free_sparse(&NNEt, cholmod_c);
      } else {
        // adja_mat = NNE
        adja_mat = cholmod_copy_sparse(NNE, cholmod_c);
      }
      break;

    case SNDPOWORDER:
    case HEURISTIC:
      adja_mat = get_second_power(NNE, algorithm, cholmod_c);
      break;

    default:
      error("Unknown MIS method.");
  }

  // adja_mat_p: array with pointer to elements for each column
  // where `i' is start and `i+1' is finish.
  // Can thus be used to get col sums.
  const int* const adja_mat_p = static_cast<const int*>(adja_mat->p);
  std::list<std::pair<int,int> > tmp_order;

  int n_vertices = static_cast<int>(NNE->ncol);
  for (int i = 0; i < n_vertices; ++i) {
    tmp_order.push_back(std::make_pair(adja_mat_p[i + 1] - adja_mat_p[i], i));
  }
  cholmod_free_sparse(&adja_mat, cholmod_c);

  // sorts first according to colsum (i.e., first el)
  // then by vertex id. As ID is unique, sorting is stable.
  tmp_order.sort();

  for (std::list<std::pair<int,int> >::const_iterator it = tmp_order.begin();
       it != tmp_order.end(); ++it) {
    ordering.push_back(it->second);
  }
}

cholmod_sparse* get_second_power(cholmod_sparse* const NNE,
                                 const int algorithm,
                                 cholmod_common* const cholmod_c) {

  cholmod_sparse* out;
  if (algorithm == DIRECTED) {
    // out = (NNE | t(NNE) | t(NNE) %*% NNE) & !I
    cholmod_sparse* NNEt = cholmod_transpose(NNE, CHOLMOD_PATTERN, cholmod_c);
    cholmod_sparse* NNEtNNE = cholmod_aat(NNEt, NULL, 0, -1, cholmod_c); // -1 = no diagnol
    cholmod_sparse* tmp = cholmod_add(NNE, NNEt, NULL, NULL, false, false, cholmod_c);
    cholmod_free_sparse(&NNEt, cholmod_c);
    out = cholmod_add(tmp, NNEtNNE, NULL, NULL, false, false, cholmod_c);
    cholmod_free_sparse(&NNEtNNE, cholmod_c);
    cholmod_free_sparse(&tmp, cholmod_c);

  } else {
    // out = (NNE | t(NNE) %*% NNE) & !I
    cholmod_sparse* NNEtNNE = cholmod_aat(NNE, NULL, 0, -1, cholmod_c); // -1 = no diagnol
    out = cholmod_add(NNE, NNEtNNE, NULL, NULL, false, false, cholmod_c);
    cholmod_free_sparse(&NNEtNNE, cholmod_c);
  }

  return out;
}

cholmod_sparse* get_cholmod_NNE(const int n_vertices,
                                const int n_edges,
                                const SEXP NNE_R,
                                cholmod_common* const cholmod_c) {

  if (n_vertices * n_edges != xlength(NNE_R)) {
    error("Supplied NNE does not match number vertices and block size.");
  }

  cholmod_sparse* out = cholmod_allocate_sparse(n_vertices, // rows
                                                n_vertices, // cols
                                                n_vertices * n_edges, // edges
                                                false, // not sorted
                                                true, // packed
                                                false, //unsymmetric
                                                CHOLMOD_PATTERN,
                                                cholmod_c);

  int* const p_ptr = static_cast<int*>(out->p);
  p_ptr[0] = 0;
  for (int i = 1; i <= n_vertices; ++i) {
    p_ptr[i] = i * n_edges;
  }

  out->i = static_cast<void*>(INTEGER(NNE_R));

  return out;
}

/*
SEXP get_R_mat(cholmod_sparse* const mat) {
  SEXP out_R = PROTECT(allocMatrix(INTSXP, mat->nrow, mat->ncol));

  int* ww = INTEGER(out_R);
  int* ww_stop = ww + mat->ncol * mat->nrow;
  for (int* it = ww; it != ww_stop; ++it) {
    *it = 0;
  }

  int* i_ptr = static_cast<int*>(mat->i);
  int* p_ptr = static_cast<int*>(mat->p);

  for (int i = 0; i < mat->ncol; ++i) {
    for (int* s = i_ptr + p_ptr[i]; s != i_ptr + p_ptr[i + 1]; ++s) {
      ww[i * mat->nrow + *s]++;
    }
  }

  return out_R;
}
*/
