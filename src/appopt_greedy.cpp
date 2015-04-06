#include <vector>
#include <list>
#include <set>
#include <map>
#include <utility>
#include <cmath>

#include <R.h>
#include <Rinternals.h>

using std::vector;
using std::list;
using std::set;
using std::map;
using std::pair;

class greedy_vertex {
private:
  list<pair<double, int> > dists;
  map<int, list<pair<double, int> >::iterator> refs;

public:
  void add_edge(const int j, const double dist) {
    refs[j] = dists.insert(dists.end(), std::make_pair(dist, j));
  }

  void remove_edge(const int j) {
    map<int, list<pair<double, int> >::iterator>::iterator to_delete = refs.find(j);
    dists.erase(to_delete->second);
    refs.erase(to_delete);
  }

  void sort() {
    dists.sort();
  }

  double get_edge_dist(const int j) {
    return refs[j]->first;
  }

  vector<int> get_k_nn(const int k) {
    vector<int> k_nn;
    for (list<pair<double,int> >::const_iterator it = dists.begin();
         static_cast<int>(k_nn.size()) < k; ++it) {
      k_nn.push_back(it->second);
    }
    return k_nn;
  }

  double get_max_dist() {
    return dists.back().first;
  }

  int get_max_edge() {
    return dists.back().second;
  }
};


class greedy_block {
private:
  set<int> block;
  vector<greedy_vertex>* vertices_ptr;

  pair<int,int> get_vertices_max_dist() {
    vector<greedy_vertex>& vertices = *vertices_ptr;
    set<int>::const_iterator it = block.begin();
    pair<int,int> max_pair = std::make_pair(*it, vertices[*it].get_max_edge());
    double dist_max = vertices[*it].get_max_dist();

    for (++it; it != block.end(); ++it) {
      if (vertices[*it].get_max_dist() > dist_max) {
        max_pair = std::make_pair(*it, vertices[*it].get_max_edge());
        dist_max = vertices[*it].get_max_dist();
      }
    }

    // Make blocking indices stable
    if (max_pair.first > max_pair.second) {
      std::swap(max_pair.first, max_pair.second);
    }

    return max_pair;
  }

public:
  greedy_block(vector<greedy_vertex>* init_vertices_ptr) : block(), vertices_ptr(init_vertices_ptr) { }

  int size() {
    return block.size();
  }

  vector<int> get_vertices() {
    return vector<int>(block.begin(), block.end());
  }

  void add_vertex(const int i) {
    block.insert(i);
  }

  void separate_vertex(const int i) {
    for (set<int>::const_iterator it = block.begin(); it != block.end(); ++it) {
      (*vertices_ptr)[i].remove_edge(*it);
      (*vertices_ptr)[*it].remove_edge(i);
    }
  }

  void break_block(greedy_block& other_block, const int k) {
    vector<greedy_vertex>& vertices = *vertices_ptr;

    pair<int,int> centers = get_vertices_max_dist();

    set<int> old_block;
    block.swap(old_block);

    this->add_vertex(centers.first);
    old_block.erase(centers.first);

    other_block.add_vertex(centers.second);
    old_block.erase(centers.second);
    this->separate_vertex(centers.second);

    vector<int> nn_first = vertices[centers.first].get_k_nn(k - 1);
    for (vector<int>::const_iterator it_f = nn_first.begin(); it_f != nn_first.end(); ++it_f) {
      this->add_vertex(*it_f);
      old_block.erase(*it_f);
      other_block.separate_vertex(*it_f);
    }

    vector<int> nn_second = vertices[centers.second].get_k_nn(k - 1);
    for (vector<int>::const_iterator it_s = nn_second.begin(); it_s != nn_second.end(); ++it_s) {
      other_block.add_vertex(*it_s);
      old_block.erase(*it_s);
      this->separate_vertex(*it_s);
    }

    for (set<int>::const_iterator it = old_block.begin(); it != old_block.end(); ++it) {
      if (vertices[*it].get_edge_dist(centers.first) <= vertices[*it].get_edge_dist(centers.second)) {
        this->add_vertex(*it);
        other_block.separate_vertex(*it);
      } else {
        other_block.add_vertex(*it);
        this->separate_vertex(*it);
      }
    }
  }
};


class greedy_blocking {
private:
  const int k;
  vector<greedy_block> blocking;
  vector<greedy_vertex> vertices;

  inline double get_distance(const int i, const int j, const int dimensions, const double* const data_matrix) {
    double out = 0;
    for (int d = 0; d < dimensions; ++d) {
      out += std::pow(data_matrix[i * dimensions + d] - data_matrix[j * dimensions + d], 2);
    }
    return std::sqrt(out);
  }

public:
  greedy_blocking(const int init_k,
                  const int n_vertices,
                  const int* const init_blocking,
                  const int dimensions,
                  const double* const data_matrix) : k(init_k), blocking(), vertices(n_vertices) {

    blocking.reserve(1 + n_vertices / k);

    for (int i = 0; i < n_vertices; ++i) {
      if (init_blocking[i] + 1 > static_cast<int>(blocking.size())) {
        blocking.resize(init_blocking[i] + 1, greedy_block(&vertices));
      }
      blocking[init_blocking[i]].add_vertex(i);
    }

    for (vector<greedy_block>::iterator b_it = blocking.begin(); b_it != blocking.end(); ++b_it) {
      if (b_it->size() >= 2 * k) {
        vector<int> block_vertices = b_it->get_vertices();
        for (vector<int>::const_iterator i_it = block_vertices.begin();
             i_it != block_vertices.end(); ++i_it) {
          for (vector<int>::const_iterator j_it = i_it + 1;
               j_it != block_vertices.end(); ++j_it) {
            double dist = get_distance(*i_it, *j_it, dimensions, data_matrix);
            vertices[*i_it].add_edge(*j_it, dist);
            vertices[*j_it].add_edge(*i_it, dist);
          }
          vertices[*i_it].sort();
        }
      }
    }
  }

  void construct_blocking() {
    for (int b = 0; b < static_cast<int>(blocking.size()); ) {
      if (blocking[b].size() >= 2 * k) {
        blocking.push_back(greedy_block(&vertices));
        blocking[b].break_block(blocking.back(), k);
      } else {
        ++b;
      }
    }
  }

  void write_blocking_to_array(int* const blocking_array) {
    for (int b = 0; b < static_cast<int>(blocking.size()); ++b) {
      vector<int> block_vertices = blocking[b].get_vertices();
      for (vector<int>::const_iterator i_it = block_vertices.begin();
             i_it != block_vertices.end(); ++i_it) {
        blocking_array[*i_it] = b;
      }
    }
  }

};

extern "C" {

  SEXP cpp_get_greedy_blocking(const SEXP k_R,
                               const SEXP prev_blocking_R,
                               const SEXP data_matrix_R) {

    const int k = asInteger(k_R);
    const int n_vertices = length(prev_blocking_R);
    const int dimensions = INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[0];

    if (INTEGER(getAttrib(data_matrix_R, R_DimSymbol))[1] != n_vertices) {
      error("Invalid data matrix.");
    }

    const int* const prev_blocking = INTEGER(prev_blocking_R);
    const double* const data_matrix = REAL(data_matrix_R);

    greedy_blocking greedy_obj(k, n_vertices, prev_blocking, dimensions, data_matrix);

    greedy_obj.construct_blocking();

    const SEXP new_blocking_R = PROTECT(allocVector(INTSXP, n_vertices));
    int* const new_blocking = INTEGER(new_blocking_R);
    std::fill_n(new_blocking, n_vertices, 0);

    greedy_obj.write_blocking_to_array(new_blocking);

    UNPROTECT(1);
    return new_blocking_R;
  }

}

