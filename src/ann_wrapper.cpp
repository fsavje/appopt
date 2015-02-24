#include <R.h>
#include <Rinternals.h>
#include <ANN/ANN.h>

extern "C" {
  SEXP c_temp(SEXP data) {
	ANNpointArray s_data = new ANNpoint[1];
	s_data[0] = REAL(data);
	
	ANNpointSet* search_tree = new ANNbruteForce(s_data, 1, xlength(data));
	
	delete search_tree;
	delete[] s_data;
  }
}
