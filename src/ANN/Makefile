include $(MAKECONF)

PKG_CPPFLAGS = -I../include

LIBOUT = ../ANN.a

OBJECTS = ANN.o brute.o perf.o kd_dump.o kd_split.o kd_util.o \
			bd_fix_rad_search.o bd_pr_search.o bd_search.o bd_tree.o \
			kd_fix_rad_search.o kd_pr_search.o kd_search.o kd_tree.o

library: $(LIBOUT)

$(LIBOUT): $(OBJECTS)
	$(AR) -rucs $(LIBOUT) $(OBJECTS)

%.o: source/%.cpp
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c $< -o $@

clean:
	$(RM) *.o
