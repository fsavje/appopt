include $(MAKECONF)

BUILDDIR = build
TOBUILD = ANN.o brute.o perf.o kd_dump.o kd_split.o kd_util.o \
			bd_fix_rad_search.o bd_pr_search.o bd_search.o bd_tree.o \
			kd_fix_rad_search.o kd_pr_search.o kd_search.o kd_tree.o

OBJECTS = $(addprefix $(BUILDDIR)/,$(TOBUILD))

LIBOUT = ../$(LIBDIR)/ANN.a
PKG_CPPFLAGS = -I../include

library: $(BUILDDIR) $(LIBOUT)

$(LIBOUT): $(OBJECTS)
	$(AR) -rucs $(LIBOUT) $(OBJECTS)

$(BUILDDIR)/%.o: source/%.cpp
	$(CXX) $(ALL_CPPFLAGS) $(ALL_CXXFLAGS) -c -Wno-unused-const-variable $< -o $@

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

clean:
	$(RM) $(BUILDDIR)/*.o
