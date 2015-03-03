include $(MAKECONF)

BUILDDIR = build
TOBUILD = cholmod_aat.o cholmod_add.o cholmod_band.o cholmod_change_factor.o \
            cholmod_common.o cholmod_complex.o cholmod_copy.o cholmod_dense.o \
            cholmod_error.o cholmod_factor.o cholmod_memory.o cholmod_sparse.o \
            cholmod_transpose.o cholmod_triplet.o cholmod_version.o SuiteSparse_config.o

EXTRA_CFLAGS = -Wno-unused-variable -Wno-unused-but-set-variable

OBJECTS = $(addprefix $(BUILDDIR)/,$(TOBUILD))

LIBOUT = ../$(LIBDIR)/CHOLMOD.a
PKG_CPPFLAGS = -I../include/CHOLMOD/ -DNCHECK -DNCHOLESKY \
                -DNPARTITION -DCAMD -DNGPL -DNMATRIXOPS \
                -DNMODIFY -DNSUPERNODAL -DNTIMER -DNPRINT

library: $(BUILDDIR) $(LIBOUT)

$(LIBOUT): $(OBJECTS)
	$(AR) -rucs $(LIBOUT) $(OBJECTS)

$(BUILDDIR)/SuiteSparse_config.o: SuiteSparse_config/SuiteSparse_config.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) $(EXTRA_CFLAGS) -c $< -o $@

$(BUILDDIR)/%.o: Core/%.c
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) $(EXTRA_CFLAGS) -c $< -o $@

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

clean:
	$(RM) $(BUILDDIR)/*.o
