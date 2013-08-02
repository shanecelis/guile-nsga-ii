include $(top_srcdir)/support/automake/noweb.am
include $(top_srcdir)/support/automake/guile.am

.SECONDARY:

BUILT_SOURCES = $(NOWEB_BUILT_FILES) nsga-ii.c.x 
CLEANFILES = $(NOWEB_CLEANFILES) nsga-ii.c.x 

WARN_NOTANGLE = $(top_builddir)/bin/warn-notangle

dist-hook:
	cp $(distdir)/*.pdf $(top_distdir)

NOWEB_DOCS = nsga-ii.pdf
NOWEB_FILES = nsga-ii.nw
NOWEB_PRODUCTS = nsga-ii.c nsga-ii.h nsga-ii.scm nsga-ii-test.c nsga-ii-test.scm
EXTRA_DIST = $(NOWEB_DOCS) $(NOWEB_EXTRA_DIST)

nsga-ii.o : nsga-ii.c.x

check_PROGRAMS = 

TESTS = nsga-ii-test.scm
TEST_EXTENSIONS = .scm 
SCM_LOG_COMPILER = $(ENV) guile 

lib_LTLIBRARIES = libguile-nsga-ii.la

guilemoduledir = $(prefix)/share/guile/site/
dist_guilemodule_DATA = nsga-ii.scm

AM_CFLAGS  = $(GUILE_CFLAGS) -I $(NSGA2_HOME) $(GUILE_LOGGING_CFLAGS)

libguile_nsga_ii_la_LIBADD  = $(GUILE_LIBS) $(GUILE_LOGGING_LIBS)
libguile_nsga_ii_la_SOURCES = nsga-ii.c $(NSGA2_HOME)/allocate.c $(NSGA2_HOME)/auxiliary.c $(NSGA2_HOME)/crossover.c $(NSGA2_HOME)/crowddist.c $(NSGA2_HOME)/decode.c $(NSGA2_HOME)/display.c $(NSGA2_HOME)/dominance.c $(NSGA2_HOME)/eval.c $(NSGA2_HOME)/fillnds.c $(NSGA2_HOME)/initialize.c $(NSGA2_HOME)/list.c $(NSGA2_HOME)/merge.c $(NSGA2_HOME)/mutation.c $(NSGA2_HOME)/rand.c $(NSGA2_HOME)/rank.c $(NSGA2_HOME)/report.c $(NSGA2_HOME)/sort.c $(NSGA2_HOME)/tourselect.c 
include_HEADERS = 

MAINTAINERCLEANFILES = Makefile.in config.h.in nsga-ii-test
