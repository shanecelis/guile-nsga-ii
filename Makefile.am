ACLOCAL_AMFLAGS         = -I support/m4

SUBDIRS = bin src tests 

EXTRA_DIST = README.md support

# I am using the following guidelines for the clean targets.
# http://ftp.gnu.org/old-gnu/Manuals/automake-1.7.2/html_chapter/automake_14.html
MAINTAINERCLEANFILES = Makefile.in libtool configure aclocal.m4 \
support/build-aux/config.guess \
support/build-aux/config.sub \
support/build-aux/depcomp \
support/build-aux/install-sh \
support/build-aux/ltmain.sh \
support/build-aux/missing \
support/build-aux/test-driver \
support/m4/libtool.m4 \
support/m4/ltoptions.m4 \
support/m4/ltsugar.m4 \
support/m4/ltversion.m4 \
support/m4/lt~obsolete.m4 \
$(distdir) \
$(distdir).tar.gz

