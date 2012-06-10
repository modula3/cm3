AUTOMAKE_OPTIONS = 1.6 gnu $(top_builddir)/ansi2knr

check_PROGRAMS = tversion tinternals tinits tisqrt tsgn tcheck tisnan \
	texceptions tset_exp tset tabs tset_d tset_f tset_q tset_si tset_str \
	tset_z tset_ld tset_sj tswap tcopysign tcmp tcmp2 tcmpabs tcmp_d \
	tcmp_ld tcomparisons teq tadd tsub tmul tdiv tsub1sp tadd1sp tadd_ui \
	tsub_ui tcmp_ui tdiv_ui tmul_ui tsqrt_ui tui_div tui_sub tadd_d \
	tsub_d td_sub tmul_d tdiv_d td_div tgmpop tsi_op tmul_2exp tfma tfms \
	tsum tdim tminmax tnext tfits tget_d tget_d_2exp tget_z tget_str \
	tget_sj tout_str tinp_str toutimpl tcan_round tround_prec tsqrt \
	tconst_log2 tconst_pi tconst_euler trandom ttrunc trint tfrac tmodf \
	texp texp2 texp10 texpm1 tlog tlog2 tlog10 tlog1p tpow tui_pow tpow3 \
	tcosh tsinh ttanh tsinh_cosh tacosh tasinh tatanh thyperbolic tasin \
	tacos tcos tatan tsin ttan tsin_cos tagm thypot tfactorial tgamma \
	terf tcbrt tzeta mpf_compat mpfr_compat reuse tsqr tstrtofr tpow_z \
	tget_f tconst_catalan troot tsec tcsc tcot teint tcoth tcsch tsech \
	tstckintc tsubnormal tlngamma tlgamma tzeta_ui tget_ld_2exp \
	tget_set_d64 tj0 tj1 tjn ty0 ty1 tyn tremquo tfmod tl2b tli2 tprintf \
	tsprintf tfprintf trec_sqrt tpow_all

EXTRA_DIST = tgeneric.c tgeneric_ui.c mpf_compat.h inp_str.data tmul.dat

LDADD = libfrtests.la $(MPFR_LIBM) $(top_builddir)/libmpfr.la
INCLUDES = -I$(top_srcdir) -I$(top_builddir)

# LOADLIBES (documented in the "GNU make" manual and equivalent to LDLIBS)
# enables to compile a program foo.c in the test directory by simply doing
# "make foo".
LOADLIBES=$(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(top_builddir)/tests/.libs/libfrtests.a $(top_builddir)/.libs/libmpfr.a $(LIBS) $(MPFR_LIBM)

check_LTLIBRARIES = libfrtests.la
libfrtests_la_SOURCES = mpfr-test.h memory.c rnd_mode.c tests.c cmp_str.c

$(top_builddir)/libmpfr.la:
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) libmpfr.la

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = MPFR_QUIET=1
