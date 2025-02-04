#######################################################################
#								      #
#	Make file for recompiling library  matvec.a     	      #
#						     		      #
#  This make file consists of 4 parts:                                #
#  1) OBJ            --                                               #
#  2) GEN            -- generic (very slow) version of some           #
#                       vector-matrix and matrix-matrix routines.     #
#  3) OPT_OBJ        -- block-optimized version of some               #
#                       vector-matrix and matrix-matrix routines.     #
#  4) OPT_OBJ_INLINE -- these are inline code versino of some         #
#                       algebraic code. NB: object files of these     #
#                       routines they are not removed with            #
#                       "make clean", but only with "make distclean"  #
#						     		      #
#						     		      #
#  %%%  27-DEC-96   makefile   v2.7  (c)  L. Petrov  19-OCT-2017 %%%  #
#								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.o	.gen_o	.opt_o	.Opt_o	.c	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

LIB     = matvec.a
NUL     = /null/dev
EXE     = $(MK5_ROOT)/bin

.c.o:
	$(MK5_C) $(MK5_C_OPENMP) -c -o $*.o $*.c

.f.o:
	$(MK5_F95) $(MK5_F_OPENMP) -c -o $*.o $*.f

.f.gen_o:
	$(MK5_F95) -O0 -c -D GEN_PREFIX -o $*.gen_o $*.f

.f.opt_o:
	$(MK5_F95_NOOPT) $(MK5_F_OPENMP) -c -o $*.opt_o $*.f
#	$(MK5_F95_OPT) $(MK5_F_OPENMP) -c -o $*.opt_o $*.f

.f.Opt_o:
	$(MK5_F95_OPTEST) $(MK5_F_OPENMP) -c -o $*.Opt_o $*.f

.f.d:
	$(MK5_F95) $(MK5_F_OPENMP) -c -o $*.o $*.f


OBJ	=			\
	addrct.Opt_o 		\
	add_vv.Opt_o		\
	addc_vv.Opt_o		\
	add_vv_v.Opt_o		\
	cheb.Opt_o		\
        dchole_s.Opt_o          \
	dgather.Opt_o		\
	dlauum.Opt_o            \
	dlauu2.Opt_o  		\
	dmach.Opt_o 		\
	dppco.Opt_o 		\
	dppco_blas.Opt_o 	\
	dppco_solve.Opt_o 	\
	dppco_solve_blas.Opt_o 	\
	dppco_solve_vec.Opt_o 	\
	dppco_vec.Opt_o 	\
        dppfa.Opt_o		\
        dppfa_blas.Opt_o	\
        dppfa_solve_blas.Opt_o	\
        dppfa_solve_mes.Opt_o 	\
        dppfa_solve_vec.Opt_o	\
        dppfa_vec.Opt_o 	\
	dppin.Opt_o 		\
	dppin_blas.Opt_o 	\
	dppin_vec.Opt_o 	\
	dppsl.Opt_o 		\
	dpotri.Opt_o 		\
	dpotrf.Opt_o 		\
	dtrtri.Opt_o 		\
	dtrti2.Opt_o 		\
	dpotf2.Opt_o 		\
	dscatter.Opt_o		\
	dxmpy.Opt_o 		\
	emvs.Opt_o		\
	ieeeck.Opt_o 		\
	ilaenv.Opt_o 		\
	invsl.Opt_o 		\
	matspavec_fast.Opt_o 	\
	mat_sumtra.Opt_o	\
	mul_vc_v.Opt_o		\
	norvec_upd.Opt_o 	\
	rct_gather.Opt_o 	\
	rct_scatter.Opt_o 	\
	sparse_dot.Opt_o 	\
	spavec.Opt_o 		\
	sub_vv.Opt_o		\
	sub_vv_v.Opt_o		\
	trg_add_sct.Opt_o	\
	trg_gather.Opt_o	\
	trg_scatter.Opt_o	\
	vec.Opt_o		\
	vec8.Opt_o

GEN_OBJ	=			\
	dp_vv_v.gen_o		\
	diad_cvt.gen_o		\
	invsf.gen_o 		\
	mul_mm_ii_i.gen_o	\
	mul_mm_ii_s.gen_o	\
	mul_mm_is_i.gen_o	\
	mul_mm_it_i.gen_o	\
	mul_mm_it_s.gen_o	\
	mul_mm_si_i.gen_o	\
	mul_mm_ss_i.gen_o	\
	mul_mm_ss_s.gen_o	\
	mul_mm_st_i.gen_o	\
	mul_mm_ti_i.gen_o	\
	mul_mm_ti_s.gen_o	\
	mul_mm_ts_i.gen_o	\
	mul_mm_tt_i.gen_o	\
	mul_mv_iv_v.gen_o	\
	mul_mv_sv_v.gen_o	\
	mul_mv_tv_v.gen_o

OPT_OBJ	=			\
	dp_vv_v.Opt_o		\
	diad_cvt.Opt_o 		\
	invs.Opt_o 		\
	invs_no_cond.Opt_o 	\
	invs_recursive.Opt_o 	\
	invsf.Opt_o		\
	invsp.Opt_o		\
	mul_mm_ii_i.Opt_o	\
	mul_mm_ii_s.Opt_o	\
	mul_mm_is_i.Opt_o	\
	mul_mm_it_i.Opt_o	\
	mul_mm_it_s.Opt_o	\
	mul_mm_si_i.Opt_o	\
	mul_mm_ss_i.Opt_o	\
	mul_mm_ss_s.Opt_o	\
	mul_mm_st_i.Opt_o 	\
	mul_mm_ti_s.Opt_o	\
	mul_mm_ts_i.Opt_o	\
	mul_mm_ti_i.Opt_o	\
	mul_mm_tt_i.Opt_o	\
	mul_mv_iv_v.Opt_o	\
	mul_mv_sv_v.Opt_o	\
	mul_mv_tv_v.Opt_o

OPT_OBJ_INLINE	=		\
	addtrg.Opt_o 		\
	dpptrf_3.Opt_o		\
	dpptrf_4.Opt_o		\
	dpptri_3.Opt_o 		\
	dpptri_4.Opt_o 		\
	dpptrk_3.Opt_o 		\
	dpptrk_4.Opt_o 		\
	emvs_3.Opt_o 		\
	emvs_4.Opt_o 		\
	emvs_5.Opt_o 		\
	emvs_6.Opt_o 		\
	emvs_7.Opt_o 		\
	emvs_8.Opt_o 		\
	emvs_9.Opt_o 		\
	emvs_10.Opt_o 		\
	emvs_11.Opt_o 		\
	emvs_12.Opt_o 		\
	emvs_13.Opt_o 		\
	emvs_14.Opt_o 		\
	emvs_15.Opt_o 		\
	emvs_16.Opt_o 		\
	emvs_17.Opt_o 		\
	emvs_18.Opt_o 		\
	emvs_19.Opt_o 		\
	emvs_20.Opt_o 		\
	emvs_21.Opt_o 		\
	emvs_22.Opt_o 		\
	emvs_23.Opt_o 		\
	emvs_24.Opt_o 		\
	emvs_25.Opt_o 		\
	emvs_26.Opt_o 		\
	emvs_27.Opt_o 		\
	emvs_28.Opt_o 		\
	emvs_29.Opt_o 		\
	emvs_30.Opt_o 		\
	emvs_31.Opt_o 		\
	emvs_32.Opt_o 		\
	invs_3.Opt_o 		\
	invs_4.Opt_o 		\
	invs_5.Opt_o 		\
	invs_6.Opt_o 		\
	invs_7.Opt_o 		\
	invs_8.Opt_o 		\
	invs_9.Opt_o 		\
	invs_10.Opt_o 		\
	invs_11.Opt_o 		\
	invs_12.Opt_o 		\
	invs_13.Opt_o 		\
	invs_14.Opt_o 		\
	invs_15.Opt_o 		\
	invs_16.Opt_o 		\
	invs_17.Opt_o 		\
	invs_18.Opt_o 		\
	invs_19.Opt_o 		\
	invs_20.Opt_o 		\
	invs_21.Opt_o 		\
	invs_22.Opt_o 		\
	invs_23.Opt_o 		\
	invs_24.Opt_o 		\
	invs_25.Opt_o 		\
	invs_26.Opt_o 		\
	invs_27.Opt_o 		\
	invs_28.Opt_o 		\
	invs_29.Opt_o 		\
	invs_30.Opt_o 		\
	invs_31.Opt_o 		\
	invs_32.Opt_o 		\
	mul_mm_ss_i_10.Opt_o	\
	mul_mm_ss_i_11.Opt_o	\
	mul_mm_ss_i_12.Opt_o	\
	mul_mm_ss_i_13.Opt_o	\
	mul_mm_ss_i_14.Opt_o	\
	mul_mm_ss_i_15.Opt_o	\
	mul_mm_ss_i_16.Opt_o	\
	mul_mm_ss_i_5.Opt_o	\
	mul_mm_ss_i_6.Opt_o	\
	mul_mm_ss_i_7.Opt_o	\
	mul_mm_ss_i_8.Opt_o	\
	mul_mm_ss_i_9.Opt_o	\
	mul_mm_ss_s_10.Opt_o	\
	mul_mm_ss_s_11.Opt_o	\
	mul_mm_ss_s_12.Opt_o	\
	mul_mm_ss_s_13.Opt_o	\
	mul_mm_ss_s_14.Opt_o	\
	mul_mm_ss_s_15.Opt_o	\
	mul_mm_ss_s_16.Opt_o	\
	mul_mm_ss_s_5.Opt_o	\
	mul_mm_ss_s_6.Opt_o	\
	mul_mm_ss_s_7.Opt_o	\
	mul_mm_ss_s_8.Opt_o	\
	mul_mm_ss_s_9.Opt_o	\
	syrk3_1.Opt_o 		\
	syrk3_2.Opt_o 		\
	syrk3_3.Opt_o 		\
	syrk3_4.Opt_o 		\
	syrk4_1.Opt_o 		\
	syrk4_2.Opt_o 		\
	syrk4_3.Opt_o 		\
	syrk4_4.Opt_o 		\
	trmm7_1.Opt_o 		\
	trmm7_2.Opt_o 		\
	trmm7_3.Opt_o 		\
	trmm7_4.Opt_o 		\
	trmm8_1.Opt_o 		\
	trmm8_2.Opt_o 		\
	trmm8_3.Opt_o 		\
	trmm8_4.Opt_o 		\
	trsm3_1.Opt_o 		\
	trsm3_2.Opt_o 		\
	trsm3_3.Opt_o 		\
	trsm3_4.Opt_o 		\
	trsm4_1.Opt_o 		\
	trsm4_2.Opt_o 		\
	trsm4_3.Opt_o 		\
	trsm4_4.Opt_o


MOD	= module_matvec.d

all:	mod lib

mod:	$(MOD)

lib:	$(OBJ)         $(GEN_OBJ) $(OPT_OBJ) $(OPT_OBJ_INLINE) $(OBJ)
	ar  r  $(LIB)  $(GEN_OBJ) $(OPT_OBJ) $(OPT_OBJ_INLINE) $(OBJ)
	@echo '--- Library $(LIB) updated ---'

clean:
	rm -f $(OBJ)
	rm -f $(GEN_OBJ)
	rm -f $(OPT_OBJ)
	rm -f $(LIB)
	rm -f $(MOD)
	if ( -f module_matvec.o ) rm -f module_matvec.o
	if ( -f MATVEC.mod ) rm -f *.mod

distclean:
	rm -f $(OBJ)
	rm -f $(GEN_OBJ)
	rm -f $(OPT_OBJ)
	rm -f $(OPT_OBJ_INLINE)
	rm -f $(LIB)
	rm -f $(MOD)
	if ( -f module_matvec.o ) rm -f module_matvec.o
	if ( -f MATVEC.mod ) rm -f *.mod
