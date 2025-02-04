      FUNCTION   PIMA_COMPAR_STA ( STA1, STA2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_STA is used for comparison of       *
! *   two station data structures for sorting.                           *
! *                                                                      *
! * ## 07-JAN-2006  PIMA_COMPAR_STA  v1.0 (c)  L. Petrov  07-JAN-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIM_STA__TYPE ) :: STA1, STA2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_STA
#else
      INTEGER*2  PIMA_COMPAR_STA
#endif
!
      IF ( STA1%IVS_NAME > STA2%IVS_NAME ) THEN
           PIMA_COMPAR_STA =  1
         ELSE IF ( STA1%IVS_NAME < STA2%IVS_NAME ) THEN
           PIMA_COMPAR_STA = -1
         ELSE
           PIMA_COMPAR_STA =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_STA  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_SOU ( SOU1, SOU2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_SOU is used for comparison of       *
! *   two source data structures for sorting.                            *
! *                                                                      *
! * ### 07-JAN-2006  PIMA_COMPAR_SOU  v1.0 (c) L. Petrov 07-JAN-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIM_SOU__TYPE ) :: SOU1, SOU2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_SOU
#else
      INTEGER*2  PIMA_COMPAR_SOU
#endif
!
      IF ( SOU1%ALPHA > SOU2%ALPHA ) THEN
           PIMA_COMPAR_SOU =  1
         ELSE IF ( SOU1%ALPHA < SOU2%ALPHA ) THEN
           PIMA_COMPAR_SOU = -1
         ELSE
           PIMA_COMPAR_SOU =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_SOU  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_FRQ ( FRQ1, FRQ2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_FRQ is used for comparison of       *
! *   two station data structures for sorting.                           *
! *                                                                      *
! * ## 14-JAN-2006  PIMA_COMPAR_FRQ  v1.0 (c)  L. Petrov  14-JAN-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIM_FRQ__TYPE ) :: FRQ1, FRQ2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_FRQ
#else
      INTEGER*2  PIMA_COMPAR_FRQ
#endif
!
      IF ( FRQ1%FREQ_I8 > FRQ2%FREQ_I8 ) THEN
           PIMA_COMPAR_FRQ =  1
         ELSE IF ( FRQ1%FREQ_I8 < FRQ2%FREQ_I8 ) THEN
           PIMA_COMPAR_FRQ = -1
         ELSE
           PIMA_COMPAR_FRQ =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_FRQ  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_FRG ( FRG1, FRG2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_FRQ is used for comparison of       *
! *   two arrays frequency structures for sorting.                       *
! *                                                                      *
! * ## 14-JAN-2006  PIMA_COMPAR_FRG  v1.0 (c)  L. Petrov  14-JAN-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      REAL*8     FRG1, FRG2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_FRG
#else
      INTEGER*2  PIMA_COMPAR_FRG
#endif
!
      IF ( FRG1 > FRG2 ) THEN
           PIMA_COMPAR_FRG =  1
         ELSE IF ( FRG1 <  FRG2 ) THEN
           PIMA_COMPAR_FRG = -1
         ELSE
           PIMA_COMPAR_FRG =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_FRG  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_SCA ( SCA1, SCA2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_SCA is used for comparison of       *
! *   two scan data structures for sorting.                              *
! *                                                                      *
! * ### 08-JAN-2006  PIMA_COMPAR_SCA  v1.1 (c) L. Petrov 23-FEB-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIM_SCA__TYPE ) :: SCA1, SCA2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_SCA
#else
      INTEGER*2  PIMA_COMPAR_SCA
#endif
!
      IF ( SCA1%TIM_IND > SCA2%TIM_IND ) THEN
           PIMA_COMPAR_SCA =  1
         ELSE IF ( SCA1%TIM_IND < SCA2%TIM_IND ) THEN
           PIMA_COMPAR_SCA = -1
         ELSE
           IF ( SCA1%NUM_EPC < SCA2%NUM_EPC ) THEN
                PIMA_COMPAR_SCA =  1
              ELSE IF ( SCA1%NUM_EPC > SCA2%NUM_EPC ) THEN
                PIMA_COMPAR_SCA = -1
              ELSE
                IF ( SCA1%IND_ROOT > SCA2%IND_ROOT ) THEN
                     PIMA_COMPAR_SCA =  1
                   ELSE IF ( SCA1%IND_ROOT < SCA2%IND_ROOT ) THEN
                     PIMA_COMPAR_SCA = -1
                   ELSE
                     IF ( SCA1%SOU_IND > SCA2%SOU_IND ) THEN
                          PIMA_COMPAR_SCA =  1
                        ELSE IF ( SCA1%SOU_IND < SCA2%SOU_IND ) THEN
                          PIMA_COMPAR_SCA = -1
                        ELSE 
                          PIMA_COMPAR_SCA =  0
                     END IF
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_SCA  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_UV_IND ( UV_IND1, UV_IND2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_UV_IND is used for comparison of    *
! *   two uv data structures for sorting.                                *
! *                                                                      *
! * ## 09-JAN-2006 PIMA_COMPAR_UV_IND v1.2 (c) L. Petrov 06-NOV-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIM_UVIND__TYPE ) :: UV_IND1, UV_IND2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_UV_IND
#else
      INTEGER*2  PIMA_COMPAR_UV_IND
#endif
!
      IF ( UV_IND1%TIM_IND > UV_IND2%TIM_IND ) THEN
           PIMA_COMPAR_UV_IND =  1
         ELSE IF ( UV_IND1%TIM_IND < UV_IND2%TIM_IND ) THEN
           PIMA_COMPAR_UV_IND = -1
         ELSE
           IF ( UV_IND1%SOU_IND > UV_IND2%SOU_IND ) THEN
                PIMA_COMPAR_UV_IND =  1
              ELSE IF ( UV_IND1%SOU_IND < UV_IND2%SOU_IND ) THEN
                PIMA_COMPAR_UV_IND = -1
              ELSE
                IF ( UV_IND1%STA_IND(1) > UV_IND2%STA_IND(1) ) THEN
                     PIMA_COMPAR_UV_IND =  1
                  ELSE IF ( UV_IND1%STA_IND(1) < UV_IND2%STA_IND(1) ) THEN
                     PIMA_COMPAR_UV_IND = -1
                  ELSE
                     IF ( UV_IND1%STA_IND(2) > UV_IND2%STA_IND(2) ) THEN
                          PIMA_COMPAR_UV_IND =  1
                        ELSE IF ( UV_IND1%STA_IND(2) < UV_IND2%STA_IND(2) ) THEN
                          PIMA_COMPAR_UV_IND = -1
                        ELSE
                          IF ( UV_IND1%FRG_IND > UV_IND2%FRG_IND ) THEN
                               PIMA_COMPAR_UV_IND =  1
                            ELSE IF ( UV_IND1%FRG_IND < UV_IND2%FRG_IND ) THEN
                               PIMA_COMPAR_UV_IND = -1
                            ELSE
                               PIMA_COMPAR_UV_IND =  0
                          END IF
                     END IF
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_UV_IND  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_NOISE ( AMP1, AMP2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_NOISE is used for comparison of     *
! *   two amplitudes data structures for sorting. NB: sorting is done    *
! *   in reverse order.                                                  *
! *                                                                      *
! * ## 23-JAN-2006  PIMA_COMPAR_NOISE  v1.1 (c) L. Petrov 09-DEC-2018 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      REAL*8     AMP1, AMP2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_NOISE
#else
      INTEGER*2  PIMA_COMPAR_NOISE
#endif
!
      IF ( AMP1 < AMP2 ) THEN
           PIMA_COMPAR_NOISE =  1
         ELSE IF ( AMP1 > AMP2 ) THEN
           PIMA_COMPAR_NOISE = -1
         ELSE
           PIMA_COMPAR_NOISE =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_NOISE  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_AMPL ( AMP1, AMP2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_AMPL is used for comparison of      *
! *   two amplitudes data structures for sorting. NB: sorting is done    *
! *   in reverse order.                                                  *
! *                                                                      *
! * ## 23-JAN-2006  PIMA_COMPAR_AMPL   v1.0 (c) L. Petrov 23-JAN-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_AMPL
#else
      INTEGER*2  PIMA_COMPAR_AMPL
#endif
      TYPE     ( PIM__AMPL_SEARCH ) :: AMP1, AMP2
!
      IF ( AMP1%AMPL < AMP2%AMPL ) THEN
           PIMA_COMPAR_AMPL =  1
         ELSE IF ( AMP1%AMPL > AMP2%AMPL ) THEN
           PIMA_COMPAR_AMPL = -1
         ELSE
           PIMA_COMPAR_AMPL =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_AMPL  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_UV ( UV1, UV2 )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_COMPAR_UV
! *                                                                      *
! * ### 02-APR-2006   PIMA_COMPAR_UV  v1.0 (c) L. Petrov 02-APR-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      COMPLEX*8  UV1, UV2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_UV
#else
      INTEGER*2  PIMA_COMPAR_UV
#endif
!
      IF ( ABS(UV1) > ABS(UV2) ) THEN
           PIMA_COMPAR_UV =  1
         ELSE IF ( ABS(UV1) < ABS(UV2) ) THEN
           PIMA_COMPAR_UV = -1
         ELSE
           PIMA_COMPAR_UV =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_UV  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_R8 ( VAL1_R8, VAL2_R8 )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_COMPAR_R8
! *                                                                      *
! * ### 30-MAY-2006   PIMA_COMPAR_R8  v1.0 (c) L. Petrov 30-MAY-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_R8
#else
      INTEGER*2  PIMA_COMPAR_R8
#endif
      REAL*8     VAL1_R8, VAL2_R8
!
      IF ( VAL1_R8 > VAL2_R8 ) THEN
           PIMA_COMPAR_R8 =  1
         ELSE IF ( VAL1_R8 < VAL2_R8 ) THEN
           PIMA_COMPAR_R8 = -1
         ELSE
           PIMA_COMPAR_R8 =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_R8  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_I8 ( VAL1_I8, VAL2_I8 )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_COMPAR_I8
! *                                                                      *
! * ### 30-MAY-2006   PIMA_COMPAR_I8  v1.0 (c) L. Petrov 30-MAY-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_I8
#else
      INTEGER*2  PIMA_COMPAR_I8
#endif
      INTEGER*8  VAL1_I8, VAL2_I8
!
      IF ( VAL1_I8 > VAL2_I8 ) THEN
           PIMA_COMPAR_I8 =  1
         ELSE IF ( VAL1_I8 < VAL2_I8 ) THEN
           PIMA_COMPAR_I8 = -1
         ELSE
           PIMA_COMPAR_I8 =  0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_I8  !#!#
!
! ------------------------------------------------------------------------
!
!
      FUNCTION   PIMA_COMPAR_MKDB_SCA ( SCA1, SCA2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_MKDB_SCA is used for comparison of  *
! *   arrays of observation index in the database scans.                 *
! *                                                                      *
! * # 04-JUL-2009 PIMA_COMPAR_MKDB_SCA  v1.0 (c) L. Petrov 04-JUL-2009 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      INTEGER*4  SCA1(PIM__MBAS), SCA2(PIM__MBAS)
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_MKDB_SCA
#else
      INTEGER*2  PIMA_COMPAR_MKDB_SCA
#endif
      INTEGER*4  OBS_MIN_IND_1, OBS_MIN_IND_2, J1
!
      OBS_MIN_IND_1 = PIM__MOBS + 1
      OBS_MIN_IND_2 = PIM__MOBS + 1
      DO 410 J1=1,PIM__MBAS
         IF ( SCA1(J1) == 0 ) GOTO 410
         IF ( SCA2(J1) == 0 ) GOTO 410
         OBS_MIN_IND_1 = MIN ( SCA1(J1), OBS_MIN_IND_1 )
         OBS_MIN_IND_2 = MIN ( SCA2(J1), OBS_MIN_IND_2 )
 410  CONTINUE
!
      IF ( OBS_MIN_IND_1 > OBS_MIN_IND_2 ) THEN
           PIMA_COMPAR_MKDB_SCA  =  1
         ELSE IF ( OBS_MIN_IND_1 < OBS_MIN_IND_2 ) THEN
           PIMA_COMPAR_MKDB_SCA = -1
         ELSE
           PIMA_COMPAR_MKDB_SCA = 0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_MKDB_SCA  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_MOD ( MOD1, MOD2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_MOD is used for comparison of       *
! *   arrays of model data structure. First comparison is made over the  *
! *   source name, second comparison is made over the time tag.          *
! *                                                                      *
! * ### 25-JUL-2009  PIMA_COMPAR_MOD  v1.0 (c) L. Petrov 25-JUL-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE ( PIM_MOD__TYPE ) MOD1, MOD2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_MOD
#else
      INTEGER*2  PIMA_COMPAR_MOD
#endif
!
      IF ( MOD1%SOU_IND > MOD2%SOU_IND ) THEN
           PIMA_COMPAR_MOD =  1
         ELSE IF ( MOD1%SOU_IND < MOD2%SOU_IND ) THEN
           PIMA_COMPAR_MOD = -1
         ELSE
           IF ( MOD1%TIM_BEG > MOD2%TIM_BEG ) THEN
                PIMA_COMPAR_MOD =  1
              ELSE IF ( MOD1%TIM_BEG < MOD2%TIM_BEG ) THEN
                PIMA_COMPAR_MOD = -1
              ELSE
                PIMA_COMPAR_MOD =  0
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_TABL ( TABL1, TABL2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_TABL is used for comparison of      *
! *   arrays of fits primary table model data structure. They are sorted *
! *   first over time, then over baaseline index.                        *
! *                                                                      *
! * ### 08-APR-2011  PIMA_COMPAR_TABL v1.0 (c) L. Petrov 08-APR-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( FITS_PRIM__STRU ) :: TABL1, TABL2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_TABL
#else
      INTEGER*2  PIMA_COMPAR_TABL
#endif
!
      IF ( TABL1%GRP_ARR(5) > TABL2%GRP_ARR(5) ) THEN
           PIMA_COMPAR_TABL =  1
         ELSE IF ( TABL1%GRP_ARR(5) < TABL2%GRP_ARR(5) ) THEN
           PIMA_COMPAR_TABL = -1
         ELSE
           IF ( TABL1%GRP_ARR(6) > TABL2%GRP_ARR(6) ) THEN
                PIMA_COMPAR_TABL =  1
              ELSE IF ( TABL1%GRP_ARR(6) < TABL2%GRP_ARR(6) ) THEN
                PIMA_COMPAR_TABL = -1
              ELSE
                IF ( TABL1%GRP_ARR(4) > TABL2%GRP_ARR(4) ) THEN
                     PIMA_COMPAR_TABL =  1
                   ELSE IF ( TABL1%GRP_ARR(4) < TABL2%GRP_ARR(4) ) THEN
                     PIMA_COMPAR_TABL = -1
                   ELSE
                     PIMA_COMPAR_TABL =  0
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_TABL  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_UVO ( UVO1, UVO2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_UVO is used for comparison of       *
! *   arrays of UVO objects. They are sorted first over time, then over  *
! *   the first staion index, then over the second station index.        *
! *                                                                      *
! * ### 19-MAY-2012  PIMA_COMPAR_UVO  v1.0 (c) L. Petrov 19-MAY-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( UVO__TYPE ) :: UVO1, UVO2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_UVO
#else
      INTEGER*2  PIMA_COMPAR_UVO
#endif
!
      IF ( UVO1%IND_SEG > UVO2%IND_SEG ) THEN
           PIMA_COMPAR_UVO =  1
         ELSE IF ( UVO1%IND_SEG < UVO2%IND_SEG ) THEN
           PIMA_COMPAR_UVO = -1
         ELSE
           IF ( UVO1%STA_IND(1) > UVO2%STA_IND(1) ) THEN
                PIMA_COMPAR_UVO =  1
              ELSE IF ( UVO1%STA_IND(1) < UVO2%STA_IND(1) ) THEN
                PIMA_COMPAR_UVO = -1
              ELSE
                IF ( UVO1%STA_IND(2) > UVO2%STA_IND(2) ) THEN
                     PIMA_COMPAR_UVO =  1
                   ELSE IF ( UVO1%STA_IND(2) > UVO2%STA_IND(2) ) THEN
                     PIMA_COMPAR_UVO = -1
                   ELSE
                     PIMA_COMPAR_UVO =  0
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_UVO  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_SFXC ( SFXC1, SFXC2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_SFXC is used for comparison of      *
! *   arrays of objects of type PIMA__SFXC_TYPE. They are sorted first   *
! *   over station name, time, then over time, then over source name.    *
! *                                                                      *
! * ### 16-SEP-2015 PIMA_COMPAR_SFXC  v1.0 (c) L. Petrov 16-SEP-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__SFXC_TYPE ) :: SFXC1, SFXC2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_SFXC
#else
      INTEGER*2  PIMA_COMPAR_SFXC
#endif
!
      IF ( SFXC1%STA_NAM > SFXC2%STA_NAM ) THEN
           PIMA_COMPAR_SFXC =  1
         ELSE IF ( SFXC1%STA_NAM < SFXC2%STA_NAM ) THEN
           PIMA_COMPAR_SFXC = -1
         ELSE
           IF ( SFXC1%SOU_NAM > SFXC2%SOU_NAM ) THEN
                PIMA_COMPAR_SFXC =  1
             ELSE IF ( SFXC1%SOU_NAM < SFXC2%SOU_NAM ) THEN
                PIMA_COMPAR_SFXC = -1
             ELSE
                IF ( SFXC1%MJD*86400.0D0 + SFXC1%TAI > SFXC2%MJD*86400.0D0 + SFXC2%TAI ) THEN
                     PIMA_COMPAR_SFXC =  1
                   ELSE IF ( SFXC1%MJD*86400.0D0 + SFXC1%TAI < SFXC2%MJD*86400.0D0 + SFXC2%TAI ) THEN
                     PIMA_COMPAR_SFXC = -1
                   ELSE
                     IF ( SFXC1%FIL_DEL_IND > SFXC2%FIL_DEL_IND ) THEN
                          PIMA_COMPAR_SFXC =  1
                       ELSE IF ( SFXC1%FIL_DEL_IND < SFXC2%FIL_DEL_IND ) THEN
                          PIMA_COMPAR_SFXC = -1
                       ELSE
                          PIMA_COMPAR_SFXC =  0
                     END IF
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_SFXC  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_COMPAR_KJCC ( KJCC1, KJCC2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_COMPAR_KJCC is used for comparison of      *
! *   arrays of objects of type PIMA__KJCC_TYPE. They are sorted first   *
! *   over station name, time, then over time, then over source name.    *
! *                                                                      *
! * ### 13-DEC-2015 PIMA_COMPAR_KJCC  v1.0 (c) L. Petrov 13-DEC-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__KJCC_TYPE ) :: KJCC1, KJCC2
#ifdef GNU
      INTEGER*4  PIMA_COMPAR_KJCC
#else
      INTEGER*2  PIMA_COMPAR_KJCC
#endif
!
      IF ( KJCC1%STA_NAM > KJCC2%STA_NAM ) THEN
           PIMA_COMPAR_KJCC =  1
         ELSE IF ( KJCC1%STA_NAM < KJCC2%STA_NAM ) THEN
           PIMA_COMPAR_KJCC = -1
         ELSE
           IF ( KJCC1%SOU_NAM > KJCC2%SOU_NAM ) THEN
                PIMA_COMPAR_KJCC =  1
             ELSE IF ( KJCC1%SOU_NAM < KJCC2%SOU_NAM ) THEN
                PIMA_COMPAR_KJCC = -1
             ELSE
                IF ( KJCC1%MJD*86400.0D0 + KJCC1%TAI > KJCC2%MJD*86400.0D0 + KJCC2%TAI ) THEN
                     PIMA_COMPAR_KJCC =  1
                   ELSE IF ( KJCC1%MJD*86400.0D0 + KJCC1%TAI < KJCC2%MJD*86400.0D0 + KJCC2%TAI ) THEN
                     PIMA_COMPAR_KJCC = -1
                   ELSE
                     PIMA_COMPAR_KJCC =  0
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  PIMA_COMPAR_KJCC  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   MAX_I4 ( N, ARR_I4 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary function MAX_I4 finds the maximum value of the array.   *
! *                                                                      *
! *  ### 18-AUG-2011    MAX_I4     v1.0 (c)  L. Petrov  18-AUG-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MAX_I4
      INTEGER*4  N, ARR_I4(N)
      INTEGER*4  J1
!
      MAX_I4 = ARR_I4(1)
      DO 410 J1=1,N
         IF ( ARR_I4(J1) > MAX_I4 ) MAX_I4 = ARR_I4(J1)
 410  CONTINUE 
      RETURN
      END  !#!  
