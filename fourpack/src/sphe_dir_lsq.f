      SUBROUTINE SPHE_DIR_LSQ ( FSH, NP, LAT_VEC, LON_VEC, DAT_VEC, &
     &                          DEG, NORM, IPHS, SIG_CNS, SPH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_DIR_LSQ  expands a set of discrete data points into  *
! *   spherical harmonics using a least squares inversion. When there    *
! *   are more data points than spherical harmonic coefficients          *
! *   NP > DEG**2 the solution of the overdetermined system is           *
! *   determined by least squares.                                       *
! *                                                                      *
! *   To avoid singularities when the data matrix is too sparse,         *
! *   constraint with reciprocal weight SIG_CNS is imposed.              *
! *   If SIG_CNS == 0.0D0, no constraint is imposed.                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal *
! *                          arrays with intermediate results and their  *
! *                          status for possible re-use.                 *
! *      NP ( INTEGER*4 ) -- The number of points of the input function. *
! * LAT_VEC ( REAL*8    ) -- Array of latitudes of the points. Units:    *
! *                          radians. Dimension: NP.                     *
! * LON_VEC ( REAL*8    ) -- Array of longitudes of the points. Units:   *
! *                          radians. Dimension: NP.                     *
! * DAT_VEC ( REAL*8    ) -- The function that is to be expanded.        *
! *                          Dimension: NP.                              *
! *     DEG ( INTEGER*4 ) -- Maximum degree of the transform. Should not *
! *                          exceed MD.                                  *
! *    NORM ( INTEGER*4 ) -- Normalization to be used when calculating   *
! *                          Legendre functions                          *
! *                          1 -- "geodesy";                             *
! *                          2 -- Schmidt;                               *
! *                          3 -- unnormalized;                          *
! *                          4 -- orthonormalized;                       *
! *   IPHS ( INTEGER*4 ) -- Phase flag.                                  *
! *                          1: Do not include the Condon-Shortley phase *
! *                             factor of (-1)^m.                        *
! *                         -1: Apply the Condon-Shortley phase factor   *
! *                           of (-1)^m.                                 *
! * SIG_CNS ( REAL*8    ) -- Reciprocal weight for constraints.          *
! *                          This weight is scaled by an average         *
! *                          value of the function.                      *
! *                                     
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SPH ( REAL*8    ) -- Array with spherical transform coefficients.   *
! *                       Dimension: (2,0:MD,0:MD). The first dimesion   *
! *                       runs over cosine/sine compoenent, the second   *
! *                       dimension runs over order l, the third         *
! *                       dimension runs over degre.                     *
! *                       NB: only coefficients l =< m are filled!       *
! *                       The part of array SPH l > m is filled with     *
! *                       zeroes.                                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 2004-2012, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ## 21-JU:-2012 SPHE_DIR_LSQ v1.0 modified by L. Petrov 22-AUG-2012 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  NP, DEG, NORM, IPHS, IUER
      REAL*8     SIG_CNS
      REAL*8     LAT_VEC(NP), LON_VEC(NP), DAT_VEC(NP), SPH(2,0:DEG,0:DEG)
      REAL*8     RC, DAT_AVR, LON_LAST, DIA_AVR
      CHARACTER  STR*128, STR1*128
      REAL*8,    ALLOCATABLE :: GG(:,:), NOR_MAT(:), NOR_VEC(:), MATO(:), &
     &                          EST(:), LON_CO(:), LON_SI(:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           NBLO, KBLO, KP, MPAR, MPA2, LBLO, IND, L, M, ND, IER 
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6811, IUER, 'SPHE_DIR_LSQ', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
!
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6812, IUER, 'SPHE_DIR_LSQ', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
!
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6813, IUER, 'SPHE_DIR_LSQ', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6814, IUER, 'SPHE_DIR_LSQ', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6815, IUER, 'SPHE_DIR_LSQ', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6816, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to precompute multiplicative factors used '// &
     &         ' in recursion relationships F1 and F2' )
           RETURN 
      END IF
!
      NBLO = MIN ( NP, 8192 )  ! Block size
      KBLO = NP/NBLO           ! Number of blocks
      IF ( KBLO*NBLO < NP ) KBLO = KBLO + 1
!
      MPAR = (DEG+1)**2
      MPA2 = (MPAR*(MPAR+1))/2
!
      ALLOCATE ( GG(MPAR,NBLO), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MPAR*NBLO, STR )
           CALL ERR_LOG ( 6817, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array GG' ) 
           RETURN 
      END IF
      ALLOCATE ( NOR_MAT(MPA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MPA2, STR )
           CALL ERR_LOG ( 6818, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array NOR_MAT' ) 
           RETURN 
      END IF
      ALLOCATE ( NOR_VEC(MPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MPAR, STR )
           CALL ERR_LOG ( 6819, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array NOR_VEC' ) 
           RETURN 
      END IF
      ALLOCATE ( LON_CO(0:DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*(DEG+1), STR )
           CALL ERR_LOG ( 6820, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LON_CO' ) 
           RETURN 
      END IF
      ALLOCATE ( LON_SI(0:DEG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*(DEG+1), STR )
           CALL ERR_LOG ( 6821, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array LON_SI' ) 
           RETURN 
      END IF
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DAT_AVR = 0.0D0
!
      KP = 0
      LON_LAST = -1.D30
!
      DO 410 J1=1,KBLO
         LBLO = 0
         DO 420 J2=1,NBLO
            KP = KP + 1
            IF ( KP > NP ) GOTO 420
!
            CALL ERR_PASS ( IUER, IER ) 
            CALL SPHE_LEG ( FSH, DEG, NORM, IPHS, LAT_VEC(KP), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6822, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &               'an attempt to precompute Legendre polynomials' )
                 RETURN 
            END IF
!		
	    IND = 0
            IF ( DABS(LON_VEC(KP) - LON_LAST) < FSH__ANG_EPS ) THEN
!
! -------------- The same longitude as in the last time
!
                 CONTINUE 
               ELSE 
!
! -------------- Precompute trigonometric function of longutude
!
                 LON_CO(0) = 1.0D0
                 LON_SI(0) = 0.0D0
                 LON_CO(1) = COS(LON_VEC(KP))
                 LON_SI(1) = SIN(LON_VEC(KP))
                 DO 430 J3=2,DEG
                    LON_CO(J3) = LON_CO(J3-1)*LON_CO(1) - LON_SI(J3-1)*LON_SI(1)
                    LON_SI(J3) = LON_SI(J3-1)*LON_CO(1) + LON_CO(J3-1)*LON_SI(1)
 430             CONTINUE 
                 LON_LAST = LON_VEC(KP)
            END IF
!		
            DO 440 J4=0,DEG ! L
!
! ------------ Do cos terms
!
               DO 450 J5=0,J4 ! M
                  IND = IND + 1
                 GG(IND,J2) = FSH%PL(J4,J5) * LON_CO(J5)
 450           CONTINUE 
!
! ------------ Do sin terms
!
               DO 460 J6=1,J4 ! M
                  IND = IND + 1
                  GG(IND,J2) = FSH%PL(J4,J6)* LON_SI(J6)
 460           CONTINUE 
 440        CONTINUE 
!				
            DO 470 J7=1,MPAR
               NOR_VEC(J7) = NOR_VEC(J7) + GG(J7,J2)*DAT_VEC(KP)
 470        CONTINUE 
            LBLO = LBLO + 1
            DAT_AVR = DAT_AVR + DAT_VEC(KP)
 420     CONTINUE 
!
! ------ Update of normal matrix
!
         IER = -8 ! Imortant! This kludge tells to *update* not compute NOR_MAT
         CALL MUL_MM_IT_S ( MPAR, LBLO, GG, MPAR, LBLO, GG, MPAR, NOR_MAT, IER )
 410  CONTINUE 
!
      DAT_AVR = DAT_AVR/NP
      IF ( SIG_CNS > 0.0D0 ) THEN
!
! -------- Compute the average diagonal element among the non-zero elements 
!
           DIA_AVR = 0.0D0
           IND = 1
           ND = 0
           DO 480 J8=1,MPAR
              IF ( NOR_MAT(IND) > SIG_CNS*DAT_AVR ) THEN
                   DIA_AVR = DIA_AVR + NOR_MAT(IND) 
                   ND = ND + 1
              END IF
              IND = IND + J8 + 1
 480       CONTINUE 
           IF ( ND > 0 ) THEN
                DIA_AVR = DIA_AVR/ND
           END IF
!
           IND = 1
           KP = 0
!
! -------- Impose constraints
!
           DO 490 J9=0,DEG
              DO 4100 J10=0,J9
                 KP = KP + 1
                 NOR_MAT(IND) = NOR_MAT(IND) + DIA_AVR*SIG_CNS**2
                 IND = IND + KP + 1
 4100          CONTINUE 
              DO 4110 J11=1,J9
                 KP = KP + 1
                 NOR_MAT(IND) = NOR_MAT(IND) + DIA_AVR*SIG_CNS**2
                 IND = IND + KP + 1
 4110         CONTINUE 
 490       CONTINUE 
      END IF
      DEALLOCATE ( GG )
      DEALLOCATE ( LON_CO )
      DEALLOCATE ( LON_SI )
!
      ALLOCATE   ( EST(MPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MPAR, STR )
           CALL ERR_LOG ( 6823, IUER, 'SPHE_DIR_LSQ', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array EST' ) 
           RETURN 
      END IF
!
! --- Normal matrix inversion 
!
      CALL ERR_PASS( IUER, IER )
      CALL INVS ( MPAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 )  THEN
           CALL ERR_LOG ( 6824, IUER, 'SPHE_DIR_LSQ', 'Failure to '// &
     &         'invert mormal matrix' )
           RETURN 
      END IF     
!@  write ( 6 ,* ) ' dat_avr= ', dat_avr, ' rc= ', rc  ! %%%%%%%%%%
!     
! --- Find vector of parameter estimates 
!
      IER=-1
      CALL MUL_MV_SV_V ( MPAR, NOR_MAT, MPAR, NOR_VEC, MPAR, EST, IER )
!
! --- Spread the vector of parameter estimates
!
      SPH = 0.0D0
      KP = 0
      DO 4120 J12=0,DEG
         DO 4130 J13=0,J12
            KP = KP + 1
            SPH(1,J12,J13) = EST(KP)
 4130     CONTINUE 
         DO 4140 J14=1,J12
            KP = KP + 1
            SPH(2,J12,J14) = EST(KP)
 4140    CONTINUE 
 4120 CONTINUE 
      DEALLOCATE ( EST,     STAT=IER )
      DEALLOCATE ( NOR_MAT, STAT=IER )
      DEALLOCATE ( NOR_VEC, STAT=IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_DIR_LSQ  !#!#
