      SUBROUTINE COMP_IONO_SPL ( VIO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_IONO_SPL  computes the coefficients of the 3D spline  *
! *   that interpolates the ionosphere TEC maps for the entire selestial *
! *   sphere for the range of dates specified in internal fields of      *
! *   the data structure VIO.                                            *
! *                                                                      *
! *   It is assumed that the data with the ionosphere TEC maps have been *
! *   read from thedata file and loaded in VIO.                          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *                                                                      *
! *      VIO ( IONO__TYPE ) -- Data structure that keeps the data        *
! *                            related to ionosphere TEC maps.           *
! *                            It contains the header that describes     *
! *                            the dataset and the data.                 *
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
! * ### 12-MAY-2010  COMP_IONO_SPL  v1.0 (c) L. Petrov  12-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      TYPE     ( IONO__TYPE ) :: VIO
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  MEQ, MEQQ
      PARAMETER  ( MEQ = 3, MEQQ = (MEQ*(MEQ+1))/2 )
      REAL*8     NOR_MAT(MEQQ), NOR_VEC(MEQ), EQU_OBS(MEQ), EST(MEQ), RC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           DIMS(3), IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      IF ( VIO%STATUS_VAL .NE. VIO__READ ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*VIO%HEADER%NLON*VIO%HEADER%NLAT*VIO%HEADER%NEPC, STR )
           CALL ERR_LOG ( 4431, IUER, 'COMP_IONO_SPL', 'Trap of internal '// &
     &                   'control TEC values have not been read into '// &
     &                   'the data strucuture VIO' )
           RETURN 
      END IF 
!
      IF ( VIO%STATUS_SPL == VIO__ALLO .OR. &
     &     VIO%STATUS_SPL == VIO__READ .OR. &
     &     VIO%STATUS_SPL == VIO__COMP      ) THEN
           DEALLOCATE ( VIO%TEC_SPL )
      END IF
!
      ALLOCATE ( VIO%TEC_SPL(1-VIO__M_DEG:VIO%HEADER%NLON,1-VIO__M_DEG:VIO%HEADER%NLAT,1-VIO__M_DEG:VIO%HEADER%NEPC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*VIO%HEADER%NLON*VIO%HEADER%NLAT*VIO%HEADER%NEPC, STR )
           CALL ERR_LOG ( 4432, IUER, 'COMP_IONO_SPL', 'Failure to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                   'dynamic memory for TEC spline' )
           RETURN 
      END IF
      CALL NOUT_R4 ( (VIO%HEADER%NLON+VIO__M_DEG)*(VIO%HEADER%NLAT+VIO__M_DEG)*(VIO%HEADER%NEPC+VIO__M_DEG), &
     &               VIO%TEC_SPL )
!
! --- Copy values from TEC_VAL to TEC_SPL and apply scaling
!
      DO 410 J1=1,VIO%HEADER%NEPC
         DO 420 J2=1,VIO%HEADER%NLAT
            DO 430 J3=1,VIO%HEADER%NLON 
               IF ( VIO%TEC_VAL(J3,J2,J1) .EQ. VIONO__MISSING ) THEN
                    VIO%TEC_SPL(J3,J2,J1) = 0.0
                  ELSE
                    VIO%TEC_SPL(J3,J2,J1) = VIO%HEADER%SCALE* &
     &                                      VIO%TEC_VAL(J3,J2,J1) 
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( VIO%TEC_VAL(1,1,1) .EQ. VIONO__MISSING ) THEN
!
! -------- TEC values for the south pole were missing. We compute
! -------- them using quadratic interpolation near the pole
!
           DO 440 J4=1,VIO%HEADER%NEPC
!
! ----------- ... and we will do it for each epoch separately
!
              CALL NOUT_R8 ( MEQQ, NOR_MAT )
              CALL NOUT_R8 ( MEQ,  NOR_VEC )
!
! ----------- We use halp of merdians, since, moving south, aftect crossing the 
! ----------- south pole we continue the same meridian, but shifter at pi
!
              DO 450 J5=1,VIO%HEADER%NLON/2
!
! -------------- -2 nodes before the pole
!
                 EQU_OBS(1) =  ( 2.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  (-2.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J5,3,J4), EQU_OBS, &
     &                             NOR_VEC )
!
! -------------- -1 node before the pole
!
                 EQU_OBS(1) =  ( 1.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  (-1.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J5,2,J4), EQU_OBS, &
     &                             NOR_VEC )
!
! -------------- 1 node after teh pole
!
                 EQU_OBS(1) =  ( 1.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  ( 1.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J5+VIO%HEADER%NLON/2,2,J4), &
     &                             EQU_OBS, NOR_VEC )
!
! -------------- 2 nodes after the pole
!
                 EQU_OBS(1) =  ( 2.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  ( 2.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J5+VIO%HEADER%NLON/2,3,J4), &
     &                             EQU_OBS, NOR_VEC )
 450          CONTINUE 
!
! ----------- Invert the normal system...
!
              CALL ERR_PASS ( IUER, IER ) 
              CALL INVS ( MEQ, NOR_MAT, RC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4433, IUER, 'COMP_IONO_SPL', 'Failure '// &
     &                 'to invert normal matrix of equations for '// &
     &                 'interpolation of the TEC values ot the soutern '//&
     &                 'pole' )
                   RETURN 
              END IF
!
! ----------- ... and find the estimate. 
! ----------- The value of teh free term will give us the the bast value at the 
! ----------- the south pole
!
              CALL MUL_MV_SV_V ( MEQ, NOR_MAT, MEQ, NOR_VEC, MEQ, EST, IER )
              DO 460 J6=1,VIO%HEADER%NLON
                 VIO%TEC_SPL(J6,1,J4) = EST(3)
 460          CONTINUE 
 440       CONTINUE 
      END IF
!
      IF ( VIO%TEC_VAL(1,VIO%HEADER%NLAT,1) .EQ. VIONO__MISSING ) THEN
!
! -------- TEC values for the north pole were missing. We compute
! -------- them using quadratic interpolation near the pole
!
           DO 470 J7=1,VIO%HEADER%NEPC
!
! ----------- ... and we weill do it for each epoch separately
!
              CALL NOUT_R8 ( MEQQ, NOR_MAT )
              CALL NOUT_R8 ( MEQ,  NOR_VEC )
!
! ----------- We use halp of merdians, since, moving south, aftect crossing the 
! ----------- north pole we continue the same meridian, but shifter at pi
!
              DO 480 J8=1,VIO%HEADER%NLON/2
!
! -------------- -2 nodes before the pole
!
                 EQU_OBS(1) =  ( 2.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  (-2.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J8,VIO%HEADER%NLAT-2,J7), EQU_OBS, &
     &                             NOR_VEC )
!
! -------------- -1 node before the pole
!
                 EQU_OBS(1) =  ( 1.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  (-1.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J8,VIO%HEADER%NLAT-1,J7), EQU_OBS, &
     &                             NOR_VEC )
!
! -------------- 1 node after teh pole
!
                 EQU_OBS(1) =  ( 1.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  ( 1.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J8+VIO%HEADER%NLON/2,VIO%HEADER%NLAT-1,J7), &
     &                             EQU_OBS, NOR_VEC )
!
! -------------- 2 nodes after the pole
!
                 EQU_OBS(1) =  ( 2.0D0*VIO%HEADER%LAT_STEP)**2
                 EQU_OBS(2) =  ( 2.0D0*VIO%HEADER%LAT_STEP)
                 EQU_OBS(3) =  1.0D0
                 CALL DIAD_CVT_S ( 1.0D0, MEQ, EQU_OBS, EQU_OBS, NOR_MAT )
                 CALL NORVEC_UPD ( MEQ, 1.0D0, 1.0D0*VIO%TEC_SPL(J8+VIO%HEADER%NLON/2,VIO%HEADER%NLAT-2,J7), &
     &                             EQU_OBS, NOR_VEC )
 480          CONTINUE 
!
! ----------- Invert the normal system...
!
              CALL ERR_PASS ( IUER, IER ) 
              CALL INVS ( MEQ, NOR_MAT, RC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4433, IUER, 'COMP_IONO_SPL', 'Failure '// &
     &                 'to invert normal matrix of equations for '// &
     &                 'interpolation of the TEC values ot the soutern '//&
     &                 'pole' )
                   RETURN 
              END IF
!
! ----------- ... and find the estimate. 
! ----------- The value of teh free term will give us the the bast value at the 
! ----------- the north pole
!
              CALL MUL_MV_SV_V ( MEQ, NOR_MAT, MEQ, NOR_VEC, MEQ, EST, IER )
              DO 490 J9=1,VIO%HEADER%NLON
                 VIO%TEC_SPL(J9,VIO%HEADER%NLAT,J7) = EST(3)
 490          CONTINUE 
 470       CONTINUE 
      END IF
!
! --- Allocate memory for arrays with value of longitude, latitude and 
! --- time grids
!
      IF ( ASSOCIATED ( VIO%LON_VAL ) ) THEN
           DEALLOCATE ( VIO%LON_VAL )
      END IF
      ALLOCATE ( VIO%LON_VAL(VIO%HEADER%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*VIO%HEADER%NLON, STR )
           CALL ERR_LOG ( 4434, IUER, 'COMP_IONO_SPL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for fo array LON_VAL' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( VIO%LAT_VAL ) ) THEN
           DEALLOCATE ( VIO%LAT_VAL )
      END IF
      ALLOCATE ( VIO%LAT_VAL(VIO%HEADER%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*VIO%HEADER%NLAT, STR )
           CALL ERR_LOG ( 4435, IUER, 'COMP_IONO_SPL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for fo array LAT_VAL' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( VIO%TIM_VAL ) ) THEN
           DEALLOCATE ( VIO%TIM_VAL )
      END IF
      ALLOCATE ( VIO%TIM_VAL(VIO%HEADER%NEPC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*VIO%HEADER%NEPC, STR )
           CALL ERR_LOG ( 4436, IUER, 'COMP_IONO_SPL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for fo array TIM_VAL' )
           RETURN 
      END IF
!
! --- Fill axes
!
      DO 4100 J10=1,VIO%HEADER%NLON
         VIO%LON_VAL(J10) = VIO%HEADER%LON_MIN + (J10-1)*VIO%HEADER%LON_STEP
 4100 CONTINUE 
      DO 4110 J11=1,VIO%HEADER%NLAT
         VIO%LAT_VAL(J11) = VIO%HEADER%LAT_MIN + (J11-1)*VIO%HEADER%LAT_STEP
 4110 CONTINUE 
      DO 4120 J12=1,VIO%HEADER%NEPC
         VIO%TIM_VAL(J12) = (J12-1)
 4120 CONTINUE 
!
! --- Gather dimensions
!
      DIMS(1) = VIO%HEADER%NLON
      DIMS(2) = VIO%HEADER%NLAT
      DIMS(3) = VIO%HEADER%NEPC
!
! --- Compute coefficients of the 3D-spline that interpolates TEC
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL4_3D_CMP ( VIO__M_DEG, 10, DIMS, VIO%LON_VAL, VIO%LAT_VAL, &
     &                    VIO%TIM_VAL, VIO%TEC_SPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4437, IUER, 'COMP_IONO_SPL', 'Error in '// &
     &         'an attempt to computed the 3D spline for TEC maps' )
           RETURN 
      END IF
      VIO%STATUS_SPL = VIO__COMP
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_IONO_SPL  !#!#
