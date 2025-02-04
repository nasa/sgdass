      SUBROUTINE PIMA_WVR_USE ( PIM, LCHN, LFRQ, LTIM, IND_OBS, UV )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_WVR_USE modifies UV data by subtracting the phase     *
! *   of path delay in tghe atmosphere measured by the Water Vapor       *
! *   Radiometer(s).                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     PIM ( PIMA__TYP ) -- Object with information related to program  *
! *                          PIMA.                                       *
! *    LCHN ( INTEGER*4 ) -- The number of frquency channels within      *
! *                          an IF.                                      * 
! *    LFRQ ( INTEGER*4 ) -- The number of IFs.                          *
! *    LTIM ( INTEGER*4 ) -- The number of accumulation periods in the   *
! *                          observation.                                *
! * IND_OBS ( INTEGER*4 ) -- index of the observation.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      UV ( COMPLEX*8 ) -- Array of visibiilities. Dimension:          *
! *                          LCHN, LFRQ, LTIM.                           *
! *                                                                      *
! *  ### 08-SEP-2015 PIMA_WVR_USE  v1.0 (c)  L. Petrov  08-SEP-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LCHN, LFRQ, LTIM, IND_OBS
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      CHARACTER  STR*128
      REAL*4     PHS
      INTEGER*4  J1, J2, J3
!
! --- Do not aopply WVR phases of PIMAVAER_WVR_PLOT_ONLY is set
!
      CALL GETENVAR ( 'PIMAVAR_WVR_PLOT_ONLY', STR )
      IF ( STR == 'YES' ) THEN
           RETURN 
      END IF
      DO 410 J1=1,LTIM
         DO 420 J2=1,LFRQ
            DO 430 J3=1,LCHN
               PHS = PI2*PIM%FREQ_ARR(J3,J2,PIM%CONF%FRQ_GRP)*PIM%OBS(IND_OBS)%WVR_DELAY(J1)
               UV(J3,J2,J1) = UV(J3,J2,J1)/CMPLX( COS(PHS), SIN(PHS) )
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE PIMA_WVR_USE  !#!#
