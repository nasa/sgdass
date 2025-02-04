      SUBROUTINE REPA_PLOT_SCALE ( N_POI, ARG, VAL, ERR, ARG_SCL, VAL_SCL )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine REPA_PLOT_SCALE scales arrays ARG, VAL, ERR,    *
! *   with ARG_SCL and VAL_SCL                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   N_POI ( INTEGER*4 ) -- The number of points.                       *
! * ARG_SCL ( REAL*8    ) -- Scale for arguments.                        *
! * ARG_VAL ( REAL*8    ) -- Scale for values.                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     ARG ( REAL*8    ) -- Array of arguments. Dimenstion: N_POI.      *
! *     VAL ( REAL*8    ) -- Array of value.  Dimenstion: N_POI.         *
! *     ERR ( REAL*8    ) -- Array of errors. Dimenstion: N_POI.         *
! *                                                                      *
! * ### 03-DEC-2004  REPA_PLOT_SCALE v1.0 (c) L. Petrov  03-DEC-2004 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N_POI
      REAL*8     ARG(N_POI), VAL(N_POI), ERR(N_POI), ARG_SCL, VAL_SCL
      INTEGER*4  J1
!
      IF ( N_POI .LT. 1 ) RETURN 
      DO 410 J1=1,N_POI
         ARG(J1) = ARG(J1)*ARG_SCL
         VAL(J1) = VAL(J1)*VAL_SCL
         ERR(J1) = ERR(J1)*VAL_SCL
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  REPA_PLOT_SCALE 
