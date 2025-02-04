      FUNCTION   GVH_COMPAR_LCT ( REC1, REC2 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine for sorting lct records.                        *
! *                                                                      *
! * ### 22-NOV-2005  GVH_COMPAR_LCT  v1.1 (c) L. Petrov  17-JAN-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
#ifdef GNU
      INTEGER*4  GVH_COMPAR_LCT 
#else
      INTEGER*2  GVH_COMPAR_LCT 
#endif
      TYPE     ( GVH_LCT__STRU ) :: REC1, REC2
!
      IF ( REC1%LCODE_I8 < REC2%LCODE_I8 ) THEN
           GVH_COMPAR_LCT = -1
         ELSE IF ( REC1%LCODE_I8 > REC2%LCODE_I8 ) THEN
           GVH_COMPAR_LCT =  1
         ELSE 
           GVH_COMPAR_LCT =  0
      END IF
      RETURN
      END  FUNCTION  GVH_COMPAR_LCT  !#!  
