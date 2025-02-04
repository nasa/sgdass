      SUBROUTINE VTD_GET_APR_PRP ( VTD, L_PRP, C_PRP, RA_PRP, DEC_PRP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_GET_APR_PRP returns array of a priori proper motions   *
! *   defined in the VTD internal data structure.                        *
! *                                                                      *
! * ## 12-MAR-2023  VTD_GET_APR_PRP  v1.0 (c)  L. Petrov  14-MAR-2023 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'prfil.i'
      INCLUDE   'socom.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( VTD_SOU__TYPE      ), POINTER :: SOU(:)
      INTEGER*4  L_PRP, IUER
      CHARACTER  C_PRP(MAX_SOU)*(*)
      REAL*8     RA_PRP(MAX_SOU), DEC_PRP(MAX_SOU)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = VTD__M_SCC*VTD__M_SRC )
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, IOS, L_SOU, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      L_PRP = NUMSTR
      DO 410 J1=1,NUMSTR
         C_PRP(J1)   = ISTRN_CHR(J1)
         RA_PRP(J1)  = 0.0D0
         DEC_PRP(J1) = 0.0D0
 410  CONTINUE 
!
! --- Check whether the catalogue of source proper motions and parallaxes
! --- has been defined. If not, then quit.
!
      IF ( ILEN(VTD%CONF%FINAM_SOUPRL_PRP) == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Allocate dynamic memory for buffers
!
      ALLOCATE ( BUF(MBUF), SOU(VTD%L_SOU), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 256*MBUF + MAX_SOU*SIZEOF(SOU), STR )
           CALL ERR_LOG ( 3961, IUER, 'VTD_GET_APR_PRP', 'Error in an attempt '// &
     &         'to allocate '//TRIM(STR)//' bytes of dynamic memory' )
           RETURN
      END IF
!
      SOU(1:VTD%L_SOU) = VTD%SOU(1:VTD%L_SOU)
      L_SOU = VTD%L_SOU
      DO 420 J2=1,L_SOU
         CALL NOUT ( SIZEOF(VTD%SOU(1)), VTD%SOU(J2) )
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_LOAD_SOUCOO ( VTD, L_PRP, C_PRP, MBUF, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3963, IUER, 'VTD_GET_APR_PRP', 'Error in an attempt '// &
     &         'to load source coordinates from the input file '// &
     &          VTD%CONF%FINAM_SOUCOO )
!
           VTD%L_SOU = L_SOU 
           VTD%SOU(1:VTD%L_SOU) = SOU(1:VTD%L_SOU) 
           DEALLOCATE ( BUF, SOU )
           RETURN
      END IF
!
! --- Load source coordinates from the external file specifed in VTD%CONF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_LOAD_SOUPRL ( VTD, MBUF, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3964, IUER, 'VTD_GET_APR_PRP', 'Error in an attempt '// &
     &         'to load source coordinates from the input file '// &
     &          VTD%CONF%FINAM_SOUPRL_PRP )
           VTD%L_SOU = L_SOU 
           VTD%SOU(1:VTD%L_SOU) = SOU(1:VTD%L_SOU) 
           DEALLOCATE ( BUF, SOU )
           RETURN
      END IF
!
      DO 430 J3=1,L_PRP
         C_PRP(J3)   = VTD%SOU(J3)%IVS_NAME
         RA_PRP(J3)  = VTD%SOU(J3)%ALPHA_RATE
         DEC_PRP(J3) = VTD%SOU(J3)%DELTA_RATE
 430  CONTINUE 
!
      VTD%L_SOU = L_SOU 
      VTD%SOU(1:VTD%L_SOU) = SOU(1:VTD%L_SOU) 
      DEALLOCATE ( BUF, SOU )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_GET_APR_PRP  !#!#
