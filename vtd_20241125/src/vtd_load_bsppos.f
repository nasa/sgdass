      SUBROUTINE VTD_LOAD_BSPPOS ( VTD, I_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_BSPPOS reads the file with site position         *
! *   variations parameterized in the form of expansion into B-spline    *
! *   basis in BSPPOS format into the dynamic memory area.               *
! *   The file name is defined in VTD as the I_PSV-th element of the     *
! *   array VTD%CONF%POSVAR_FIL. The routine reads the file, checks its  *
! *   format, learns the number of stations, number of knots of          *
! *   B-spline, epochs and coeeficients of the spline, allocates         *
! *   dynamic memory for these arrays and for the array with             *
! *   displacement. Then it reads the file and stores information in     *
! *   the VTD object.                                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * I_PSV ( INTEGER*4 ) -- Index of the position variation file in the   *
! *                        array VTD%CONF%POSVAR_FIL.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! * ### 30-OCT-2007  VTD_LOAD_BSPPOS  v1.0 (c) L. Petrov 30-OCT-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( BSPPOS__TYPE ), POINTER :: BSP(:)
      INTEGER*4  I_PSV, IUER
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, IER 
#ifdef SUN
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
#endif
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check argumetns
!
      IF ( I_PSV .LE. 0  .OR.  I_PSV .GT. VTD__M_PSF  ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 3451, IUER, 'VTD_LOAD_BINDISP', 'Wrong parameter '// &
     &         'I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( ILEN(VTD%CONF%POSVAR_FIL(I_PSV)) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 3452, IUER, 'VTD_LOAD_BINDISP', 'Empty directory '// &
     &         'name for the '//STR(1:I_LEN(STR))//'-th position variation '// &
     &         'file' )
           RETURN
      END IF
!
! --- Allocate memory for temporary BSP objects
!
      ALLOCATE ( BSP(VTD__M_BSP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VTD__M_BSP*SIZEOF(BSP(1)), STR )
           CALL ERR_LOG ( 3453, IUER, 'VTD_LOAD_BSPPOS', 'Faulre to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'an array of temporary BSPPOS objects' )
           RETURN 
      END IF
!
! --- Read the input BSP objects
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_READ_BSP ( VTD%CONF%POSVAR_FIL(I_PSV), .FALSE., VTD__M_BSP, &
     &                    VTD%POSVAR(I_PSV)%N_PSVSTA, BSP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3454, IUER, 'VTD_LOAD_BSPPOS', 'Error in an '// &
     &         'attempt to load file '// &
     &          VTD%CONF%POSVAR_FIL(I_PSV)(1:I_LEN(VTD%CONF%POSVAR_FIL(I_PSV)))// &
     &          ' with B-spline position variations ' )
           RETURN 
      END IF
!
      IF ( VTD%POSVAR(I_PSV)%N_PSVSTA == 0 ) THEN
           VTD%POSVAR(I_PSV)%PSV_TYPE   = PSV__BSP
           VTD%POSVAR(I_PSV)%STS_BSPPOS = VTD__NOAV
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Allocate memory for permanent BSP objects
!
      ALLOCATE ( VTD%POSVAR(I_PSV)%BSP(VTD%POSVAR(I_PSV)%N_PSVSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VTD%POSVAR(I_PSV)%N_PSVSTA*SIZEOF(BSP(1)), STR )
           CALL ERR_LOG ( 3455, IUER, 'VTD_LOAD_BSPPOS', 'Faulre to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'BSPPOS objects' )
           RETURN 
      END IF
!
! --- Copy BSP objects
!
      DO 410 J1=1,VTD%POSVAR(I_PSV)%N_PSVSTA
         VTD%POSVAR(I_PSV)%BSP(J1) = BSP(J1)
 410  CONTINUE 
! 
      DEALLOCATE ( BSP )
!
! --- Allocate memory for station names and station coordinates
!
      VTD%POSVAR(I_PSV)%LEN_NAMSIT = 8*VTD%POSVAR(I_PSV)%N_PSVSTA*1
      VTD%POSVAR(I_PSV)%LEN_STACOO = 3*VTD%POSVAR(I_PSV)%N_PSVSTA*8
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL GRAB_MEM ( IER, VTD%POSVAR(I_PSV)%MEM_LEN,    &
     &                     VTD%POSVAR(I_PSV)%MEM_ADR, 2, &
     &     VTD%POSVAR(I_PSV)%LEN_NAMSIT,  VTD%POSVAR(I_PSV)%ADR_NAMSIT,  &
     &     VTD%POSVAR(I_PSV)%LEN_STACOO,  VTD%POSVAR(I_PSV)%ADR_STACOO   )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VTD%POSVAR(I_PSV)%N_PSVSTA*SIZEOF(BSP(1)), STR )
           CALL ERR_LOG ( 3456, IUER, 'VTD_LOAD_BSPPOS', 'Faulre to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'arrays of station names and station coordinates' )
           RETURN 
      END IF
!
! --- Copy station names and station coordintes
!
      DO 420 J2=1,VTD%POSVAR(I_PSV)%N_PSVSTA
#ifdef SUN
         CALL LIB$MOVC3 (   8, %VAL(LOC__SUN$$_STR(VTD%POSVAR(I_PSV)%BSP(J2)%STATION)), &
     &                         %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT + (J2-1)*8) )
#else
         CALL LIB$MOVC3 (   8, %REF(VTD%POSVAR(I_PSV)%BSP(J2)%STATION), &
     &                         %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT + (J2-1)*8) )
#endif
         CALL LIB$MOVC3 ( 3*8, VTD%POSVAR(I_PSV)%BSP(J2)%COO, &
     &                         %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO + (J2-1)*8*3) )
 420  CONTINUE 
      VTD%POSVAR(I_PSV)%RD_AREA = BSPPOS__RD_AREA
!
! --- Set status
!
      VTD%POSVAR(I_PSV)%PSV_TYPE   = PSV__BSP
      VTD%POSVAR(I_PSV)%STS_BSPPOS = VTD__LOAD
      VTD%POSVAR(I_PSV)%STS_NAMSIT = PSV__REA
      VTD%POSVAR(I_PSV)%STS_STACOO = PSV__REA
      VTD%POSVAR(I_PSV)%STATUS     = VTD__LOAD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_BSPPOS  !#!#
