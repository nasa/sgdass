      SUBROUTINE B1B3D_GETMEM ( B3DOBJ, B1B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B1B3D_GETMEM  allocates dynamic memory for internal       *
! *   fields of the object B1B3DOBJ.                                     *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  25-FEB-97   B3D_GETMEM   v1.1  (c)  L. Petrov  12-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  IUER, NBS, SB, J0, J1, J2, J3, J4, J5, J6, J7
      INTEGER*8  G, L
      INTEGER*8  ISH 
      CHARACTER  STR*20
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( B1B3DOBJ%MEM_STAT .NE. F__MFR ) THEN
           CALL ERR_LOG ( 6301, IUER, 'B1B3D_GETMEM', 'B1B3DOBJ has not '// &
     &         'been initialized' )
           RETURN
      END IF
!
! --- Antiinitalization
!
      B1B3DOBJ%AD_W00 = -1
      B1B3DOBJ%AD_Z00 = -1
      B1B3DOBJ%AD_E00 = -1
      B1B3DOBJ%AD_U00 = -1
!
      B1B3DOBJ%AD_WI0 = -1
      B1B3DOBJ%AD_BI0 = -1
      B1B3DOBJ%AD_ZI0 = -1
      B1B3DOBJ%AD_EI0 = -1
      B1B3DOBJ%AD_UI0 = -1
!
      DO 400 J0 = 1,MAX_SEG
         B1B3DOBJ%AD_WIJ(J0) = -1
         B1B3DOBJ%AD_BIJ(J0) = -1
         B1B3DOBJ%AD_CIJ(J0) = -1
         B1B3DOBJ%AD_DIJ(J0) = -1
         B1B3DOBJ%AD_ZIJ(J0) = -1
         B1B3DOBJ%AD_EIJ(J0) = -1
         B1B3DOBJ%AD_UIJ(J0) = -1
 400  CONTINUE
!
! --- Calculation the size of memory and belonging to element B1B3DOBJ shifts
! --- with respect to the beginning of the requested dynamic memory
!
      G  = B3DOBJ%N_GLO
      L  = B3DOBJ%N_LOC
      SB = B3DOBJ%SB
      NBS= B3DOBJ%NBS
      ISH = 0
!C
      B1B3DOBJ%AD_W00  = ISH
         ISH = ISH + 8*(G*(G+1))/2
      B1B3DOBJ%AD_Z00  = ISH
         ISH = ISH + 8*G
      B1B3DOBJ%AD_E00  = ISH
         ISH = ISH + 8*G
      B1B3DOBJ%AD_U00  = ISH
         ISH = ISH + 8*G
!C
      B1B3DOBJ%AD_WI0  = ISH
         ISH = ISH + 8*(L*G)
      B1B3DOBJ%AD_BI0  = ISH
         ISH = ISH + 8*(L*(L+1))/2
      B1B3DOBJ%AD_ZI0  = ISH
         ISH = ISH + 8*L
      B1B3DOBJ%AD_EI0  = ISH
         ISH = ISH + 8*L
      B1B3DOBJ%AD_UI0  = ISH
         ISH = ISH + 8*L
!
      DO 410 J1=1,NBS
         B1B3DOBJ%AD_WIJ(J1) = ISH
            ISH = ISH + 8*SB*G
 410  CONTINUE
!
      DO 420 J2=1,NBS
         B1B3DOBJ%AD_BIJ(J2) = ISH
            ISH = ISH + 8*(SB*L)
 420  CONTINUE
!
      DO 430 J3=1,NBS
         B1B3DOBJ%AD_CIJ(J3) = ISH
            ISH = ISH + 8*(SB*(SB+1))/2
 430  CONTINUE
!
      DO 440 J4=1,NBS
         B1B3DOBJ%AD_DIJ(J4) = ISH
            ISH = ISH + 8*SB*SB
 440  CONTINUE
!
      DO 450 J5=1,NBS
         B1B3DOBJ%AD_ZIJ(J5) = ISH
            ISH = ISH + 8*SB
 450  CONTINUE
!
      DO 460 J6=1,NBS
         B1B3DOBJ%AD_EIJ(J6) = ISH
            ISH = ISH + 8*SB
 460  CONTINUE
!
      DO 470 J7=1,NBS
         B1B3DOBJ%AD_UIJ(J7) = ISH
            ISH = ISH + 8*SB
 470  CONTINUE
!
      B1B3DOBJ%MEM_SIZE2 = ISH  !  Size of memory to be written on disk
!
      B1B3DOBJ%AD_NGG = ISH
         ISH = ISH + 8*(G*(G+1))/2
      B1B3DOBJ%AD_NLG = ISH
         ISH = ISH + 8*L*G
      B1B3DOBJ%AD_NLL = ISH
         ISH = ISH + 8*(L*(L+1))/2
      B1B3DOBJ%AD_NS1G = ISH
         ISH = ISH + 8*SB*G
      B1B3DOBJ%AD_NS1L = ISH
         ISH = ISH + 8*SB*L
      B1B3DOBJ%AD_NS1S1 = ISH
         ISH = ISH + 8*(SB*(SB+1))/2
      B1B3DOBJ%AD_NS2G = ISH
         ISH = ISH + 8*SB*G
      B1B3DOBJ%AD_NS2L = ISH
         ISH = ISH + 8*SB*L
      B1B3DOBJ%AD_NS2S2 = ISH
         ISH = ISH + 8*(SB*(SB+1))/2
      B1B3DOBJ%AD_NS2S1 = ISH
         ISH = ISH + 8*SB*SB
      B1B3DOBJ%AD_NS1S2 = ISH
         ISH = ISH + 8*SB*SB
      B1B3DOBJ%AD_VG   = ISH
         ISH = ISH + 8*G
      B1B3DOBJ%AD_VL   = ISH
         ISH = ISH + 8*L
      B1B3DOBJ%AD_VS1  = ISH
         ISH = ISH + 8*SB
      B1B3DOBJ%AD_VS2  = ISH
         ISH = ISH + 8*SB
!
! --- Properly getting dynamic memory
!
      B1B3DOBJ%MEM_SIZE = ISH
      CALL GET_MEM ( B1B3DOBJ%MEM_SIZE, B1B3DOBJ%MEM_ADR )
      IF ( B1B3DOBJ%MEM_ADR .EQ. 0 ) THEN
           WRITE ( 6, * ) ' G =',  G, ' L =', L
           WRITE ( 6, * ) ' SB=', SB, ' NBS=', NBS
!
           CALL CLRCH ( STR )
           CALL IINCH ( B1B3DOBJ%MEM_SIZE, STR )
           CALL ERR_LOG ( 6302, IUER, 'B1B3D_GETMEM', 'Error '// &
     &         'during attempt to get '//STR(1:I_LEN(STR))// &
     &         ' bytes dynamic memory' )
#ifdef HPUX
           CALL MEMORYMAP ( %VAL(1) )
#endif
           RETURN
      END IF
!
! --- Setting actual addresses
!
      B1B3DOBJ%AD_W00 = B1B3DOBJ%AD_W00 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_Z00 = B1B3DOBJ%AD_Z00 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_E00 = B1B3DOBJ%AD_E00 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_U00 = B1B3DOBJ%AD_U00 + B1B3DOBJ%MEM_ADR
!
      B1B3DOBJ%AD_WI0 = B1B3DOBJ%AD_WI0 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_BI0 = B1B3DOBJ%AD_BI0 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_ZI0 = B1B3DOBJ%AD_ZI0 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_EI0 = B1B3DOBJ%AD_EI0 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_UI0 = B1B3DOBJ%AD_UI0 + B1B3DOBJ%MEM_ADR
!
      DO 510 J1=1,NBS
         B1B3DOBJ%AD_WIJ(J1) = B1B3DOBJ%AD_WIJ(J1) + B1B3DOBJ%MEM_ADR
 510  CONTINUE
!
      DO 520 J2=1,NBS
         B1B3DOBJ%AD_BIJ(J2) = B1B3DOBJ%AD_BIJ(J2) + B1B3DOBJ%MEM_ADR
 520  CONTINUE
!
      DO 530 J3=1,NBS
         B1B3DOBJ%AD_CIJ(J3) = B1B3DOBJ%AD_CIJ(J3) + B1B3DOBJ%MEM_ADR
 530  CONTINUE
!
      DO 540 J4=1,NBS
         B1B3DOBJ%AD_DIJ(J4)= B1B3DOBJ%AD_DIJ(J4)+ B1B3DOBJ%MEM_ADR
 540  CONTINUE
!
      DO 550 J5=1,NBS
         B1B3DOBJ%AD_ZIJ(J5)= B1B3DOBJ%AD_ZIJ(J5)+ B1B3DOBJ%MEM_ADR
 550  CONTINUE
!
      DO 560 J6=1,NBS
         B1B3DOBJ%AD_EIJ(J6)= B1B3DOBJ%AD_EIJ(J6)+ B1B3DOBJ%MEM_ADR
 560  CONTINUE
!
      DO 570 J7=1,NBS
         B1B3DOBJ%AD_UIJ(J7)= B1B3DOBJ%AD_UIJ(J7)+ B1B3DOBJ%MEM_ADR
 570  CONTINUE
!
      B1B3DOBJ%AD_NGG   = B1B3DOBJ%AD_NGG   + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NLG   = B1B3DOBJ%AD_NLG   + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NLL   = B1B3DOBJ%AD_NLL   + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS1S1 = B1B3DOBJ%AD_NS1S1 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS1L  = B1B3DOBJ%AD_NS1L  + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS1G  = B1B3DOBJ%AD_NS1G  + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS2S2 = B1B3DOBJ%AD_NS2S2 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS2L  = B1B3DOBJ%AD_NS2L  + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS2G  = B1B3DOBJ%AD_NS2G  + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS2S1 = B1B3DOBJ%AD_NS2S1 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_NS1S2 = B1B3DOBJ%AD_NS1S2 + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_VG    = B1B3DOBJ%AD_VG    + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_VL    = B1B3DOBJ%AD_VL    + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_VS1   = B1B3DOBJ%AD_VS1   + B1B3DOBJ%MEM_ADR
      B1B3DOBJ%AD_VS2   = B1B3DOBJ%AD_VS2   + B1B3DOBJ%MEM_ADR
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'B1B3D_GETMEM:   b1b3dobj.mem_size = ',b1b3dobj.mem_size ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      B1B3DOBJ%MEM_STAT = F__MSL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_GETMEM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE B1B3D_FREEMEM ( B3DOBJ, B1B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B1B3D_FREEMEM  frees dynamic memory for internal field    *
! *   of the object  B1B3DOBJ  allocated earlier by B1B3D_GETMEM.        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  25-FEB-97   B3D_FREEMEM  v1.1  (c)  L. Petrov  26-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  IUER, J1, J2, J3, J4, J5, J6, J7, NBS
!
      IF ( B1B3DOBJ%MEM_STAT .NE. F__MSL ) THEN
           CALL ERR_LOG ( 6311, IUER, 'B1B3D_FREEMEM', 'Dynamic memory for '// &
     &         'B1B3DOBJ has not been allocated earlier' )
           RETURN
      END IF
!
      NBS= B3DOBJ%NBS
!
! --- Actual retriving memory pool
!
      CALL FREE_MEM ( B1B3DOBJ%MEM_ADR )
!
! --- Diasabling addresses. It is being done to prevent acidental using
! --- memory used previous addresses. Errors occured with applying to
! --- addresses -1 are less hard then applying to addresses of memory
! --- having been freed and allocated for another purposes...
!
      B1B3DOBJ%AD_W00 = -1
      B1B3DOBJ%AD_Z00 = -1
      B1B3DOBJ%AD_E00 = -1
      B1B3DOBJ%AD_U00 = -1
!
      B1B3DOBJ%AD_WI0 = -1
      B1B3DOBJ%AD_BI0 = -1
      B1B3DOBJ%AD_ZI0 = -1
      B1B3DOBJ%AD_EI0 = -1
      B1B3DOBJ%AD_UI0 = -1
!
      DO 510 J1=1,NBS
         B1B3DOBJ%AD_WIJ(J1) = -1
 510  CONTINUE
!
      DO 520 J2=1,NBS
         B1B3DOBJ%AD_BIJ(J2) = -1
 520  CONTINUE
!
      DO 530 J3=1,NBS
         B1B3DOBJ%AD_CIJ(J3) = -1
 530  CONTINUE
!
      DO 540 J4=1,NBS
         B1B3DOBJ%AD_DIJ(J4) = -1
 540  CONTINUE
!
      DO 550 J5=1,NBS
         B1B3DOBJ%AD_ZIJ(J5) = -1
 550  CONTINUE
!
      DO 560 J6=1,NBS
         B1B3DOBJ%AD_EIJ(J6) = -1
 560  CONTINUE
!
      DO 570 J7=1,NBS
         B1B3DOBJ%AD_UIJ(J7) = -1
 570  CONTINUE
!
      B1B3DOBJ%AD_NGG   = -1
      B1B3DOBJ%AD_NLG   = -1
      B1B3DOBJ%AD_NLL   = -1
      B1B3DOBJ%AD_NS1S1 = -1
      B1B3DOBJ%AD_NS1L  = -1
      B1B3DOBJ%AD_NS1G  = -1
      B1B3DOBJ%AD_NS2S2 = -1
      B1B3DOBJ%AD_NS2L  = -1
      B1B3DOBJ%AD_NS2G  = -1
      B1B3DOBJ%AD_NS2S1 = -1
      B1B3DOBJ%AD_NS1S2 = -1
      B1B3DOBJ%AD_VG    = -1
      B1B3DOBJ%AD_VL    = -1
      B1B3DOBJ%AD_VS1   = -1
      B1B3DOBJ%AD_VS2   = -1
!
      B1B3DOBJ%MEM_SIZE = 0
      B1B3DOBJ%MEM_SIZE2 = 0
      B1B3DOBJ%MEM_STAT = F__MFR
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_FREEMEM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRNOR_B1B3D ( FINAM, B3DOBJ, B1B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRNOR_B3D  open file FINAM, writes down on disk two       *
! *   records 1) object B3DOBJ; 2) object B1B3DOBJ; 2) content of        *
! *   dynamic memory allocated in object  B1B3DOBJ and after that closes *
! *   the file. This routine implemented for transferring objects B3DOBJ *
! *   and  B1B3DOBJ from the executable to executable.                   *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    FINAM ( CHARACTER ) -- File name where B3DOBJ will be written.    *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *            Input: swicth IUER=0 -- no error messages will be         *
! *                                 generated even in the case of error. *
! *                          IUER=-1 -- in the case of error the message *
! *                                  will pe put on stdout.              *
! *            Output: 0 in the case of successfull completion and       *
! *                    non-zero in the case of error.                    *
! *                                                                      *
! *  ###   25-FEB-97  WRNOR_B1B3D  v3.4  (c)  L. Petrov 05-APR-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      CHARACTER  FINAM*(*)
      INTEGER*8  LEN_B3DOBJ, LENR, LEN_B1B3DOBJ
      INTEGER*4  IUER, LUN, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( FINAM(1:9) == '/dev/null' .OR. &
     &     FINAM(1:9) == '/dev/NULL' .OR. &
     &     FINAM(1:9) == '/dev/Null'      ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Opening file for writing binary data
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6331, IUER, 'WRNOR_B1B3D', 'Error during '// &
     &         'opening output file '//FINAM(1:I_LEN(FINAM))// &
     &         ' for writing normal equations for B1B3D algorithm' )
           RETURN
      END IF
!
! --- Putting the indentifier of the current version into B3D and B1B3D records
!
        B3DOBJ%IDENT = IDENT_B3D
      B1B3DOBJ%IDENT = IDENT_B1B3D
!
! --- Calculation length (in bytes) of the object B3DOBJ needed to be written
!
      LEN_B3DOBJ = LOC(B3DOBJ%LAST_FIELD) - LOC(B3DOBJ%FIRST_FIELD) + 4
!
! --- Calculation length (in bytes) of the object B1B3DOBJ needed to be written
!
      LEN_B1B3DOBJ = LOC(B1B3DOBJ%LAST_FIELD) - LOC(B1B3DOBJ%FIRST_FIELD) + 4
!
! --- Calculation length (in bytes) of fields to be written in first record
!
      LENR = LOC(B3DOBJ%MARKER_1) - LOC(B3DOBJ%FIRST_FIELD) + 4
!
! --- Writing the object B3DOBJ in file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, INT(LENR,KIND=4), B3DOBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6332, IUER, 'WRNOR_B1B3D', 'Error during '// &
     &         'writing the first record in output file '// &
     &          FINAM(1:I_LEN(FINAM))//' for normal equations of B1B3D '// &
     &         'algorithm' )
         RETURN
      END IF
!
! --- Writing various fields B3DOBJ
!
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%BLO,          IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%PL,           IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%CURR,         IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%NEXT,         IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_GLO,  B3DOBJ%INF_GLO,      IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_LOC,  B3DOBJ%INF_LOC,      IER )
      CALL WRBIN_RECORD ( LUN, 4*MAX_PSG*B3DOBJ%NBS, B3DOBJ%INF_SGM, IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%SX,     B3DOBJ%INF_SGX,      IER )
      CALL WRBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%DT,           IER )
      CALL WRBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%RT,           IER )
!
! --- Writing the object B1B3DOBJ in file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, INT(LEN_B1B3DOBJ,KIND=4), B1B3DOBJ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6333, IUER, 'WRNOR_B1B3D', 'Error during '// &
     &       'writing the record in output file '// &
     &        FINAM(1:I_LEN(FINAM))//' for normal equations of B1B3D '// &
     &       'algorithm' )
         RETURN
      END IF
!
! --- Writing the content of the dynamic memory allocated in various fields
! --- held by B1B3DOBJ object
!
      CALL WRBIN_RECORD ( LUN, INT(B1B3DOBJ%MEM_SIZE2,KIND=4), %VAL(B1B3DOBJ%MEM_ADR), &
     &                    IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6334, IUER, 'WRNOR_B1B3D', 'Error during '// &
     &         'writing the last record in output file '// &
     &          FINAM(1:I_LEN(FINAM))//' for normal equations of B1B3D '// &
     &         'algorithm' )
           RETURN
      END IF
!
! --- Closing the files
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6335, IUER, 'WRNOR_B1B3D', 'Error during '// &
     &         'closing output file '//FINAM(1:I_LEN(FINAM))// &
     &         ' for writing normal equations for B1B3D algorithm' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRNOR_B1B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RDNOR_B1B3D ( FINAM, B3DOBJ, B1B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RDNOR_B1B3D  open file FINAM, read from disk two records  *
! *   1) object B1B3DOBJ, 2) and content of dynamic memory allocated     *
! *   in object  B1B3DOBJ during previous work, gets dynamic memory in   *
! *   object  B1B3DOBJ, puts there the second record and after that      *
! *   closes the file. This routine intended for transferring object     *
! *   B1B3DOBJ from the executable to executable.                        *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    FINAM ( CHARACTER ) -- File name where B1B3DOBJ will be written.  *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  25-FEB-97   RDNOR_B1B3D  v3.0  (c)  L. Petrov  04-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      CHARACTER  FINAM*(*)
      INTEGER*8  LENR, LEN_B1B3DOBJ
      INTEGER*4  IUER, LUN, NB, NBYTES_READ, IER
      INTEGER*4  I_LEN
!
! --- Test: was dynamic memory allocated?
!
      IF ( B1B3DOBJ%MEM_STAT .EQ. F__MSL ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL B1B3D_FREEMEM ( B1B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6341, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &              'freeing dynamic memory' )
                RETURN
           END IF
      END IF
!
! --- Opening file for reading binary data
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6342, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &         'opening input file '//FINAM(1:I_LEN(FINAM))// &
     &         ' with normal equations of B1B3D algorithm' )
         RETURN
      END IF
!
! --- Calculation length (in bytes) of fields to be read in first record
!
      LENR = LOC(B3DOBJ%MARKER_1) - LOC(B3DOBJ%FIRST_FIELD) + 4
!
! --- Reading the first part of B3DOBJ
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, INT(LENR,KIND=4), B3DOBJ, NBYTES_READ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6343, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &       'reading the first record of input file '// &
     &        FINAM(1:I_LEN(FINAM))//' for normal equations of B1B3D '// &
     &       'algorithm' )
         RETURN
      END IF
!
! --- Test: Does the length coincide?
!
      IF ( LENR .NE. NBYTES_READ ) THEN
         WRITE ( 6, * ) ' LENR =',LENR, &
     &          ' NBYTES_READ     =',NBYTES_READ
         CALL ERR_LOG ( 6344, IUER, 'RDNOR_B1B3D', 'Internal error. '// &
     &       'Such a situation may occur when the beginning of the file '// &
     &        FINAM(1:I_LEN(FINAM))//' corrupted or it is not '// &
     &       'the ARC-file in B1B3D format' )
         RETURN
      END IF
!
! --- Test: Does the first letters of the IDENT are the B1B3D ?
!
      IF ( B3DOBJ%IDENT(1:3) .NE. 'B3D' ) THEN
         CALL ERR_LOG ( 6345, IUER, 'RDNOR_B1B3D', 'File '// &
     &        FINAM(1:I_LEN(FINAM))//' contains wrong context' )
         RETURN
      END IF
!
! --- Test of the coincidence of the IDENT
!
      IF ( B3DOBJ%IDENT .NE. IDENT_B3D ) THEN
           CALL ERR_LOG ( 6346, IUER, 'RDNOR_B1B3D', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' contains records with wrong version '// &
     &         'of fast.i: "'//B3DOBJ%IDENT//'" instead of "'//IDENT_B3D//'"' )
         RETURN
      END IF
!
! --- Reading others fields of the object B3DOBJ
!
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%BLO,          NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%PL,           NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%CURR,         NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%NEXT,         NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_GLO,  B3DOBJ%INF_GLO,      NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_LOC,  B3DOBJ%INF_LOC,      NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*MAX_PSG*B3DOBJ%NBS, B3DOBJ%INF_SGM, NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%SX,     B3DOBJ%INF_SGX,      NB, IER )
      CALL RDBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%DT,           NB, IER )
      CALL RDBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%RT,           NB, IER )
!
! --- Calculation length (in bytes) of the object B1B3DOBJ
!
      LEN_B1B3DOBJ = LOC(B1B3DOBJ%LAST_FIELD) - LOC(B1B3DOBJ%FIRST_FIELD) + 4
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, INT(LEN_B1B3DOBJ,KIND=4), B1B3DOBJ, NBYTES_READ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6347, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &       'reading the second record of input file '// &
     &        FINAM(1:I_LEN(FINAM))//' for normal equations of B1B3D '// &
     &       'algorithm' )
         RETURN
      END IF
!
! --- Test: Does the length coincide?
!
      IF ( LEN_B1B3DOBJ .NE. NBYTES_READ ) THEN
         WRITE ( 6, * ) ' LEN_B1B3DOBJ =',LEN_B1B3DOBJ, &
     &          ' NBYTES_READ     =',NBYTES_READ
         CALL ERR_LOG ( 6348, IUER, 'RDNOR_B1B3D', 'Internal error' )
         RETURN
      END IF
!
! --- Test: Does the first letters of the IDENT are the B1B3D ?
!
      IF ( B1B3DOBJ%IDENT(1:5) .NE. 'B1B3D' ) THEN
         CALL ERR_LOG ( 6349, IUER, 'RDNOR_B1B3D', 'File '// &
     &        FINAM(1:I_LEN(FINAM))//' contains wrong context' )
         RETURN
      END IF
!
! --- Test of the coincidence of the IDENT
!
      IF ( B1B3DOBJ%IDENT .NE. IDENT_B1B3D ) THEN
         CALL ERR_LOG ( 6350, IUER, 'RDNOR_B1B3D', 'File '// &
     &        FINAM(1:I_LEN(FINAM))//' contains records with wrong version '// &
     &       'of fast.i: "'//B1B3DOBJ%IDENT//'" instead of "'//IDENT_B1B3D// &
     &       '"' )
         RETURN
      END IF
!
! --- Alolocation of dynamic memory
!
      B1B3DOBJ%MEM_STAT = F__MFR
      CALL ERR_PASS ( IUER, IER )
      CALL B1B3D_GETMEM ( B3DOBJ, B1B3DOBJ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6351, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &       'getting dynamic memory for allocation blocks of normal '// &
     &       'equations' )
         RETURN
      END IF
!
! --- Reading data to the pool of allocated dynamic memory
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, INT(B1B3DOBJ%MEM_SIZE2,KIND=4), %VAL(B1B3DOBJ%MEM_ADR), &
     &                    NBYTES_READ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6352, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &       'reading the third record from the input file '// &
     &        FINAM(1:I_LEN(FINAM))//' for normal equations of B1B3D '// &
     &       'algorithm' )
         RETURN
      END IF
!
! --- Test: Were all data read?
!
      IF ( B1B3DOBJ%MEM_SIZE2 .NE. NBYTES_READ ) THEN
         WRITE ( 6, * ) ' B1B3DOBJ%MEM_SIZE2 =',B1B3DOBJ%MEM_SIZE2, &
     &          ' NBYTES_READ      =',NBYTES_READ
         CALL ERR_LOG ( 6353, IUER, 'RDNOR_B1B3D', 'Internal error' )
         RETURN
      END IF
!
! --- Closing input file
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6354, IUER, 'RDNOR_B1B3D', 'Error during '// &
     &       'closing input file '//FINAM(1:I_LEN(FINAM))// &
     &       ' for normal equations of B1B3D algorithm' )
         RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RDNOR_B1B3D  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   FULL_B1B3D ( B3DOBJ, B1B3DOBJ, IROW, ICOL, TYP_I, NBS_I, &
     &                        IR_I, IC_I )
! ************************************************************************
! *                                                                      *
! *   Function  FULL_B1B3D  returns the address of the element of        *
! *   submatrix of normal matrix in B1B3D parametrization which          *
! *   corresponds to a specified element in full matrix.                 *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *     IROW ( INTEGER*4 ) -- Row in full matris for the specified       *
! *                           element.                                   *
! *     ICOL ( INTEGER*4 ) -- Column in full matris for the specified    *
! *                           element.                                   *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *    TYP_I ( CHARACTER, OPT  ) -- Type of the submartix. One of        *
! *                                 "G", "M", "L", "W", "B", "C", "D".   *
! *            "G" -- global-global.                                     *
! *            "M" -- local-global.                                      *
! *            "L" -- local-local.                                       *
! *            "W" -- segmented-global.                                  *
! *            "B" -- segmented-local.                                   *
! *            "C" -- diagonal segmented-segmented.                      *
! *            "D" -- down-diagonal segmented-segmented.                 *
! *    NBS_I ( INTEGER*4, OPT  ) -- Number of block.                     *
! *     IR_I ( INTEGER*4, OPT  ) -- Row in submatrix for corresponding   *
! *                                 element.                             *
! *     IC_I ( INTEGER*4, OPT  ) -- Column in submatrix for              *
! *                                 corresponding element.               *
! *                                                                      *
! *   In the case when an element specified which doesn't have           *
! *   correspondence to non-zero element of double-bordered              *
! *   block-threediagonal matrix, FULL_B1B3D = -1 (and also NBS_I = -999,*
! *   ( IR_I = -1, IC = -1 ).                                            *
! *                                                                      *
! *  ###   26-FEB-97   FULL_B1B3D   v1.0 (c)  L. Petrov  26-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      ADDRESS__TYPE :: FULL_B1B3D
      INTEGER*4  IROW, ICOL, NBS_I, IR_I, IC_I, NBS, IR, IC
      CHARACTER  TYP_I*(*)
      CHARACTER  TYP*1
      INTEGER*4  G, L, SB, SX, POS
      INTEGER*4  N, ISWAP, IR_BLO, IC_BLO, IR_PLA, IC_PLA, &
     &           I, J
      INTEGER*8  LOCC
      LOCC(I,J) = INT8(min(I,J)) + (INT8(max(I,J))*INT8(max(I,J)-1))/2
!
      N  = B3DOBJ%NBS
      G  = B3DOBJ%N_GLO
      L  = B3DOBJ%N_LOC
      SB = B3DOBJ%SB
      SX = B3DOBJ%SX
!
! --- Setting default for the case when we won't find correspondence
!
      TYP =  '?'
      NBS =  -999
      IR  =  -1
      IC  =  -1
      FULL_B1B3D = -1
!
! --- Test of validity input data
!
      IF ( IROW .LE. 0  .OR.  IROW .GT.  G + L +(N-1)*SB + SX ) GOTO 810 ! End
      IF ( ICOL .LE. 0  .OR.  ICOL .GT.  G + L +(N-1)*SB + SX ) GOTO 810 ! End
!
! --- Extracting number of block and place in the block for parameters for
! --- rows and columns
!
      IR_BLO = B3DOBJ%BLO(IROW)
      IR_PLA = B3DOBJ%PL (IROW)
      IC_BLO = B3DOBJ%BLO(ICOL)
      IC_PLA = B3DOBJ%PL (ICOL)
!
! --- Testing various combinations of IR_BLO, IC_BLO
!
      IF ( IR_BLO .EQ. 0   .AND.   IC_BLO .EQ. 0 ) THEN
!
! -------- Oh! It is, of course, global-global block.
!
           TYP = 'G'
           NBS =  0
           IR  =  IR_PLA
           IC  =  IC_PLA
           POS  =  LOCC( IR, IC ) - 1
           FULL_B1B3D = B1B3DOBJ%AD_W00 + 8*POS
      END IF
!
      IF ( ( IR_BLO .EQ. -1   .AND.   IC_BLO .EQ. 0 ) .OR. &
     &     ( IC_BLO .EQ. -1   .AND.   IR_BLO .EQ. 0 )      )THEN
!
! -------- It is local-global block.
!
           TYP = 'M'
           IF ( IC_BLO .EQ. 0 ) THEN
                NBS =  IR_BLO
                IR  =  IC_PLA
                IC  =  IR_PLA
             ELSE
                NBS =  IC_BLO
                IR  =  IR_PLA
                IC  =  IC_PLA
           END IF
           NBS =  -1
           IR  =  IR_PLA
           IC  =  IC_PLA
           POS  =  LOCC( IR, IC ) - 1
           FULL_B1B3D = B1B3DOBJ%AD_WI0 + 8*POS
      END IF
!
      IF ( IR_BLO .EQ. -1   .AND.   IC_BLO .EQ. -1 ) THEN
!
! -------- It is local-local block.
!
           TYP = 'L'
           NBS =  -1
           IR  =  IR_PLA
           IC  =  IC_PLA
           POS  =  LOCC( IR, IC ) - 1
           FULL_B1B3D = B1B3DOBJ%AD_BI0 + 8*POS
      END IF
!
      IF ( ( IR_BLO .EQ. -1   .AND.   IC_BLO .GT. 0 )  .OR. &
     &     ( IC_BLO .EQ. -1   .AND.   IR_BLO .GT. 0 )        ) THEN
!
! -------- It is local-segmented block.
!
           TYP = 'B'
           IF ( IC_BLO .GT. 0 ) THEN
                NBS =  IC_BLO
                IR  =  IC_PLA
                IC  =  IR_PLA
             ELSE
                NBS =  IR_BLO
                IR  =  IR_PLA
                IC  =  IC_PLA
           END IF
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                POS = (IC-1)*SB + IR -1
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                POS = (IC-1)*SX + IR -1
           END IF
           FULL_B1B3D = B1B3DOBJ%AD_BIJ(NBS) + 8*POS
      END IF
!
      IF ( ( IR_BLO .EQ. 0   .AND.   IC_BLO .GT. 0 )  .OR. &
     &     ( IC_BLO .EQ. 0   .AND.   IR_BLO .GT. 0 )        ) THEN
!
! -------- Oh! It is, of course, global-segmented block.
!
           TYP = 'W'
           NBS =  IC_BLO
           IR  =  IC_PLA
           IC  =  IR_PLA
           IF ( IC_BLO .GT. 0 ) THEN
                NBS =  IC_BLO
                IR  =  IC_PLA
                IC  =  IR_PLA
             ELSE
                NBS =  IR_BLO
                IR  =  IR_PLA
                IC  =  IC_PLA
           END IF
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                POS = (IC-1)*SB + IR -1
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                POS = (IC-1)*SX + IR -1
           END IF
           FULL_B1B3D = B1B3DOBJ%AD_WIJ(NBS) + 8*POS
      END IF
!
      IF ( IR_BLO .GT. 0  .AND.  IR_BLO .EQ. IC_BLO ) THEN
!
! -------- Oh! It is, of course, diagonal segmented-segmented block.
!
           TYP = 'C'
           NBS =  IC_BLO
           IR  =  IC_PLA
           IC  =  IR_PLA
           POS = LOCC( IR, IC ) -1
           FULL_B1B3D = B1B3DOBJ%AD_CIJ(NBS) + 8*POS
      END IF
!
      IF ( ( IR_BLO .GT. 0  .AND.  ( IC_BLO - IR_BLO .EQ. 1 ) ) .OR. &
     &     ( IC_BLO .GT. 0  .AND.  ( IR_BLO - IC_BLO .EQ. 1 ) )      ) THEN
!
! -------- Oh! It is, of course, down-diagonal cross segmented-segmented block.
!
           TYP = 'D'
           IF ( IC_BLO - IR_BLO .EQ.  1 ) THEN
                NBS =  IC_BLO
                IR  =  IC_PLA
                IC  =  IR_PLA
             ELSE
                NBS =  IR_BLO
                IR  =  IR_PLA
                IC  =  IC_PLA
           END IF
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                POS = (IC-1)*SB + IR -1
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                POS = (IC-1)*SX + IR -1
           END IF
           FULL_B1B3D = B1B3DOBJ%AD_DIJ(NBS) + 8*POS
      END IF
!
      IF ( (TYP .EQ. 'G'  .OR.  TYP .EQ. 'L'  .OR.  TYP .EQ. 'C' )  .AND. &
     &     (IR .GT. IC) ) THEN
!
! -------- Permutation rows-columns for symmetric matrix
!
           ISWAP = IR
           IR    = IC
           IC    = ISWAP
      END IF
!
 810  CONTINUE
!
! --- Testing: was actual argument were set up. If yes, moving there
! --- output parameters
!
      IF ( LOC(TYP_I) .NE. 0 ) TYP_I = TYP
      IF ( LOC(NBS_I) .NE. 0 ) NBS_I = NBS
      IF ( LOC(IR_I)  .NE. 0 ) IR_I  = IR
      IF ( LOC(IC_I)  .NE. 0 ) IC_I  = IC
!
      RETURN
      END  !#!  FULL_B1B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EXPAND_B1B3D ( MAT_MODE, B3DOBJ, B1B3DOBJ, MAT, VEC, &
     &                          DSP, SCL )
! ************************************************************************
! *                                                                      *
! *   Routine  EXPAND_B1B3D  expands normal submatrices and subvectors   *
! *   enclosed in object B1B3DOBJ  to full matrix and full vectors. It   *
! *   is assumed that matrix MAT was zeroed earlier. Only those elements *
! *   of MAT which correspond elements B1B3D are being changed. Shitch   *
! *   MODE controls which groups of parameterss are to be moved. It is   *
! *   done for saving time when not elements are required to be moved.   *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! * MAT_MODE ( INTEGER*4 ) --  switch if the working mode for moving     *
! *                            matrices (all vectors are to be moved     *
! *                            regradless MODE_MAT ).                    *
! *            MAT_MODE = 1 -  only global-global matrix is to be moved. *
! *            MAT_MODE = 2 -  global-global, galobal-local and          *
! *                            local-local matrices are to be moved.     *
! *            MAT_MODE = 3 -  all matrices are to be moved.             *
! *   B3DOBJ ( RECORD    ) -- object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *      MAT ( REAL*8    ) -- Full normal matrix. Only 1) global-global  *
! *                           symmetric, 2) band of global-local         *
! *                           parameters, 3) local-local parameters;     *
! *                           4) band of global-segmented parameters;    *
! *                           5) main block diagonal band;               *
! *                           6) down-diagonal band are filled. Other    *
! *                           elements of matrix MAT remain unchanged.   *
! *      VEC ( REAL*8    ) -- Full vector of estimates (E-vector).       *
! *      DSP ( REAL*8    ) -- Full vector of right parts of normal       *
! *                           equations (Z-vector). It can also contain  *
! *                           variances of the estimates.                *
! *      SCL ( REAL*8    ) -- Full vector of scales of the parameters    *
! *                           (S-vector).                                *
! *                                                                      *
! *  ###  26-FEB-97  EXPAND_B1B3D   v1.1 (c)  L. Petrov  03-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      REAL*8     MAT(*), VEC(*), DSP(*), SCL(*)
      INTEGER*4  MAT_MODE, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11
      INTEGER*4  NP
      INTEGER*8  G, GA, L, LA, SB, SBA, SX, S
      ADDRESS__TYPE :: AD_G, AD_B,  AD_W,  AD_C, AD_D,  AD_E, AD_Z, AD_U, &
     &                 AD_BS, AD_WS,       AD_DS
      INTEGER*4  IND_FULL
      INTEGER*4  I, J
      INTEGER*8  LOCC
      LOCC(I,J) = INT8(min(I,J)) + (INT8(max(I,J))*INT8(max(I,J)-1))/2
!
      G   = B3DOBJ%N_GLO
      GA  = (INT8(G)*INT8(G+1))/2
      L   = B3DOBJ%N_LOC
      LA  = (L*(L+1))/2
      SB  = B3DOBJ%SB
      SBA = (SB*(SB+1))/2
      SX  = B3DOBJ%SX
      SBA = (SX*(SX+1))/2
!
! --- MOVING GLOBAL PARAMETERS
!     ~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Setting up initital addresses of the matrix and vectors
!
      AD_G = B1B3DOBJ%AD_W00
      AD_E = B1B3DOBJ%AD_E00
      AD_Z = B1B3DOBJ%AD_Z00
      AD_U = B1B3DOBJ%AD_U00
!
      NP = 0  ! Pointer of the column of the FULL matrix
      DO 410 J1=1,G
         NP = NP + 1
         DO 420 J2=1,J1
            IND_FULL = LOCC ( B3DOBJ%INF_GLO(J1), B3DOBJ%INF_GLO(J2) )
            CALL MOVE_R8 ( %VAL(AD_G), MAT(IND_FULL) )
            AD_G = AD_G + 8
 420     CONTINUE
!C
         IND_FULL = B3DOBJ%INF_GLO(J1)
         CALL MOVE_R8 ( %VAL(AD_E), VEC(IND_FULL) )
         CALL MOVE_R8 ( %VAL(AD_Z), DSP(IND_FULL) )
         CALL MOVE_R8 ( %VAL(AD_U), SCL(IND_FULL) )
!
         AD_E = AD_E + 8
         AD_Z = AD_Z + 8
         AD_U = AD_U + 8
 410  CONTINUE
!
! --- MOVING LOCAL PARAMETERS
!     ~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Setting up initital addresses of the matrix and vectors
!
      AD_B  = B1B3DOBJ%AD_BI0
      AD_WS = B1B3DOBJ%AD_WI0
      AD_E  = B1B3DOBJ%AD_EI0
      AD_Z  = B1B3DOBJ%AD_ZI0
      AD_U  = B1B3DOBJ%AD_UI0
!
! --- Setting up initital addresses of the matrix and vectors
!
      DO 430 J3=1,L
         NP = NP + 1
         AD_W = AD_WS + 8*(J3-1)
         IF ( MAT_MODE .GE. 2 ) THEN
            DO 440 J4=1,G
               IND_FULL = LOCC ( B3DOBJ%INF_LOC(J3), B3DOBJ%INF_GLO(J4) )
               CALL MOVE_R8 ( %VAL(AD_W), MAT(IND_FULL) )
               AD_W = AD_W + 8*L
 440        CONTINUE
!
            DO 450 J5=1,J3
               IND_FULL = LOCC ( B3DOBJ%INF_LOC(J3), B3DOBJ%INF_LOC(J5) )
               CALL MOVE_R8 ( %VAL(AD_B), MAT(IND_FULL) )
               AD_B = AD_B + 8
 450        CONTINUE
         END IF
!
! ------ Moving vector elements to full vectors
!
         IND_FULL = B3DOBJ%INF_LOC(J3)
         CALL MOVE_R8 ( %VAL(AD_E), VEC(IND_FULL) )
         CALL MOVE_R8 ( %VAL(AD_Z), DSP(IND_FULL) )
         CALL MOVE_R8 ( %VAL(AD_U), SCL(IND_FULL) )
!
         AD_E = AD_E + 8
         AD_Z = AD_Z + 8
         AD_U = AD_U + 8
 430  CONTINUE
!
! --- CYCLE ON BLOCKS OF SEGMENTED PARAMETERS
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DO 460 J6=1,B3DOBJ%NBS
!
! ------ Setting up address of the first elements of vectors and matrices
! ------ associated with local parameters
!
         AD_WS= B1B3DOBJ%AD_WIJ(J6)
         AD_BS= B1B3DOBJ%AD_BIJ(J6)
         AD_C = B1B3DOBJ%AD_CIJ(J6)
         AD_DS= B1B3DOBJ%AD_DIJ(J6)
         AD_E = B1B3DOBJ%AD_EIJ(J6)
         AD_Z = B1B3DOBJ%AD_ZIJ(J6)
         AD_U = B1B3DOBJ%AD_UIJ(J6)
!
         IF        ( J6 .LT. B3DOBJ%NBS ) THEN
              S    = SB
           ELSE IF ( J6 .EQ. B3DOBJ%NBS ) THEN
              S    = SX
         END IF
!
! ------ Cycle on segmented parameters of the J6-th block
!
         DO 470 J7=1,S
!
            NP = NP + 1
!
! --------- Scanning of J7-th ROW of J6-th global-segmented block of parameters
!
! --------- Moving segmented-global parameters
!
            IF ( MAT_MODE .EQ. 3 ) THEN
               AD_W = AD_WS + 8*(J7-1) ! Address of the element WIJ(J7,1)
               DO 480 J8=1,G
                  IND_FULL = LOCC ( B3DOBJ%INF_SGM(J7,J6), B3DOBJ%INF_GLO(J8) )
                  CALL MOVE_R8 ( %VAL(AD_W), MAT(IND_FULL) )
                  AD_W = AD_W +8*S ! Now AD_W will keep address WIJ(J7,J8+1)
 480           CONTINUE
!
! ------------ Moving segmented-local parameters
!
               AD_B = AD_BS + 8*(J7-1) ! Address of the element BIJ(J7,1)
               DO 490 J9=1,L
                  IND_FULL = LOCC ( B3DOBJ%INF_SGM(J7,J6), B3DOBJ%INF_LOC(J9) )
                  CALL MOVE_R8 ( %VAL(AD_B), MAT(IND_FULL) )
                  AD_B = AD_B +8*S ! Now AD_B will keep address BIJ(J7,J9+1)
 490           CONTINUE
!
               IF ( J6 .GT. 1 ) THEN
!
! ----------------- Moving segmented(current)-segmented(previous) parameters
!
                    AD_D = AD_DS + 8*(J7-1) ! Address of element D(J7,1)
                    DO 4100 J10=1,SB
                       IND_FULL = LOCC ( B3DOBJ%INF_SGM(J7,J6), &
     &                                   B3DOBJ%INF_SGM(J10,J6-1) )
                       CALL MOVE_R8 ( %VAL(AD_D), MAT(IND_FULL)   )
                       AD_D = AD_D +8*S ! Now AD_D keeps address D(J7,J10+1)
 4100               CONTINUE
               END IF
!
! ------------ Moving segmented(current)-segmented(current) parameters
!
               DO 4110 J11=1,J7
                  IND_FULL = LOCC ( B3DOBJ%INF_SGM(J7,J6), &
     &                              B3DOBJ%INF_SGM(J11,J6) )
                   CALL MOVE_R8 ( %VAL(AD_C), MAT(IND_FULL) )
                   AD_C = AD_C +8 ! Now AD_C will keep address of the next
!                                 ! element of the matrix C
 4110          CONTINUE
            END IF
!
! --------- Moving vector elements to full vectors
!
            IND_FULL = B3DOBJ%INF_SGM(J7,J6)
            CALL MOVE_R8 ( %VAL(AD_E), VEC(IND_FULL) )
            CALL MOVE_R8 ( %VAL(AD_Z), DSP(IND_FULL) )
            CALL MOVE_R8 ( %VAL(AD_U), SCL(IND_FULL) )
!
            AD_E = AD_E + 8
            AD_Z = AD_Z + 8
            AD_U = AD_U + 8
 470     CONTINUE
 460  CONTINUE
!
      RETURN
      END  SUBROUTINE  EXPAND_B1B3D  !#!#
