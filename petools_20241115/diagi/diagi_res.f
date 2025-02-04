      SUBROUTINE DIAGI_RES ( FINAM, DIAGI_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_RES  reads saved DiaGI plot from the input file     *
! *   FINAM and fills data struc\ture DIAGI_S.                           *
! *   The plot may be build by call DIAGI.                               *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      FINAM ( CHARACTER ) -- File name with saved plot. It is assumed *
! *                             that this file has been made by          *
! *                             DIAGI_SAV.                               *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *      DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI       *
! *                               internal parameters.                   *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  21-MAY-98    DIAGI_RES   v1.4  (c)  L. Petrov 13-JUL-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  FINAM*(*), STR*20
      INTEGER*4  IUER, LUN, NBT, NBT_READ, NEL_READ, J1, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Openning input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4171, IUER, 'DIAGI_RES', 'Error during attempt to '// &
     &                   'open input file '//FINAM )
           GOTO 810
      END IF
!
! --- Reading DiaGI data structure
!
      NBT = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL ERR_PASS     ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, NBT, DIAGI_S, NBT_READ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4172, IUER, 'DIAGI_RES', 'Error during reading '// &
     &                   'DiaGI data structure from the file '//FINAM )
           GOTO 810
      END IF
!
      DO 410 J1=1,DIAGI_S%NCLR
!
! ------ Allocation memory for arguments
!
#ifdef ADR_32BIT
         CALL GET_MEM32  ( 8*DIAGI_S%NPOI(J1),       DIAGI_S%ADR_X8(J1) )
#else
         CALL GET_MEM    ( INT8(8*DIAGI_S%NPOI(J1)), DIAGI_S%ADR_X8(J1) )
#endif
         IF ( DIAGI_S%ADR_X8(J1) .EQ. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*DIAGI_S%NPOI(J1), STR )
              CALL ERR_LOG ( 4173, 'DIAGI_RES', 'Error during attempt '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory ' )
              RETURN
         END IF
!
! ------ Reading arguments
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'R8', DIAGI_S%NPOI(J1), &
     &                      %VAL(DIAGI_S%ADR_X8(J1)), NEL_READ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH   ( STR )
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 4174, IUER, 'DIAGI_RES', 'Error during reading '// &
     &                       'array of arguments for the '//STR(1:I_LEN(STR))// &
     &                       '-th color from the file '//FINAM )
              GOTO 810
         END IF
!
! ------ Allocation memory for values
!
#ifdef ADR_32BIT
         CALL GET_MEM32  ( 8*DIAGI_S%NPOI(J1), DIAGI_S%ADR_Y8(J1) )
#else
         CALL GET_MEM    ( INT8(8*DIAGI_S%NPOI(J1)), DIAGI_S%ADR_Y8(J1) )
#endif
         IF ( DIAGI_S%ADR_Y8(J1) .EQ. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*DIAGI_S%NPOI(J1), STR )
              CALL ERR_LOG ( 4175, 'DIAGI_RES', 'Error during attempt '// &
     &            'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory ' )
              RETURN
         END IF
!
! ------ Reading values
!
         CALL ERR_PASS    ( IUER, IER )
         CALL RDBIN_ARRAY ( LUN, 'R8', DIAGI_S%NPOI(J1), &
     &                      %VAL(DIAGI_S%ADR_Y8(J1)), NEL_READ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH   ( STR )
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 4176, IUER, 'DIAGI_RES', 'Error during reading '// &
     &                       'array of values for the '//STR(1:I_LEN(STR))// &
     &                       '-th color from the file '//FINAM )
              GOTO 810
         END IF
         IF ( DIAGI_S%LER(J1) ) THEN
!
! ----------- Allocation memory for errors
!
#ifdef ADR_32BIT
              CALL GET_MEM32  ( 8*DIAGI_S%NPOI(J1), DIAGI_S%ADR_E8(J1) )
#else
              CALL GET_MEM    ( INT8(8*DIAGI_S%NPOI(J1)), DIAGI_S%ADR_E8(J1) )
#endif
              IF ( DIAGI_S%ADR_E8(J1) .EQ. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*DIAGI_S%NPOI(J1), STR )
                   CALL ERR_LOG ( 4177, 'DIAGI_RES', 'Error during attempt '// &
     &                 'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory ' )
                   RETURN
              END IF
!
! ----------- Writing errors
!
              CALL ERR_PASS    ( IUER, IER )
              CALL RDBIN_ARRAY ( LUN, 'R8', DIAGI_S%NPOI(J1), &
     &                           %VAL(DIAGI_S%ADR_E8(J1)), NEL_READ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH   ( STR )
                   CALL INCH    ( J1, STR )
                   CALL ERR_LOG ( 4178, IUER, 'DIAGI_RES', 'Error during '// &
     &                 'reading array of errors for the '//STR(1:I_LEN(STR))// &
     &                 '-th color from the file '//FINAM )
                   GOTO 810
              END IF
         END IF
 410  CONTINUE
!
! --- Initialization of DiaGI data structure: first allocation of memory
!
      DIAGI_S%STATUS  = DIA__DEF
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI_INT (  1,  DIAGI_S, DIAGI_S%ICLR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4179, IUER, 'DIAGI_RES', 'Internal error' )
           GOTO 810
      END IF
!
      DIAGI_S%NUSER_FUNC = 0
 810  CONTINUE
!
! --- Closing input file
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .EQ. 0 ) CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  DIAGI_RES  #!#
