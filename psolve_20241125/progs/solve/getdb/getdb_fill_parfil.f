      SUBROUTINE GETDB_FILL_PARFIL ( GVH, L_STA, C_STA, L_SOU, C_SOU, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GETDB_FILL_PARFIL
! *                                                                      *
! * ## 21-NOV-2005  GETDB_FILL_PARFIL  v1.2 (c) L. Petrov 28-OCT-2019 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'oborg.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  IUER
      INTEGER*4  DIMS(2), L_STA, L_SOU, J1, J2, J3, J4, CLASS, TYP, &
     &           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, IER
      ADDRESS__TYPE  ADR_DATA
      CHARACTER   C_STA(MAX_ARC_STA)*8, C_SOU(MAX_ARC_SRC)*8, DESCR*256
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL NOUT ( JPARFIL_BYTES, IFIRST_PRFIL_I2 )
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, INT4(NUMSTA)*8, &
     &                  DIMS(1), DIMS(2), ISITN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8911, IUER, 'GETDB_FILL_PARFIL', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SRCNAMES', 0, 0, INT4(NUMSTR)*8, &
     &                  DIMS(1), DIMS(2), ISTRN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8912, IUER, 'GETDB_FILL_PARFIL', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SIT_COOR', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8913, IUER, 'GETDB_FILL_PARFIL', 'Error in '// &
     &         'inquiring lcode N_CALIB ' )
           RETURN
      END IF
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SIT_COOR', 0, 0, 3*INT4(NUMSTA)*8, &
     &                       DIMS(1), DIMS(2), VSITEC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8914, IUER, 'GETDB_FILL_PARFIL', 'Error in '// &
     &              'getting lcode SIT_COOR' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SOU_COOR', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8915, IUER, 'GETDB_FILL_PARFIL', 'Error in '// &
     &         'inquiring lcode N_CALIB ' )
           RETURN
      END IF
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SOU_COOR', 0, 0, 2*INT4(NUMSTR)*8, &
     &                       DIMS(1), DIMS(2), VSTARC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8916, IUER, 'GETDB_FILL_PARFIL', 'Error in '// &
     &              'getting lcode SOU_COOR' )
                RETURN 
           END IF
      END IF
!
      L_STA = NUMSTA
      DO 410 J1=1,L_STA
         CALL VTD_NAME_REPAIR ( ISITN_CHR(J1) )
 410  CONTINUE 
      CALL LIB$MOVC3 ( L_STA*8, ISITN_CHR, C_STA )
!
      L_SOU = NUMSTR
      CALL LIB$MOVC3 ( L_SOU*8, ISTRN_CHR, C_SOU )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_FILL_PARFIL  !#!#
