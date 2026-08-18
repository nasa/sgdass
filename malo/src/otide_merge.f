      PROGRAM    OTIDE_MERGE
! ************************************************************************
! *                                                                      *
! *   Program OTIDE_MERGE merges two files with ocean tide heights or    *
! *   ocean tide loadings in HEB format and merges them in one output    *
! *   file in HEB format.
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 18-SEP-2025  OTIDE_MERGE  v1.0 (d)  L. Petrov  18-SEP-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_IN(2), HEB_OUT
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      CHARACTER  FILIN(2)*128, FILOUT*128
      INTEGER*4  J1, J2, J3, LIND(2), IND(2,MIND,2), IUER
!
      FILIN(1) = '/imls/oper_model/got55p_otide_short_periods_d2699_6cells.heb'
      FILIN(2) = '/imls/oper_model/got55p_otide_long_periods_d2699_6cells.heb'
      FILOUT   = '/imls/oper_model/got55p_otide_d2699_6cells.heb'
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage: otide_merge fil1 fil2 filout' 
           CALL  EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN(1) )
           CALL GETARG ( 2, FILIN(2) )
           CALL GETARG ( 3, FILOUT   )
      END IF
!
      DO 410 J1=1,2
         IUER = -1
         CALL READ_HEB ( FILIN(J1), HEB_IN(J1), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 6381, IUER, 'OTIDE_MERGE', 'Error in reading '// &
     &            'input heb-file '//FILIN(J1) )
              CALL EXIT ( 1 )
         END IF 
         CALL EXWORD ( HEB_IN(J1)%SDS_NAME, MIND, LIND(J1), IND(1,1,J1), ' ', IUER )
         IF ( LIND(J1) < 4 ) THEN
              IUER = -2
              CALL ERR_LOG ( 6382, IUER, 'OTIDE_MERGE', 'Error in parsing'// &
     &            'input heb-file '//TRIM(FILIN(J1))//' for SDS_NAME LIND < 4' )
              CALL EXIT ( 1 )
         END IF
 410  CONTINUE 
      HEB_OUT = HEB_IN(1)
      HEB_OUT%DIMS(4) = HEB_IN(1)%DIMS(4) + HEB_IN(2)%DIMS(4)
      HEB_OUT%SDS_NAME = HEB_IN(1)%SDS_NAME(IND(1,1,1):IND(2,LIND(1),1))//','// &
     &                   HEB_IN(2)%SDS_NAME(IND(1,LIND(2),2):IND(2,LIND(2),2))
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)) )
      HEB_OUT%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1:2,1:HEB_IN(1)%DIMS(4)) = &
     &            HEB_IN(1)%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1:2,1:HEB_IN(1)%DIMS(4))
      HEB_OUT%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1:2,HEB_IN(1)%DIMS(4)+1:HEB_IN(1)%DIMS(4)+HEB_IN(2)%DIMS(4)) = &
     &            HEB_IN(2)%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1:2,1:HEB_IN(2)%DIMS(4))
!
! --- Write the output HEB-dataset into the output file FILOUT
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6383, IUER, 'OTIDE_MERGE', 'Failure to write '// &
     &         'the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  OTIDE_MERGE  !#!  
