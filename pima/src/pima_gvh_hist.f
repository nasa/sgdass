      SUBROUTINE PIMA_GVH_HIST ( PIM, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GVH_HIST 
! *                                                                      *
! *  ### 15-JUL-2009  PIMA_GVH_HIST  v1.1 (c)  L. Petrov 12-APR-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'pima_db.i'
      INCLUDE   'gvh.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  IUER
      INTEGER*4  M__HST
      PARAMETER  ( M__HST = 256 )
      CHARACTER  TITLE*128, DB_NAME*10, STR*128, BUF(M__HST)*80
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
!
      INTEGER*4    GVF__FR1, GVF__FR2, GVF__CL1, GVF__SL1
      PARAMETER  ( GVF__FR1 = 1 )
      PARAMETER  ( GVF__FR2 = 2 )
      PARAMETER  ( GVF__CL1 = 3 )
      PARAMETER  ( GVF__SL1 = 4 )
      INTEGER*4  J1, J2, J3, J4, NLIN, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IER = -1
      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%UTC_MTAI, IER )
      DB_NAME = STR(1:4)//STR(6:7)//STR(9:10)//'_'//PIM%CONF%MKDB_SUFFIX
!
      TITLE = 'Comments of the geo VLBI database '//DB_NAME// &
     &        ' Version 1  '//GET_CDATE()
      NLIN = 8
!
      BUF(1) = 'Generating subroutine: pima_mkdb'
      BUF(2) = 'Parameters of the PIMA control file for database generation:'
      IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_MID_SCAN ) THEN
           BUF(3) = 'MKDB.SRT:              '//TRIM(PIMA__MKDB_MID_SCAN)
        ELSE IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_SRT_FRT ) THEN
           BUF(3) = 'MKDB.SRT:              '//TRIM(PIMA__MKDB_SRT_FRT)
        ELSE
           BUF(3) = 'MKDB.SRT:              '//TRIM(PIM%CONF%MKDB_SRT_FILE)
      END IF
      IF ( PIM%CONF%MKDB_FILTER == PIMA__FILTER_NO ) THEN
           BUF(4) = 'MKDB.FILTER:           '//TRIM(PIMA__FILTER_NO)
        ELSE 
           BUF(4) = 'MKDB.FILTER:           '//TRIM(PIMA__ONLY_DET)
      END IF
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           BUF(5) = 'MKDB.FRINGE_ALGORITHM: '//TRIM(PIMA__FRA_DRF)
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           BUF(5) = 'MKDB.FRINGE_ALGORITHM: '//TRIM(PIMA__FRA_LSQ)
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           BUF(5) = 'MKDB.FRINGE_ALGORITHM: '//TRIM(PIMA__FRA_MUL)
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           BUF(5) = 'MKDB.FRINGE_ALGORITHM: '//TRIM(PIMA__FRA_ADD)
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FINE_SEARCH_NO ) THEN
           BUF(5) = 'MKDB.FRINGE_ALGORITHM: '//TRIM(PIMA__FINE_SEARCH_NO)
      END IF
      WRITE ( UNIT=BUF(6), FMT=110 ) 'MKDB.GD_MAX_ADD_ERROR', PIM%CONF%MKDB_GD_MAX_ADD_ERROR
      WRITE ( UNIT=BUF(7), FMT=110 ) 'MKDB.GD_MAX_SCL_ERROR', PIM%CONF%MKDB_GD_MAX_SCL_ERROR
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      BUF(8) = 'Analyst: '//TRIM(USER_REALNAME)
 110  FORMAT ( A, ':', 1X, 1PD10.3 )
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL GVH_PTEXT_CHP ( GVH, GVF__FR1, TITLE, NLIN, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7651, IUER, 'PIMA_GVH_HIST', 'Error in an '// &
     &         'attempt to put history section, version 1, to the GVH file' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GVH_HIST  !#!#
