      SUBROUTINE USER_INIT ( PROG_NAME, PROG_DATE, USER_BUFFER, DB_NAME, &
     &                       DB_VERS, NOBS )
! ************************************************************************
! *                                                                      *
! *   Program USER_INIT initializes pSolve common blocks, sets the user  *
! *   program name and user program date, returns the user_buffer from   *
! *   the pSolve batch control file, returns the experiment name,        *
! *   experiment version, and the total number of observations of a      *
! *   given experiment.
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * PROG_NAME   ( CHARACTER ) -- Name of the user program limited to     *
! *                              8 characters. Propagates to the         *
! *                              solution listing.                       *
! * PROG_DATE   ( CHARACTER ) -- Date of the user program in a form      *
! *                              YYYY.MM.DD. Propagates to the           *
! *                              solution listing.                       *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * USER_BUFFER ( CHARACTER ) -- Buffer specified in USER_BUFFER command *
! *                              of the pSolve control file. USER_BUFFER *
! *                              is limited to 80 characters.            *
! * DB_NAME     ( CHARACTER ) -- Database name                           *
! * DB_VERS     ( INTEGER*4 ) -- Database version                        *
! * NOSB        ( INTEGER*4 ) -- Number of observations in this          *
! *                              experiment.                             *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 30-MAY-2025   USER_INIT  v1.0 (d)  L. Petrov  30-MAY-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE       
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'precm.i'
      INCLUDE    'prfil.i'
      INCLUDE    'socom.i'
      CHARACTER  PROG_NAME*(*), PROG_DATE*(*), USER_BUFFER*(*), DB_NAME*(*)
      CHARACTER  BUF*80
      INTEGER*2  IBUF_I2(40)
      INTEGER*4  DB_VERS, NOBS
      INTEGER*2  LDBNAM(5,15), IDBV(15), NUMDB
      CHARACTER  CDBNAM(15)*10
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
      INTEGER*4  IDBE(15)
!
      CALL   PRE_PROG()
!
! --- Read user buffer from the pipe
!
      CALL   CLRCH ( USER_BUFFER )
      CALL   CLRCH ( BUF         )
      CALL   USE_BUFFER  ( IBUF_I2, INT2(40), 'ORC' )
      CALL   MEMCPY ( BUF, IBUF_I2 )
      USER_BUFFER = BUF
      CALL   SET_VERSION ( 'U-CAL', PROG_DATE, PROG_NAME )
!
! --- Read a number of scratch file and load their contents
!
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_4 ( 'RC'  )
      CALL USE_COMMON   ( 'ORC' )
      CALL USE_PARFIL   ( 'ORC' )
!
! --- Get database name DBNAME and the number of observations NOBS from NAMFIL
!
      CALL DBPOX  ( NUMDB, LDBNAM, IDBV, IDBE )
      CALL CLRCH ( DB_NAME )
      DB_NAME = CDBNAM(1)
      DB_VERS = IDBV(1)
      NOBS = IDBE(1)
!
      RETURN
      END  SUBROUTINE USER_INIT  !#!  
