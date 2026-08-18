      FUNCTION   GET_DBNAME ()
! ************************************************************************
! *                                                                      *
! *   Auxiliary function GET_DBNAME returns the name of the first        *
! *   loaded database or superfile.                                      *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 25-SEP-2002   GET_DBNAME  v1.0 (d)  L. Petrov  25-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER     GET_DBNAME*10
      INTEGER*4     IDBE(15)
      INTEGER*2     LDBNAM(5,15), IDBV(15), NUMD
      CHARACTER     CDBNAM(15)*10
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
!
      CALL OPENNAMFIL ( )
      NUMD = 1
      CALL DBPOX ( NUMD, LDBNAM, IDBV, IDBE )
      GET_DBNAME = CDBNAM(1)
      RETURN
      END  !#!  GET_DBNAME  #!#
