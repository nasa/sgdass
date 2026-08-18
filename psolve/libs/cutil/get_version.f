      FUNCTION GET_VERSION ( )
! ************************************************************************
! *                                                                      *
! *   Progrram  GET_VERSION  returns a line with progran name, program   *
! *   version and comment. F.e.:                                         *
! *   PROC  Ver. 1999.10.11 Test version 1.55                            *
! *                                                                      *
! *   It is assumed that SET_VERSION has been called before the call of  *
! *   GET_VERSION.                                                       *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ###  11-OCT-99  GET_VERSION   v1.0  (d)  L. Petrov  11-OCT-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pvers.i'
      EXTERNAL   INIT_PVERS
      CHARACTER  GET_VERSION*54
!
      GET_VERSION = CUR_VERSION
      RETURN
      END  !#!  GET_VERSION  #!#
#ifdef DARWIN
      INCLUDE    'init_pvers.f'
#endif
