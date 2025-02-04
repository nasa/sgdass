      FUNCTION   SOTID_INQ ( REQUEST, TIDCNF, OUT_STR, OUT_STR_LEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Function SOTID_INQ  makes an inquiry of the current SOTID          *
! *   configuration and returns the status. One inquiry at the time      *
! *   can be made. SOTID_INQ returns the value of the requested          *
! *   parameter as well as the output string with parameter description. *
! *                                                                      *
! * ________________________ Input parameters:  ________________________ *
! *                                                                      *
! *     REQUEST ( INTEGER*4 ) -- Code of the request. The following      *
! *                              symbolic names for a request are        *
! *                              supported:                              *
! *               SOTID__REQ_MODEL_2D -- what is the model for           *
! *                                      computation of the tides of the *
! *                                      second degree.                  *
! *               SOTID__REQ_MODEL_3D -- what is the model for           *
! *                                      computation of the tides of the *
! *                                      third degree.                   *
! *               SOTID__REQ_GEN_LOVE -- which generalized Love numbers  *
! *                                      to take into account.           *
! *               SOTID__REQ_ORDER_2D -- waves of which order, degree 2  *
! *                                      to take into account.           *
! *               SOTID__REQ_ZF_LOVE  -- what is the model for Love      *
! *                                      numbers for the zero frequency. *
! *               SOTID__REQ_N_STA    -- for how many stations STATID    *
! *                                      record was set up.              *
! *               SOTID__REQ_NW_D2    -- how many tidal waves of the     *
! *                                      potential degree 2 are used.    *
! *               SOTID__REQ_NW_D3    -- how many tidal waves of the     *
! *                                      potential degree 3 are used.    *
! *               If the code of thre request is not the one from the    *
! *               list above, then SOTID_INQ will return the answer      *
! *               SOTID__UNDEFINED, "undefined".                         *
! *                                                                      *
! *      TIDCNF ( RECORD    ) -- Object which holds configuration        *
! *                              parameters of SOTID.                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     OUT_STR ( CHARACTER ) -- Descriptive string of the answer to the *
! *                              request.                                *
! * OUT_STR_LEN ( CHARACTER ) -- The effective length of OUT_STR         *
! *                              (position of the last character which   *
! *                               is not a blank).                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 10-JUL-2002   SOTID_INQ   v1.0 (c)  L. Petrov  10-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'sotid_type.i'
      INCLUDE   'sotid_data.i'
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      INTEGER*4  SOTID_INQ
      INTEGER*4  REQUEST, OUT_STR_LEN, IUER
      CHARACTER  OUT_STR*(*)
      CHARACTER  STR*16
      EXTERNAL   ILEN
      INTEGER*4  ILEN
!
      IF ( REQUEST .EQ. SOTID__REQ_GEN_LOVE ) THEN
!
! -------- Return the configuration for the generalized Love numbers
!
           IF ( TIDCNF%GEN_LOVE .GT. 0  .AND. &
     &          TIDCNF%GEN_LOVE .LE. SOTID__MAX_DEF ) THEN
!
                SOTID_INQ = TIDCNF%GEN_LOVE
                OUT_STR = SOTID__DSC(SOTID_INQ)
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
              ELSE
                SOTID_INQ = SOTID__UNDEFINED
                OUT_STR = 'Undefined'
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
           END IF
        ELSE IF ( REQUEST .EQ. SOTID__REQ_MODEL_2D ) THEN
!
! -------- Return the model for Love numbers of the 2-nd degree
!
! -------- Check whether the value TIDCNF.MODEL_2D is in
! -------- the range of allowed vlaues
!
           IF ( TIDCNF%MODEL_2D .GT. 0  .AND. &
     &          TIDCNF%MODEL_2D .LE. SOTID__MAX_DEF ) THEN
!
                SOTID_INQ = TIDCNF%MODEL_2D
                OUT_STR = SOTID__DSC(SOTID_INQ)
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
              ELSE
                SOTID_INQ = SOTID__UNDEFINED
                OUT_STR = 'Undefined'
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
           END IF
         ELSE IF ( REQUEST .EQ. SOTID__REQ_ORDER_2D ) THEN
!
! -------- Return the configuration for the order2 degree 2 potiential
!
           IF ( TIDCNF%ORDER_2D .GT. 0  .AND. &
     &          TIDCNF%ORDER_2D .LE. SOTID__MAX_DEF ) THEN
!
                SOTID_INQ = TIDCNF%ORDER_2D
                OUT_STR = SOTID__DSC(SOTID_INQ)
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
              ELSE
                SOTID_INQ = SOTID__UNDEFINED
                OUT_STR = 'Undefined'
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
           END IF
         ELSE IF ( REQUEST .EQ. SOTID__REQ_ZF_LOVE  ) THEN
!
! -------- Return the model for Love numbers of the zeroth frequency
!
           IF ( TIDCNF%ZF_LOVE .GT. 0  .AND. &
     &          TIDCNF%ZF_LOVE .LE. SOTID__MAX_DEF ) THEN
!
                SOTID_INQ = TIDCNF%ZF_LOVE
                OUT_STR = SOTID__DSC(SOTID_INQ)
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
              ELSE
                SOTID_INQ = SOTID__UNDEFINED
                OUT_STR = 'Undefined'
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
           END IF
         ELSE IF ( REQUEST .EQ. SOTID__REQ_MODEL_3D ) THEN
!
! -------- Return the model for Love numbers of the 3-rd degree
!
           IF ( TIDCNF%MODEL_3D .GT. 0  .AND. &
     &          TIDCNF%MODEL_3D .LE. SOTID__MAX_DEF ) THEN
!
                SOTID_INQ = TIDCNF%MODEL_3D
                OUT_STR = SOTID__DSC(SOTID_INQ)
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
              ELSE
                SOTID_INQ = SOTID__UNDEFINED
                OUT_STR = 'Undefined'
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
           END IF
         ELSE IF ( REQUEST .EQ. SOTID__REQ_N_STA    ) THEN
!
! -------- Returne the number of stations
!
           IF ( TIDCNF%N_STA .GT. 0  .AND. &
     &          TIDCNF%N_STA .LE. SOTID__MAX_STA ) THEN
!
                SOTID_INQ = TIDCNF%N_STA
                CALL CLRCH ( STR )
                WRITE ( UNIT=OUT_STR, FMT='(I8)' ) TIDCNF%N_STA
                CALL CHASHL ( OUT_STR )
                OUT_STR = 'Number of stations: '//OUT_STR
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
              ELSE
                SOTID_INQ = SOTID__UNDEFINED
                OUT_STR = 'Undefined'
                OUT_STR_LEN = ILEN(OUT_STR)
                CALL ERR_LOG ( 0, IUER, ' ', ' ' )
                RETURN
           END IF
         ELSE IF ( REQUEST .EQ. SOTID__REQ_NW_D2 ) THEN
!
! -------- Return the number of waves of degree 2
!
           SOTID_INQ = TIDCNF%N_STA
           CALL CLRCH ( STR )
           WRITE ( UNIT=OUT_STR, FMT='(I8)' ) NW
           CALL CHASHL ( OUT_STR )
           OUT_STR = 'Number of tidal waves of potential degree 2: '//OUT_STR
           OUT_STR_LEN = ILEN(OUT_STR)
           CALL ERR_LOG ( 0, IUER, ' ', ' ' )
           RETURN
         ELSE IF ( REQUEST .EQ. SOTID__REQ_NW_D3 ) THEN
!
! -------- Return the number of waves of degree 3
!
           SOTID_INQ = TIDCNF%N_STA
           CALL CLRCH ( STR )
           WRITE ( UNIT=OUT_STR, FMT='(I8)' ) NW3
           CALL CHASHL ( OUT_STR )
           OUT_STR = 'Number of tidal waves of potential degree 3: '//OUT_STR
           OUT_STR_LEN = ILEN(OUT_STR)
           CALL ERR_LOG ( 0, IUER, ' ', ' ' )
           RETURN
         ELSE
!
! -------- Unrecognized request
!
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) REQUEST
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5721, IUER, 'SOTID_INQ', 'Invalid request. '// &
     &         'REQUEST = '//STR )
           OUT_STR_LEN = ILEN(OUT_STR)
           SOTID_INQ = SOTID__UNDEFINED
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  SOTID_INQ  #!#
