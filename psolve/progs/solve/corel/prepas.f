      SUBROUTINE PREPAS(kref)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      INCLUDE 'corpar.i'
      INCLUDE 'corema.i'
      INCLUDE 'corcom.i'
!
!     modifications
!
!     kdb 960513 Adding a little documentation.  Correcting and expanding
!                two error messages.  Renaming local max_par to
!                max_summed_par to distinguish it from the solve.i max_par.
!
      LOGICAL*2 kref
      CHARACTER*64 ROWNAM,COLNAM
      INTEGER*2 ROWS,COLS,ROWNUM,COLNUM
      INTEGER*4 IPOS,mp
      LOGICAL*2 KEND
      REAL*8 ADJ(MAX_PER),tot(MAX_PER)
!
      ARCS=0
      IPOS=1
      KEND=.FALSE.
      CALL GETHED(KEND,ROWNAM,COLNAM,ROWS,COLS,ROWNUM,COLNUM )
      DO WHILE (.NOT.KEND)
        IF(ROWNUM.EQ.COLNUM) THEN
          ARCS=ARCS+1
          IF(ARCS.GT.MAX_ARC) THEN
            WRITE(LUOP,9910) MAX_ARC
9910        FORMAT('Too many arcs, maximum is ',I10)
            STOP
          ENDIF
!
!         Note:
!         The control file's $output section's covariances keyword sets a
!         list of global and/or arc parameters for which covariances
!         should be generated.
!         This may be STAtion, NUTation, EOP, SOUrce or ALL of the preceding
!         types.
!         The "rows" variable shows the number of parameters from that
!         list that are applicable to the current arc.  KDB 5/13/96
!
          IF(ROWS.GT.MAX_PER) THEN
            WRITE(LUOP,9905) ROWS,ARCS,MAX_PER
9905        &
     &       FORMAT(I10,' covariance parameters in arc ',I5,/, &
     &           'maximum is ',I10)
            WRITE(LUOP,"('See corel/prepas.f for more information.')")
             STOP
          ENDIF
          ARCNUM(ARCS)=ROWNUM
          ARCPOS(ARCS)=IPOS
!         Test to prevent a range error (accessing an invalid parsig slot).
!         This comment was added 5/13/96 by kdb to advise against fulfilling
!         future requests to eliminate this test.
!         (Someone had requested the elimination of this test, apparently
!         not realizing that the max_par in the parsig declaration is the
!         local max_par, and not the solve.i max_par.)
          IF(IPOS+ROWS-1.GT.MAX_SUMMED_PAR) THEN
            mp = MAX_SUMMED_PAR
            WRITE(LUOP,9920) ipos+rows-1,arcs,mp
9920        FORMAT(I10,' covariance parameters as of arc ',I10, &
     &             ',maximum is ',I10)
            STOP
          ENDIF
          CALL GETPAR(KEND,PARNAM,PARSIG(IPOS),ADJ,ROWS,tot,kref )
          IPOS=IPOS+ROWS
          CALL GETTRI(KEND,MAT,ROWS )
        ELSE
          CALL GETREC(KEND,MAT,ROWS,COLS )
        ENDIF
        CALL GETHED(KEND,ROWNAM,COLNAM,ROWS,COLS,ROWNUM,COLNUM )
      ENDDO
!
      CALL REDSIG(PARSIG,IPOS-1 )
      RETURN
      END
