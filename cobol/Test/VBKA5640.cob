./ ADD NAME=VBKA5640  01.18 20100622 20100922 100647 00185 00175 MCSK020
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5640'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : SEIKYU YOU TOKUISAKI MEISYOU  *
      *                                  FILE SAKUSEI                  *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MEISYOU FILE        *
      *                                                      ( SI.TMF )*
      *                 * OUTPUT FILE  : TOKUISAKI MEISYOU FILE        *
      *                                                    ( SFMFILE ) *
      *                                                                *
      *              <> AUTHOR         : CSK-M.KODAMA                  *
      *              <> DATE           : 10/06/22                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SFMFILE ASSIGN    TO SFMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-SFM
                          FILE  STATUS S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  SFMFILE
           LABEL RECORD STANDARD.
       01  SFM-REC.
           02 KEY-SFM                       PIC  X(07).
           02 FILLER                        PIC  X(203).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTMF END-EXEC.
       01  TMF REDEFINES DCLTMF.
           COPY VTABTMF  SUPPRESS.
       01  SFM-R.
           COPY VFILSFM  SUPPRESS.
       01  P89-PARM.
           COPY VPARB89  SUPPRESS.
       01  EDIT-AREA.
           02 WED1-SQLCD               PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( DCLTMF = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN OUTPUT SFMFILE.
           PERFORM M11-OPEN-TMF THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   TMF'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : OPEN-TMF                                             *
      *----------------------------------------------------------------*
       M11-OPEN-TMF.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE          END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR     END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1,A2,A3,A4,A5
              FROM    TMF
              ORDER BY A0
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-TMF THRU M111-EXIT.
           IF DCLTMF = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBKA5640 TABLE(SI.TMF) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBKA5640 TABLE(SI.TMF) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : FETCH-TMF                                          *
      *----------------------------------------------------------------*
       M111-FETCH-TMF.
           EXEC SQL WHENEVER NOT FOUND  GO TO M111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M111-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLTMF.A0,:DCLTMF.A1,:DCLTMF.A2,:DCLTMF.A3,
                   :DCLTMF.A4,:DCLTMF.A5
           END-EXEC.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO DCLTMF.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBKA5640 TABLE(SI.TMF) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE  'Z '          TO S-CODE.
           MOVE SPACE          TO SFM-R.
           MOVE TOKCD OF TMF   TO SFM-TOKCD.
           MOVE TMSJU OF TMF   TO SFM-TMSJU.
           MOVE TMSJL OF TMF   TO SFM-TMSJL.
           MOVE TMSKA OF TMF   TO SFM-TMSKA.
           MOVE HJADJ OF TMF   TO SFM-HJADJ.
           MOVE HEYMD OF TMF   TO SFM-HEYMD.
           WRITE SFM-REC FROM SFM-R.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA5640 SFMFILE WRITE ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
                UPON CONSOLE
                PERFORM M3-FINL THRU M3-EXIT
               CALL 'VSBABEND'.
           PERFORM M111-FETCH-TMF THRU M111-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE SFMFILE.
           PERFORM M31-CLOSE-TMF THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-TMF                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-TMF.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKA5640 TABLE(SI.TMF) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBKA5640 END                                         *
      *----------------------------------------------------------------*
