./ ADD NAME=VBKF1330  01.05 20101117 20101118 095225 00258 00315 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1330'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : TOKUISAKI JYOHOU HONYAKU      *
      *                                  (SEIKYUSAKI)                  *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MASTER(SI.TMS)      *
      *                                  KAISYU JYOHOU DATA            *
      *                                : (KASOU KOUZA NUMBER HANEIGO)  *
      *                                                  (DWKG)        *
      *                 * OUTPUT FILE  : KASOU KOUZA JYOHOU            *
      *                                  (SEIKYUSYO)     (DWKH)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/11/17                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKGFILE ASSIGN    TO S-DWKG.
           SELECT WKHFILE ASSIGN    TO S-DWKH.
       DATA DIVISION.
       FILE SECTION.
       FD  WKGFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKG-REC.
           02 FILLER                        PIC  X(40).
       FD  WKHFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKH-REC.
           02 FILLER                        PIC  X(50).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTMS END-EXEC.
       01  TMS REDEFINES DCLTMS.
           COPY VTABTMS SUPPRESS.
       01  WKG-R.
           02 WKG-KDTKCD                    PIC  X(07).
           02 WKG-TOKCD                     PIC  X(07).
           02 WKG-NOSCD                     PIC  X(02).
           02 WKG-KKZNO                     PIC  X(10).
           02 WKG-GHBCD                     PIC  X(04).
           02 FILLER                        PIC  X(10).
       01  WKH-R.
           02 WKH-TOKCD                     PIC  X(07).
           02 WKH-NOSCD                     PIC  X(02).
           02 WKH-KKZNO                     PIC  X(10).
           02 WKH-GHBCD                     PIC  X(04).
           02 WKH-SESCD                     PIC  X(07).
           02 WKH-OYAKO                     PIC  X(01).
           02 WKH-SEBUN                     PIC  X(01).
           02 FILLER                        PIC  X(18).
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-TOKCD              PIC  X(07).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN2-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  WKGFILE
                OUTPUT WKHFILE.
           PERFORM M11-READ-WKG THRU M11-EXIT.
           PERFORM M12-OPEN-TMS THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   TMS'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKGFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKG.
           READ WKGFILE INTO WKG-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M11-EXIT.
           MOVE WKG-TOKCD TO WIN2-TOKCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : OPEN-TMS                                             *
      *----------------------------------------------------------------*
       M12-OPEN-TMS.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M12-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M12-ERR END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A8,C1,D4
              FROM    TMS
              ORDER BY A0
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M12-L01.
           GO TO M12-EXIT.
       M12-L01.
           DISPLAY 'M VBKF1330 TABLE(SI.TMS) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M12-EXIT.
       M12-ERR.
           DISPLAY 'R VBKF1330 TABLE(SI.TMS) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : FETCH-TMS                                          *
      *----------------------------------------------------------------*
       M121-FETCH-TMS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M121-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M121-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M121-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLTMS.A0,:DCLTMS.A8,:DCLTMS.C1,
                   :DCLTMS.D4
           END-EXEC.
           MOVE TOKCD OF TMS TO WIN1-TOKCD.
           GO TO M121-EXIT.
       M121-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M121-EXIT.
       M121-ERR.
           DISPLAY 'R VBKF1330 TABLE(SI.TMS) FETCH ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-TMSONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKGONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1  TO WSV2-KEY1.
           PERFORM M211-WRITE-WKH THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M21-L01.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE-WKH                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKH.
           MOVE SPACE        TO WKH-R.
           MOVE WKG-TOKCD    TO WKH-TOKCD.
           MOVE WKG-NOSCD    TO WKH-NOSCD.
           MOVE WKG-KKZNO    TO WKH-KKZNO.
           MOVE WKG-GHBCD    TO WKH-GHBCD.
           MOVE STKCD OF TMS TO WKH-SESCD.
           MOVE OYAKO OF TMS TO WKH-OYAKO.
           MOVE SEBUN OF TMS TO WKH-SEBUN.
           WRITE WKH-REC   FROM WKH-R.
       M211-L01.
           PERFORM M11-READ-WKG THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : SI.TMS ONLY                                          *
      *----------------------------------------------------------------*
       M22-TMSONL.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKGFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKGONL.
           PERFORM M11-READ-WKG THRU M11-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKGFILE
                 WKHFILE.
           PERFORM M31-CLOSE-TMS THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-TMS                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-TMS.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKF1330 TABLE(SI.TMS) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF1330 END                                           *
      *----------------------------------------------------------------*
