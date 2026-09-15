./ ADD NAME=VBKF1360  01.03 20101202 20101202 095739 00264 00342 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1360'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : TOKUISAKI JYOHOU HONYAKU      *
      *                                  (YUSOUSAKI)                   *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MASTER(SI.TMS)      *
      *                                : KASOU KOUZA JYOHOU DATA       *
      *                                  (SEIKYUGOUKEI HYOU YOU)       *
      *                                                  (DWKF)        *
      *                 * OUTPUT FILE  : KASOU KOUZA JYOHOU WORK       *
      *                                                  (DWKG)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/12/02                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKFFILE ASSIGN    TO S-DWKF.
           SELECT WKGFILE ASSIGN    TO S-DWKG.
       DATA DIVISION.
       FILE SECTION.
       FD  WKFFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKF-REC.
           02 FILLER                        PIC  X(70).
       FD  WKGFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKG-REC.
           02 FILLER                        PIC  X(70).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTMS END-EXEC.
       01  TMS REDEFINES DCLTMS.
           COPY VTABTMS SUPPRESS.
       01  WKF-R.
           02 WKF-SESCD                     PIC  X(07).
           02 WKF-SQNCD                     PIC  X(02).
           02 WKF-GHBCD                     PIC  X(04).
           02 WKF-TRBKJ.
              03 WKF-KKZNO.
                 04 WKF-GNSCD               PIC  X(03).
                 04 WKF-KZANO               PIC  X(07).
              03 WKF-KOZKJ                  PIC  X(04).
              03 WKF-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(13).
       01  WKG-R.
           02 WKG-SESCD                     PIC  X(07).
           02 WKG-SQNCD                     PIC  X(02).
           02 WKG-TRBKJ.
              03 WKG-KKZNO.
                 04 WKG-GNSCD               PIC  X(03).
                 04 WKG-KZANO               PIC  X(07).
              03 WKG-KOZKJ                  PIC  X(04).
              03 WKG-HJBSJ                  PIC  X(30).
           02 WKG-YUSCD                     PIC  X(07).
           02 FILLER                        PIC  X(10).
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SESCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-SESCD              PIC  X(07).
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
           OPEN INPUT  WKFFILE
                OUTPUT WKGFILE.
           PERFORM M11-READ-WKF THRU M11-EXIT.
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
      *    M1.1 : READ WKFFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKF.
           READ WKFFILE INTO WKF-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M11-EXIT.
           MOVE WKF-SESCD TO WIN2-SESCD.
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
              SELECT A0,G9
              FROM    TMS
              ORDER BY A0
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M12-L01.
           GO TO M12-EXIT.
       M12-L01.
           DISPLAY 'M VBKF1360 TABLE(SI.TMS) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M12-EXIT.
       M12-ERR.
           DISPLAY 'R VBKF1360 TABLE(SI.TMS) OPEN ERROR !'
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
              INTO :DCLTMS.A0,:DCLTMS.G9
           END-EXEC.
           MOVE TOKCD OF TMS TO WIN1-TOKCD.
           GO TO M121-EXIT.
       M121-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M121-EXIT.
       M121-ERR.
           DISPLAY 'R VBKF1360 TABLE(SI.TMS) FETCH ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-TMSONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKFONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M211-WRITE-WKG THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M21-L01.
           PERFORM M121-FETCH-TMS THRU M121-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE-WKG                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKG.
           IF SEYCD OF TMS NOT = SPACE
              MOVE SPACE        TO WKG-R
              MOVE WKF-SESCD    TO WKG-SESCD
              MOVE WKF-SQNCD    TO WKG-SQNCD
              MOVE WKF-TRBKJ    TO WKG-TRBKJ
              MOVE SEYCD OF TMS TO WKG-YUSCD
              WRITE WKG-REC   FROM WKG-R.
       M211-L01.
           PERFORM M11-READ-WKF THRU M11-EXIT.
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
      *    M2.3 : WKFFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKFONL.
           DISPLAY 'R VBKF1360 DWKF ONLY KEY=' WIN2-KEY1
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKFFILE
                 WKGFILE.
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
           DISPLAY 'R VBKF1360 TABLE(SI.TMS) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF1360 END                                           *
      *----------------------------------------------------------------*
