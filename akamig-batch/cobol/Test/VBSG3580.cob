./ ADD NAME=VBSG3580  01.03 20040726 20150511 084426 00298 00314 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBSG3580'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S                             *
      *              <> PG.OUTLINE     : HANBAI KAKAKU FILE(ZENRYOU)   *
      *                                  SAKUSEI                       *
      *                 * INPUT FILE   : HANBAI KAKAKU FILE(HAKFILE)   *
      *                 * INPUT FILE   : KAKAKU KOUSIN DATA(SI.KKD)    *
      *                 * OUTPUT FILE  : HANBAI KAKAKU FILE(ZENRYOU)   *
      *                                                    (DWKA)      *
      *              <> AUTHOR         : CSK-T.SUMI                    *
      *              <> DATE           : 02/12/02                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-704              ÍÝº³   : 04/07/26  CSK-T.SUMI          *
      * (02) K-1700             ÍÝº³   : 15/05/11  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT HAKFILE ASSIGN    TO S-HAKFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(51).
       FD  HAKFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  HAK-REC.
           02 FILLER                        PIC  X(57).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKKD END-EXEC.
       01  KKD REDEFINES DCLKKD.
           COPY VTABKKD SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  P229-PARM.
           COPY VPARB229 SUPPRESS.
       01  HAK-R.
           COPY VTABHKF SUPPRESS.
       01  WKA-R.
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-SYOCD                     PIC  X(09).
           02 WKA-TKYBI                     PIC  X(08).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-HTANK                     PIC S9(09)  COMP-3.
           02 WKA-SMHBK                     PIC S9(09)  COMP-3.
           02 WKA-KEIKB                     PIC  X(01).
           02 WKA-KEINO                     PIC  X(10).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-SYOKB                     PIC  X(01).
           02 WKA-KNSKB                     PIC  X(01).
           02 WKA-KKHKB                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-TOKCD                 PIC  X(07).
              03 WIN1-SYOCD                 PIC  X(09).
           02 WIN2-KEY.
              03 WIN2-TOKCD                 PIC  X(07).
              03 WIN2-SYOCD                 PIC  X(09).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-DATE                     PIC  X(08).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE AND
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-DATE TO P229-DATE.
           MOVE 1         TO P229-YOKU.
           MOVE 0         TO P229-LOADS.
           CALL 'VSBA2290' USING P229-PARM.
           MOVE P229-DATE TO WED1-DATE.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  HAKFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-HAK THRU M11-EXIT.
           PERFORM M12-OPEN-KKD THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   KKD'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ HAKFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-HAK.
           READ HAKFILE INTO HAK-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE TOKCD OF HAK-R TO WIN1-TOKCD.
           MOVE SYOCD OF HAK-R TO WIN1-SYOCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : OPEN-KKD                                             *
      *----------------------------------------------------------------*
       M12-OPEN-KKD.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE          END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M12-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M12-ERR     END-EXEC.
           EXEC SQL DECLARE CSR2 CURSOR FOR
              SELECT *
              FROM    KKD
              WHERE A5 > :WED1-DATE
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR2 END-EXEC.
           PERFORM M121-FETCH-KKD THRU M121-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M12-L01.
           GO TO M12-EXIT.
       M12-L01.
           DISPLAY 'M VBSG3580 TABLE(SI.KKD) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M12-EXIT.
       M12-ERR.
           DISPLAY 'R VBSG3580 TABLE(SI.KKD) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : FETCH-KKD                                          *
      *----------------------------------------------------------------*
       M121-FETCH-KKD.
           EXEC SQL WHENEVER NOT FOUND  GO TO M121-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M121-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M121-ERR END-EXEC.
           EXEC SQL FETCH CSR2
              INTO :DCLKKD
           END-EXEC.
           MOVE TOKCD OF KKD TO WIN2-TOKCD.
           MOVE SYOCD OF KKD TO WIN2-SYOCD.
           GO TO M121-EXIT.
       M121-L01.
           MOVE HIGH-VALUE TO WIN2-KEY.
           GO TO M121-EXIT.
       M121-ERR.
           DISPLAY 'R VBSG3580 TABLE(SI.KKD) FETCH  ERROR !'
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
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-HAKONL THRU M22-EXIT
              ELSE
                 PERFORM M23-KKDONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M211-WRITE-WKA THRU M211-EXIT.
           PERFORM M212-EDIT-WKA  THRU M212-EXIT.
           IF KETKB OF KKD = SPACE
              MOVE KETKB OF HAK-R TO WKA-KETKB
           ELSE
              MOVE KETKB OF KKD TO WKA-KETKB.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-HAK THRU M11-EXIT.
           PERFORM M121-FETCH-KKD THRU M121-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKAFILE                                      *
      *----------------------------------------------------------------*
       M211-WRITE-WKA.
           MOVE SPACE TO WKA-R.
           INITIALIZE    WKA-R.
           MOVE TOKCD OF HAK-R TO WKA-TOKCD.
           MOVE SYOCD OF HAK-R TO WKA-SYOCD.
           MOVE ZERO           TO WKA-TKYBI.
           MOVE KETKB OF HAK-R TO WKA-KETKB.
           MOVE HTANK OF HAK-R TO WKA-HTANK.
           MOVE SMHBK OF HAK-R TO WKA-SMHBK.
           MOVE KEIKB OF HAK-R TO WKA-KEIKB.
           MOVE KEINO OF HAK-R TO WKA-KEINO.
           MOVE NOSCD OF HAK-R TO WKA-NOSCD.
           MOVE SYOKB OF HAK-R TO WKA-SYOKB.
           MOVE '1'            TO WKA-KKHKB.
           WRITE WKA-REC FROM WKA-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : EDIT WKAFILE                                       *
      *----------------------------------------------------------------*
       M212-EDIT-WKA.
           MOVE SPACE TO WKA-R.
           INITIALIZE    WKA-R.
           MOVE TOKCD OF KKD TO WKA-TOKCD.
           MOVE SYOCD OF KKD TO WKA-SYOCD.
           MOVE TKYBI OF KKD TO WKA-TKYBI.
           MOVE HTANK OF KKD TO WKA-HTANK.
           MOVE KEIPS OF KKD TO WKA-SMHBK.
           MOVE KEIKB OF KKD TO WKA-KEIKB.
           MOVE KEINO OF KKD TO WKA-KEINO.
           MOVE NOSCD OF KKD TO WKA-NOSCD.
           MOVE '1'          TO WKA-SYOKB.
           MOVE KNSKB OF KKD TO WKA-KNSKB.
           MOVE '2'          TO WKA-KKHKB.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : HAKFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-HAKONL.
           PERFORM M211-WRITE-WKA THRU M211-EXIT.
           PERFORM M11-READ-HAK THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : KKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-KKDONL.
           PERFORM M212-EDIT-WKA THRU M212-EXIT.
           IF KETKB OF KKD = SPACE
              MOVE '1'          TO WKA-KETKB
           ELSE
              MOVE KETKB OF KKD TO WKA-KETKB.
           IF KEIKB OF KKD = SPACE
              MOVE '0'          TO WKA-KEIKB.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M121-FETCH-KKD THRU M121-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 HAKFILE.
           PERFORM M31-CLOSE-KKD THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KKD                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KKD.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR2 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBSG3580 TABLE(SI.KKD) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBSG3580 END                                         *
      *----------------------------------------------------------------*
