./ ADD NAME=VBKF0760  10.03 20000809 20120711 230333 00450 00448 MCSK082
       ID  DIVISION.
       PROGRAM-ID.     'VBKF0760'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : TOKUISAKI JOHO DATA SAKUSEI   *
      *                                                                *
      *                 * INPUT FILE   : SYSIN                         *
      *                                : TOKUISAKI DATA      (DWKA)    *
      *                                : KAISYU JOHO MASTER  (SI.KJH)  *
      *                                : SYUUKIN DAIKOU GAISYA MASTER  *
      *                                                      (SI.SKD)  *
      *                 * OUTPUT FILE  : TOKUISAKI JOHO DATA (DWKB)    *
      *                                                                *
      *              <> AUTHOR         : CSK-N.UEMURA                  *
      *              <> DATE           : 99/03/08                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-443            ØºÝÊß²Ù  : 00/08/09  CSK-K.ITO           *
      * (02) H-010  O-8179      ÍÝº³   : 12/07/10  CSK-K.KOYAMA        *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(20).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(40).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKJH END-EXEC.
       01  KJH REDEFINES DCLKJH.
           COPY VTABKJH SUPPRESS.
           EXEC SQL INCLUDE VDCLSKD END-EXEC.
       01  SKD REDEFINES DCLSKD.
           COPY VTABSKD SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  P331-PARM.
           COPY VPARB331 SUPPRESS.
       01  WKA-R.
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-STKCD                     PIC  X(07).
           02 WKA-SHIME                     PIC  9(02).
           02 FILLER                        PIC  X(02).
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-STKCD                     PIC  X(07).
           02 WKB-KSSBT                     PIC  X(02).
           02 WKB-KSJK                      PIC  X(01).
           02 WKB-SKDKCD                    PIC  X(04).
           02 WKB-KZHBI                     PIC  9(08).
           02 WKB-KFTKB                     PIC  X(01).
           02 FILLER                        PIC  X(08).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-SHIFT.
              03 WED1-SHIFTR                PIC  9(02).
           02 WED1-YYMM                     PIC  9(06).
           02 WED1-YYMMR REDEFINES WED1-YYMM.
              03 WED1-YY                    PIC  9(04).
              03 WED1-MM                    PIC  9(02).
       01  COUNT-AREA.
           02 J                             PIC S9(04) COMP.
       01  TABLE-AREA.
           02 WTB1-SYSIN OCCURS 100.
              03 WTB1-SKDKCD                PIC  X(04).
              03 WTB1-SHIFT.
                 04 WTB1-SHIFTR             PIC  9(02).
       01  SYSIN-AREA.
           02 SYS-CARD1.
              03 SYS-SKDKCD                 PIC  X(04).
              03 FILLER                     PIC  X(01).
              03 SYS-SHIFT.
                 04 SYS-SHIFTR              PIC  9(02).
              03 FILLER                     PIC  X(73).
           02 SYS-REDE1           REDEFINES SYS-CARD1.
              03 SYS-ENDCD                  PIC  X(03).
              03 FILLER                     PIC  X(77).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE OR
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           MOVE LOW-VALUE    TO TABLE-AREA.
           CALL 'VSBA2280' USING P228-PARM.
           PERFORM M11-SYSIN1   THRU M11-EXIT
              VARYING J FROM 1 BY 1
              UNTIL (SYS-ENDCD = 'END').
       M1-L01.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
           PERFORM M12-READ-WKA THRU M12-EXIT.
           PERFORM M13-OPEN-KJH THRU M13-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   KJH'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE '   SKD'   TO P89-TBLID(2).
           MOVE 'S'        TO P89-LTYPE(2).
           MOVE ALL '*'    TO P89-TABLE(3).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SYSIN1                                               *
      *----------------------------------------------------------------*
       M11-SYSIN1.
           MOVE LOW-VALUE    TO SYS-CARD1.
           ACCEPT SYS-CARD1.
       M11-L01.
           IF SYS-CARD1 = LOW-VALUE
              DISPLAY 'R VBKF0760 SYSIN END NASI ! '
              UPON CONSOLE
              CALL 'VSBABEND'.
       M11-L02.
           IF SYS-ENDCD = 'END'
              GO TO M11-EXIT.
       M11-L03.
           IF J > 100
              DISPLAY 'R VBKF0760 SYSIN OVER ! '
              UPON CONSOLE
              CALL 'VSBABEND'.
       M11-L04.
           IF SYS-SHIFT NOT NUMERIC
              DISPLAY 'R VBKF0760 SYSIN ERROR ! '
              UPON CONSOLE
              CALL 'VSBABEND'.
       M11-L05.
           MOVE SYS-SKDKCD   TO WTB1-SKDKCD(J).
           MOVE SYS-SHIFT    TO WTB1-SHIFT(J).
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M12-EXIT.
           MOVE WKA-TOKCD TO WIN1-TOKCD.
           MOVE WKA-NOSCD TO WIN1-NOSCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3 : OPEN-KJH                                             *
      *----------------------------------------------------------------*
       M13-OPEN-KJH.
           EXEC SQL WHENEVER SQLERROR   GO TO M13-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M13-ERR END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1,A2,A3,A4,A5,B5,C8
              FROM    KJH
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M131-FETCH-KJH THRU M131-EXIT.
           IF WIN2-KEY = HIGH-VALUE
              GO TO M13-L01.
           GO TO M13-EXIT.
       M13-L01.
           DISPLAY 'M VBKF0760 TABLE(SI.KJH) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M13-EXIT.
       M13-ERR.
           DISPLAY 'R VBKF0760 TABLE(SI.KJH) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3.1 : FETCH-KJH                                          *
      *----------------------------------------------------------------*
       M131-FETCH-KJH.
           EXEC SQL WHENEVER NOT FOUND  GO TO M131-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M131-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M131-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLKJH.A0,:DCLKJH.A1,:DCLKJH.A2,:DCLKJH.A3,
                   :DCLKJH.A4,:DCLKJH.A5,:DCLKJH.B5,:DCLKJH.C8
           END-EXEC.
           MOVE TOKCD OF KJH TO WIN2-TOKCD.
           MOVE NOSCD OF KJH TO WIN2-NOSCD.
           GO TO M131-EXIT.
       M131-L01.
           MOVE HIGH-VALUE TO WIN2-KEY.
           GO TO M131-EXIT.
       M131-ERR.
           DISPLAY 'R VBKF0760 TABLE(SI.KJH) FETCH ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M131-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-KJHONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           IF (KDTKCD OF KJH NOT = SPACE) AND
              ((TOKCD OF KJH NOT = KDTKCD OF KJH) OR
               (NOSCD OF KJH NOT = SPACE))
              PERFORM M211-SELECT-KJH THRU M211-EXIT
              IF DCLKJH = HIGH-VALUE
                 GO TO M21-L02.
       M21-L01.
           IF KSSBT OF KJH = '04'
              PERFORM M212-WRITE-WKB  THRU M212-EXIT.
       M21-L02.
           PERFORM M12-READ-WKA  THRU M12-EXIT.
           PERFORM M131-FETCH-KJH THRU M131-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SELECT-KJH                                         *
      *----------------------------------------------------------------*
       M211-SELECT-KJH.
           EXEC SQL WHENEVER NOT FOUND  GO TO M211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           MOVE KDTKCD OF KJH TO TOKCD OF KJH.
           MOVE SPACE         TO NOSCD OF KJH.
           EXEC SQL SELECT A2,A4,B5,C8
              INTO :DCLKJH.A2,:DCLKJH.A4,:DCLKJH.B5,:DCLKJH.C8
              FROM    KJH
              WHERE A0=:DCLKJH.A0
                AND A1=:DCLKJH.A1
           END-EXEC.
           GO TO M211-EXIT.
       M211-L01.
           MOVE HIGH-VALUE    TO DCLKJH.
           GO TO M211-EXIT.
       M211-ERR.
           DISPLAY 'R VBKF0760 TABLE(SI.KJH) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE-WKB                                          *
      *----------------------------------------------------------------*
       M212-WRITE-WKB.
           MOVE SPACE         TO WKB-R.
           INITIALIZE WKB-R.
       M212-L01.
           MOVE WKA-TOKCD     TO WKB-TOKCD.
           MOVE WKA-NOSCD     TO WKB-NOSCD.
           MOVE WKA-STKCD     TO WKB-STKCD.
           MOVE KSSBT  OF KJH TO WKB-KSSBT.
           MOVE KSJK   OF KJH TO WKB-KSJK.
           MOVE SKDKCD OF KJH TO WKB-SKDKCD.
           PERFORM M2121-SET-KZHBI THRU M2121-EXIT.
           MOVE KFTKB  OF KJH TO WKB-KFTKB.
           WRITE WKB-REC FROM WKB-R.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1 : SET-KZHBI                                        *
      *----------------------------------------------------------------*
       M2121-SET-KZHBI.
           PERFORM M21211-SELECT-SKD THRU M21211-EXIT.
           IF DCLSKD = HIGH-VALUE OR
              ( HAYMD OF SKD NOT = ZERO )
              GO TO M2121-EXIT
           END-IF.
           MOVE SPACE        TO WED1-SHIFT.
           MOVE ZERO         TO J.
           PERFORM M21212-SEARCH-SKDKCD THRU M21212-EXIT
              UNTIL (WED1-SHIFT NOT = SPACE) OR
                    (J NOT < 100).
           IF WED1-SHIFT = SPACE
              PERFORM M21213-SET-SHIFT THRU M21213-EXIT.
       M2121-L01.
           MOVE P228-YYMM    TO WED1-YYMM.
           COMPUTE WED1-MM = WED1-MM + WED1-SHIFTR.
       M2121-L02.
           IF WED1-MM > 12
              COMPUTE WED1-YY = WED1-YY + 1
              COMPUTE WED1-MM = WED1-MM - 12
              GO TO M2121-L02.
       M2121-L03.
           MOVE WED1-YYMM    TO P331-SYMDO.
           MOVE WKB-SKDKCD   TO P331-SKDCD.
           CALL 'VSBA3310' USING P331-PARM.
           MOVE P331-KZHBI   TO WKB-KZHBI.
       M2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1.1 : SELECT-SKD                                     *
      *----------------------------------------------------------------*
       M21211-SELECT-SKD.
           EXEC SQL WHENEVER NOT FOUND  GO TO M21211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M21211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M21211-ERR END-EXEC.
           MOVE WKB-SKDKCD TO SKDKCD OF SKD.
           EXEC SQL SELECT A0,B9
              INTO :DCLSKD.A0,:DCLSKD.B9
              FROM    SKD
              WHERE A0=:DCLSKD.A0
           END-EXEC.
           GO TO M21211-EXIT.
       M21211-L01.
           MOVE HIGH-VALUE TO DCLSKD.
           GO TO M21211-EXIT.
       M21211-ERR.
           DISPLAY 'R VBKF0760 TABLE(SI.SKD) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M21211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1.2 : SEARCH-SKDKCD                                  *
      *----------------------------------------------------------------*
       M21212-SEARCH-SKDKCD.
           ADD 1 TO J.
           IF WKB-SKDKCD = WTB1-SKDKCD(J)
              MOVE WTB1-SHIFTR(J) TO WED1-SHIFTR
              GO TO M21212-EXIT.
       M21212-L01.
           IF WTB1-SYSIN(J) = LOW-VALUE
              MOVE 100       TO J.
       M21212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1.3 : SET-SHIFT                                      *
      *----------------------------------------------------------------*
       M21213-SET-SHIFT.
           IF WKA-SHIME = 31
              MOVE ZERO TO WED1-SHIFTR
              GO TO M21213-EXIT
           END-IF.
           IF KSYTB OF KJH NOT > WKA-SHIME
              MOVE 1    TO WED1-SHIFTR
           ELSE
              MOVE ZERO TO WED1-SHIFTR
           END-IF.
       M21213-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : KJHFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-KJHONL.
           PERFORM M131-FETCH-KJH THRU M131-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
           PERFORM M31-CLOSE-KJH THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KJH                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KJH.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKF0760 TABLE(SI.KJH) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF0760 END                                           *
      *----------------------------------------------------------------*
