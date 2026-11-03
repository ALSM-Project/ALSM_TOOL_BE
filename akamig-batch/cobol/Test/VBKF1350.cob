./ ADD NAME=VBKF1350  01.21 20101122 20110222 104733 00369 00347 MCSK040
       ID  DIVISION.
       PROGRAM-ID.     'VBKF1350'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : TORIHIKI GINKOU MASTER        *
      *                                  JYOHOU HONYAKU (KASOU KOUZA)  *
      *                                                                *
      *                 * INPUT FILE   : KASOU KOUZA NUMBER HYOJIYOU   *
      *                                  DATA (SEIKYUSYO)(DWKJ)        *
      *                                : TORIHIKI GINKOU MASTER        *
      *                                                  (SI.TBK)      *
      *                                : GINKOU MASTER   (SI.BKM)      *
      *                 * OUTPUT FILE  : KASOU KOUZA NUMBER HYOJIYOU   *
      *                                  DATA (SEIKYUSYO)(DWKK)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/11/22                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      Õ›∫≥   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       ¬≤∂   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKJFILE ASSIGN    TO S-DWKJ.
           SELECT WKKFILE ASSIGN    TO S-DWKK.
       DATA DIVISION.
       FILE SECTION.
       FD  WKJFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKJ-REC.
           02 FILLER                        PIC  X(70).
       FD  WKKFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKK-REC.
           02 FILLER                        PIC  X(70).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTBK END-EXEC.
       01  TBK REDEFINES DCLTBK.
           COPY VTABTBK SUPPRESS.
           EXEC SQL INCLUDE VDCLBKM END-EXEC.
       01  BKM REDEFINES DCLBKM.
           COPY VTABBKM SUPPRESS.
       01  WKJ-R.
           02 WKJ-SESCD                     PIC  X(07).
           02 WKJ-SQNCD                     PIC  X(02).
           02 WKJ-GHBCD                     PIC  X(04).
           02 WKJ-TRBKJ.
              03 WKJ-KKZNO.
                 04 WKJ-GNSCD               PIC  X(03).
                 04 WKJ-KZANO               PIC  X(07).
              03 WKJ-KOZKJ                  PIC  X(04).
              03 WKJ-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(13).
       01  WKK-R.
           02 WKK-SESCD                     PIC  X(07).
           02 WKK-SQNCD                     PIC  X(02).
           02 WKK-GHBCD                     PIC  X(04).
           02 WKK-TRBKJ.
              03 WKK-KKZNO.
                 04 WKK-GNSCD               PIC  X(03).
                 04 WKK-KZANO               PIC  X(07).
              03 WKK-KOZKJ                  PIC  X(04).
              03 WKK-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(13).
       01  P89-PARM.
           COPY VPARB89  SUPPRESS.
       01  P365-PARM.
           COPY VPARB365 SUPPRESS.
       01  P56-PARM.
           COPY VPARB56  SUPPRESS.
       01  P197-PARM.
           COPY VPARB197 SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-GHBCD              PIC  X(04).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-GHBCD              PIC  X(04).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-GHBCD              PIC  X(04).
       01  EDIT-AREA.
           02 WED1-HJBSJ.
              03 WED1-HJBSJ1                PIC  X(30).
              03 FILLER                     PIC  X(70).
           02 WED1-KZANO                    PIC  X(07).
           02 WED1-KZANOZ                   PIC  ZZZZZZZ.
           02 WED1-SQLCD                    PIC  ---9.
       01  KANJI-AREA.
           02 WKJ1-MS1                      PIC  X(06)
                        VALUE 'ïÅí '.
           02 WKJ1-MS1X             REDEFINES WKJ1-MS1.
              03 FILLER                     PIC  X(01).
              03 WKJ1-MS1Y                  PIC  X(04).
              03 FILLER                     PIC  X(01).
           02 WKJ1-MS2                      PIC  X(06)
                        VALUE 'ìñç¿'.
           02 WKJ1-MS2X             REDEFINES WKJ1-MS2.
              03 FILLER                     PIC  X(01).
              03 WKJ1-MS2Y                  PIC  X(04).
              03 FILLER                     PIC  X(01).
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
           OPEN INPUT  WKJFILE
                OUTPUT WKKFILE.
           PERFORM M11-READ-WKJ THRU M11-EXIT.
           PERFORM M12-OPEN-TBK THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   TBK'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE '   BKM'   TO P89-TBLID(2).
           MOVE 'S'        TO P89-LTYPE(2).
           MOVE ALL '*'    TO P89-TABLE(3).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKJFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKJ.
           READ WKJFILE INTO WKJ-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M11-EXIT.
           MOVE WKJ-GHBCD TO WIN2-GHBCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : OPEN-TBK                                             *
      *----------------------------------------------------------------*
       M12-OPEN-TBK.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M12-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M12-ERR END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1,A2,A3
              FROM    TBK
              ORDER BY A0
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M121-FETCH-TBK THRU M121-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M12-L01.
           GO TO M12-EXIT.
       M12-L01.
           DISPLAY 'M VBKF1350 TABLE(SI.TBK) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M12-EXIT.
       M12-ERR.
           DISPLAY 'R VBKF1350 TABLE(SI.TBK) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : FETCH-TBK                                          *
      *----------------------------------------------------------------*
       M121-FETCH-TBK.
           EXEC SQL WHENEVER NOT FOUND  GO TO M121-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M121-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M121-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLTBK.A0,:DCLTBK.A1,:DCLTBK.A2,
                   :DCLTBK.A3
           END-EXEC.
           MOVE GHBCD OF TBK TO WIN1-GHBCD.
           GO TO M121-EXIT.
       M121-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M121-EXIT.
       M121-ERR.
           DISPLAY 'R VBKF1350 TABLE(SI.TBK) FETCH ERROR !'
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
                 PERFORM M22-TBKONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKJONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE SPACE      TO WED1-HJBSJ.
           MOVE WIN2-GHBCD TO P365-GHCKA.
           CALL 'VSBA3650' USING P365-PARM.
           IF P365-RTNCD NOT = '0'
              DISPLAY 'R VBKF1350 GINKO HOJYOBO CODE ERROR ='
              WIN2-GHBCD
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
           IF P365-GISTH NOT = '2'
              PERFORM M211-SELECT-BKM THRU M211-EXIT
              IF P365-GISTH = '0'
                 MOVE GIKAJ OF BKM  TO P56-KUMKA
                 MOVE GSKAJ OF BKM  TO P56-KUMKB
                 MOVE 1             TO P56-SPACE
                 CALL 'VSBA0560' USING P56-PARM
                 MOVE P56-KUMKR TO WED1-HJBSJ
              ELSE
                 IF P365-GISTH = '1'
                    MOVE P365-SEIGN    TO P56-KUMKA
                    MOVE GSKAJ OF BKM  TO P56-KUMKB
                    MOVE 1             TO P56-SPACE
                    CALL 'VSBA0560' USING P56-PARM
                    MOVE P56-KUMKR TO WED1-HJBSJ
                 END-IF
              END-IF
           END-IF.
           MOVE WIN2-KEY1  TO WSV2-KEY1.
           PERFORM M212-WRITE-WKK THRU M212-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M21-L01.
           PERFORM M121-FETCH-TBK THRU M121-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SELECT BKM                                         *
      *----------------------------------------------------------------*
       M211-SELECT-BKM.
           EXEC SQL WHENEVER NOT FOUND  GO TO M211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           MOVE GNCD1 OF TBK TO GNCD1 OF BKM.
           MOVE GNCD2 OF TBK TO GNCD2 OF BKM.
           EXEC SQL SELECT A3,A5
              INTO :DCLBKM.A3,:DCLBKM.A5
              FROM    BKM
              WHERE A0=:DCLBKM.A0
                AND A1=:DCLBKM.A1
           END-EXEC.
           GO TO M211-EXIT.
       M211-L01.
           DISPLAY 'R VBKF1350 SI.BKM NOT FOUND KEY='
           GNCD1 OF TBK GNCD2 OF TBK
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
           GO TO M211-EXIT.
       M211-ERR.
           DISPLAY 'R VBKF1350 TABLE(SI.BKM) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE-WKK                                          *
      *----------------------------------------------------------------*
       M212-WRITE-WKK.
           MOVE SPACE          TO WKK-R.
           MOVE WKJ-SESCD      TO WKK-SESCD.
           MOVE WKJ-SQNCD      TO WKK-SQNCD.
           MOVE WKJ-GHBCD      TO WKK-GHBCD.
           MOVE WKJ-GNSCD      TO WKK-GNSCD.
           MOVE '3'            TO P197-SORKB.
           MOVE 7              TO P197-EDKET.
           MOVE WKJ-KZANO      TO WED1-KZANOZ.
           MOVE WED1-KZANOZ    TO WED1-KZANO.
           MOVE WED1-KZANO     TO P197-EDKOU.
           CALL 'VSBA1970' USING P197-PARM.
           MOVE P197-EDKOU     TO WKK-KZANO.
           IF YKNSM OF TBK = '1'
              MOVE WKJ1-MS1Y   TO WKK-KOZKJ
           ELSE
              MOVE WKJ1-MS2Y   TO WKK-KOZKJ.
           IF P365-GISTH = '2'
              MOVE P365-SEIGN  TO WKK-HJBSJ
           ELSE
              MOVE WED1-HJBSJ1 TO WKK-HJBSJ.
           WRITE WKK-REC FROM WKK-R.
       M212-L01.
           PERFORM M11-READ-WKJ THRU M11-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : SI.TBK ONLY                                          *
      *----------------------------------------------------------------*
       M22-TBKONL.
           PERFORM M121-FETCH-TBK THRU M121-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKJFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKJONL.
           DISPLAY 'R VBKF1350 DWKJ ONLY KEY=' WIN2-KEY1
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKJFILE
                 WKKFILE.
           PERFORM M31-CLOSE-TBK THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-TBK                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-TBK.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKF1350 TABLE(SI.TBK) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKF1350 END                                           *
      *----------------------------------------------------------------*
