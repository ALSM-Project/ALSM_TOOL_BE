./ ADD NAME=VBKE1250  01.11 20171225 20180117 084625 00377 00348 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBKE1250'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYU YOTEI KINGAKU          *
      *                                     BUNKATU SYORI              *
      *                                                                *
      *                 * INPUT FILE   : KAISYU YOTEI FILE DATA  (DWKC)*
      *                                : FURIKOMI TESURYO MASTER       *
      *                                                        (SI.FTM)*
      *                 * OUTPUT FILE  : KAISYU YOTEI FILE DATA  (DWKA)*
      *                                                                *
      *              <> AUTHOR         : CSK-N.EGUCHI                  *
      *              <> DATE           : 17/12/25                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(174).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(110).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLFTM END-EXEC.
       01  FTM REDEFINES DCLFTM.
           COPY VTABFTM  SUPPRESS.
       01  WKA-R.
           COPY VTABKYF  SUPPRESS.
       01  P89-PARM.
           COPY VPARB89  SUPPRESS.
       01  PB3-PARM.
           COPY VPARBB3  SUPPRESS.
       01  PC6-PARM.
           COPY VPARBC6  SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKC-R.
           COPY VTABKYF  SUPPRESS.
           02 WKC-SHIME                     PIC  9(02).
           02 WKC-TMSEI                     PIC S9(13)  COMP-3.
           02 WKC-TMUGK                     PIC S9(13)  COMP-3.
           02 WKC-SKNBHS                    PIC  X(01).
           02 WKC-KFTKB                     PIC  X(01).
           02 WKC-KFSSB                     PIC  X(01).
           02 WKC-SKFKSB                    PIC  X(01).
           02 WKC-KSYSJ                     PIC  X(44).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
           02 WIN1-KSYBI.
              03 WIN1-KSYYM                 PIC  X(06).
              03 WIN1-KSYDD                 PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-TOKCD                 PIC  X(07).
              03 WSV1-NOSCD                 PIC  X(02).
       01  EDIT-AREA.
           02 WED1-LOW.
              03 WED1-LOW7                  PIC  X(07).
              03 WED1-LOW2                  PIC  X(02).
           02 WED1-HIGH.
              03 WED1-HIGH7                 PIC  X(07).
              03 WED1-HIGH2                 PIC  X(02).
           02 WED1-DATE.
              03 WED1-SYSYM2.
                 04 WED1-SYSYY              PIC  9(04).
                 04 WED1-SYSMM              PIC  9(02).
              03 WED1-START.
                 04 WED1-STRYM.
                    05 WED1-STRYY           PIC  9(04).
                    05 WED1-STRMM           PIC  9(02).
                 04 WED1-STRDD              PIC  9(02).
           02 WED1-KEISAN.
              03 WED1-SYO                   PIC S9(11) COMP-3.
              03 WED1-AMARI                 PIC S9(11) COMP-3.
              03 WED1-KYTGS                 PIC S9(11) COMP-3.
           02 WED1-SQLCD                    PIC  ---9.
       01  COUNT-AREA.
           02 WCT1-KENSU                    PIC S9(04) COMP.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN1-KEY = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  WKCFILE
                OUTPUT WKAFILE.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE ZERO          TO PB3-LDFLG.
           MOVE ZERO          TO PC6-LDFLG.
           PERFORM M11-READ-WKC THRU M11-EXIT.
           PERFORM M12-OPEN-FTM THRU M12-EXIT.
       M1-L01.
           IF P228-MM > 10
              COMPUTE WED1-SYSYY = P228-YY + 1
              COMPUTE WED1-SYSMM = P228-MM - 10
           ELSE
              MOVE P228-YY    TO WED1-SYSYY
              COMPUTE WED1-SYSMM = P228-MM + 2.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   FTM'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE TOKCD OF WKC-R     TO WIN1-TOKCD.
           MOVE NOSCD OF WKC-R     TO WIN1-NOSCD.
           MOVE KSYBI OF WKC-R     TO WIN1-KSYBI.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : OPEN-FTM                                             *
      *----------------------------------------------------------------*
       M12-OPEN-FTM.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M12-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M12-ERR END-EXEC.
           MOVE LOW-VALUE     TO WED1-LOW.
           MOVE HIGH-VALUE    TO WED1-HIGH.
           MOVE '2'           TO DATKB OF FTM.
           MOVE ZERO          TO SEQ   OF FTM.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1,B1
              FROM    FTM
              WHERE A0 BETWEEN :WED1-LOW7 AND :WED1-HIGH7
                AND A1 BETWEEN :WED1-LOW2 AND :WED1-HIGH2
                AND A2=:DCLFTM.A2
                AND A3=:DCLFTM.A3
              ORDER BY A0,A1,A2,A3
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M121-FETCH-FTM THRU M121-EXIT.
           IF WIN2-KEY = HIGH-VALUE
              GO TO M12-L01.
           GO TO M12-EXIT.
       M12-L01.
           DISPLAY 'M VBKE1250 TABLE(SI.FTM) DATA 0 KEN !?'
           UPON CONSOLE.
           GO TO M12-EXIT.
       M12-ERR.
           DISPLAY 'R VBKE1250 TABLE(SI.FTM) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2.1 : FETCH-FTM                                          *
      *----------------------------------------------------------------*
       M121-FETCH-FTM.
           EXEC SQL WHENEVER NOT FOUND  GO TO M121-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M121-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M121-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLFTM.A0,:DCLFTM.A1,:DCLFTM.B1
           END-EXEC.
           MOVE TOKCD OF FTM  TO WIN2-TOKCD.
           MOVE NOSCD OF FTM  TO WIN2-NOSCD.
           GO TO M121-EXIT.
       M121-L01.
           MOVE HIGH-VALUE    TO WIN2-KEY.
           GO TO M121-EXIT.
       M121-ERR.
           DISPLAY 'R VBKE1250 TABLE(SI.FTM) FETCH ERROR !'
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
                 PERFORM M22-WKCONL THRU M22-EXIT
              ELSE
                 PERFORM M23-FTMONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M211-WRITE-WKA THRU M211-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           PERFORM M121-FETCH-FTM THRU M121-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE-WKA                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKA.
           IF ( KSSBT  OF WKC-R     = '03'        ) AND
              ( KSJK   OF WKC-R NOT = '5'         ) AND
              ( KYTGF  OF WKC-R     = ZERO        ) AND
              ( WIN1-KSYYM          < WED1-SYSYM2 ) AND
              ( SASYMD OF WKC-R     = P228-DATEX  ) AND
              ( KSYTGB OF FTM   NOT = ZERO        ) AND
              ( KSYTGB OF FTM       < KYTGS OF WKC-R )
              PERFORM M2111-BUNKATU THRU M2111-EXIT
           ELSE
              MOVE WKC-R      TO WKA-R
              WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-WKC     THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : BUNKATU AND WRITE-WKA                            *
      *----------------------------------------------------------------*
       M2111-BUNKATU.
           MOVE P228-YY          TO WED1-STRYY.
           MOVE P228-MM          TO WED1-STRMM.
           IF KSJK OF WKC-R = '2' OR '8'
              COMPUTE WED1-STRMM = WED1-STRMM + (SITTS OF WKC-R - 1)
              IF WED1-STRMM > 12
                 ADD 1           TO WED1-STRYY
                 SUBTRACT 12 FROM WED1-STRMM.
       M2111-L01.
           EVALUATE WKC-SHIME
              WHEN 15    MOVE 16 TO WED1-STRDD,
              WHEN 31    MOVE 1  TO WED1-STRDD,
              WHEN OTHER MOVE 21 TO WED1-STRDD
           END-EVALUATE.
       M2111-L02.
           MOVE WED1-START       TO PB3-SYMDR.
           CALL 'VSBA1130' USING PB3-PARM.
           IF PB3-KYJHS = '1'
              MOVE ZERO          TO PC6-KUBUN
              MOVE 1             TO PC6-EGYNS
              MOVE WED1-START    TO PC6-HZUK1
              CALL 'VSBA1260' USING PC6-PARM
              MOVE PC6-HZUK1     TO WED1-START.
       M2111-L03.
           DIVIDE KYTGS OF WKC-R BY KSYTGB OF FTM GIVING WED1-SYO
                  REMAINDER WED1-AMARI.
           IF WED1-AMARI > ZERO
              ADD 1              TO WED1-SYO.
           MOVE WKC-R            TO WKA-R.
           MOVE ZERO             TO WED1-KYTGS.
       M2111-L04.
           PERFORM VARYING WCT1-KENSU FROM 1 BY 1
              UNTIL WED1-KYTGS = KYTGS OF WKC-R
              IF WCT1-KENSU = 1
                 IF KSYBI OF WKA-R NOT = WED1-START
                    IF WED1-AMARI > ZERO
                       MOVE WED1-AMARI    TO KYTGS OF WKA-R
                    ELSE
                       MOVE KSYTGB OF FTM TO KYTGS OF WKA-R
                    END-IF
                 END-IF
              ELSE
                 MOVE '1'              TO PC6-KUBUN
                 MOVE 1                TO PC6-EGYNS
                 MOVE KSYBI OF WKA-R   TO PC6-HZUK1
                 CALL 'VSBA1260' USING PC6-PARM
                 MOVE PC6-HZUK1        TO KSYBI OF WKA-R
                 IF KSYBI OF WKA-R = WED1-START
                    COMPUTE KYTGS OF WKA-R =
                            KYTGS OF WKC-R - WED1-KYTGS
                 ELSE
                    MOVE KSYTGB OF FTM TO KYTGS OF WKA-R
                 END-IF
              END-IF
              WRITE WKA-REC FROM WKA-R
              ADD KYTGS OF WKA-R       TO WED1-KYTGS
           END-PERFORM.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKCFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKCONL.
           MOVE WIN1-KEY1        TO WSV1-KEY1.
           PERFORM M221-WRITE-WKA THRU M221-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : WRITE-WKA                                          *
      *----------------------------------------------------------------*
       M221-WRITE-WKA.
           MOVE WKC-R      TO WKA-R.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-WKC   THRU M11-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : SI.FTM ONLY                                          *
      *----------------------------------------------------------------*
       M23-FTMONL.
           PERFORM M121-FETCH-FTM THRU M121-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKAFILE.
           PERFORM M31-CLOSE-FTM THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-FTM                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-FTM.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKE1250 TABLE(SI.FTM) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBKE1250 END                                         *
      *----------------------------------------------------------------*
