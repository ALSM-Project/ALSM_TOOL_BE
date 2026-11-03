./ ADD NAME=VBKE0120  01.21 20130607 20180111 103328 01202 00970 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBKE0120'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KAISYUU YOTEIGAKU SANSYUTU    *
      *                                                                *
      *                 * INPUT FILE   : KAISYU YOTEI FILE    (SI.KYF) *
      *                                : KAISYU YOTEI DATA      (DWKC) *
      *                 * OUTPUT FILE  : KAISYU YOTEI FILE DATA (DWKA) *
      *                                : SYOKAI SIKIBETU DATA   (DWKB) *
      *                                                                *
      *              <> AUTHOR         : CSK-K.INUKAI                  *
      *              <> DATE           : 98/11/18                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-353  S-050       ÍÝº³   : 98/12/24  CSK-K.INUKAI        *
      * (02) K-444               Â²¶   : 00/08/24  CSK-H.ISHII         *
      * (03) K-444              ÍÝº³   : 00/09/26  CSK-H.ISHII         *
      * (04) K-444  C-1017       Â²¶   : 00/11/09  CSK-H.ISHII         *
      * (05) H-010  C-1063      ÍÝº³   : 01/02/05  CSK-H.ISHII         *
      * (06) K-491  S-0082       Â²¶   : 02/02/20  CSK-M.UEMURA        *
      * (07) H-010  T-7013       Â²¶   : 13/06/07  CSK-Y.MIO           *
      * (08) K-2023             ÍÝº³   : 17/12/14  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(174).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(09).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(174).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKYF END-EXEC.
       01  KYF REDEFINES DCLKYF.
           COPY VTABKYF SUPPRESS.
           EXEC SQL INCLUDE VDCLFTM END-EXEC.
       01  FTM REDEFINES DCLFTM.
           COPY VTABFTM SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  P296-PARM.
           COPY VPARB296 SUPPRESS.
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
       01  WKC-R.
           COPY VTABKYF SUPPRESS.
           02 WKC-SHIME                     PIC  9(02).
           02 WKC-TMSEI                     PIC S9(13)  COMP-3.
           02 WKC-TMUGK                     PIC S9(13)  COMP-3.
           02 WKC-SKNBHS                    PIC  X(01).
           02 WKC-KFTKB                     PIC  X(01).
           02 WKC-KFSSB                     PIC  X(01).
           02 WKC-SKFKSB                    PIC  X(01).
           02 WKC-KSYSJ.
              03 WKC-TEIGK                  PIC S9(11)   COMP-3.
              03 WKC-KKASI                  PIC  9(06).
              03 WKC-KKIGN                  PIC  9(06).
              03 WKC-KEISN1                 PIC  X(01).
              03 WKC-KEISN2                 PIC  X(01).
              03 WKC-SJTRB                  PIC  9(08).
              03 WKC-SJHNB                  PIC  9(08).
              03 WKC-SJHSB                  PIC  9(08).
       01  WKC-R-X      REDEFINES WKC-R.
           02 WKC-KYF                       PIC  X(110).
           02 FILLER                        PIC  X(64).
       01  WKA-R.
           COPY VTABKYF SUPPRESS.
           02 WKA-SHIME                     PIC  9(02).
           02 WKA-TMSEI                     PIC S9(13)  COMP-3.
           02 WKA-TMUGK                     PIC S9(13)  COMP-3.
           02 WKA-SKNBHS                    PIC  X(01).
           02 WKA-KFTKB                     PIC  X(01).
           02 WKA-KFSSB                     PIC  X(01).
           02 WKA-SKFKSB                    PIC  X(01).
           02 WKA-KSYSJ.
              03 WKA-TEIGK                  PIC S9(11)   COMP-3.
              03 WKA-KKASI                  PIC  9(06).
              03 WKA-KKIGN                  PIC  9(06).
              03 WKA-KEISN1                 PIC  X(01).
              03 WKA-KEISN2                 PIC  X(01).
              03 WKA-SJTRB                  PIC  9(08).
              03 WKA-SJHNB                  PIC  9(08).
              03 WKA-SJHSB                  PIC  9(08).
       01  WKA-R-X      REDEFINES WKA-R.
           02 WKA-KYF                       PIC  X(110).
           02 FILLER                        PIC  X(64).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
                 04 WIN2-NOSCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-TOKCD                 PIC  X(07).
              03 WSV1-NOSCD                 PIC  X(02).
           02 WSV1-KSYBI                    PIC  X(08).
           02 WSV1-KFKTF                    PIC  X(01).
       01  EDIT-AREA.
           02 WED1-LOW.
              03 WED1-TOKCDL                PIC  X(07).
              03 WED1-NOSCDL                PIC  X(02).
              03 WED1-KSYBIL                PIC  X(08).
              03 WED1-KSSBTL                PIC  X(02).
              03 WED1-TYTKJL                PIC  X(08).
              03 WED1-RECKBL                PIC  X(01).
           02 WED1-HIGH.
              03 WED1-TOKCDH                PIC  X(07).
              03 WED1-NOSCDH                PIC  X(02).
              03 WED1-KSYBIH                PIC  X(08).
              03 WED1-KSSBTH                PIC  X(02).
              03 WED1-TYTKJH                PIC  X(08).
              03 WED1-RECKBH                PIC  X(01).
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-SMEBI.
              03 WED1-SYM                   PIC  9(06).
              03 WED1-SHIME                 PIC  9(02).
           02 WED1-KYTGS                    PIC S9(13)  COMP-3.
           02 WED1-PTKGK                    PIC S9(13)V9  COMP-3.
           02 WED1-PTKGK2                   PIC S9(13)  COMP-3.
           02 WED1-OKAGE                    PIC S9(11)  COMP-3.
           02 WED1-KSYGK                    PIC S9(11)  COMP-3.
           02 WED1-SAGAK                    PIC S9(11)  COMP-3.
           02 WED1-HSCKB                    PIC  X(01).
           02 WED1-KYG1                     PIC S9(12)  COMP-3.
           02 WED1-KYG2                     PIC S9(12)  COMP-3.
           02 WED1-KSYBI.
              03 WED1-KSYBI1                PIC  9(06).
              03 WED1-KSYBI2                PIC  9(02).
           02 WED1-TMUGK                    PIC S9(13)V9 COMP-3.
       01  TOTAL-AREA.
           02 WTT1-KYTTG                    PIC S9(12)  COMP-3.
           02 WTT2-KYTTG                    PIC S9(12)  COMP-3.
           02 WTT2-SAGAK                    PIC S9(12)  COMP-3.
           02 WTT2-OKAGE                    PIC S9(12)  COMP-3.
       01  FLAG-AREA.
           02 WFL1-SETTEI                   PIC  X(01).
           02 WFL1-HANTEI                   PIC  X(01).
           02 WFL1-KEY1                     PIC  X(01).
           02 WFL1-ERR                      PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE AND
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  WKCFILE
                OUTPUT WKAFILE
                       WKBFILE.
           PERFORM M11-OPEN-KYF THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M1-L01.
           IF WIN2-KEY = HIGH-VALUE
              GO TO M1-EXIT.
       M1-L02.
           IF WKC-SHIME = 15
              MOVE 16 TO WED1-SHIME
           ELSE
              IF WKC-SHIME = 20
                 MOVE 21 TO WED1-SHIME
              ELSE
                 MOVE 01 TO WED1-SHIME.
       M1-L03.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-YYMM TO WED1-SYM.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   KYF'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE '   FTM'   TO P89-TBLID(2).
           MOVE 'S'        TO P89-LTYPE(2).
           MOVE ALL '*'    TO P89-TABLE(3).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : OPEN-KYF                                             *
      *----------------------------------------------------------------*
       M11-OPEN-KYF.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE          END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M11-ERR     END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M11-ERR     END-EXEC.
           MOVE LOW-VALUE  TO WED1-LOW.
           MOVE HIGH-VALUE TO WED1-HIGH.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT *
              FROM    KYF
              WHERE A0 BETWEEN :WED1-TOKCDL AND :WED1-TOKCDH AND
                    A1 BETWEEN :WED1-NOSCDL AND :WED1-NOSCDH AND
                    A2 BETWEEN :WED1-KSYBIL AND :WED1-KSYBIH AND
                    A3 BETWEEN :WED1-KSSBTL AND :WED1-KSSBTH AND
                    A4 BETWEEN :WED1-TYTKJL AND :WED1-TYTKJH AND
                    A5 BETWEEN :WED1-RECKBL AND :WED1-RECKBH
              ORDER BY A0,A1,A2,B4
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M111-FETCH-KYF THRU M111-EXIT.
           IF WIN1-KEY = HIGH-VALUE
              GO TO M11-L01.
           GO TO M11-EXIT.
       M11-L01.
           DISPLAY 'M VBKE0120 TABLE(SI.KYF) DATA 0 KEN?!'
           UPON CONSOLE.
           GO TO M11-EXIT.
       M11-ERR.
           DISPLAY 'R VBKE0120 TABLE(SI.KYF) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1 : FETCH-KYF                                          *
      *----------------------------------------------------------------*
       M111-FETCH-KYF.
           EXEC SQL WHENEVER NOT FOUND  GO TO M111-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M111-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M111-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLKYF
           END-EXEC.
           MOVE TOKCD OF KYF TO WIN1-TOKCD.
           MOVE NOSCD OF KYF TO WIN1-NOSCD.
           GO TO M111-EXIT.
       M111-L01.
           MOVE HIGH-VALUE TO WIN1-KEY.
           GO TO M111-EXIT.
       M111-ERR.
           DISPLAY 'R VBKE0120 TABLE(SI.KYF) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE TOKCD OF WKC-R TO WIN2-TOKCD.
           MOVE NOSCD OF WKC-R TO WIN2-NOSCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-KYFONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKCONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           MOVE SPACE     TO WFL1-KEY1.
           MOVE ZERO      TO WTT1-KYTTG.
           MOVE ZERO      TO WTT2-KYTTG.
           MOVE ZERO      TO WTT2-SAGAK.
           MOVE ZERO      TO WTT2-OKAGE.
           MOVE ZERO      TO WED1-KYTGS.
           MOVE LOW-VALUE TO WSV1-KSYBI.
           MOVE LOW-VALUE TO WSV1-KFKTF.
           PERFORM M211-TOTAL THRU M211-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           MOVE SPACE     TO WFL1-SETTEI.
           IF WKC-SJHSB  =     ZERO  AND
              WKC-KEISN1 NOT = SPACE AND
              WKC-KEISN2 NOT = SPACE
              MOVE '1' TO WFL1-SETTEI.
           MOVE ZERO       TO WED1-OKAGE.
           MOVE ZERO       TO WED1-SAGAK.
           MOVE ZERO       TO WED1-KSYGK.
           MOVE SPACE      TO WED1-HSCKB.
           PERFORM M212-GET-KYTGS1 THRU M212-EXIT.
           IF KSJK OF WKC-R = '5' OR WED1-KYTGS NOT = ZERO
              MOVE WKC-R   TO WKA-R
              PERFORM M213-GET-KFKTF1 THRU M213-EXIT
              PERFORM M214-WKA-WRITE THRU M214-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : KAISYU YOTEI GOUKEI GAKU TOTAL                     *
      *----------------------------------------------------------------*
       M211-TOTAL.
           IF KSYBI OF KYF < WED1-SMEBI
              INITIALIZE     WKA-R
              MOVE DCLKYF TO WKA-KYF
              WRITE WKA-REC FROM WKA-R
              IF KSYBI OF KYF NOT > KSYBI OF WKC-R AND
                                      KSSBT OF KYF = '04'
                 MOVE KFKTF OF KYF TO WSV1-KFKTF
                 MOVE KSYBI OF KYF TO WSV1-KSYBI
              END-IF
           ELSE
              IF ( KSJK OF WKC-R = '2' OR KSJK OF WKC-R = '8') OR
                 ( RECKB OF KYF = '1' OR KYTGF OF KYF = '1' OR
                                         SFSKF OF KYF = '1' )
                 IF KSYBI OF KYF NOT > KSYBI OF WKC-R AND
                                         KSSBT OF KYF = '04'
                    MOVE KFKTF OF KYF TO WSV1-KFKTF
                    MOVE KSYBI OF KYF TO WSV1-KSYBI
                 END-IF
                 IF KEY1 OF KYF NOT = KEY1 OF WKC-R
                    INITIALIZE     WKA-R
                    MOVE DCLKYF TO WKA-KYF
                    WRITE WKA-REC FROM WKA-R
                    IF KSJK OF WKC-R = '1' OR '2' OR '3' OR
                                       '4' OR '6' OR '8'
                       IF KYTGF OF KYF = '1'
                          COMPUTE WTT1-KYTTG = WTT1-KYTTG +
                                                    KYTGT OF KYF
                       ELSE
                          COMPUTE WTT1-KYTTG = WTT1-KYTTG +
                                                    KYTGS OF KYF
                       END-IF
                    END-IF
                 ELSE
                    IF KSJK OF WKC-R = '2' OR '8'
                       MOVE '1' TO WFL1-KEY1
                       IF KYTGF OF KYF = '1'
                          COMPUTE WTT2-KYTTG = WTT2-KYTTG +
                                                    KYTGT OF KYF
                          COMPUTE WTT1-KYTTG = WTT1-KYTTG +
                                                    KYTGT OF KYF
                       ELSE
                          COMPUTE WTT2-KYTTG = WTT2-KYTTG +
                                                    KYTGS OF KYF
                          COMPUTE WTT1-KYTTG = WTT1-KYTTG +
                                                    KYTGS OF KYF
                       END-IF
                       COMPUTE WTT2-SAGAK = WTT2-SAGAK + SAGAK OF KYF
                       COMPUTE WTT2-OKAGE = WTT2-OKAGE + OKAGE OF KYF
                    END-IF
                 END-IF
              END-IF
           END-IF.
           PERFORM M111-FETCH-KYF THRU M111-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : KAISYU YOTEIGAKU SANSYUTU (1)                      *
      *----------------------------------------------------------------*
       M212-GET-KYTGS1.
           MOVE SPACE TO WFL1-ERR.
           IF KSJK OF WKC-R = '1'
              PERFORM M2121-KSJK1-1 THRU M2121-EXIT
              IF WFL1-ERR = '1'
                 GO TO M212-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M212-EXIT.
       M212-L01.
           IF KSJK OF WKC-R = '2'
              PERFORM M2122-KSJK1-2 THRU M2122-EXIT
              IF WFL1-ERR = '1'
                 GO TO M212-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M212-EXIT.
       M212-L02.
           IF KSJK OF WKC-R = '3'
              PERFORM M2123-KSJK1-3 THRU M2123-EXIT
              IF WFL1-ERR = '1'
                 GO TO M212-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M212-EXIT.
       M212-L03.
           IF KSJK OF WKC-R = '4'
              IF WKC-TMSEI < TGKKS OF WKC-R + WTT1-KYTTG
                 COMPUTE WED1-KYTGS = WKC-TMSEI - WTT1-KYTTG
              ELSE
                 MOVE TGKKS OF WKC-R TO WED1-KYTGS
              END-IF
              GO TO M212-EXIT.
       M212-L04.
           IF KSJK OF WKC-R = '5'
              MOVE ZERO TO WED1-KYTGS
              GO TO M212-EXIT.
       M212-L05.
           IF KSJK OF WKC-R = '6'
              PERFORM M2124-KSJK1-6 THRU M2124-EXIT
              IF WFL1-ERR = '1'
                 GO TO M212-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M212-EXIT.
       M212-L06.
           IF KSJK OF WKC-R = '8'
              IF WFL1-KEY1  NOT = SPACE
                 COMPUTE WED1-KYTGS = WKC-TMUGK  + WTT2-KYTTG
                 COMPUTE WED1-OKAGE = WKC-TMUGK  + WTT2-OKAGE
                 COMPUTE WED1-KSYGK = WTT1-KYTTG - WTT2-KYTTG
              ELSE
                 MOVE WKC-TMUGK TO WED1-KYTGS
              END-IF
           END-IF.
           GO TO M212-EXIT.
       M212-L99.
           DISPLAY 'R VBKE0120 KEISANSHIKI ERROR ! 1 = '
           WKC-KEISN1 ' 2 = ' WKC-KEISN2 UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1 : KAISYUU JYOUKENN = '1'                           *
      *----------------------------------------------------------------*
       M2121-KSJK1-1.
           IF WKC-TMSEI   NOT >  ZERO
              MOVE ZERO   TO  WFL1-SETTEI.
           IF WFL1-SETTEI = '1'
              COMPUTE P296-KINGK = WKC-TMSEI - WTT1-KYTTG
              MOVE WKC-KEISN1 TO P296-KEISN1
              MOVE WKC-KEISN2 TO P296-KEISN2
              CALL 'VSBA2960' USING P296-PARM
              IF P296-RTNCD = SPACE AND
                 P296-KINGK + WTT1-KYTTG > WKC-TMSEI
                 MOVE '2' TO P296-RTNCD
              END-IF
              EVALUATE P296-RTNCD
               WHEN ' '   MOVE P296-KINGK   TO WED1-KYTGS
                          COMPUTE WED1-SAGAK =
                                  WKC-TMSEI  - WTT1-KYTTG - P296-KINGK
                          MOVE '1' TO WED1-HSCKB,
               WHEN '2'   COMPUTE WED1-KYTGS = WKC-TMSEI  - WTT1-KYTTG,
               WHEN OTHER MOVE '1' TO WFL1-ERR
                          GO TO M2121-EXIT
              END-EVALUATE
           ELSE
              COMPUTE WED1-KYTGS = WKC-TMSEI - WTT1-KYTTG
           END-IF.
       M2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.2 : KAISYUU JYOUKENN = '2'                           *
      *----------------------------------------------------------------*
       M2122-KSJK1-2.
           PERFORM M21222-SELECT-FTM THRU M21222-EXIT.
           IF WKC-TMUGK   NOT >  ZERO
              MOVE ZERO   TO WFL1-SETTEI.
       M2122-L01.
           MOVE KSYBI OF WKC-R TO WED1-KSYBI.
           IF ( WKC-KKASI   <= WED1-KSYBI1 ) AND
              ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
              ( SJPST OF FTM NOT = ZERO    )
              COMPUTE WED1-TMUGK = WKC-TMUGK * SJPST OF FTM / 100
           ELSE
              MOVE WKC-TMUGK   TO WED1-TMUGK.
           IF WFL1-SETTEI = '1'
            MOVE WED1-TMUGK TO P296-KINGK
            MOVE WKC-KEISN1 TO P296-KEISN1
            MOVE WKC-KEISN2 TO P296-KEISN2
            CALL 'VSBA2960' USING P296-PARM
            EVALUATE P296-RTNCD
             WHEN ' '
              MOVE SPACE    TO WFL1-HANTEI
              COMPUTE WED1-KYG1 = P296-KINGK + WKC-TEIGK + WTT1-KYTTG
              COMPUTE WED1-KYG2 = WKC-TEIGK  - (WED1-KYG1 - WKC-TMSEI)
              PERFORM M21221-HANTEI THRU M21221-EXIT
              IF WFL1-KEY1 = SPACE
               EVALUATE WFL1-HANTEI
                WHEN '1'  COMPUTE WED1-KYTGS = P296-KINGK + WKC-TEIGK,
                WHEN '2'  COMPUTE WED1-KYTGS = P296-KINGK + WED1-KYG2,
                WHEN OTHER  MOVE P296-KINGK TO WED1-KYTGS
               END-EVALUATE
               COMPUTE WED1-SAGAK = WKC-TMUGK - P296-KINGK
               MOVE '1' TO WED1-HSCKB
              ELSE
               EVALUATE WFL1-HANTEI
                WHEN '1'   COMPUTE WED1-KYTGS = P296-KINGK + WKC-TEIGK
                                                          + WTT2-KYTTG,
                WHEN '2'   COMPUTE WED1-KYTGS = P296-KINGK + WED1-KYG2
                                                          + WTT2-KYTTG,
                WHEN '6'   MOVE P296-KINGK  TO WED1-KYTGS,
                WHEN OTHER COMPUTE WED1-KYTGS = P296-KINGK + WTT2-KYTTG
               END-EVALUATE
               MOVE '1' TO WED1-HSCKB
               COMPUTE WED1-SAGAK = WKC-TMUGK - P296-KINGK + WTT2-SAGAK
               COMPUTE WED1-OKAGE = WKC-TMUGK  + WTT2-OKAGE
               COMPUTE WED1-KSYGK = WTT1-KYTTG - WTT2-KYTTG
              END-IF,
             WHEN '2'
              MOVE SPACE      TO WFL1-HANTEI
              COMPUTE WED1-KYG1 = WKC-TMUGK + WKC-TEIGK  + WTT1-KYTTG
              COMPUTE WED1-KYG2 = WKC-TEIGK - (WED1-KYG1 - WKC-TMSEI)
              PERFORM M21221-HANTEI THRU M21221-EXIT
              IF WFL1-KEY1 = SPACE
               EVALUATE WFL1-HANTEI
                WHEN '1'    COMPUTE WED1-KYTGS = WKC-TMUGK + WKC-TEIGK,
                WHEN '2'    COMPUTE WED1-KYTGS = WKC-TMUGK + WED1-KYG2,
                WHEN '6'   IF ( WKC-TMUGK    > ZERO        ) AND
                              ( WKC-KKASI   <= WED1-KSYBI1 ) AND
                              ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
                              ( SJPST OF FTM NOT = ZERO    )
                              COMPUTE WED1-KYTGS =
                                        WKC-TMUGK * SJPST OF FTM / 100
                           ELSE
                              MOVE WKC-TMUGK   TO WED1-KYTGS
                           END-IF,
                WHEN OTHER  MOVE WKC-TMUGK TO WED1-KYTGS
               END-EVALUATE
              ELSE
               EVALUATE WFL1-HANTEI
                WHEN '1'   COMPUTE WED1-KYTGS = WKC-TMUGK + WKC-TEIGK
                                                          + WTT2-KYTTG,
                WHEN '2'   COMPUTE WED1-KYTGS = WKC-TMUGK + WED1-KYG2
                                                          + WTT2-KYTTG,
                WHEN '6'   IF ( WKC-TMUGK    > ZERO        ) AND
                              ( WKC-KKASI   <= WED1-KSYBI1 ) AND
                              ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
                              ( SJPST OF FTM NOT = ZERO    )
                              COMPUTE WED1-KYTGS =
                                        WKC-TMUGK * SJPST OF FTM / 100
                           ELSE
                              MOVE WKC-TMUGK   TO WED1-KYTGS
                           END-IF,
                WHEN OTHER COMPUTE WED1-KYTGS = WKC-TMUGK + WTT2-KYTTG
               END-EVALUATE
               MOVE WTT2-SAGAK   TO WED1-SAGAK
               COMPUTE WED1-OKAGE = WKC-TMUGK  + WTT2-OKAGE
               COMPUTE WED1-KSYGK = WTT1-KYTTG - WTT2-KYTTG
              END-IF,
             WHEN OTHER MOVE '1' TO WFL1-ERR
                        GO TO M2122-EXIT
            END-EVALUATE
           ELSE
            MOVE SPACE      TO WFL1-HANTEI
            COMPUTE WED1-KYG1 = WKC-TMUGK + WKC-TEIGK + WTT1-KYTTG
            COMPUTE WED1-KYG2 = WKC-TEIGK - ( WED1-KYG1 - WKC-TMSEI )
            PERFORM M21221-HANTEI THRU M21221-EXIT
            IF WFL1-KEY1 = SPACE
             EVALUATE WFL1-HANTEI
              WHEN '1'   COMPUTE WED1-KYTGS = WKC-TMUGK + WKC-TEIGK,
              WHEN '2'   COMPUTE WED1-KYTGS = WKC-TMUGK + WED1-KYG2,
              WHEN '6'   IF ( WKC-TMUGK    > ZERO        ) AND
                            ( WKC-KKASI   <= WED1-KSYBI1 ) AND
                            ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
                            ( SJPST OF FTM NOT = ZERO    )
                            COMPUTE WED1-KYTGS =
                                     WKC-TMUGK * SJPST OF FTM / 100
                         ELSE
                            MOVE WKC-TMUGK   TO WED1-KYTGS
                         END-IF,
              WHEN OTHER MOVE WKC-TMUGK TO WED1-KYTGS
             END-EVALUATE
            ELSE
             EVALUATE WFL1-HANTEI
              WHEN '1'   COMPUTE WED1-KYTGS = WKC-TMUGK + WKC-TEIGK
                                                        + WTT2-KYTTG,
              WHEN '2'   COMPUTE WED1-KYTGS = WKC-TMUGK + WED1-KYG2
                                                        + WTT2-KYTTG,
              WHEN '6'   IF ( WKC-TMUGK    > ZERO        ) AND
                            ( WKC-KKASI   <= WED1-KSYBI1 ) AND
                            ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
                            ( SJPST OF FTM NOT = ZERO    )
                            COMPUTE WED1-KYTGS =
                                     WKC-TMUGK * SJPST OF FTM / 100
                         ELSE
                            MOVE WKC-TMUGK   TO WED1-KYTGS
                         END-IF,
              WHEN OTHER COMPUTE WED1-KYTGS = WKC-TMUGK + WTT2-KYTTG
             END-EVALUATE
             MOVE WTT2-SAGAK TO WED1-SAGAK
             COMPUTE WED1-OKAGE = WKC-TMUGK  + WTT2-OKAGE
             COMPUTE WED1-KSYGK = WTT1-KYTTG - WTT2-KYTTG
            END-IF
           END-IF.
       M2122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.2.1 :HANTEI                                          *
      *----------------------------------------------------------------*
       M21221-HANTEI.
           IF SJPST OF FTM NOT = ZERO
              MOVE '6'       TO WFL1-HANTEI
              GO TO M21221-EXIT.
       M21221-L01.
           IF WKC-TEIGK NOT = ZERO
              IF WKC-KKASI NOT > WED1-KSYBI1 AND
                 WKC-KKIGN NOT < WED1-KSYBI1
                 IF WED1-KYG1 NOT > WKC-TMSEI
                    MOVE '1' TO WFL1-HANTEI
                 ELSE
                    IF WED1-KYG2 > ZERO
                       MOVE '2' TO WFL1-HANTEI
                    ELSE
                       MOVE '3' TO WFL1-HANTEI
                    END-IF
                 END-IF
              ELSE
                 MOVE '4' TO WFL1-HANTEI
              END-IF
           ELSE
              MOVE '5' TO WFL1-HANTEI
           END-IF.
       M21221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.2.2 : SELECT-FTM                                     *
      *----------------------------------------------------------------*
       M21222-SELECT-FTM.
           EXEC SQL WHENEVER NOT FOUND  GO TO M21222-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M21222-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M21222-ERR END-EXEC.
           MOVE TOKCD OF WKC-R  TO TOKCD OF FTM.
           MOVE NOSCD OF WKC-R  TO NOSCD OF FTM.
           MOVE '2'             TO DATKB OF FTM.
           MOVE ZERO            TO SEQ   OF FTM.
           EXEC SQL SELECT B0
              INTO :DCLFTM.B0
              FROM    FTM
              WHERE A0=:DCLFTM.A0
                AND A1=:DCLFTM.A1
                AND A2=:DCLFTM.A2
                AND A3=:DCLFTM.A3
           END-EXEC.
           GO TO M21222-EXIT.
       M21222-L01.
           MOVE ZERO            TO SJPST OF FTM.
           GO TO M21222-EXIT.
       M21222-ERR.
           DISPLAY 'R VBKE0120 TABLE(SI.FTM) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M21222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.3 : KAISYUU JYOUKENN = '3'                           *
      *----------------------------------------------------------------*
       M2123-KSJK1-3.
           IF WKC-TMSEI NOT > ZERO
              MOVE ZERO       TO WFL1-SETTEI
              COMPUTE WED1-KYTGS = WKC-TMSEI  - WTT1-KYTTG
              GO TO M2123-EXIT.
           COMPUTE WED1-PTKGK =
            ( WKC-TMSEI - WTT1-KYTTG ) * PSTKS OF WKC-R / 100.
           MOVE WED1-PTKGK TO WED1-PTKGK2.
           IF WFL1-SETTEI = '1'
            MOVE WED1-PTKGK TO P296-KINGK
            MOVE WKC-KEISN1 TO P296-KEISN1
            MOVE WKC-KEISN2 TO P296-KEISN2
            CALL 'VSBA2960' USING P296-PARM
            EVALUATE P296-RTNCD
             WHEN ' '
              IF WKC-TMSEI < P296-KINGK + WTT1-KYTTG
                 COMPUTE WED1-KYTGS = WKC-TMSEI  - WTT1-KYTTG
              ELSE
                 MOVE P296-KINGK  TO WED1-KYTGS
                 COMPUTE WED1-SAGAK = WED1-PTKGK2 - P296-KINGK
                 MOVE '1'         TO WED1-HSCKB
              END-IF,
             WHEN '2'
              IF WKC-TMSEI < WED1-PTKGK2 + WTT1-KYTTG
                 COMPUTE WED1-KYTGS = WKC-TMSEI  - WTT1-KYTTG
              ELSE
                 MOVE WED1-PTKGK2 TO WED1-KYTGS
              END-IF,
             WHEN OTHER MOVE '1' TO WFL1-ERR
                        GO TO M2123-EXIT
            END-EVALUATE
           ELSE
            IF WKC-TMSEI < WED1-PTKGK2 + WTT1-KYTTG
             COMPUTE WED1-KYTGS = WKC-TMSEI - WTT1-KYTTG
            ELSE
             MOVE WED1-PTKGK2 TO WED1-KYTGS
            END-IF
           END-IF.
       M2123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.4 : KAISYUU JYOUKENN = '6'                           *
      *----------------------------------------------------------------*
       M2124-KSJK1-6.
           IF WKC-TMSEI NOT > ZERO
              MOVE ZERO       TO WFL1-SETTEI
              COMPUTE WED1-KYTGS = WKC-TMSEI - WTT1-KYTTG
              GO TO M2124-EXIT.
           COMPUTE WED1-PTKGK =
               ( WKC-TMSEI - WTT1-KYTTG )  / 3.
           MOVE WED1-PTKGK TO WED1-PTKGK2.
           IF WFL1-SETTEI = '1'
            MOVE WED1-PTKGK TO P296-KINGK
            MOVE WKC-KEISN1 TO P296-KEISN1
            MOVE WKC-KEISN2 TO P296-KEISN2
            CALL 'VSBA2960' USING P296-PARM
            EVALUATE P296-RTNCD
             WHEN ' '
              IF WKC-TMSEI < P296-KINGK + WTT1-KYTTG
                 COMPUTE WED1-KYTGS = WKC-TMSEI  - WTT1-KYTTG
              ELSE
                 MOVE P296-KINGK  TO WED1-KYTGS
                 COMPUTE WED1-SAGAK = WED1-PTKGK2 - P296-KINGK
                 MOVE '1'         TO WED1-HSCKB
              END-IF,
             WHEN '2'
              IF WKC-TMSEI < WED1-PTKGK2 + WTT1-KYTTG
                 COMPUTE WED1-KYTGS = WKC-TMSEI  - WTT1-KYTTG
              ELSE
                 MOVE WED1-PTKGK2 TO WED1-KYTGS
              END-IF,
             WHEN OTHER MOVE '1' TO WFL1-ERR
                        GO TO M2124-EXIT
            END-EVALUATE
           ELSE
            IF WKC-TMSEI < WED1-PTKGK2 + WTT1-KYTTG
             COMPUTE WED1-KYTGS = WKC-TMSEI - WTT1-KYTTG
            ELSE
             MOVE WED1-PTKGK2 TO WED1-KYTGS
            END-IF
           END-IF.
       M2124-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.3 : KOUZA FURIKAE KAKUTEI HYOUSIKI SANSYUTU (1)        *
      *----------------------------------------------------------------*
       M213-GET-KFKTF1.
           IF KSSBT OF WKC-R NOT = '04'
              MOVE SPACE TO KFKTF OF WKA-R
           ELSE
              MOVE ZERO  TO KFKTF OF WKA-R
              IF ( KSJK OF WKC-R NOT = '1' ) AND
                 ( KSJK OF WKC-R NOT = '2' ) AND
                 ( KSJK OF WKC-R NOT = '4' )
                 IF ( KSJK OF WKC-R  = '3' OR '6' )  AND
                    ( WKC-KEISN1 NOT = SPACE )AND
                    ( WKC-KEISN2 NOT = SPACE )
                    MOVE ZERO     TO KFKTF OF WKA-R
                 ELSE
                    MOVE '1'      TO KFKTF OF WKA-R
                 END-IF
              END-IF
              IF KFKTF OF WKA-R = ZERO
                 IF ( WKC-SKNBHS = '1' ) OR
                    ( WKC-KFTKB NOT = ZERO ) OR
                    ( WKC-KFSSB NOT = ZERO ) OR
                    ( WSV1-KFKTF = '1') OR
                    ( WSV1-KFKTF = LOW-VALUE AND WKC-SKFKSB = '1' )
                    MOVE '1'  TO KFKTF OF WKA-R
                 END-IF
              END-IF
           END-IF.
       M213-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.4 : WKAFILE WRITE                                      *
      *----------------------------------------------------------------*
       M214-WKA-WRITE.
           MOVE WKC-TMSEI  TO SEKGK OF WKA-R.
           IF WFL1-KEY1 = SPACE
              MOVE WKC-TMUGK  TO OKAGE OF WKA-R
              MOVE WTT1-KYTTG TO KSYGK OF WKA-R
           ELSE
              MOVE WED1-OKAGE TO OKAGE OF WKA-R
              MOVE WED1-KSYGK TO KSYGK OF WKA-R
           END-IF.
           MOVE WED1-SAGAK TO SAGAK OF WKA-R.
           MOVE WED1-HSCKB TO HSCKB OF WKA-R.
           MOVE WED1-KYTGS TO KYTGS OF WKA-R.
           MOVE ZERO       TO SFSKF OF WKA-R.
           WRITE WKA-REC FROM WKA-R.
       M214-L01.
           IF KSSBT OF WKA-R = '04'
              MOVE TOKCD OF WKC-R TO WKB-TOKCD
              MOVE NOSCD OF WKC-R TO WKB-NOSCD
              WRITE WKB-REC FROM WKB-R.
       M214-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : KYFFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-KYFONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M221-WKA-WRITE2 THRU M221-EXIT
               UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : WKAFILE WRITE (2)                                  *
      *----------------------------------------------------------------*
       M221-WKA-WRITE2.
           INITIALIZE     WKA-R.
           MOVE DCLKYF TO WKA-KYF.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M111-FETCH-KYF THRU M111-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKCFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKCONL.
           MOVE ZERO TO WED1-KYTGS.
           MOVE SPACE     TO WFL1-KEY1.
           MOVE SPACE     TO WFL1-SETTEI.
           IF WKC-SJHSB  =     ZERO  AND
              WKC-KEISN1 NOT = SPACE AND
              WKC-KEISN2 NOT = SPACE
              MOVE '1' TO WFL1-SETTEI.
           MOVE ZERO      TO WTT1-KYTTG.
           MOVE ZERO      TO WED1-KSYGK.
           MOVE ZERO      TO WED1-SAGAK.
           MOVE SPACE     TO WED1-HSCKB.
           PERFORM M231-GET-KYTGS2 THRU M231-EXIT.
           IF KSJK OF WKC-R = '5' OR WED1-KYTGS NOT = ZERO
              MOVE WKC-R   TO WKA-R
              PERFORM M232-GET-KFKTF2 THRU M232-EXIT
              PERFORM M214-WKA-WRITE THRU M214-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : KAISYU YOTEIGAKU SANSYUTU (2)                      *
      *----------------------------------------------------------------*
       M231-GET-KYTGS2.
           MOVE SPACE TO WFL1-ERR.
           IF KSJK OF WKC-R = '1'
              PERFORM M2311-KSJK2-1 THRU M2311-EXIT
              IF WFL1-ERR = '1'
                 GO TO M231-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M231-EXIT.
       M231-L01.
           IF KSJK OF WKC-R = '2'
              PERFORM M2312-KSJK2-2 THRU M2312-EXIT
              IF WFL1-ERR = '1'
                 GO TO M231-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M231-EXIT.
       M231-L02.
           IF KSJK OF WKC-R = '3'
              PERFORM M2313-KSJK2-3 THRU M2313-EXIT
              IF WFL1-ERR = '1'
                 GO TO M231-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M231-EXIT.
       M231-L03.
           IF KSJK OF WKC-R = '4'
              IF WKC-TMSEI < TGKKS OF WKC-R
                 MOVE WKC-TMSEI TO WED1-KYTGS
              ELSE
                 MOVE TGKKS OF WKC-R TO WED1-KYTGS
              END-IF
              GO TO M231-EXIT.
       M231-L04.
           IF KSJK OF WKC-R = '5'
              MOVE ZERO TO WED1-KYTGS
              GO TO M231-EXIT.
       M231-L05.
           IF KSJK OF WKC-R = '6'
              PERFORM M2314-KSJK2-6 THRU M2314-EXIT
              IF WFL1-ERR = '1'
                 GO TO M231-L99
              END-IF
              IF WED1-HSCKB  = SPACE AND
                 WFL1-SETTEI = '1'
                 MOVE '2' TO WED1-HSCKB
              END-IF
              GO TO M231-EXIT.
       M231-L06.
           IF KSJK OF WKC-R = '8'
              MOVE WKC-TMUGK TO WED1-KYTGS.
           GO TO M231-EXIT.
       M231-L99.
           DISPLAY 'R VBKE0120 KEISANSHIKI ERROR ! 1 = '
           WKC-KEISN1 ' 2 = ' WKC-KEISN2 UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1 : KAISYUU JYOUKENN = '1'                           *
      *----------------------------------------------------------------*
       M2311-KSJK2-1.
           IF WKC-TMSEI   NOT >  ZERO
              MOVE ZERO   TO WFL1-SETTEI.
           IF WFL1-SETTEI = '1'
              MOVE WKC-TMSEI  TO P296-KINGK
              MOVE WKC-KEISN1 TO P296-KEISN1
              MOVE WKC-KEISN2 TO P296-KEISN2
              CALL 'VSBA2960' USING P296-PARM
              IF P296-RTNCD = SPACE AND
                 P296-KINGK > WKC-TMSEI
                 MOVE '2' TO P296-RTNCD
              END-IF
              EVALUATE P296-RTNCD
               WHEN ' '   MOVE P296-KINGK TO WED1-KYTGS
                          COMPUTE WED1-SAGAK = WKC-TMSEI  - P296-KINGK
                          MOVE '1' TO WED1-HSCKB,
               WHEN '2'   MOVE WKC-TMSEI TO WED1-KYTGS,
               WHEN OTHER MOVE '1' TO WFL1-ERR
                          GO TO M2311-EXIT
              END-EVALUATE
           ELSE
              MOVE WKC-TMSEI TO WED1-KYTGS
           END-IF.
       M2311-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.2 : KAISYUU JYOUKENN = '2'                           *
      *----------------------------------------------------------------*
       M2312-KSJK2-2.
           PERFORM M21222-SELECT-FTM THRU M21222-EXIT.
           IF WKC-TMUGK   NOT >  ZERO
              MOVE ZERO   TO  WFL1-SETTEI.
       M2312-L01.
           MOVE KSYBI OF WKC-R TO WED1-KSYBI.
           IF ( WKC-KKASI   <= WED1-KSYBI1 ) AND
              ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
              ( SJPST OF FTM NOT = ZERO    )
              COMPUTE WED1-TMUGK = WKC-TMUGK * SJPST OF FTM / 100
           ELSE
              MOVE WKC-TMUGK   TO WED1-TMUGK.
           IF WFL1-SETTEI = '1'
            MOVE WED1-TMUGK TO P296-KINGK
            MOVE WKC-KEISN1 TO P296-KEISN1
            MOVE WKC-KEISN2 TO P296-KEISN2
            CALL 'VSBA2960' USING P296-PARM
            EVALUATE P296-RTNCD
             WHEN ' '
              MOVE SPACE    TO WFL1-HANTEI
              COMPUTE WED1-KYG1 = P296-KINGK + WKC-TEIGK
              COMPUTE WED1-KYG2 = WKC-TEIGK  - (WED1-KYG1 - WKC-TMSEI)
              PERFORM M21221-HANTEI THRU M21221-EXIT
              EVALUATE WFL1-HANTEI
               WHEN '1'    COMPUTE WED1-KYTGS = P296-KINGK + WKC-TEIGK,
               WHEN '2'    COMPUTE WED1-KYTGS = P296-KINGK + WED1-KYG2,
               WHEN OTHER  MOVE P296-KINGK TO WED1-KYTGS
              END-EVALUATE
              MOVE '1' TO WED1-HSCKB
              COMPUTE WED1-SAGAK = WKC-TMUGK - P296-KINGK,
             WHEN '2'
              MOVE SPACE    TO WFL1-HANTEI
              COMPUTE WED1-KYG1 = WKC-TMUGK + WKC-TEIGK
              COMPUTE WED1-KYG2 = WKC-TEIGK - (WED1-KYG1 - WKC-TMSEI)
              PERFORM M21221-HANTEI THRU M21221-EXIT
              EVALUATE WFL1-HANTEI
               WHEN '1'   COMPUTE WED1-KYTGS = WKC-TMUGK + WKC-TEIGK,
               WHEN '2'   COMPUTE WED1-KYTGS = WKC-TMUGK + WED1-KYG2,
               WHEN '6'   IF ( WKC-TMUGK    > ZERO        ) AND
                             ( WKC-KKASI   <= WED1-KSYBI1 ) AND
                             ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
                             ( SJPST OF FTM NOT = ZERO    )
                             COMPUTE WED1-KYTGS =
                                 WKC-TMUGK * SJPST OF FTM / 100
                          ELSE
                             MOVE WKC-TMUGK   TO WED1-KYTGS
                          END-IF,
               WHEN OTHER MOVE WKC-TMUGK TO WED1-KYTGS
              END-EVALUATE,
             WHEN OTHER MOVE '1' TO WFL1-ERR
                        GO TO M2312-EXIT
            END-EVALUATE
           ELSE
            MOVE SPACE      TO WFL1-HANTEI
            COMPUTE WED1-KYG1 = WKC-TMUGK + WKC-TEIGK
            COMPUTE WED1-KYG2 = WKC-TEIGK - (WED1-KYG1 - WKC-TMSEI)
            PERFORM M21221-HANTEI THRU M21221-EXIT
            EVALUATE WFL1-HANTEI
             WHEN '1' COMPUTE WED1-KYTGS = WKC-TMUGK + WKC-TEIGK,
             WHEN '2' COMPUTE WED1-KYTGS = WKC-TMUGK + WED1-KYG2,
             WHEN '6' IF ( WKC-TMUGK    > ZERO        ) AND
                         ( WKC-KKASI   <= WED1-KSYBI1 ) AND
                         ( WED1-KSYBI1 <= WKC-KKIGN   ) AND
                         ( SJPST OF FTM NOT = ZERO    )
                         COMPUTE WED1-KYTGS =
                                  WKC-TMUGK * SJPST OF FTM / 100
                      ELSE
                         MOVE WKC-TMUGK   TO WED1-KYTGS
                      END-IF,
             WHEN OTHER MOVE WKC-TMUGK TO WED1-KYTGS
            END-EVALUATE
           END-IF.
       M2312-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3 : KAISYUU JYOUKENN = '3'                           *
      *----------------------------------------------------------------*
       M2313-KSJK2-3.
           IF WKC-TMSEI NOT >  ZERO
              MOVE ZERO       TO WFL1-SETTEI
              MOVE WKC-TMSEI  TO WED1-KYTGS
              GO TO M2313-EXIT.
           COMPUTE WED1-PTKGK = WKC-TMSEI * PSTKS OF WKC-R / 100.
           MOVE WED1-PTKGK TO WED1-PTKGK2.
           IF WFL1-SETTEI = '1'
            MOVE WED1-PTKGK TO P296-KINGK
            MOVE WKC-KEISN1 TO P296-KEISN1
            MOVE WKC-KEISN2 TO P296-KEISN2
            CALL 'VSBA2960' USING P296-PARM
            EVALUATE P296-RTNCD
             WHEN ' '
               IF WKC-TMSEI < P296-KINGK
                  MOVE WKC-TMSEI  TO WED1-KYTGS
               ELSE
                COMPUTE WED1-SAGAK = WED1-PTKGK2 - P296-KINGK
                MOVE P296-KINGK TO WED1-KYTGS
                MOVE '1' TO WED1-HSCKB
               END-IF,
             WHEN '2'
               IF WKC-TMSEI < WED1-PTKGK2
                MOVE WKC-TMSEI   TO WED1-KYTGS
               ELSE
                MOVE WED1-PTKGK2 TO WED1-KYTGS
               END-IF,
             WHEN OTHER MOVE '1' TO WFL1-ERR
                        GO TO M2313-EXIT
            END-EVALUATE
           ELSE
            IF WKC-TMSEI < WED1-PTKGK2
             MOVE WKC-TMSEI   TO WED1-KYTGS
            ELSE
             MOVE WED1-PTKGK2 TO WED1-KYTGS
            END-IF
           END-IF.
       M2313-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.4 : KAISYUU JYOUKENN = '6'                           *
      *----------------------------------------------------------------*
       M2314-KSJK2-6.
           IF WKC-TMSEI NOT >  ZERO
              MOVE ZERO       TO WFL1-SETTEI
              MOVE WKC-TMSEI  TO WED1-KYTGS
              GO TO M2314-EXIT.
           COMPUTE WED1-PTKGK = WKC-TMSEI / 3.
           MOVE WED1-PTKGK TO WED1-PTKGK2.
           IF WFL1-SETTEI = '1'
            MOVE WED1-PTKGK TO P296-KINGK
            MOVE WKC-KEISN1 TO P296-KEISN1
            MOVE WKC-KEISN2 TO P296-KEISN2
            CALL 'VSBA2960' USING P296-PARM
            EVALUATE P296-RTNCD
             WHEN ' '
               IF WKC-TMSEI < P296-KINGK
                  MOVE WKC-TMSEI  TO WED1-KYTGS
               ELSE
                COMPUTE WED1-SAGAK = WED1-PTKGK2 - P296-KINGK
                MOVE P296-KINGK TO WED1-KYTGS
                MOVE '1' TO WED1-HSCKB
               END-IF,
             WHEN '2'
               IF WKC-TMSEI < WED1-PTKGK2
                MOVE WKC-TMSEI   TO WED1-KYTGS
               ELSE
                MOVE WED1-PTKGK2 TO WED1-KYTGS
               END-IF,
             WHEN OTHER MOVE '1' TO WFL1-ERR
                        GO TO M2314-EXIT
            END-EVALUATE
           ELSE
            IF WKC-TMSEI < WED1-PTKGK2
             MOVE WKC-TMSEI   TO WED1-KYTGS
            ELSE
             MOVE WED1-PTKGK2 TO WED1-KYTGS
            END-IF
           END-IF.
       M2314-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.2 : KOUZA FURIKAE KAKUTEI HYOUSIKI SANSYUTU (2)        *
      *----------------------------------------------------------------*
       M232-GET-KFKTF2.
           IF KSSBT OF WKC-R NOT = '04'
              MOVE SPACE TO KFKTF OF WKA-R
           ELSE
              MOVE ZERO  TO KFKTF OF WKA-R
              IF ( KSJK OF WKC-R NOT = '1' ) AND
                 ( KSJK OF WKC-R NOT = '2' ) AND
                 ( KSJK OF WKC-R NOT = '4' )
                 IF ( KSJK OF WKC-R  = '3' OR '6' )   AND
                    ( WKC-KEISN1 NOT = SPACE ) AND
                    ( WKC-KEISN2 NOT = SPACE )
                    MOVE ZERO     TO KFKTF OF WKA-R
                 ELSE
                    MOVE '1'      TO KFKTF OF WKA-R
                 END-IF
              END-IF
              IF KFKTF OF WKA-R = ZERO
                 IF ( WKC-SKNBHS = '1' ) OR
                    ( WKC-KFTKB NOT = ZERO ) OR
                    ( WKC-KFSSB NOT = ZERO ) OR
                    ( WKC-SKFKSB = '1' )
                    MOVE '1' TO KFKTF OF WKA-R
                 END-IF
              END-IF
           END-IF.
       M232-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE.
           PERFORM M31-CLOSE-KYF THRU M31-EXIT.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : CLOSE-KYF                                            *
      *----------------------------------------------------------------*
       M31-CLOSE-KYF.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M31-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M31-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M31-EXIT.
       M31-ERR.
           DISPLAY 'R VBKE0120 TABLE(SI.KYF) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           CALL 'VSBABEND'.
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBKE0120 END                                         *
      *----------------------------------------------------------------*
