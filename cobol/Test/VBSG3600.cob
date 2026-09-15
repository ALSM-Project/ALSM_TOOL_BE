./ ADD NAME=VBSG3600  01.30 20021202 20110812 131332 00379 00231 MCSK025
       ID  DIVISION.
       PROGRAM-ID.     'VBSG3600'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : S                             *
      *              <> PG.OUTLINE     : HANBAI KAKAKU FILE(SANKABUN)  *
      *                                  SAKUSEI                       *
      *                 * INPUT FILE   : KAKAKU KANRI GROUP FILE       *
      *                                                      (SI.KAG)  *
      *                 * INPUT FILE   : HANBAI KAKAKU FILE(KANRIBUN)  *
      *                                                      (DWKB)    *
      *                 * OUTPUT FILE  : HANBAI KAKAKU FILE(SANKABUN)  *
      *                                                      (DWKC)    *
      *                                : SANKA TABLE KEIKOKU LIST      *
      *                                                      (PRT1)    *
      *              <> AUTHOR         : CSK-T.SUMI                    *
      *              <> DATE           : 02/12/02                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1228             ÍÝº³   : 10/06/18  CSK-M.KODAMA        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT LPFILE  ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(51).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(51).
       FD  LPFILE BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           LABEL RECORD STANDARD.
       01  LP-REC.
           02 FILLER                        PIC  X(132).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLKAG END-EXEC.
       01  KAG REDEFINES DCLKAG.
           COPY VTABKAG SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  PC5-PARM.
           COPY VPARBC5  SUPPRESS.
       01  WKB-R.
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-TKYBI                     PIC  X(08).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-HTANK                     PIC S9(09)  COMP-3.
           02 WKB-SMHBK                     PIC S9(09)  COMP-3.
           02 WKB-KEIKB                     PIC  X(01).
           02 WKB-KEINO                     PIC  X(10).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-SYOKB                     PIC  X(01).
           02 WKB-KNSKB                     PIC  X(01).
           02 WKB-KKHKB                     PIC  X(01).
       01  WKC-R.
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-TKYBI                     PIC  X(08).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-HTANK                     PIC S9(09)  COMP-3.
           02 WKC-SMHBK                     PIC S9(09)  COMP-3.
           02 WKC-KEIKB                     PIC  X(01).
           02 WKC-KEINO                     PIC  X(10).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-SYOKB                     PIC  X(01).
           02 WKC-KNSKB                     PIC  X(01).
           02 WKC-KKHKB                     PIC  X(01).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-LOW                      PIC  X(07).
           02 WED1-HIGH                     PIC  X(07).
           02 WED1-ACCEPT.
              03 WED1-INPUT                 PIC  X(03).
              03 FILLER                     PIC  X(77).
       01  SAVE-AREA.
           02 WSV1-TOKCD                    PIC  X(07).
           02 WSV1-NOTFD                    PIC  X(07).
       01  TABLE-AREA.
           02 WTB1-DATA.
              03 WTB1-KAG         OCCURS  15001.
                 04 WTB1-SIRKN                 PIC  X(07).
       01  COUNT-AREA.
           02 I                             PIC S9(08)  COMP.
           02 J                             PIC S9(08)  COMP.
           02 WCT1-LINE                     PIC S9(04)  COMP.
           02 WCT1-PAGE                     PIC S9(04)  COMP.
       01  LPH-HEADER.
           02 LPH-HEAD1.
              03 FILLER       PIC  X(132) VALUE SPACE.
           02 LPH-HEAD2.
              03 FILLER       PIC  X(10)  VALUE  SPACE.
              03 FILLER       PIC  X(10)  VALUE '**  »Ý¶ Ã-'.
              03 FILLER       PIC  X(10)  VALUE 'ÌÞÙ ¹²º¸ Ø'.
              03 FILLER       PIC  X(10)  VALUE '½Ä (VBSG36'.
              03 FILLER       PIC  X(07)  VALUE '00)  **'.
              03 FILLER       PIC  X(05)  VALUE  SPACE.
              03 LPH-DATE     PIC  99/99/99.
              03 FILLER       PIC  X(02)  VALUE  SPACE.
              03 FILLER       PIC  X(02)  VALUE 'P:'.
              03 LPH-PAGE     PIC  ZZ9.
           02 LPH-HEAD3.
              03 FILLER   PIC  X(10)  VALUE SPACE.
              03 FILLER   PIC  X(10)  VALUE '¶¶¸²¯¶Â¶ÝØ'.
              03 FILLER   PIC  X(02)  VALUE '»·'.
              03 FILLER   PIC  X(07)  VALUE SPACE.
              03 FILLER   PIC  X(06)  VALUE 'Ò¯¾°¼Þ'.
       01  LPD-DETLE.
           02 LPD-DETL1.
              03 FILLER                PIC  X(10) VALUE SPACE.
              03 LPD-KIKSK             PIC  X(07).
              03 FILLER                PIC  X(12) VALUE SPACE.
              03 LPD-MESSG             PIC  X(14).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WKB-R = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           INITIALIZE SAVE-AREA.
           PERFORM M10-LOCK-KAG THRU M10-EXIT.
           OPEN INPUT  WKBFILE
                OUTPUT WKCFILE
                       LPFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-DATE TO PC5-YMDS8.
           CALL 'VSBA1250' USING PC5-PARM.
       M1-L01.
           MOVE 99       TO WCT1-LINE.
           MOVE ZERO     TO WCT1-PAGE.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-KAG                                             *
      *----------------------------------------------------------------*
       M10-LOCK-KAG.
           MOVE '   KAG'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WKB-R
                   GO TO M11-EXIT.
           IF WKB-TOKCD = WSV1-NOTFD
              GO TO M11-READ-WKB.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WKB-TOKCD NOT = WSV1-TOKCD
              MOVE WKB-TOKCD  TO WSV1-TOKCD
              INITIALIZE WTB1-DATA
              MOVE WKB-TOKCD  TO KIKSK OF KAG
              MOVE LOW-VALUE  TO WED1-LOW
              MOVE HIGH-VALUE TO WED1-HIGH
              PERFORM M21-OPEN-KAG  THRU M21-EXIT
              PERFORM M20-TABLE-SET THRU M20-EXIT
                      VARYING I FROM 1 BY 1
                      UNTIL ( KAG = HIGH-VALUE ) OR
                            ( I > 15001 )
              PERFORM M23-CLOSE-KAG THRU M23-EXIT
              IF 14001 < I
                 DISPLAY 'M VBSG3600 SANKA TABLE OVER KEIKOKU = '
                 WKB-TOKCD
                 UPON CONSOLE
                 MOVE SPACE            TO LPD-DETL1
                 MOVE 'Ã°ÌÞÙµ°ÊÞ°¹²º¸' TO LPD-MESSG
                 PERFORM M24-WRITE-PRT THRU M24-EXIT
                 IF 15001 < I
                    DISPLAY 'M VBSG3600 SANKA TABLE OVER = ' WKB-TOKCD
                    UPON CONSOLE
                    MOVE SPACE         TO LPD-DETL1
                    MOVE 'Ã°ÌÞÙµ°ÊÞ°'  TO LPD-MESSG
                    PERFORM M24-WRITE-PRT THRU M24-EXIT.
       M2-L01.
           MOVE WKB-R         TO WKC-R.
           MOVE SPACE         TO WKC-KEINO.
           MOVE SPACE         TO WKC-NOSCD.
       M2-L02.
           IF I NOT = 1
              PERFORM M25-WRITE1-WKC THRU M25-EXIT
                   VARYING J FROM 1 BY 1
                   UNTIL ( J >= I )   OR
                         ( J = 15001)
              IF 15001 < I
                 MOVE WKB-TOKCD         TO KIKSK OF KAG
                 MOVE WTB1-SIRKN(15001) TO WED1-LOW
                 MOVE HIGH-VALUE        TO WED1-HIGH
                 PERFORM M21-OPEN-KAG  THRU M21-EXIT
                 PERFORM M26-WRITE2-WKC THRU M26-EXIT
                    UNTIL ( KAG = HIGH-VALUE )
                 PERFORM M23-CLOSE-KAG THRU M23-EXIT.
           PERFORM M11-READ-WKB  THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.0 : TABLE-SET                                            *
      *----------------------------------------------------------------*
       M20-TABLE-SET.
           MOVE SIRKN OF KAG TO WTB1-SIRKN(I).
       M20-L01.
           PERFORM M211-FETCH-KAG   THRU M211-EXIT.
       M20-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : OPEN-KAG                                             *
      *----------------------------------------------------------------*
       M21-OPEN-KAG.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M21-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M21-ERR END-EXEC.
           EXEC SQL DECLARE CSR1 CURSOR FOR
              SELECT A0,A1
              FROM    KAG
              WHERE A0=:DCLKAG.A0
                AND ( A1 BETWEEN :WED1-LOW AND :WED1-HIGH )
              ORDER BY A0,A1
           END-EXEC.
           EXEC SQL OPEN CSR1 END-EXEC.
           PERFORM M211-FETCH-KAG THRU M211-EXIT.
           IF KAG = HIGH-VALUE
              GO TO M21-L01.
           GO TO M21-EXIT.
       M21-L01.
           MOVE WKB-TOKCD TO WSV1-NOTFD.
           GO TO M21-EXIT.
       M21-ERR.
           DISPLAY 'R VBSG3600 TABLE(SI.KAG) OPEN ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : FETCH-KAG                                          *
      *----------------------------------------------------------------*
       M211-FETCH-KAG.
           EXEC SQL WHENEVER NOT FOUND  GO TO M211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           EXEC SQL FETCH CSR1
              INTO :DCLKAG.A0,:DCLKAG.A1
           END-EXEC.
           GO TO M211-EXIT.
       M211-L01.
           MOVE HIGH-VALUE TO KAG.
           GO TO M211-EXIT.
       M211-ERR.
           DISPLAY 'R VBSG3600 TABLE(SI.KAG) FETCH  ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : CLOSE-KAG                                            *
      *----------------------------------------------------------------*
       M23-CLOSE-KAG.
           EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M23-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M23-ERR END-EXEC.
           EXEC SQL CLOSE CSR1 END-EXEC.
           GO TO M23-EXIT.
       M23-ERR.
           DISPLAY 'R VBSG3600 TABLE(SI.KAG) CLOSE ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4 : PRT WRITE                                            *
      *----------------------------------------------------------------*
       M24-WRITE-PRT.
           IF WCT1-LINE NOT < 60
              PERFORM M241-HEAD-PRT THRU M241-EXIT.
           MOVE WKB-TOKCD    TO LPD-KIKSK.
           WRITE LP-REC FROM LPD-DETL1 AFTER 1.
           ADD 1 TO WCT1-LINE.
       M24-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4.1 : HEADER PRT                                         *
      *----------------------------------------------------------------*
       M241-HEAD-PRT.
           MOVE PC5-YMDW6 TO LPH-DATE.
           COMPUTE WCT1-PAGE = WCT1-PAGE + 1.
           MOVE WCT1-PAGE TO LPH-PAGE.
           WRITE LP-REC   FROM  LPH-HEAD1  AFTER  PAGE.
           WRITE LP-REC   FROM  LPH-HEAD2  AFTER  1.
           WRITE LP-REC   FROM  LPH-HEAD3  AFTER  2.
           MOVE  4        TO WCT1-LINE.
       M241-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.5 : WRITE1 WKCFILE                                       *
      *----------------------------------------------------------------*
       M25-WRITE1-WKC.
           MOVE WTB1-SIRKN(J) TO WKC-TOKCD.
           WRITE WKC-REC FROM WKC-R.
       M25-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.6 : WRITE2 WKCFILE                                       *
      *----------------------------------------------------------------*
       M26-WRITE2-WKC.
           MOVE SIRKN OF KAG  TO WKC-TOKCD.
           WRITE WKC-REC FROM WKC-R.
           PERFORM M211-FETCH-KAG THRU M211-EXIT.
       M26-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           IF WCT1-PAGE = ZERO
               GO TO M3-L99.
           DISPLAY '*----- V B S G 3 6 0 0 -----*' UPON CONSOLE.
           DISPLAY '*                           *' UPON CONSOLE.
           DISPLAY '*   SANKA TEBLE KEIKOKU     *' UPON CONSOLE.
           DISPLAY '*   YOKUJITU KAIHATUKA      *' UPON CONSOLE.
           DISPLAY '*   RENRAKU DE OK!          *' UPON CONSOLE.
           DISPLAY '*   NIPOU KINYU GO YES WO   *' UPON CONSOLE.
           DISPLAY '*   KEY-IN SEYO!            *' UPON CONSOLE.
           DISPLAY '*                           *' UPON CONSOLE.
           DISPLAY '*----- V B S G 3 6 0 0 -----*' UPON CONSOLE.
       M3-L01.
           ACCEPT WED1-ACCEPT FROM CONSOLE.
           IF WED1-INPUT NOT = 'YES'
               GO TO M3-L01.
       M3-L99.
           CLOSE WKBFILE
                 WKCFILE
                 LPFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBSG3600 END                                         *
      *----------------------------------------------------------------*
