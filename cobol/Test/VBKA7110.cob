./ ADD NAME=VBKA7110  01.99 20190702 20190828 162218 00528 00497 MCSK004
       ID  DIVISION.
       PROGRAM-ID. 'VBKA7110'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10SYSTEM              *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : SEIKYU GOUKEI HYO ZEIRITU BETU*
      *                                  GOUKEI RECORD SAKUSEI         *
      *                 * INPUT FILE   : SEIKYU SYO DATA               *
      *                                  (SEIKYU MEISAI)         (DWKC)*
      *                 * OUTPUT FILE  : SEIKYU GOUKEI HYO JYOUHOU     *
      *                                  (ZEIRITU BETU GOUKEI)   (DWKD)*
      *              <> AUTHOR         : SCSK-A.ISIKAWA                *
      *              <> DATE           : 19/07/01                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      Õ›∫≥   : YY/MM/DD CSK-X.XXXXXX         *
      * (02) XXXXXX XXXXXX      ¬≤∂    : YY/MM/DD CSK-X.XXXXXX         *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(300).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(120).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  PC0-PARM.
           COPY VPARBC0  SUPPRESS.
       01  P205-PARM.
           COPY VPARB205 SUPPRESS.
       01  P48-PARM.
           COPY VPARB48  SUPPRESS.
       01  WKC-R.
           02 WKC-CTLKEY.
              03 WKC-KAINO                  PIC  X(02).
              03 WKC-YTKBN                  PIC  X(01).
              03 WKC-YSYBN.
                 04 WKC-YSYB1               PIC  X(03).
                 04 WKC-YSYB2               PIC  X(04).
              03 WKC-YSBNO                  PIC  X(03).
              03 WKC-YSSTN                  PIC  X(03).
              03 WKC-YSCKG                  PIC  X(05).
              03 WKC-YUSCD                  PIC  X(07).
              03 WKC-YUNCD                  PIC  X(02).
              03 WKC-YSALL                  PIC  9(04).
              03 WKC-INFLG                  PIC  X(01).
              03 WKC-YSTPG                  PIC  9(04).
              03 WKC-PAGEN.
                 04 WKC-PAGE1               PIC  9(04).
                 04 WKC-PAGE2               PIC  9(04).
              03 WKC-SESCD                  PIC  X(07).
              03 WKC-SQNCD                  PIC  X(02).
           02 WKC-LPDAT                     PIC  X(219).
           02 WKC-DATA66  REDEFINES  WKC-LPDAT.
              03 WKC-GYO66                  PIC  X(02).
              03 WKC-RKB66                  PIC  X(02).
              03 WKC-MESI66.
                 04 WKC-MIDSAU6             PIC  X(50).
                 04 WKC-MIDSBD6             PIC  X(50).
                 04 WKC-ZOKDK               PIC S9(11)   COMP-3.
                 04 WKC-ZSHZI               PIC S9(11)   COMP-3.
                 04 WKC-ZOKGDK              PIC S9(11)   COMP-3.
                 04 WKC-ZEIRT6              PIC S9(02)V9 COMP-3.
                 04 WKC-KGZRT6              PIC  X(01).
                 04 FILLER                  PIC  X(94).
           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKD-R.
           02 WKD-CTLKEY.
              03 WKD-KAINO                  PIC  X(02).
              03 WKD-YTKBN                  PIC  X(01).
              03 WKD-YSYBN.
                 04 WKD-YSYB1               PIC  X(03).
                 04 WKD-YSYB2               PIC  X(04).
              03 WKD-YSBNO                  PIC  X(03).
              03 WKD-YSSTN                  PIC  X(03).
              03 WKD-YSCKG                  PIC  X(05).
              03 WKD-YUSCD                  PIC  X(07).
              03 WKD-YUNCD                  PIC  X(02).
              03 WKD-YSALL                  PIC  9(04).
              03 WKD-INFLG                  PIC  X(01).
              03 WKD-YSTPG                  PIC  9(04).
              03 WKD-PAGEN.
                 04 WKD-PAGE1               PIC  9(04).
                 04 WKD-PAGE2               PIC  9(04).
              03 WKD-SESCD                  PIC  X(07).
              03 WKD-SQNCD                  PIC  X(02).
              03 WKD-GYONO                  PIC  9(02).
              03 WKD-RECKB                  PIC  X(02).
              03 WKD-ZBEGK.
                 04 WKD-MIDS1               PIC  X(22).
                 04 WKD-ZOKDK               PIC S9(13)   COMP-3.
                 04 WKD-MIDS2               PIC  X(08).
                 04 WKD-ZSHZI               PIC S9(13)   COMP-3.
                 04 WKD-ZEIRT               PIC S9(02)V9 COMP-3.
                 04 WKD-KGZRT               PIC  X(01).
                 04 FILLER                  PIC  X(13).

       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEYB.
                 04 WIN1-KEYA.
                    05 WIN1-YUSCD           PIC  X(07).
                    05 WIN1-YUNCD           PIC  X(02).
                 04 WIN1-ZEIRT6             PIC S9(02)V9 COMP-3.
                 04 WIN1-KGZRT6             PIC  X(01).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEYB.
                 04 WSV1-KEYA.
                    05 WSV1-YUSCD           PIC  X(07).
                    05 WSV1-YUNCD           PIC  X(02).
                 04 WSV1-ZEIRT6             PIC S9(02)V9 COMP-3.
                 04 WSV1-KGZRT6             PIC  X(01).
           02 WSV1-YMD.
              03 WSV1-YY                    PIC  9(04).
              03 WSV1-MM                    PIC  9(02).
              03 WSV1-DD                    PIC  9(02).
       01  STORE-AREA.
           02 WST1-FIRST.
              03 WST1-KAINO                 PIC  X(02).
              03 WST1-YTKBN                 PIC  X(01).
              03 WST1-YSYBN.
                 04 WST1-YSYB1              PIC  X(03).
                 04 WST1-YSYB2              PIC  X(04).
              03 WST1-YSBNO                 PIC  X(03).
              03 WST1-YSSTN                 PIC  X(03).
              03 WST1-YSCKG                 PIC  X(05).
              03 WST1-YUSCD                 PIC  X(07).
              03 WST1-YUNCD                 PIC  X(02).
              03 WST1-SESCD                 PIC  X(07).
              03 WST1-SQNCD                 PIC  X(02).
              03 WST1-ZEIRT6                PIC S9(02)V9 COMP-3.
              03 WST1-KGZRT6                PIC  X(01).
       01  KANJI-AREA.
           02 WKJ1-MIDS1                    PIC  X(22).
      *
           02 WKJ1-PTAIS-K                  PIC  X(14) VALUE
                            'ÅìëŒè€Å@ê≈çû'.
           02 WKJ1-PTAIS-R  REDEFINES WKJ1-PTAIS-K.
              03 FILLER                     PIC  X(01).
              03 WKJ1-PTAIS                 PIC  X(12).
              03 FILLER                     PIC  X(01).
      *
           02 WKJ1-PZEIK-K                  PIC  X(12) VALUE
                            'ÅìÅ@Å@ê≈çû'.
           02 WKJ1-PZEIK-R  REDEFINES WKJ1-PZEIK-K.
              03 FILLER                     PIC  X(01).
              03 WKJ1-PZEIK                 PIC  X(10).
              03 FILLER                     PIC  X(01).
      *
           02 WKJ1-KGZEI-K                  PIC  X(06) VALUE
                            'åyå∏'.
           02 WKJ1-KGZEI-R  REDEFINES WKJ1-KGZEI-K.
              03 FILLER                     PIC  X(01).
              03 WKJ1-KGZEI                 PIC  X(04).
              03 FILLER                     PIC  X(01).
      *
           02 WKJ1-KYUZE-K                  PIC  X(04) VALUE
                            'ãå'.
           02 WKJ1-KYUZE-R  REDEFINES WKJ1-KYUZE-K.
              03 FILLER                     PIC  X(01).
              03 WKJ1-KYUZE                 PIC  X(02).
              03 FILLER                     PIC  X(01).
      *
           02 WKJ1-HIKAZ-K                  PIC  X(12) VALUE
                            'îÒâ€ê≈ëŒè€'.
           02 WKJ1-HIKAZ-R  REDEFINES WKJ1-HIKAZ-K.
              03 FILLER                     PIC  X(01).
              03 WKJ1-HIKAZ                 PIC  X(10).
              03 FILLER                     PIC  X(01).
      *
           02 WKJ1-SOHZE-K                  PIC  X(10) VALUE
                            'ì‡è¡îÔê≈'.
           02 WKJ1-SOHZE-R  REDEFINES WKJ1-SOHZE-K.
              03 FILLER                     PIC  X(01).
              03 WKJ1-SOHZE                 PIC  X(08).
              03 FILLER                     PIC  X(01).
      *
           02 WKJ1-ZRITU.
              03 FILLER                     PIC  X(40).
           02 FILLER REDEFINES WKJ1-ZRITU.
              03 FILLER                     PIC  X(02).
              03 WKJ1-ZRIT2                 PIC  X(02).
              03 FILLER                     PIC  X(36).
           02 FILLER REDEFINES WKJ1-ZRITU.
              03 FILLER                     PIC  X(02).
              03 WKJ1-ZRIT23                PIC  X(06).
              03 FILLER                     PIC  X(32).
           02 FILLER REDEFINES WKJ1-ZRITU.
              03 WKJ1-ZRIT12                PIC  X(04).
              03 FILLER                     PIC  X(36).
           02 FILLER REDEFINES WKJ1-ZRITU.
              03 WKJ1-ZRIT123               PIC  X(08).
              03 FILLER                     PIC  X(32).
       01  COUNT-AREA.
           02 WCT1-CNT                      PIC  9(02).
       01  TOTAL-AREA.
           02 WTT1-ZOKDK                    PIC S9(13)  COMP-3.
           02 WTT1-ZSHZI                    PIC S9(13)  COMP-3.
       01  EDIT-AREA.
           02 WED1-ZEIRT                    PIC  99.9.
           02 WED1-ZEIRTX REDEFINES WED1-ZEIRT.
              03 WED1-ZEI1                  PIC  X(01).
              03 WED1-ZEI2                  PIC  X(01).
              03 FILLER                     PIC  X(01).
              03 WED1-ZEI3                  PIC  X(01).
           02 WED1-TOKCD.
              03 WED1-TKCD1                 PIC  X(01).
              03 FILLER                     PIC  X(06).
           02 WED1-YMD.
              03 WED1-YY                    PIC  9(04).
              03 WED1-MM                    PIC  9(02).
              03 WED1-DD                    PIC  9(02).
       01  SYSIN-AREA.
           02 SYS-SIMEB                     PIC  X(02).
           02 FILLER                        PIC  X(78).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKCFILE
                OUTPUT WKDFILE.
           CALL 'VSBA2280' USING P228-PARM.
       M1-L01.
           PERFORM M11-SYSIN    THRU M11-EXIT.
       M1-L02.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SYSIN SHORI                                          *
      *----------------------------------------------------------------*
       M11-SYSIN.
           MOVE LOW-VALUE TO SYSIN-AREA.
           ACCEPT SYSIN-AREA.
           IF SYS-SIMEB = '20' OR '15' OR '31'
              NEXT SENTENCE
           ELSE
              DISPLAY 'R VBKA7110 SYSIN ERROR = ' SYS-SIMEB
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M11-L01.
           IF SYS-SIMEB = '20'
              MOVE P228-YY   TO WSV1-YY
              MOVE P228-MM   TO WSV1-MM
              MOVE SYS-SIMEB TO WSV1-DD
              GO TO M11-L03.
           IF SYS-SIMEB = '15'
              MOVE P228-YY   TO WSV1-YY
              MOVE P228-MM   TO WSV1-MM
              MOVE SYS-SIMEB TO WSV1-DD
              GO TO M11-L03.
       M11-L02.
           IF SYS-SIMEB = '31'
              PERFORM M111-SYSIN-MATU THRU M111-EXIT
           END-IF.
       M11-L03.
           MOVE WSV1-YMD          TO P205-YMDS8-X.
           MOVE '0'               TO P205-KGZKB.
           COMPUTE P205-SHZRT  = 0.
           CALL 'VSBA2050' USING P205-PARM.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1  :   SYSIN SHORI (SIMEB = '31')                      *
      *----------------------------------------------------------------*
       M111-SYSIN-MATU.
           MOVE P228-YY    TO WED1-YY.
           MOVE P228-MM    TO WED1-MM.
       M111-L01.
           IF P228-DD < 25
              COMPUTE WED1-MM = WED1-MM - 1
              IF WED1-MM = 0
                 MOVE 12   TO WED1-MM
                 COMPUTE WED1-YY = WED1-YY - 1.
           MOVE WED1-YMD   TO PC0-RYMDS8.
       M111-L02.
           CALL 'VSBA1200' USING PC0-PARM.
           MOVE PC0-RYMDS8 TO WSV1-YMD.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKCFILE ( æ≤∑≠≥ºÆ “≤ª≤√ﬁ-¿ )                    *
      *----------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M12-EXIT.
       M12-L01.
           MOVE WKC-YUSCD  TO WIN1-YUSCD.
           MOVE WKC-YUNCD  TO WIN1-YUNCD.
           MOVE WKC-ZEIRT6 TO WIN1-ZEIRT6.
           MOVE WKC-KGZRT6 TO WIN1-KGZRT6.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KEY   TO WSV1-KEY.
           COMPUTE WTT1-ZOKDK = 0.
           COMPUTE WTT1-ZSHZI = 0.
           COMPUTE WCT1-CNT   = 0.
       M2-L01.
           PERFORM M21-YUSCD-BREAK  THRU M21-EXIT
               UNTIL ( WIN1-KEYA NOT = WSV1-KEYA ).
       M2-L02.
           PERFORM M22-YUSCD-TOTAL THRU M22-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : YUSCD-BREAK                                          *
      *----------------------------------------------------------------*
       M21-YUSCD-BREAK.
           MOVE WIN1-KEYB  TO WSV1-KEYB.
           MOVE WKC-KAINO  TO WST1-KAINO.
           MOVE WKC-YTKBN  TO WST1-YTKBN.
           MOVE WKC-YSYBN  TO WST1-YSYBN.
           MOVE WKC-YSBNO  TO WST1-YSBNO.
           MOVE WKC-YSSTN  TO WST1-YSSTN.
           MOVE WKC-YSCKG  TO WST1-YSCKG.
           MOVE WKC-YUSCD  TO WST1-YUSCD.
           MOVE WKC-YUNCD  TO WST1-YUNCD.
           MOVE WKC-SESCD  TO WST1-SESCD.
           MOVE WKC-SQNCD  TO WST1-SQNCD.
           MOVE WKC-ZEIRT6 TO WST1-ZEIRT6.
           MOVE WKC-KGZRT6 TO WST1-KGZRT6.
       M21-L01.
           PERFORM M211-ZEIRT-BREAK THRU M211-EXIT
                   UNTIL ( WIN1-KEYB NOT = WSV1-KEYB ).
       M21-L02.
           PERFORM M212-ZEIRT-TOTAL THRU M212-EXIT.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1.1   : ZEIRT-BREAK                                    *
      *-------------------------------------------------------------*
       M211-ZEIRT-BREAK.
       M211-L01.
           COMPUTE WTT1-ZOKDK = WTT1-ZOKDK + WKC-ZOKDK.
           COMPUTE WTT1-ZSHZI = WTT1-ZSHZI + WKC-ZSHZI.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1.2   : ZEIRT-TOTAL                                    *
      *-------------------------------------------------------------*
       M212-ZEIRT-TOTAL.
           COMPUTE WCT1-CNT = WCT1-CNT + 1.
           PERFORM M2121-MIDASI-WKD   THRU M2121-EXIT.
           PERFORM M2122-EDIT-WKD     THRU M2122-EXIT.
           WRITE WKD-REC FROM WKD-R.
           COMPUTE WTT1-ZOKDK = 0.
           COMPUTE WTT1-ZSHZI = 0.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1 :MIDASI-WKD                                        *
      *----------------------------------------------------------------*
       M2121-MIDASI-WKD.
           MOVE WSV1-ZEIRT6    TO WED1-ZEIRT.
       M2121-L01.
           MOVE WED1-ZEIRT     TO P48-INDAT-X.
           MOVE 4              TO P48-INLEN.
           CALL 'VSBA0480' USING P48-PARM.
           MOVE P48-OTDAT-X    TO WKJ1-ZRITU.
       M2121-L02.
           MOVE SPACE          TO WKJ1-MIDS1.
           IF WSV1-ZEIRT6 = P205-SHZRT
              PERFORM M21211-MIDASI-A THRU M21211-EXIT
           ELSE
              PERFORM M21212-MIDASI-B THRU M21212-EXIT
           END-IF.
       M2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1.1 :  MIDASI-A                                      *
      *----------------------------------------------------------------*
       M21211-MIDASI-A.
           IF WED1-ZEI1 = ZERO
              IF WED1-ZEI3 = ZERO
                 STRING WKJ1-ZRIT2 WKJ1-PTAIS
                    DELIMITED BY SIZE INTO WKJ1-MIDS1
              ELSE
                 STRING WKJ1-ZRIT23 WKJ1-PTAIS
                    DELIMITED BY SIZE INTO WKJ1-MIDS1
              END-IF
           ELSE
              IF WED1-ZEI3 = ZERO
                 STRING WKJ1-ZRIT12 WKJ1-PTAIS
                    DELIMITED BY SIZE INTO WKJ1-MIDS1
              ELSE
                 STRING WKJ1-ZRIT123 WKJ1-PTAIS
                    DELIMITED BY SIZE INTO WKJ1-MIDS1
              END-IF
           END-IF.
       M21211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1.2 :  MIDASI-B                                      *
      *----------------------------------------------------------------*
       M21212-MIDASI-B.
           IF WSV1-KGZRT6 = '1'
              IF WED1-ZEI1 = ZERO
                 IF WED1-ZEI3 = ZERO
                    STRING WKJ1-KGZEI WKJ1-ZRIT2 WKJ1-PZEIK
                       DELIMITED BY SIZE INTO WKJ1-MIDS1
                 ELSE
                    STRING WKJ1-KGZEI WKJ1-ZRIT23 WKJ1-PZEIK
                       DELIMITED BY SIZE INTO WKJ1-MIDS1
                 END-IF
              ELSE
                 IF WED1-ZEI3 = ZERO
                    STRING WKJ1-KGZEI WKJ1-ZRIT12 WKJ1-PZEIK
                       DELIMITED BY SIZE INTO WKJ1-MIDS1
                 ELSE
                    STRING WKJ1-KGZEI WKJ1-ZRIT123 WKJ1-PZEIK
                       DELIMITED BY SIZE INTO WKJ1-MIDS1
                END-IF
           ELSE
             IF WSV1-ZEIRT6 NOT = ZERO
                IF WED1-ZEI1 = ZERO
                   IF WED1-ZEI3 = ZERO
                      STRING WKJ1-KYUZE WKJ1-ZRIT2 WKJ1-PTAIS
                         DELIMITED BY SIZE INTO WKJ1-MIDS1
                   ELSE
                      STRING WKJ1-KYUZE WKJ1-ZRIT23 WKJ1-PTAIS
                         DELIMITED BY SIZE INTO WKJ1-MIDS1
                   END-IF
                ELSE
                   IF WED1-ZEI3 = ZERO
                      STRING WKJ1-KYUZE WKJ1-ZRIT12 WKJ1-PTAIS
                         DELIMITED BY SIZE INTO WKJ1-MIDS1
                   ELSE
                      STRING WKJ1-KYUZE WKJ1-ZRIT123 WKJ1-PTAIS
                         DELIMITED BY SIZE INTO WKJ1-MIDS1
                   END-IF
             ELSE
                STRING WKJ1-HIKAZ
                   DELIMITED BY SIZE INTO WKJ1-MIDS1
             END-IF
           END-IF.
       M21212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.2 : EDIT-WKD                                         *
      *----------------------------------------------------------------*
       M2122-EDIT-WKD.
           MOVE SPACE       TO WKD-R.
           MOVE WST1-KAINO  TO  WKD-KAINO.
           MOVE WST1-YTKBN  TO  WKD-YTKBN.
           MOVE WST1-YSYBN  TO  WKD-YSYBN.
           MOVE WST1-YSBNO  TO  WKD-YSBNO.
           MOVE WST1-YSSTN  TO  WKD-YSSTN.
           MOVE WST1-YSCKG  TO  WKD-YSCKG.
           MOVE WST1-YUSCD  TO  WKD-YUSCD.
           MOVE WST1-YUNCD  TO  WKD-YUNCD.
           MOVE ZERO        TO  WKD-YSALL.
           MOVE '1'         TO  WKD-INFLG.
           MOVE ZERO        TO  WKD-YSTPG.
           MOVE ZERO        TO  WKD-PAGE1.
           MOVE ZERO        TO  WKD-PAGE2.
           MOVE WST1-YUSCD  TO  WKD-SESCD.
           MOVE WST1-YUNCD  TO  WKD-SQNCD.
           COMPUTE WKD-GYONO = WCT1-CNT.
           MOVE '08'        TO  WKD-RECKB.
           MOVE WKJ1-MIDS1  TO  WKD-MIDS1
           IF WST1-ZEIRT6 NOT = ZERO
              COMPUTE WKD-ZOKDK = (WTT1-ZOKDK + WTT1-ZSHZI) / 2
           ELSE
              COMPUTE WKD-ZOKDK = WTT1-ZOKDK + WTT1-ZSHZI.
           MOVE WKJ1-SOHZE  TO  WKD-MIDS2.
           MOVE WTT1-ZSHZI  TO  WKD-ZSHZI.
           MOVE WST1-ZEIRT6 TO  WKD-ZEIRT.
           MOVE WST1-KGZRT6 TO  WKD-KGZRT.
       M2122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : YUSCD-TOTAL                                          *
      *----------------------------------------------------------------*
       M22-YUSCD-TOTAL.
           IF WCT1-CNT > 5
              DISPLAY
               'R VBKA7110 ZEIRITU BETU GOUKEI 5 RECORD OVER!! '
               WSV1-YUSCD WSV1-YUNCD  UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'
           ELSE
              COMPUTE WCT1-CNT  = 0
           END-IF.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    END    VBKA7110                                             *
      *----------------------------------------------------------------*
