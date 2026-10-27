./ ADD NAME=VBKA5030  01.00 20191001 20191001 145454 00548 00548 MCSK009
       ID  DIVISION.
       PROGRAM-ID. 'VBKA5030'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : SEIKYU GOUKEIHYOU URIKAKEKIN  *
      *                                  GOUKEI REKO-DO SAKUSEI        *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEIHYOU JOUHOU      *
      *                                                          (DWKB)*
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU DATA  (DWKA)*
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU             *
      *                                               PAGE DATA  (DWKC)*
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/07/30                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K02163             ÍÝº³   : 19/07/23  SCSK-A.ISIKAWA      *
      * (02) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(280).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(120).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(20).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       01  WKB-R.
           02 WKB-CNTKEY.
              03 WKB-KAINO                  PIC  X(02).
              03 WKB-YTKBN                  PIC  X(01).
              03 WKB-YSYBN.
                 04 WKB-YSYB1               PIC  X(03).
                 04 WKB-YSYB2               PIC  X(04).
              03 WKB-YUSBNO                 PIC  X(03).
              03 WKB-YUSNO                  PIC  X(03).
              03 WKB-YUSCKK                 PIC  X(05).
              03 WKB-YUSCD                  PIC  X(07).
              03 WKB-YUNCD                  PIC  X(02).
              03 WKB-YSALL                  PIC  9(04).
              03 WKB-INFLG                  PIC  X(01).
              03 WKB-YSTPG                  PIC  9(04).
              03 WKB-PAGEN.
                 04 WKB-PAGE1               PIC  9(04).
                 04 WKB-PAGE2               PIC  9(04).
              03 WKB-SESCD                  PIC  X(07).
              03 WKB-SQNCD                  PIC  X(02).
           02 WKB-GYOBG                     PIC  9(02).
           02 WKB-RECKB                     PIC  X(02).
           02 WKB-ZENGT.
              03 WKB-ZSKDK                  PIC S9(13)  COMP-3.
              03 WKB-ZNKDK                  PIC S9(13)  COMP-3.
              03 WKB-ZSYNB                  PIC S9(09)  COMP-3.
              03 WKB-ZKRDK                  PIC S9(13)  COMP-3.
           02 WKB-TOGTU.
              03 WKB-TKIDK                  PIC S9(13)  COMP-3.
              03 WKB-TSKDK                  PIC S9(13)  COMP-3.
              03 WKB-MSKSK                  PIC S9(13)  COMP-3.
              03 WKB-TURZN                  PIC S9(13)  COMP-3.
           02 WKB-SHZEI                     PIC S9(13)  COMP-3.
           02 WKB-TKAIG                     PIC S9(13)  COMP-3.
           02 WKB-SESM1                     PIC  X(40).
           02 WKB-SESM2                     PIC  X(40).
           02 WKB-SENME                     PIC  X(30).
           02 WKB-HIFLG                     PIC  X(01).
           02 WKB-SQBNO                     PIC  X(03).
           02 WKB-SQSTN                     PIC  X(03).
           02 WKB-SQCKK                     PIC  X(05).
           02 WKB-SMEYMD                    PIC  9(08).
           02 WKB-INKAN                     PIC  X(01).
           02 FILLER                        PIC  X(21).
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
       01  WKA-R.
           02 WKA-CNTKEY.
             03 WKA-KAINO                   PIC  X(02).
             03 WKA-YTKBN                   PIC  X(01).
             03 WKA-YSYBN.
                04 WKA-YSYB1                PIC  X(03).
                04 WKA-YSYB2                PIC  X(04).
             03 WKA-YUSBNO                  PIC  X(03).
             03 WKA-YUSNO                   PIC  X(03).
             03 WKA-YUSCKK                  PIC  X(05).
             03 WKA-YUSCD                   PIC  X(07).
             03 WKA-YUNCD                   PIC  X(02).
             03 WKA-YSALL                   PIC  9(04).
             03 WKA-INFLG                   PIC  X(01).
             03 WKA-YSTPG                   PIC  9(04).
             03 WKA-PAGEN.
                04 WKA-PAGE1                PIC  9(04).
                04 WKA-PAGE2                PIC  9(04).
             03 WKA-SESCD                   PIC  X(07).
             03 WKA-SQNCD                   PIC  X(02).
           02 WKA-LPDAT                     PIC  X(219).
           02 WKA-DATA03  REDEFINES  WKA-LPDAT.
              03 WKA-GNO03                  PIC  9(02).
              03 WKA-RKB03                  PIC  X(02).
              03 WKA-MESAI3.
                 04 WKA-TJYSO.
                    05 WKA-TJYSA            PIC  X(40).
                    05 WKA-TJYSB            PIC  X(30).
                    05 WKA-TKBLMJ           PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKA-DATA04  REDEFINES  WKA-LPDAT.
              03 WKA-GNO04                  PIC  9(02).
              03 WKA-RKB04                  PIC  X(02).
              03 WKA-MESAI4.
                 04 WKA-TNMJO4              PIC  X(40).
                 04 WKA-TNMGE4              PIC  X(40).
                 04 WKA-NOSKA4              PIC  X(40).
                 04 WKA-YSBN04              PIC  X(03).
                 04 WKA-YSSN04              PIC  X(03).
                 04 WKA-YSCK04              PIC  X(05).
                 04 WKA-SMEYMD.
                    05 WKA-SIMYY            PIC  9(04).
                    05 WKA-SIMMM            PIC  9(02).
                    05 WKA-SIMDD            PIC  9(02).
                 04 FILLER                  PIC  X(76).
           02 WKA-DATA07  REDEFINES  WKA-LPDAT.
              03 WKA-GNO07                  PIC  9(02).
              03 WKA-RKB07                  PIC  X(02).
              03 WKA-MESAI7.
                 04 WKA-ZENGT7.
                    05 WKA-ZSKDK7           PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK7           PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB7           PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK7           PIC S9(13)  COMP-3.
                 04 WKA-TOGTU7.
                    05 WKA-TKIDK7           PIC S9(13)  COMP-3.
                    05 WKA-TSKDK7           PIC S9(13)  COMP-3.
                    05 WKA-MSKSK7           PIC S9(13)  COMP-3.
                    05 WKA-TURZN7           PIC S9(13)  COMP-3.
                 04 WKA-SHZEI7              PIC S9(13)  COMP-3.
                 04 WKA-TKAIG7              PIC S9(13)  COMP-3.
                 04 FILLER                  PIC  X(147).
           02 WKA-DATA08  REDEFINES  WKA-LPDAT.
              03 WKA-GNO08                  PIC  9(02).
              03 WKA-RKB08                  PIC  X(02).
              03 WKA-ZBEGK.
                 04 WKA-MIDS1               PIC  X(22).
                 04 WKA-ZOKDK               PIC S9(13)   COMP-3.
                 04 WKA-MIDS2               PIC  X(08).
                 04 WKA-ZSHZI               PIC S9(13)   COMP-3.
                 04 WKA-ZEIRT               PIC S9(02)V9 COMP-3.
                 04 WKA-KGZRT               PIC  X(01).
                 04 FILLER                  PIC X(168).
           02 WKA-DATA09  REDEFINES  WKA-LPDAT.
              03 WKA-GNO09                  PIC  9(02).
              03 WKA-RKB09                  PIC  X(02).
              03 WKA-MESAI9.
                 04 WKA-ZENGT9.
                    05 WKA-ZSKDK9           PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK9           PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB9           PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK9           PIC S9(13)  COMP-3.
                 04 WKA-TOGTU9.
                    05 WKA-TKIDK9           PIC S9(13)  COMP-3.
                    05 WKA-TSKDK9           PIC S9(13)  COMP-3.
                    05 WKA-MSKSK9           PIC S9(13)  COMP-3.
                    05 WKA-TURZN9           PIC S9(13)  COMP-3.
                 04 WKA-SHZEI9              PIC S9(13)  COMP-3.
                 04 WKA-TKAIG9              PIC S9(13)  COMP-3.
                 04 WKA-TNMJO9              PIC  X(40).
                 04 WKA-TNMGE9              PIC  X(40).
                 04 WKA-NOSKA9              PIC  X(30).
                 04 WKA-HIFLG9              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  WKC-R.
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-PAGEU                     PIC  9(04).
           02 FILLER                        PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-YUSCD                 PIC  X(07).
                 04 WIN1-YUNCD                 PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-YUSCD                 PIC  X(07).
                 04 WIN2-YUNCD                 PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-YUSCD                 PIC  X(07).
              03 WSV1-YUNCD                 PIC  X(02).
           02 WSVK-CNTKEY.
              03 WSVK-KAINO                  PIC  X(02).
              03 WSVK-YTKBN                  PIC  X(01).
              03 WSVK-YSYBN.
                 04 WSVK-YSYB1               PIC  X(03).
                 04 WSVK-YSYB2               PIC  X(04).
              03 WSVK-YUSBNO                 PIC  X(03).
              03 WSVK-YUSNO                  PIC  X(03).
              03 WSVK-YUSCKK                 PIC  X(05).
              03 WSVK-YUSCD                  PIC  X(07).
              03 WSVK-YUNCD                  PIC  X(02).
              03 WSVK-YSALL                  PIC  9(04).
              03 WSVK-INFLG                  PIC  X(01).
              03 WSVK-YSTPG                  PIC  9(04).
              03 WSVK-PAGEN.
                 04 WSVK-PAGE1               PIC  9(04).
                 04 WSVK-PAGE2               PIC  9(04).
              03 WSVK-SESCD                  PIC  X(07).
              03 WSVK-SQNCD                  PIC  X(02).
           02 WSV1-SMEYMD                   PIC  9(08).
       01  COUNT-AREA.
           02 WCT1-COUNT.
              03 WCT1-GYO                   PIC  9(02).
              03 WCT1-PAGE                  PIC  9(04).
       01  TOTAL-AREA.
           02 WTT1-ZSKDK                    PIC S9(13)  COMP-3.
           02 WTT1-ZNKDK                    PIC S9(13)  COMP-3.
           02 WTT1-ZSYNB                    PIC S9(09)  COMP-3.
           02 WTT1-ZKRDK                    PIC S9(13)  COMP-3.
           02 WTT1-TKIDK                    PIC S9(13)  COMP-3.
           02 WTT1-TSKDK                    PIC S9(13)  COMP-3.
           02 WTT1-MSKSK                    PIC S9(13)  COMP-3.
           02 WTT1-TURZN                    PIC S9(13)  COMP-3.
           02 WTT1-SHZEI                    PIC S9(13)  COMP-3.
           02 WTT1-TKAIG                    PIC S9(13)  COMP-3.
       PROCEDURE DIVISION.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                       WKDFILE
                OUTPUT WKAFILE
                       WKCFILE.
       M1-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-WKD THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKB-YUSCD TO WIN1-YUSCD.
           MOVE WKB-YUNCD TO WIN1-YUNCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.2 : READ WKDFILE                                      *
      *-------------------------------------------------------------*
       M12-READ-WKD.
           READ WKDFILE INTO WKD-R
                 AT END
                    MOVE HIGH-VALUE TO WIN2-KEY
                    GO TO M12-EXIT.
       M12-L01.
           MOVE WKD-YUSCD TO WIN2-YUSCD.
           MOVE WKD-YUNCD TO WIN2-YUNCD.
       M12-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN       (MATCHING)                               *
      *-------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
             IF WIN1-KEY < WIN2-KEY
                PERFORM M22-WKBONL THRU M22-EXIT
             ELSE
                PERFORM M23-WKDONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1 : MATCH                                             *
      *-------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           INITIALIZE         TOTAL-AREA.
           MOVE WKB-CNTKEY TO WSVK-CNTKEY.
           MOVE WKB-SMEYMD TO WSV1-SMEYMD.
           MOVE 1          TO WCT1-PAGE.
           MOVE ZERO       TO WCT1-GYO.
           PERFORM M211-EDIT09-WKA THRU M211-EXIT
                   UNTIL WIN1-KEY NOT = WSV1-KEY1.

           PERFORM M212-EDIT03-WKA THRU M212-EXIT.
           PERFORM M213-EDIT04-WKA THRU M213-EXIT.
           PERFORM M214-EDIT07-WKA THRU M214-EXIT.
           PERFORM M215-EDITXX-WKC THRU M215-EXIT.
           IF WIN2-KEY     = HIGH-VALUE
              GO TO M21-EXIT.
             PERFORM M216-DWKD-WKA   THRU M216-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV1-KEY1.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.1 : EDIT09    (WRITE WKA-REC : RECORD KUBUN = 09 )  *
      *-------------------------------------------------------------*
       M211-EDIT09-WKA.
           MOVE SPACE      TO WKA-R.
           INITIALIZE         WKA-R.
           MOVE WKB-KAINO  TO WKA-KAINO.
           MOVE WKB-YTKBN  TO WKA-YTKBN.
           MOVE WKB-YSYBN  TO WKA-YSYBN.
           MOVE WKB-YUSBNO TO WKA-YUSBNO.
           MOVE WKB-YUSNO  TO WKA-YUSNO.
           MOVE WKB-YUSCKK TO WKA-YUSCKK.
           MOVE WKB-YUSCD  TO WKA-YUSCD.
           MOVE WKB-YUNCD  TO WKA-YUNCD.
           MOVE WKB-YSALL  TO WKA-YSALL.
           MOVE WKB-INFLG  TO WKA-INFLG.
           MOVE WKB-YSTPG  TO WKA-YSTPG.
           MOVE WKB-PAGE1  TO WKA-PAGE1.
           ADD  1          TO WCT1-GYO.
           IF WCT1-GYO > 6 AND WCT1-PAGE = 1
              ADD  1       TO WCT1-PAGE
              MOVE 1       TO WCT1-GYO
           ELSE
              IF WCT1-GYO > 10 AND WCT1-PAGE NOT = 1
                 ADD  1   TO WCT1-PAGE
                 MOVE 1   TO WCT1-GYO.
           MOVE WCT1-PAGE TO WKA-PAGE2.
           MOVE WCT1-GYO  TO WKA-GNO09.
           IF WCT1-PAGE = 1
              ADD  5      TO WKA-GNO09.
           MOVE WKB-RECKB TO WKA-RKB09.
           MOVE WKB-ZENGT TO WKA-ZENGT9.
           MOVE WKB-TOGTU TO WKA-TOGTU9.
           MOVE WKB-SHZEI TO WKA-SHZEI9.
           MOVE WKB-TKAIG TO WKA-TKAIG9.
           MOVE WKB-SESM1 TO WKA-TNMJO9.
           MOVE WKB-SESM2 TO WKA-TNMGE9.
           MOVE WKB-SENME TO WKA-NOSKA9.
           MOVE WKB-HIFLG TO WKA-HIFLG9.
           MOVE WKB-SESCD TO WKA-SESCD.
           MOVE WKB-SQNCD TO WKA-SQNCD.
           WRITE WKA-REC FROM WKA-R.
       M211-L01.
           COMPUTE WTT1-ZSKDK = WTT1-ZSKDK + WKB-ZSKDK.
           COMPUTE WTT1-ZNKDK = WTT1-ZNKDK + WKB-ZNKDK.
           COMPUTE WTT1-ZSYNB = WTT1-ZSYNB + WKB-ZSYNB.
           COMPUTE WTT1-ZKRDK = WTT1-ZKRDK + WKB-ZKRDK.
           COMPUTE WTT1-TKIDK = WTT1-TKIDK + WKB-TKIDK.
           COMPUTE WTT1-TSKDK = WTT1-TSKDK + WKB-TSKDK.
           COMPUTE WTT1-MSKSK = WTT1-MSKSK + WKB-MSKSK.
           COMPUTE WTT1-TURZN = WTT1-TURZN + WKB-TURZN.
           COMPUTE WTT1-SHZEI = WTT1-SHZEI + WKB-SHZEI.
           COMPUTE WTT1-TKAIG = WTT1-TKAIG + WKB-TKAIG.
       M211-L02.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.2 : EDIT03    (WRITE WKA-REC : RECORD KUBUN = 03 )  *
      *-------------------------------------------------------------*
       M212-EDIT03-WKA.
           MOVE SPACE       TO WKA-DATA03.
           INITIALIZE          WKA-DATA03.
           MOVE WSVK-CNTKEY TO WKA-CNTKEY.
           COMPUTE  WKA-PAGE2  = 1.
           MOVE ZERO        TO WKA-GNO03.
           MOVE '03'        TO WKA-RKB03.
           WRITE WKA-REC FROM WKA-R.
       M212-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.3 : EDIT04    (WRITE WKA-REC : RECORD KUBUN = 04 )  *
      *-------------------------------------------------------------*
       M213-EDIT04-WKA.
           MOVE SPACE       TO WKA-DATA04.
           INITIALIZE          WKA-DATA04.
           MOVE WSVK-CNTKEY TO WKA-CNTKEY.
           COMPUTE  WKA-PAGE2  = 1.
           MOVE ZERO        TO WKA-GNO04.
           MOVE '04'        TO WKA-RKB04.
           MOVE WSVK-YUSBNO TO WKA-YSBN04.
           MOVE WSVK-YUSNO  TO WKA-YSSN04.
           MOVE WSVK-YUSCKK TO WKA-YSCK04.
           MOVE WSV1-SMEYMD TO WKA-SMEYMD.
           WRITE WKA-REC FROM WKA-R.
       M213-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.4 : EDIT07    (WRITE WKA-REC : RECORD KUBUN = 07 )  *
      *-------------------------------------------------------------*
       M214-EDIT07-WKA.
           MOVE SPACE       TO WKA-DATA07.
           INITIALIZE          WKA-DATA07.
           MOVE WSVK-CNTKEY TO WKA-CNTKEY.
           COMPUTE  WKA-PAGE2  = 1.
           MOVE ZERO        TO WKA-GNO07.
           MOVE '07'        TO WKA-RKB07.
           MOVE WTT1-ZSKDK  TO WKA-ZSKDK7.
           MOVE WTT1-ZNKDK  TO WKA-ZNKDK7.
           MOVE WTT1-ZSYNB  TO WKA-ZSYNB7.
           MOVE WTT1-ZKRDK  TO WKA-ZKRDK7.
           MOVE WTT1-TKIDK  TO WKA-TKIDK7.
           MOVE WTT1-TSKDK  TO WKA-TSKDK7.
           MOVE WTT1-MSKSK  TO WKA-MSKSK7.
           MOVE WTT1-TURZN  TO WKA-TURZN7.
           MOVE WTT1-SHZEI  TO WKA-SHZEI7.
           MOVE WTT1-TKAIG  TO WKA-TKAIG7.
           WRITE WKA-REC FROM WKA-R.
       M214-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.5 : EDIT WKCFILE                                    *
      *-------------------------------------------------------------*
       M215-EDITXX-WKC.
           MOVE SPACE      TO WKC-R.
           INITIALIZE         WKC-R.
           MOVE WSVK-YUSCD TO WKC-YUSCD.
           MOVE WSVK-YUNCD TO WKC-YUNCD.
           MOVE WCT1-PAGE  TO WKC-PAGEU.
           WRITE WKC-REC FROM WKC-R.
       M215-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.6 : WKD TO WKAFILE                                  *
      *-------------------------------------------------------------*
       M216-DWKD-WKA.
           MOVE SPACE      TO WKA-R.
           INITIALIZE         WKA-R.
       M216-L01.
           MOVE WSVK-CNTKEY TO WKA-CNTKEY.
           MOVE WKD-YSALL   TO WKA-YSALL.
           MOVE WKD-INFLG   TO WKA-INFLG.
           MOVE WKD-YSTPG   TO WKA-YSTPG.
           MOVE WKD-PAGE1   TO WKA-PAGE1.
           COMPUTE  WKA-PAGE2  = 1.
           MOVE WKD-GYONO   TO WKA-GNO08.
           MOVE WKD-RECKB   TO WKA-RKB08.
           MOVE WKD-MIDS1   TO WKA-MIDS1.
           MOVE WKD-ZOKDK   TO WKA-ZOKDK.
           MOVE WKD-MIDS2   TO WKA-MIDS2.
           MOVE WKD-ZSHZI   TO WKA-ZSHZI.
           MOVE WKD-ZEIRT   TO WKA-ZEIRT.
           MOVE WKD-KGZRT   TO WKA-KGZRT.
           WRITE WKA-REC FROM WKA-R.
       M216-L02.
           PERFORM M12-READ-WKD THRU M12-EXIT.
       M216-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2 : WKBONL                                            *
      *-------------------------------------------------------------*
       M22-WKBONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           INITIALIZE         TOTAL-AREA.
           MOVE WKB-CNTKEY TO WSVK-CNTKEY.
           MOVE WKB-SMEYMD TO WSV1-SMEYMD.
           MOVE 1          TO WCT1-PAGE.
           MOVE ZERO       TO WCT1-GYO.
           PERFORM M211-EDIT09-WKA THRU M211-EXIT
                   UNTIL WIN1-KEY NOT = WSV1-KEY1.

           PERFORM M212-EDIT03-WKA THRU M212-EXIT.
           PERFORM M213-EDIT04-WKA THRU M213-EXIT.
           PERFORM M214-EDIT07-WKA THRU M214-EXIT.
           PERFORM M215-EDITXX-WKC THRU M215-EXIT.
       M22-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.3 : WKDONL                                            *
      *-------------------------------------------------------------*
       M23-WKDONL.
           DISPLAY 'R VBKA5030 SEIKYU GOUKEI HYO JOHO (DWKD) ONLY!! '
                   WIN2-KEY  UPON CONSOLE.
           CALL 'VSBABEND'.
       M23-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKDFILE
                 WKAFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA5030  END                                          *
      *-------------------------------------------------------------*


