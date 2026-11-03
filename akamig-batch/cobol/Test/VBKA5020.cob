./ ADD NAME=VBKA5020  01.14 20031126 20101005 150319 00272 00283 MCSK036
       ID  DIVISION.
       PROGRAM-ID. 'VBKA5020'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : SEIKYU GOUKEIHYOU             *
      *                                  SEIKYUSAKIMEI HONYAKU         *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEIHYOU JOUHOU      *
      *                                                          (DWKB)*
      *                 * INPUT FILE   : NOUSEI MASTER        (NSMFILE)*
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MEISYO FILE(SFMFILE)*
      *                                                                *
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU JOUHOU      *
      *                                                          (DWKA)*
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/07/29                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-568              ÍÝº³   : 03/11/26  CSK-H.YOSHIMOTO     *
      * (02) K-1228             ÍÝº³   : 10/07/05  CSK-M.KODAMA        *
      * (03) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SFMFILE ASSIGN    TO SFMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-SFM
                          FILE  STATUS S-CODE.
           SELECT NSMFILE ASSIGN    TO NSMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-NSM
                          FILE  STATUS S-CODE.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  SFMFILE
           LABEL RECORD STANDARD.
       01  SFM-REC.
           02 KEY-SFM                       PIC  X(07).
           02 FILLER                        PIC  X(203).
       FD  NSMFILE
           LABEL RECORD STANDARD.
       01  NSM-REC.
           02 KEY-NSM.
              03 KEY-NSM1                   PIC  X(07).
              03 KEY-NSM2                   PIC  X(02).
           02 FILLER                        PIC  X(91).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(280).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(280).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  SFM-R.
           COPY VFILSFM SUPPRESS.
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
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
              03 WKB-SESCD.
                 04 WKB-SESCD1              PIC  X(01).
                 04 WKB-SESCD2              PIC  X(06).
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
       01  WKA-R.
           02 WKA-CNTKEY.
              03 WKA-KAINO                  PIC  X(02).
              03 WKA-YTKBN                  PIC  X(01).
              03 WKA-YSYBN.
                 04 WKA-YSYB1               PIC  X(03).
                 04 WKA-YSYB2               PIC  X(04).
              03 WKA-YUSBNO                 PIC  X(03).
              03 WKA-YUSNO                  PIC  X(03).
              03 WKA-YUSCKK                 PIC  X(05).
              03 WKA-YUSCD                  PIC  X(07).
              03 WKA-YUNCD                  PIC  X(02).
              03 WKA-YSALL                  PIC  9(04).
              03 WKA-INFLG                  PIC  X(01).
              03 WKA-YSTPG                  PIC  9(04).
              03 WKA-PAGEN.
                 04 WKA-PAGE1               PIC  9(04).
                 04 WKA-PAGE2               PIC  9(04).
              03 WKA-SESCD                  PIC  X(07).
              03 WKA-SQNCD                  PIC  X(02).
           02 WKA-GYOBG                     PIC  9(02).
           02 WKA-RECKB                     PIC  X(02).
           02 WKA-ZENGT.
              03 WKA-ZSKDK                  PIC S9(13)  COMP-3.
              03 WKA-ZNKDK                  PIC S9(13)  COMP-3.
              03 WKA-ZSYNB                  PIC S9(09)  COMP-3.
              03 WKA-ZKRDK                  PIC S9(13)  COMP-3.
           02 WKA-TOGTU.
              03 WKA-TKIDK                  PIC S9(13)  COMP-3.
              03 WKA-TSKDK                  PIC S9(13)  COMP-3.
              03 WKA-MSKSK                  PIC S9(13)  COMP-3.
              03 WKA-TURZN                  PIC S9(13)  COMP-3.
           02 WKA-SHZEI                     PIC S9(13)  COMP-3.
           02 WKA-TKAIG                     PIC S9(13)  COMP-3.
           02 WKA-SESM1                     PIC  X(40).
           02 WKA-SESM2                     PIC  X(40).
           02 WKA-SENME                     PIC  X(30).
           02 WKA-HIFLG                     PIC  X(01).
           02 WKA-SQBNO                     PIC  X(03).
           02 WKA-SQSTN                     PIC  X(03).
           02 WKA-SQCKK                     PIC  X(05).
           02 WKA-SMEYMD                    PIC  9(08).
           02 WKA-INKAN                     PIC  X(01).
           02 FILLER                        PIC  X(21).
       PROCEDURE DIVISION.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WKB-R = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                       SFMFILE
                       NSMFILE
                OUTPUT WKAFILE.
       M1-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                 AT END
                    MOVE HIGH-VALUE TO WKB-R
                    GO TO M11-EXIT.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           MOVE WKB-R TO WKA-R.
           IF WKB-INKAN NOT = '1'
              PERFORM M21-EDIT-WKA THRU M21-EXIT.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1 : EDIT WKAFILE                                      *
      *-------------------------------------------------------------*
       M21-EDIT-WKA.
           MOVE WKB-SESCD    TO KEY-SFM.
           PERFORM M211-READ-SFM THRU M211-EXIT.
       M21-L01.
           IF ( WKB-SQNCD NOT = SPACE ) AND ( WKB-SQNCD NOT = '00' )
              MOVE WKB-SESCD TO KEY-NSM1
              MOVE WKB-SQNCD TO KEY-NSM2
              PERFORM M212-READ-NSM THRU M212-EXIT
           ELSE
              MOVE SPACE      TO WKA-SENME.
           IF WKB-SESCD1 = '8' AND
              ( WKB-SQNCD NOT = SPACE ) AND ( WKB-SQNCD NOT = '00' )
              MOVE SPACE     TO WKA-SESM1
              MOVE SPACE     TO WKA-SESM2.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.1 : READ SFMFILE                                    *
      *-------------------------------------------------------------*
       M211-READ-SFM.
           MOVE 'Z '      TO S-CODE.
           READ SFMFILE INTO SFM-R
                INVALID KEY
                        MOVE SPACE TO WKA-SESM1
                        MOVE SPACE TO WKA-SESM2
                        GO TO M211-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA5020 SFMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO SFM-R.
           MOVE SFM-TMSJU TO WKA-SESM1.
           MOVE SFM-TMSJL TO WKA-SESM2.
       M211-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.2 : READ NSMFILE                                    *
      *-------------------------------------------------------------*
       M212-READ-NSM.
           MOVE 'Z '      TO S-CODE.
           READ NSMFILE INTO NSM-R
                INVALID KEY
                        MOVE SPACE TO WKA-SENME
                        GO TO M212-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA5020 NSMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO NSM-R.
           MOVE NSM-NOSKA TO WKA-SENME.
       M212-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 SFMFILE
                 NSMFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA5020  END                                          *
      *-------------------------------------------------------------*
