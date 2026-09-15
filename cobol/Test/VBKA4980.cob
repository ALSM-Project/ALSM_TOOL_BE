./ ADD NAME=VBKA4980  01.13 20031122 20101001 143208 00329 00383 MCSK020
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4980'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K.A                           *
      *              <> PG.OUTLINE     : TOKUISAKI SEISIKI MEISYOU NO  *
      *                                  HONYAKU  (SEIKYUU SAKI)       *
      *                                                                *
      *                 * INPUT FILE   : SEIKYUUSYO DATA               *
      *                                  (SEIKYUU KAGAMI) (DWKB)       *
      *                 * INPUT FILE   : TOKUISAKI MEISYOU FILE        *
      *                                                  (SFMFILE)     *
      *                 * INPUT FILE   : NOUSEI MASTER   (NSMFILE)     *
      *                 * OUTPUT FILE  : SEIKYUUSYO DATA               *
      *                                  (SEIKYUU KAGAMI) (DWKA)       *
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/08/19                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-568              ÍÝº³   : 03/11/22  CSK-H.YOSHIMOTO     *
      * (02) K-1228             ÍÝº³   : 10/06/22  CSK-M.KODAMA        *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SFMFILE ASSIGN    TO SFMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
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
           02 FILLER                        PIC  X(300).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  SFM-R.
           COPY VFILSFM SUPPRESS.
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
       01  WKA-R.
           02 WKA-CTLKEY.
              03 WKA-KAINO                     PIC  X(02).
              03 WKA-YTKBN                     PIC  X(01).
              03 WKA-YSYBN.
                 04 WKA-YSYB1                  PIC  X(03).
                 04 WKA-YSYB2                  PIC  X(04).
              03 WKA-YSBNO                     PIC  X(03).
              03 WKA-YSSTN                     PIC  X(03).
              03 WKA-YSCKG                     PIC  X(05).
              03 WKA-YUSCD                     PIC  X(07).
              03 WKA-YUNCD                     PIC  X(02).
              03 WKA-YSALL                     PIC  9(04).
              03 WKA-INFLG                     PIC  X(01).
              03 WKA-YSTPG                     PIC  9(04).
              03 WKA-PAGEN.
                 04 WKA-PAGE1                  PIC  9(04).
                 04 WKA-PAGE2                  PIC  9(04).
              03 WKA-SESCD                     PIC  X(07).
              03 WKA-SQNCD                     PIC  X(02).
           02 WKA-LPDAT                        PIC  X(219).
           02 WKA-DATA20  REDEFINES  WKA-LPDAT.
              03 WKA-GNO20                     PIC  9(02).
              03 WKA-RKB20                     PIC  X(02).
              03 WKA-MESAI2.
                 04 WKA-TMSJU                  PIC  X(40).
                 04 WKA-TMSJL                  PIC  X(40).
                 04 WKA-NOSMJ                  PIC  X(40).
                 04 WKA-SQBNO                  PIC  X(03).
                 04 WKA-SQSTN                  PIC  X(03).
                 04 WKA-SQCKK                  PIC  X(05).
                 04 WKA-SMEYMD.
                    05 WKA-SIMYY               PIC  9(04).
                    05 WKA-SIMMM               PIC  9(02).
                    05 WKA-SIMDD               PIC  9(02).
                 04 WKA-INKAN                  PIC  X(01).
                 04 FILLER                     PIC  X(75).
           02 WKA-PGMSU                        PIC  X(02).
           02 WKA-SORTJ                        PIC  X(20).
           02 FILLER                           PIC  X(03).
       01  WKB-R.
           02 WKB-CTLKEY.
              03 WKB-KAINO                     PIC  X(02).
              03 WKB-YTKBN                     PIC  X(01).
              03 WKB-YSYBN.
                 04 WKB-YSYB1                  PIC  X(03).
                 04 WKB-YSYB2                  PIC  X(04).
              03 WKB-YSBNO                     PIC  X(03).
              03 WKB-YSSTN                     PIC  X(03).
              03 WKB-YSCKG                     PIC  X(05).
              03 WKB-YUSCD                     PIC  X(07).
              03 WKB-YUNCD                     PIC  X(02).
              03 WKB-YSALL                     PIC  9(04).
              03 WKB-INFLG                     PIC  X(01).
              03 WKB-YSTPG                     PIC  9(04).
              03 WKB-PAGEN.
                 04 WKB-PAGE1                  PIC  9(04).
                 04 WKB-PAGE2                  PIC  9(04).
              03 WKB-SESCD                     PIC  X(07).
              03 WKB-SQNCD                     PIC  X(02).
           02 WKB-LPDAT                        PIC  X(219).
           02 WKB-DATA20  REDEFINES  WKB-LPDAT.
              03 WKB-GNO20                     PIC  9(02).
              03 WKB-RKB20                     PIC  X(02).
              03 WKB-MESAI2.
                 04 WKB-TMSJU                  PIC  X(40).
                 04 WKB-TMSJL                  PIC  X(40).
                 04 WKB-NOSMJ                  PIC  X(40).
                 04 WKB-SQBNO                  PIC  X(03).
                 04 WKB-SQSTN                  PIC  X(03).
                 04 WKB-SQCKK                  PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY               PIC  9(04).
                    05 WKB-SIMMM               PIC  9(02).
                    05 WKB-SIMDD               PIC  9(02).
                 04 WKB-INKAN                  PIC  X(01).
                 04 FILLER                     PIC  X(75).
           02 WKB-PGMSU                        PIC  X(02).
           02 WKB-SORTJ                        PIC  X(20).
           02 FILLER                           PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SESCD                 PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD                 PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SESCD                    PIC  X(07).
       01  EDIT-AREA.
           02 WED1-SESCD.
              03 WED1-SESCD1                   PIC  X(01).
              03 FILLER                        PIC  X(06).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  SFMFILE
                       NSMFILE
                       WKBFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-SFM THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKB-SESCD TO WIN1-SESCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ SFMFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-SFM.
           MOVE 'Z '    TO   S-CODE.
           READ SFMFILE INTO SFM-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA4980 SFMFILE READ ERROR !!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN1-KEY
              GO TO M12-EXIT.
           MOVE SFM-TOKCD TO WIN2-TOKCD.
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
                 PERFORM M22-WKBONL THRU M22-EXIT
              ELSE
                 PERFORM M23-SFMONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M211-EDIT-WKA THRU M211-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
           PERFORM M12-READ-SFM THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT WKA                                           *
      *----------------------------------------------------------------*
       M211-EDIT-WKA.
           MOVE WKB-R     TO WKA-R.
           IF WKA-RKB20 NOT = '20'
              GO TO M211-L01
           END-IF.
           MOVE SPACE     TO WKA-TMSJU.
           MOVE SPACE     TO WKA-TMSJL.
           MOVE SPACE     TO WKA-NOSMJ.
           MOVE WKB-SESCD TO WED1-SESCD.
      *
           IF WKB-INKAN  =  '1'
              GO TO M211-L01
           END-IF.
      *
           IF WKB-SQNCD NOT = SPACE AND WKB-SQNCD NOT = '00'
              PERFORM M2111-READ-NSM THRU M2111-EXIT
              IF WED1-SESCD1 NOT = '8'
                 MOVE SFM-TMSJU TO WKA-TMSJU
                 MOVE SFM-TMSJL TO WKA-TMSJL
              END-IF
           ELSE
              MOVE SFM-TMSJU TO WKA-TMSJL
              MOVE SFM-TMSJL TO WKA-NOSMJ
           END-IF.
       M211-L01.
           WRITE WKA-REC  FROM  WKA-R.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : READ NSMFILE                                     *
      *----------------------------------------------------------------*
       M2111-READ-NSM.
           MOVE WKB-SESCD TO KEY-NSM1.
           MOVE WKB-SQNCD TO KEY-NSM2.
           MOVE 'Z '      TO S-CODE.
           READ NSMFILE INTO NSM-R
                INVALID KEY
                        MOVE SPACE TO WKA-NOSMJ
                        GO TO M2111-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA4980 NSMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO NSM-R.
           MOVE NSM-NOSKA   TO WKA-NOSMJ.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKBONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M221-WRITE-WKA  THRU M221-EXIT
                   UNTIL WIN1-KEY1 NOT = WSV1-KEY1.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : WRITE WKA                                          *
      *----------------------------------------------------------------*
       M221-WRITE-WKA.
           MOVE WKB-R    TO WKA-R.
           WRITE WKA-REC FROM WKA-R.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : SFMFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-SFMONL.
           PERFORM M12-READ-SFM THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE SFMFILE
                 NSMFILE
                 WKBFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *           VBKA4980 END                                         *
      *----------------------------------------------------------------*
