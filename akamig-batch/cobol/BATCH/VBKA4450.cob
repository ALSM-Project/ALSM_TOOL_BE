./ ADD NAME=VBKA4450  01.09 20030819 20030922 141516 00355 00349 MCSK042
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4450'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : KOUZA FURIKAE BUN HANEI SYORI *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU DATA (KOUZA FURIKAE)   *
      *                                                         (DWKB) *
      *                                : SEIKYUSYO DATA                *
      *                                  (SEIKYUU KAGAMI)       (DWKC) *
      *                 * OUTPUT FILE  : SEIKYUSYO DATA                *
      *                                  (SEIKYUU KAGAMI)       (DWKA) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 99/03/08                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) H-010              ÍÝº³   : 99/09/09  CSK-H.YOSHIMOTO     *
      * (02) K-568              ÍÝº³   : 02/08/19  CSK-C.YAMAGATA      *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(030).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(300).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  P48-PARM.
           COPY VPARB48  SUPPRESS.
       01  WKB-R.
           02 WKB-SKTCD                     PIC  X(07).
           02 WKB-SKNCD                     PIC  X(02).
           02 WKB-KZHKB                     PIC  9(08).
           02 WKB-HKOGK                     PIC S9(11)  COMP-3.
           02 WKB-HJPTN                     PIC  X(01).
           02 FILLER                        PIC  X(06).
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
           02 WKC-DATA30  REDEFINES  WKC-LPDAT.
              03 WKC-GNO30                  PIC  9(02).
              03 WKC-RKB30                  PIC  X(02).
              03 WKC-MESAI3.
                 04 WKC-HJPTN1              PIC  X(01).
                 04 WKC-MIDAS               PIC  X(08).
                 04 WKC-TRBNK  OCCURS  3.
                    05 WKC-TGSME            PIC  X(30).
                    05 WKC-KOZKJ            PIC  X(04).
                    05 WKC-KZABG            PIC  X(07).
                 04 WKC-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKA-R.
           02 WKA-CTLKEY.
              03 WKA-KAINO                  PIC  X(02).
              03 WKA-YTKBN                  PIC  X(01).
              03 WKA-YSYBN.
                 04 WKA-YSYB1               PIC  X(03).
                 04 WKA-YSYB2               PIC  X(04).
              03 WKA-YSBNO                  PIC  X(03).
              03 WKA-YSSTN                  PIC  X(03).
              03 WKA-YSCKG                  PIC  X(05).
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
           02 WKA-LPDAT                     PIC  X(219).
           02 WKA-DATA30  REDEFINES  WKA-LPDAT.
              03 WKA-GNO30                  PIC  9(02).
              03 WKA-RKB30                  PIC  X(02).
              03 WKA-MESAI3.
                 04 WKA-HJPTN1              PIC  X(01).
                 04 WKA-MIDAS               PIC  X(08).
                 04 WKA-TRBNK  OCCURS  3.
                    05 WKA-TGSME            PIC  X(30).
                    05 WKA-KOZKJ            PIC  X(04).
                    05 WKA-KZABG            PIC  X(07).
                 04 WKA-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
              03 WKA-MEIS31   REDEFINES  WKA-MESAI3.
                 04 WKA-HJPTN2              PIC  X(01).
                 04 WKA-HYOJI1              PIC  X(22).
                 04 WKA-HYOJI2              PIC  X(16).
                 04 WKA-HYOJI3              PIC  X(26).
                 04 WKA-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 WKA-MEIS32  REDEFINES  WKA-MESAI3.
                 04 WKA-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SKTCD              PIC  X(07).
                 04 WIN1-SKNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SESCD              PIC  X(07).
                 04 WIN2-SQNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-SKTCD              PIC  X(07).
                 04 WSV2-SKNCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-KZHKB                    PIC  9(08).
           02 WED1-KZHKB-X  REDEFINES       WED1-KZHKB.
              03 WED1-KZHYY                 PIC  9(04).
              03 WED1-KZHMM                 PIC  9(02).
              03 WED1-KZHDD                 PIC  9(02).
           02 WED1-HIKIKGK.
              03 WED1-HKOGK                 PIC  ZZZ,ZZZ,ZZ9.
           02 WED1-HKOGK-1  REDEFINES       WED1-HIKIKGK
                                            PIC  X(01)  OCCURS 11.
           02 WED1-HKOGK-X.
              03 WED1-HKOGK-2               PIC  X(01)  OCCURS 11.
           02 WED1-HANKAKU.
              03 WED1-H-MM                  PIC  Z9.
              03 WED1-H-DD                  PIC  Z9.
              03 WED1-H-HKOGK               PIC  X(11).
              03 FILLER                     PIC  X(03).
           02 WED1-KANJI.
              03 WED1-K-MM                  PIC  X(04).
              03 WED1-K-DD                  PIC  X(04).
              03 WED1-K-HKOGK               PIC  X(22).
              03 FILLER                     PIC  X(06).
           02 WED1-HYOJI.
              03 WED1-HYOJI1                PIC  X(22).
              03 FILLER                     PIC  X(02) VALUE '¦¥'.
           02 WED1-HYOJI-R  REDEFINES WED1-HYOJI.
              03 WED1-HYOJI-R1              PIC  X(01) OCCURS 24.
           02 WED1-HYOJI-X.
              03 WED1-HYOJI-X1              PIC  X(01) OCCURS 24.
       01  COUNT-AREA.
           02 I                             PIC S9(02) COMP.
           02 J                             PIC S9(02) COMP.
       01  KANJI-AREA.
           02 WKJ1-TUKI                     PIC  X(02) VALUE
                                            '¥/'.
           02 WKJ1-HI                       PIC  X(06) VALUE
                                            '¥a¤Æ  '.
           02 WKJ1-SITEI                    PIC  X(16) VALUE
                                            '¤D§w¥X¤É¥¸¨€¤Ö¤Ø'.
           02 WKJ1-FURIKAE                  PIC  X(20) VALUE
                                            '¨6.C¤»¤¾¤Ã(\¤·¤Ï¤½£¡'.
           02 WKJ1-GINKO                    PIC  X(08) VALUE
                                            '¦Ñ¦§»¥Ï'.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN2-KEY  =  HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                       WKCFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-WKB THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
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
           MOVE WKB-SKTCD          TO WIN1-SKTCD.
           MOVE WKB-SKNCD          TO WIN1-SKNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKC-SESCD          TO WIN2-SESCD.
           MOVE WKC-SQNCD          TO WIN2-SQNCD.
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
                 PERFORM M23-WKCONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           IF ( WKC-RKB30  =  '30' )
              PERFORM M211-EDIT-WKA   THRU M211-EXIT
           ELSE
              MOVE    WKC-R           TO   WKA-R
              PERFORM M212-WRITE-WKA  THRU M212-EXIT
           END-IF.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : EDIT   WKAFILE                                     *
      *----------------------------------------------------------------*
       M211-EDIT-WKA.
           MOVE WKC-R              TO WKA-R.
           IF ( WKB-HJPTN  =  '1' )
                MOVE SPACE         TO WKA-MEIS31
                MOVE SPACE         TO WED1-HANKAKU
                MOVE WKB-KZHKB     TO WED1-KZHKB
                MOVE WED1-KZHMM    TO WED1-H-MM
                MOVE WED1-KZHDD    TO WED1-H-DD
                MOVE WKB-HKOGK     TO WED1-HKOGK
                MOVE SPACE         TO WED1-HKOGK-X
                MOVE ZERO          TO J
                PERFORM VARYING I FROM 1 BY 1 UNTIL ( I  >  11 )
                  IF ( WED1-HKOGK-1(I)  NOT =  SPACE )
                     COMPUTE  J  =  J  +  1
                     MOVE WED1-HKOGK-1(I) TO WED1-HKOGK-2(J)
                  END-IF
                END-PERFORM
                MOVE WED1-HKOGK-X  TO WED1-H-HKOGK
                MOVE WED1-HANKAKU  TO P48-INDAT-X
                MOVE 15            TO P48-INLEN
                CALL 'VSBA0480' USING P48-PARM
                MOVE P48-OTDAT-X   TO WED1-KANJI
                STRING WED1-K-MM WKJ1-TUKI WED1-K-DD WKJ1-HI
                 DELIMITED BY SIZE INTO WKA-HYOJI1
                MOVE WKJ1-SITEI    TO WKA-HYOJI2
                MOVE WED1-K-HKOGK  TO WED1-HYOJI1
                MOVE SPACE         TO WED1-HYOJI-X
                MOVE ZERO          TO J
                PERFORM VARYING I FROM 1 BY 1 UNTIL ( I  >  24 )
                  IF ( WED1-HYOJI-R1(I)  NOT =  SPACE )
                     COMPUTE  J  =  J  +  1
                     MOVE WED1-HYOJI-R1(I) TO WED1-HYOJI-X1(J)
                  END-IF
                END-PERFORM
                MOVE WED1-HYOJI-X  TO WKA-HYOJI3
                MOVE WKJ1-FURIKAE  TO WKA-HYOJI4
                MOVE '1'           TO WKA-HJPTN2
           ELSE
                MOVE SPACE         TO WKA-MEIS32
                MOVE '2'           TO WKA-HJPTN3
           END-IF.
       M211-L01.
           PERFORM M212-WRITE-WKA  THRU M212-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE  WKAFILE                                     *
      *----------------------------------------------------------------*
       M212-WRITE-WKA.
           WRITE   WKA-REC         FROM WKA-R.
       M212-L01.
           PERFORM M12-READ-WKC    THRU M12-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKBONL.
           PERFORM M11-READ-WKB  THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKCFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKCONL.
           MOVE    WIN2-KEY1       TO  WSV2-KEY1.
           PERFORM UNTIL ( WIN2-KEY1  NOT =  WSV2-KEY1 )
               MOVE WKC-R              TO   WKA-R
               IF ( WKC-RKB30  =  '30' )
                  MOVE WKJ1-GINKO      TO   WKA-MIDAS
                  MOVE SPACE           TO   WKA-HJPTN1
               END-IF
               PERFORM M212-WRITE-WKA  THRU M212-EXIT
           END-PERFORM.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKCFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA4450 END                                           *
      *----------------------------------------------------------------*
