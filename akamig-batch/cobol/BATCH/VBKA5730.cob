./ ADD NAME=VBKA5730  01.06 20101201 20101201 174640 00455 00348 MCSK036
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5730'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : SEIKYUSYO TORIHIKI GINKOU     *
      *                                  HONYAKU (KASOU KOUZA)         *
      *                                                                *
      *                 * INPUT FILE   : KASOU KOUZA NUMBER HYOZIYOU   *
      *                                  DATA (SEIKYUSYO)(DWKC)        *
      *                                : SEIKYUSYO DATA (SEIKYUKAGAMI) *
      *                                                  (DWKA)        *
      *                 * OUTPUT FILE  : SEIKYUSYO DATA (SEIKYUKAGAMI) *
      *                                                  (DWKB)        *
      *                                : KASOU KOUZA JYOUHOU DATA      *
      *                                  (SEIKYU GOUKEIHYOU YOU)       *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/12/01                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKFFILE ASSIGN    TO S-DWKF.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(70).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       FD  WKFFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKF-REC.
           02 FILLER                        PIC  X(70).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKC-R.
           02 WKC-SESCD                     PIC  X(07).
           02 WKC-SQNCD                     PIC  X(02).
           02 WKC-GHBCD                     PIC  X(04).
           02 WKC-TRBKJ.
              03 WKC-KKZNO.
                 04 WKC-GNSCD               PIC  X(03).
                 04 WKC-KZANO               PIC  X(07).
              03 WKC-KOZKJ                  PIC  X(04).
              03 WKC-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(13).
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
           02 WKA-DATA10  REDEFINES  WKA-LPDAT.
              03 WKA-GNO10                  PIC  9(02).
              03 WKA-RKB10                  PIC  X(02).
              03 WKA-MESAI1.
                 04 WKA-TJYSO.
                    05 WKA-TJYSA            PIC  X(40).
                    05 WKA-TJYSB            PIC  X(30).
                    05 WKA-BILME            PIC  X(30).
                 04 WKA-SEYBN.
                    05 WKA-SEYB1            PIC  X(03).
                    05 WKA-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
           02 WKA-DATA20  REDEFINES  WKA-LPDAT.
              03 WKA-GNO20                  PIC  9(02).
              03 WKA-RKB20                  PIC  X(02).
              03 WKA-MESAI2.
                 04 WKA-TMSJU               PIC  X(40).
                 04 WKA-TMSJL               PIC  X(40).
                 04 WKA-NOSMJ               PIC  X(40).
                 04 WKA-SQBNO               PIC  X(03).
                 04 WKA-SQSTN               PIC  X(03).
                 04 WKA-SQCKK               PIC  X(05).
                 04 WKA-SMEYMD.
                    05 WKA-SIMYY            PIC  9(04).
                    05 WKA-SIMMM            PIC  9(02).
                    05 WKA-SIMDD            PIC  9(02).
                 04 WKA-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
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
              03 FILLER  REDEFINES  WKA-MESAI3.
                 04 WKA-HJPTN2              PIC  X(01).
                 04 WKA-HYOJI1              PIC  X(22).
                 04 WKA-HYOJI2              PIC  X(16).
                 04 WKA-HYOJI3              PIC  X(26).
                 04 WKA-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKA-MESAI3.
                 04 WKA-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKA-DATA40  REDEFINES  WKA-LPDAT.
              03 WKA-GNO40                  PIC  9(02).
              03 WKA-RKB40                  PIC  X(02).
              03 WKA-MESAI4.
                 04 WKA-STNMJ4              PIC  X(18).
                 04 WKA-STYBN               PIC  X(07).
                 04 WKA-STTEL               PIC  X(13).
                 04 WKA-NSTJJ.
                    05 WKA-NSTJ1            PIC  X(36).
                    05 WKA-NSTJ2            PIC  X(36).
                 04 FILLER                  PIC  X(105).
           02 WKA-DATA50  REDEFINES  WKA-LPDAT.
              03 WKA-GNO50                  PIC  9(02).
              03 WKA-RKB50                  PIC  X(02).
              03 WKA-MEISA5.
                 04 WKA-ZENGT.
                    05 WKA-ZSKDK            PIC S9(13) COMP-3.
                    05 WKA-ZNKDK            PIC S9(13) COMP-3.
                    05 WKA-ZSYNB            PIC S9(09) COMP-3.
                    05 WKA-ZKRDK            PIC S9(13) COMP-3.
                 04 WKA-TOGTU.
                    05 WKA-TKIDK            PIC S9(13) COMP-3.
                    05 WKA-TSKDK            PIC S9(13) COMP-3.
                    05 WKA-MSKSK            PIC S9(13) COMP-3.
                    05 WKA-TURZN            PIC S9(13) COMP-3.
                 04 WKA-ZANKB               PIC  X(01).
                 04 WKA-SHZEI               PIC S9(13) COMP-3.
                 04 WKA-TKAIG               PIC S9(13) COMP-3.
                 04 FILLER                  PIC  X(146).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKB-R.
           02 WKB-CTLKEY.
              03 WKB-KAINO                  PIC  X(02).
              03 WKB-YTKBN                  PIC  X(01).
              03 WKB-YSYBN.
                 04 WKB-YSYB1               PIC  X(03).
                 04 WKB-YSYB2               PIC  X(04).
              03 WKB-YSBNO                  PIC  X(03).
              03 WKB-YSSTN                  PIC  X(03).
              03 WKB-YSCKG                  PIC  X(05).
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
           02 WKB-LPDAT                     PIC  X(219).
           02 WKB-DATA10  REDEFINES  WKB-LPDAT.
              03 WKB-GNO10                  PIC  9(02).
              03 WKB-RKB10                  PIC  X(02).
              03 WKB-MESAI1.
                 04 WKB-TJYSO.
                    05 WKB-TJYSA            PIC  X(40).
                    05 WKB-TJYSB            PIC  X(30).
                    05 WKB-BILME            PIC  X(30).
                 04 WKB-SEYBN.
                    05 WKB-SEYB1            PIC  X(03).
                    05 WKB-SEYB2            PIC  X(04).
                 04 FILLER                  PIC  X(108).
           02 WKB-DATA20  REDEFINES  WKB-LPDAT.
              03 WKB-GNO20                  PIC  9(02).
              03 WKB-RKB20                  PIC  X(02).
              03 WKB-MESAI2.
                 04 WKB-TMSJU               PIC  X(40).
                 04 WKB-TMSJL               PIC  X(40).
                 04 WKB-NOSMJ               PIC  X(40).
                 04 WKB-SQBNO               PIC  X(03).
                 04 WKB-SQSTN               PIC  X(03).
                 04 WKB-SQCKK               PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY            PIC  9(04).
                    05 WKB-SIMMM            PIC  9(02).
                    05 WKB-SIMDD            PIC  9(02).
                 04 WKB-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKB-DATA30  REDEFINES  WKB-LPDAT.
              03 WKB-GNO30                  PIC  9(02).
              03 WKB-RKB30                  PIC  X(02).
              03 WKB-MESAI3.
                 04 WKB-HJPTN1              PIC  X(01).
                 04 WKB-MIDAS               PIC  X(08).
                 04 WKB-TRBNK  OCCURS  3.
                    05 WKB-TGSME            PIC  X(30).
                    05 WKB-KOZKJ            PIC  X(04).
                    05 WKB-KZABG            PIC  X(07).
                 04 WKB-BNKSU               PIC  9(01).
                 04 FILLER                  PIC  X(82).
              03 FILLER  REDEFINES  WKB-MESAI3.
                 04 WKB-HJPTN2              PIC  X(01).
                 04 WKB-HYOJI1              PIC  X(22).
                 04 WKB-HYOJI2              PIC  X(16).
                 04 WKB-HYOJI3              PIC  X(26).
                 04 WKB-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKB-MESAI3.
                 04 WKB-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKB-DATA40  REDEFINES  WKB-LPDAT.
              03 WKB-GNO40                  PIC  9(02).
              03 WKB-RKB40                  PIC  X(02).
              03 WKB-MESAI4.
                 04 WKB-STNMJ4              PIC  X(18).
                 04 WKB-STYBN               PIC  X(07).
                 04 WKB-STTEL               PIC  X(13).
                 04 WKB-NSTJJ.
                    05 WKB-NSTJ1            PIC  X(36).
                    05 WKB-NSTJ2            PIC  X(36).
                 04 FILLER                  PIC  X(105).
           02 WKB-DATA50  REDEFINES  WKB-LPDAT.
              03 WKB-GNO50                  PIC  9(02).
              03 WKB-RKB50                  PIC  X(02).
              03 WKB-MEISA5.
                 04 WKB-ZENGT.
                    05 WKB-ZSKDK            PIC S9(13) COMP-3.
                    05 WKB-ZNKDK            PIC S9(13) COMP-3.
                    05 WKB-ZSYNB            PIC S9(09) COMP-3.
                    05 WKB-ZKRDK            PIC S9(13) COMP-3.
                 04 WKB-TOGTU.
                    05 WKB-TKIDK            PIC S9(13) COMP-3.
                    05 WKB-TSKDK            PIC S9(13) COMP-3.
                    05 WKB-MSKSK            PIC S9(13) COMP-3.
                    05 WKB-TURZN            PIC S9(13) COMP-3.
                 04 WKB-ZANKB               PIC  X(01).
                 04 WKB-SHZEI               PIC S9(13) COMP-3.
                 04 WKB-TKAIG               PIC S9(13) COMP-3.
                 04 FILLER                  PIC  X(146).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKF-R.
           02 WKF-SESCD                     PIC  X(07).
           02 WKF-SQNCD                     PIC  X(02).
           02 WKF-GHBCD                     PIC  X(04).
           02 WKF-TRBKJ.
              03 WKF-KKZNO.
                 04 WKF-GNSCD               PIC  X(03).
                 04 WKF-KZANO               PIC  X(07).
              03 WKF-KOZKJ                  PIC  X(04).
              03 WKF-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(13).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SESCD              PIC  X(07).
                 04 WIN1-SQNCD              PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-SESCD              PIC  X(07).
                 04 WIN2-SQNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-SESCD              PIC  X(07).
                 04 WSV2-SQNCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-FLG                      PIC  X(01).
           02 WED1-FAST                     PIC  X(01).
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
           OPEN INPUT  WKCFILE
                       WKAFILE
                OUTPUT WKBFILE
                       WKFFILE.
           PERFORM M11-READ-WKC THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKC-SESCD TO WIN1-SESCD.
           MOVE WKC-SQNCD TO WIN1-SQNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKA-SESCD TO WIN2-SESCD.
           MOVE WKA-SQNCD TO WIN2-SQNCD.
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
                 PERFORM M22-WKCONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE SPACE     TO WED1-FLG.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M211-WRITE1-WKB THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           PERFORM M212-WRITE2-WKF THRU M212-EXIT.
       M21-L01.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE1-WKB                                         *
      *----------------------------------------------------------------*
       M211-WRITE1-WKB.
           IF ( WKA-RKB30 = '30' )   AND
              ( WKA-HJPTN1 = SPACE )
              MOVE SPACE        TO WKB-R
              MOVE WKA-CTLKEY   TO WKB-CTLKEY
              MOVE WKA-GNO30    TO WKB-GNO30
              MOVE WKA-RKB30    TO WKB-RKB30
              MOVE WKA-HJPTN1   TO WKB-HJPTN1
              MOVE WKA-MIDAS    TO WKB-MIDAS
              MOVE WKC-HJBSJ    TO WKB-TGSME(1)
              MOVE WKC-KOZKJ    TO WKB-KOZKJ(1)
              MOVE WKC-KZANO    TO WKB-KZABG(1)
              MOVE '1'          TO WKB-BNKSU
              MOVE WKA-PGMSU    TO WKB-PGMSU
              MOVE WKA-SORTJ    TO WKB-SORTJ
              MOVE '1'          TO WED1-FLG
              WRITE WKB-REC   FROM WKB-R
           ELSE
              MOVE WKA-R        TO WKB-R
              WRITE WKB-REC   FROM WKB-R.
       M211-L01.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : WRITE2-WKF                                         *
      *----------------------------------------------------------------*
       M212-WRITE2-WKF.
           IF WED1-FLG = '1'
              MOVE WKC-R        TO WKF-R
              WRITE WKF-REC   FROM WKF-R
           ELSE
              MOVE SPACE        TO WKF-R
              MOVE WKC-SESCD    TO WKF-SESCD
              MOVE WKC-SQNCD    TO WKF-SQNCD
              WRITE WKF-REC   FROM WKF-R.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKCFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKCONL.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           MOVE SPACE     TO WED1-FAST.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M231-WRITE3 THRU M231-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : WRITE3                                             *
      *----------------------------------------------------------------*
       M231-WRITE3.
           MOVE WKA-R        TO WKB-R.
           WRITE WKB-REC   FROM WKB-R.
           IF WED1-FAST = SPACE
              MOVE '1'          TO WED1-FAST
              MOVE SPACE        TO WKF-R
              MOVE WSV2-SESCD   TO WKF-SESCD
              MOVE WSV2-SQNCD   TO WKF-SQNCD
              WRITE WKF-REC   FROM WKF-R.
       M231-L01.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKAFILE
                 WKBFILE
                 WKFFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5730 END                                           *
      *----------------------------------------------------------------*
