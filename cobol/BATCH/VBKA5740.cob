./ ADD NAME=VBKA5740  01.06 20101203 20110303 162122 00410 00361 MCSK042
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5740'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : SEIKYU GOUKEIHYO TORIHIKI     *
      *                                  GINKOU HONYAKU (KASOU KOUZA)  *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEIHYO DATA         *
      *                                                  (DWKC)        *
      *                                : KASOU KOUZA NUMBER HYOZIYOU   *
      *                                  DATA (SEIKYU GOUKEIHYO)       *
      *                                                  (DWKG)        *
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYO DATA         *
      *                                                  (DWKA)        *
      *                                                                *
      *              <> AUTHOR         : CSK-A.KIKUCHI                 *
      *              <> DATE           : 10/12/03                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKGFILE ASSIGN    TO S-DWKG.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(300).
       FD  WKGFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKG-REC.
           02 FILLER                        PIC  X(60).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKC-R.
           02 WKC-CNTKEY.
             03 WKC-NRKNO                   PIC  X(02).
             03 WKC-YTKBN                   PIC  X(01).
             03 WKC-YSYBN.
                04 WKC-YSYB1                PIC  X(03).
                04 WKC-YSYB2                PIC  X(04).
             03 WKC-YUSBNO                  PIC  X(03).
             03 WKC-YUSNO                   PIC  X(03).
             03 WKC-YUSCKK                  PIC  X(05).
             03 WKC-YUSCD                   PIC  X(07).
             03 WKC-YUNCD                   PIC  X(02).
             03 WKC-YSALL                   PIC  9(04).
             03 WKC-INFLG                   PIC  X(01).
             03 WKC-YSTPG                   PIC  9(04).
             03 WKC-PAGEN.
                04 WKC-PAGE1                PIC  9(04).
                04 WKC-PAGE2                PIC  9(04).
             03 WKC-SESCD                   PIC  X(07).
             03 WKC-SQNCD                   PIC  X(02).
           02 WKC-LPDAT                     PIC  X(219).
           02 WKC-DATA03  REDEFINES  WKC-LPDAT.
              03 WKC-GNO03                  PIC  9(02).
              03 WKC-RKB03                  PIC  X(02).
              03 WKC-MESAI3.
                 04 WKC-TJYSO.
                    05 WKC-TJYSA            PIC  X(40).
                    05 WKC-TJYSB            PIC  X(30).
                    05 WKC-TKBLMJ           PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKC-DATA04  REDEFINES  WKC-LPDAT.
              03 WKC-GNO04                  PIC  9(02).
              03 WKC-RKB04                  PIC  X(02).
              03 WKC-MESAI4.
                 04 WKC-TNMJO4              PIC  X(40).
                 04 WKC-TNMGE4              PIC  X(40).
                 04 WKC-NOSKA4              PIC  X(40).
                 04 WKC-YSBN04              PIC  X(03).
                 04 WKC-YSSN04              PIC  X(03).
                 04 WKC-YSCK04              PIC  X(05).
                 04 WKC-SMEYMD.
                    05 WKC-SIMYY            PIC  9(04).
                    05 WKC-SIMMM            PIC  9(02).
                    05 WKC-SIMDD            PIC  9(02).
                 04 FILLER                  PIC  X(76).
           02 WKC-DATA05  REDEFINES  WKC-LPDAT.
              03 WKC-GNO05                  PIC  9(02).
              03 WKC-RKB05                  PIC  X(02).
              03 WKC-MESAI5.
                 04 WKC-HJPTN5              PIC  X(01).
                 04 WKC-MIDAK5              PIC  X(08).
                 04 WKC-TGNME5  OCCURS  3.
                    05 WKC-GSKAJ5           PIC  X(30).
                    05 WKC-KOZKJ5           PIC  X(04).
                    05 WKC-KZABG5           PIC  X(07).
                 04 WKC-BNKSU5              PIC  9(01).
                 04 FILLER                  PIC  X(82).
           02 WKC-DATA06  REDEFINES  WKC-LPDAT.
              03 WKC-GNO06                  PIC  9(02).
              03 WKC-RKB06                  PIC  X(02).
              03 WKC-MESAI6.
                 04 WKC-STNMJ6              PIC  X(18).
                 04 WKC-STYBN6              PIC  X(07).
                 04 WKC-STTEL6              PIC  X(13).
                 04 WKC-NSTJJ6.
                    05 WKC-NSTJ16           PIC  X(36).
                    05 WKC-NSTJ26           PIC  X(36).
                 04 FILLER                  PIC  X(105).
           02 WKC-DATA07  REDEFINES  WKC-LPDAT.
              03 WKC-GNO07                  PIC  9(02).
              03 WKC-RKB07                  PIC  X(02).
              03 WKC-MESAI7.
                 04 WKC-ZENGT7.
                    05 WKC-ZSKDK7           PIC S9(13)  COMP-3.
                    05 WKC-ZNKDK7           PIC S9(13)  COMP-3.
                    05 WKC-ZSYNB7           PIC S9(09)  COMP-3.
                    05 WKC-ZKRDK7           PIC S9(13)  COMP-3.
                 04 WKC-TOGTU7.
                    05 WKC-TKIDK7           PIC S9(13)  COMP-3.
                    05 WKC-TSKDK7           PIC S9(13)  COMP-3.
                    05 WKC-MSKSK7           PIC S9(13)  COMP-3.
                    05 WKC-TURZN7           PIC S9(13)  COMP-3.
                 04 WKC-SHZEI7              PIC S9(13)  COMP-3.
                 04 WKC-TKAIG7              PIC S9(13)  COMP-3.
                 04 FILLER                  PIC  X(147).
           02 WKC-DATA08  REDEFINES  WKC-LPDAT.
              03 WKC-GNO08                  PIC  9(02).
              03 WKC-RKB08                  PIC  X(02).
              03 WKC-MESAI8.
                 04 WKC-ZENGT8.
                    05 WKC-ZSKDK8           PIC S9(13)  COMP-3.
                    05 WKC-ZNKDK8           PIC S9(13)  COMP-3.
                    05 WKC-ZSYNB8           PIC S9(09)  COMP-3.
                    05 WKC-ZKRDK8           PIC S9(13)  COMP-3.
                 04 WKC-TOGTU8.
                    05 WKC-TKIDK8           PIC S9(13)  COMP-3.
                    05 WKC-TSKDK8           PIC S9(13)  COMP-3.
                    05 WKC-MSKSK8           PIC S9(13)  COMP-3.
                    05 WKC-TURZN8           PIC S9(13)  COMP-3.
                 04 WKC-SHZEI8              PIC S9(13)  COMP-3.
                 04 WKC-TKAIG8              PIC S9(13)  COMP-3.
                 04 WKC-TNMJO8              PIC  X(40).
                 04 WKC-TNMGE8              PIC  X(40).
                 04 WKC-NOSKA8              PIC  X(30).
                 04 WKC-HIFLG8              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  WKG-R.
           02 WKG-YUSCD                     PIC  X(07).
           02 WKG-TRBKJ.
              03 WKG-KKZNO.
                 04 WKG-GNSCD               PIC  X(03).
                 04 WKG-KZANO               PIC  X(07).
              03 WKG-KOZKJ                  PIC  X(04).
              03 WKG-HJBSJ                  PIC  X(30).
           02 FILLER                        PIC  X(09).
       01  WKA-R.
           02 WKA-CNTKEY.
             03 WKA-NRKNO                   PIC  X(02).
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
           02 WKA-DATA05  REDEFINES  WKA-LPDAT.
              03 WKA-GNO05                  PIC  9(02).
              03 WKA-RKB05                  PIC  X(02).
              03 WKA-MESAI5.
                 04 WKA-HJPTN5              PIC  X(01).
                 04 WKA-MIDAK5              PIC  X(08).
                 04 WKA-TGNME5  OCCURS  3.
                    05 WKA-GSKAJ5           PIC  X(30).
                    05 WKA-KOZKJ5           PIC  X(04).
                    05 WKA-KZABG5           PIC  X(07).
                 04 WKA-BNKSU5              PIC  9(01).
                 04 FILLER                  PIC  X(82).
           02 WKA-DATA06  REDEFINES  WKA-LPDAT.
              03 WKA-GNO06                  PIC  9(02).
              03 WKA-RKB06                  PIC  X(02).
              03 WKA-MESAI6.
                 04 WKA-STNMJ6              PIC  X(18).
                 04 WKA-STYBN6              PIC  X(07).
                 04 WKA-STTEL6              PIC  X(13).
                 04 WKA-NSTJJ6.
                    05 WKA-NSTJ16           PIC  X(36).
                    05 WKA-NSTJ26           PIC  X(36).
                 04 FILLER                  PIC  X(105).
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
              03 WKA-MESAI8.
                 04 WKA-ZENGT8.
                    05 WKA-ZSKDK8           PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK8           PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB8           PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK8           PIC S9(13)  COMP-3.
                 04 WKA-TOGTU8.
                    05 WKA-TKIDK8           PIC S9(13)  COMP-3.
                    05 WKA-TSKDK8           PIC S9(13)  COMP-3.
                    05 WKA-MSKSK8           PIC S9(13)  COMP-3.
                    05 WKA-TURZN8           PIC S9(13)  COMP-3.
                 04 WKA-SHZEI8              PIC S9(13)  COMP-3.
                 04 WKA-TKAIG8              PIC S9(13)  COMP-3.
                 04 WKA-TNMJO8              PIC  X(40).
                 04 WKA-TNMGE8              PIC  X(40).
                 04 WKA-NOSKA8              PIC  X(30).
                 04 WKA-HIFLG8              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-YUSCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-YUSCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV2-KEY1.
                 04 WSV2-YUSCD              PIC  X(07).
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
                       WKGFILE
                OUTPUT WKAFILE.
           PERFORM M11-READ-WKG THRU M11-EXIT.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKGFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKG.
           READ WKGFILE INTO WKG-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKG-YUSCD TO WIN1-YUSCD.
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
           MOVE WKC-YUSCD TO WIN2-YUSCD.
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
                 PERFORM M22-WKGONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKCONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M211-WRITE-WKA THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M21-L01.
           PERFORM M11-READ-WKG THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE-WKA                                          *
      *----------------------------------------------------------------*
       M211-WRITE-WKA.
           IF WKC-RKB05 = '05'
              MOVE SPACE         TO WKA-R
              MOVE WKC-CNTKEY    TO WKA-CNTKEY
              MOVE WKC-GNO05     TO WKA-GNO05
              MOVE WKC-RKB05     TO WKA-RKB05
              MOVE WKG-HJBSJ     TO WKA-GSKAJ5(1)
              MOVE WKG-KOZKJ     TO WKA-KOZKJ5(1)
              MOVE WKG-KZANO     TO WKA-KZABG5(1)
              IF WKA-TGNME5(1) = SPACE
                 MOVE '2'         TO WKA-HJPTN5
                 MOVE SPACE       TO WKA-MIDAK5
                 MOVE ZERO        TO WKA-BNKSU5
              ELSE
                 MOVE WKC-HJPTN5  TO WKA-HJPTN5
                 MOVE WKC-MIDAK5  TO WKA-MIDAK5
                 MOVE 1           TO WKA-BNKSU5
              END-IF
              MOVE WKC-PGMSU     TO WKA-PGMSU
              MOVE WKC-SORTJ     TO WKA-SORTJ
              WRITE WKA-REC FROM WKA-R
           ELSE
              MOVE WKC-R      TO WKA-R
              WRITE WKA-REC FROM WKA-R.
       M211-L01.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKGFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKGONL.
           PERFORM M11-READ-WKG THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKCFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKCONL.
           MOVE WKC-R      TO WKA-R.
           WRITE WKA-REC FROM WKA-R.
       M23-L01.
           PERFORM M12-READ-WKC THRU M12-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKGFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5740 END                                           *
      *----------------------------------------------------------------*
