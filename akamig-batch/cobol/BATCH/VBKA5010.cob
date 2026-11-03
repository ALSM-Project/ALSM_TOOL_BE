./ ADD NAME=VBKA5010  01.16 20030728 20190708 090234 00344 00148 MCSK013
       ID  DIVISION.
       PROGRAM-ID. 'VBKA5010'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : SEIKYU GOUKEIHYOU URIKAKEKIN  *
      *                                  MEISAI REKO-DO SAKUSEI        *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEIHYOU SAKUSEI     *
      *                                        TAISYOUSAKI       (DWKC)*
      *                 * INPUT FILE   : SEIKYUSYO DATA                *
      *                                          (SEIKYU KAGAMI) (DWKA)*
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU JOUHOU      *
      *                                                          (DWKB)*
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/07/28                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-02163            ÍÝº³   : 19/07/08  SCSK-A.ISIKAWA      *
      * (02) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(20).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(280).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       01  WKC-R.
           02 WKC-YUSCD                     PIC  X(07).
           02 WKC-YUNCD                     PIC  X(02).
           02 WKC-SESCD                     PIC  X(07).
           02 WKC-SQNCD                     PIC  X(02).
           02 FILLER                        PIC  X(02).
       01  WKA-R.
           02 WKA-CTLKEY.
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
                    05 WKA-ZSKDK            PIC S9(13)  COMP-3.
                    05 WKA-ZNKDK            PIC S9(13)  COMP-3.
                    05 WKA-ZSYNB            PIC S9(09)  COMP-3.
                    05 WKA-ZKRDK            PIC S9(13)  COMP-3.
                 04 WKA-TOGTU.
                    05 WKA-TKIDK            PIC S9(13)  COMP-3.
                    05 WKA-TSKDK            PIC S9(13)  COMP-3.
                    05 WKA-MSKSK            PIC S9(13)  COMP-3.
                    05 WKA-TURZN            PIC S9(13)  COMP-3.
                 04 WKA-ZANKB               PIC  X(01).
                 04 WKA-SHZEI               PIC S9(13)  COMP-3.
                 04 WKA-TKAIG               PIC S9(13)  COMP-3.
                 04 FILLER                  PIC  X(146).
           02 WKA-PGMSU                     PIC  X(02).
           02 WKA-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
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
       PROCEDURE DIVISION.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ) OR
                         ( WIN2-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKCFILE
                       WKAFILE
                OUTPUT WKBFILE.
       M1-L01.
           PERFORM M11-READ-WKC THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKCFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKC.
           READ WKCFILE INTO WKC-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKC-SESCD   TO WIN1-SESCD.
           MOVE WKC-SQNCD   TO WIN1-SQNCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                      *
      *-------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                 AT END
                    MOVE HIGH-VALUE TO WIN2-KEY
                    GO TO M12-EXIT.
       M12-L01.
           MOVE WKA-SESCD   TO WIN2-SESCD.
           MOVE WKA-SQNCD   TO WIN2-SQNCD.
       M12-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1  =  WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-WKCONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1 : MATCH                                             *
      *-------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1   TO WSV2-KEY1.
           MOVE SPACE       TO WKB-R.
           INITIALIZE          WKB-R.
           MOVE '1'         TO WKB-INFLG.
           MOVE '09'        TO WKB-RECKB.
           MOVE WKA-KAINO   TO WKB-KAINO.
           MOVE WKA-YTKBN   TO WKB-YTKBN.
           MOVE WKA-YSYBN   TO WKB-YSYBN.
           MOVE WKA-YUSBNO  TO WKB-YUSBNO.
           MOVE WKA-YUSNO   TO WKB-YUSNO.
           MOVE WKA-YUSCKK  TO WKB-YUSCKK.
           MOVE WKA-YUSCD   TO WKB-YUSCD.
           MOVE WKA-YUNCD   TO WKB-YUNCD.
           MOVE WKA-SESCD   TO WKB-SESCD.
           MOVE WKA-SQNCD   TO WKB-SQNCD.
           PERFORM M211-EDIT-WKB THRU M211-EXIT
             UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           WRITE WKB-REC FROM WKB-R.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1.1 : EDIT WKBFILE                                    *
      *-------------------------------------------------------------*
       M211-EDIT-WKB.
           IF WKA-RKB20 = '20'
              MOVE WKA-SQBNO  TO WKB-SQBNO
              MOVE WKA-SQSTN  TO WKB-SQSTN
              MOVE WKA-SQCKK  TO WKB-SQCKK
              MOVE WKA-SMEYMD TO WKB-SMEYMD
              MOVE WKA-INKAN  TO WKB-INKAN.
           IF WKA-RKB50 = '50'
              MOVE WKA-ZENGT  TO WKB-ZENGT
              MOVE WKA-TOGTU  TO WKB-TOGTU
              MOVE WKA-SHZEI  TO WKB-SHZEI
              MOVE WKA-TKAIG  TO WKB-TKAIG.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2 : WKCFILE ONLY                                      *
      *-------------------------------------------------------------*
       M22-WKCONL.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                      *
      *-------------------------------------------------------------*
       M23-WKAONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M12-READ-WKA THRU M12-EXIT
             UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M23-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKAFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA5010  END                                          *
      *-------------------------------------------------------------*
