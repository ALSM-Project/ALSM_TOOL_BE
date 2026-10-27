./ ADD NAME=VBKA5060  01.06 20030804 20100708 114648 00431 00440 MCSK020
       ID  DIVISION.
       PROGRAM-ID. 'VBKA5060'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : TOKUISAKI SEISIKI MEI         *
      *                                  HONNYAKU (YUSOSAKI)           *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEIHYOU DATA  (DWKA)*
      *                                : TOKUISAKI MASTER     (TMSFILE)*
      *                                : TOKUISAKI MEISYO  FILE        *
      *                                :                      (SFMFILE)*
      *                                : NOSEI MASTER         (NSMFILE)*
      *                 * OUTPUT FILE  : SEIKYU GOUKEIHYOU DATA  (DWKB)*
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/08/04                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1228             ÍÝº³   : 10/07/05  CSK-M.KODAMA        *
      * (02) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
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
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  TMSFILE
           LABEL RECORD STANDARD.
       01  TMS-REC.
           02 KEY-TMS                       PIC  X(07).
           02 FILLER                        PIC  X(293).
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
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  SFM-R.
           COPY VFILSFM SUPPRESS.
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
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
       01  WKB-R.
           02 WKB-CNTKEY.
             03 WKB-NRKNO                   PIC  X(02).
             03 WKB-YTKBN                   PIC  X(01).
             03 WKB-YSYBN.
                04 WKB-YSYB1                PIC  X(03).
                04 WKB-YSYB2                PIC  X(04).
             03 WKB-YUSBNO                  PIC  X(03).
             03 WKB-YUSNO                   PIC  X(03).
             03 WKB-YUSCKK                  PIC  X(05).
             03 WKB-YUSCD                   PIC  X(07).
             03 WKB-YUNCD                   PIC  X(02).
             03 WKB-YSALL                   PIC  9(04).
             03 WKB-INFLG                   PIC  X(01).
             03 WKB-YSTPG                   PIC  9(04).
             03 WKB-PAGEN.
                04 WKB-PAGE1                PIC  9(04).
                04 WKB-PAGE2                PIC  9(04).
             03 WKB-SESCD                   PIC  X(07).
             03 WKB-SQNCD                   PIC  X(02).
           02 WKB-LPDAT                     PIC  X(219).
           02 WKB-DATA03  REDEFINES  WKB-LPDAT.
              03 WKB-GNO03                  PIC  9(02).
              03 WKB-RKB03                  PIC  X(02).
              03 WKB-MESAI3.
                 04 WKB-TJYSO.
                    05 WKB-TJYSA            PIC  X(40).
                    05 WKB-TJYSB            PIC  X(30).
                    05 WKB-TKBLMJ           PIC  X(30).
                 04 FILLER                  PIC  X(115).
           02 WKB-DATA04  REDEFINES  WKB-LPDAT.
              03 WKB-GNO04                  PIC  9(02).
              03 WKB-RKB04                  PIC  X(02).
              03 WKB-MESAI4.
                 04 WKB-TNMJO4              PIC  X(40).
                 04 WKB-TNMGE4              PIC  X(40).
                 04 WKB-NOSKA4              PIC  X(40).
                 04 WKB-YSBN04              PIC  X(03).
                 04 WKB-YSSN04              PIC  X(03).
                 04 WKB-YSCK04              PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY            PIC  9(04).
                    05 WKB-SIMMM            PIC  9(02).
                    05 WKB-SIMDD            PIC  9(02).
                 04 FILLER                  PIC  X(76).
           02 WKB-DATA07  REDEFINES  WKB-LPDAT.
              03 WKB-GNO07                  PIC  9(02).
              03 WKB-RKB07                  PIC  X(02).
              03 WKB-MESAI7.
                 04 WKB-ZENGT7.
                    05 WKB-ZSKDK7           PIC S9(13)  COMP-3.
                    05 WKB-ZNKDK7           PIC S9(13)  COMP-3.
                    05 WKB-ZSYNB7           PIC S9(09)  COMP-3.
                    05 WKB-ZKRDK7           PIC S9(13)  COMP-3.
                 04 WKB-TOGTU7.
                    05 WKB-TKIDK7           PIC S9(13)  COMP-3.
                    05 WKB-TSKDK7           PIC S9(13)  COMP-3.
                    05 WKB-MSKSK7           PIC S9(13)  COMP-3.
                    05 WKB-TURZN7           PIC S9(13)  COMP-3.
                 04 WKB-SHZEI7              PIC S9(13)  COMP-3.
                 04 WKB-TKAIG7              PIC S9(13)  COMP-3.
                 04 FILLER                  PIC  X(147).
           02 WKB-DATA08  REDEFINES  WKB-LPDAT.
              03 WKB-GNO08                  PIC  9(02).
              03 WKB-RKB08                  PIC  X(02).
              03 WKB-MESAI8.
                 04 WKB-ZENGT8.
                    05 WKB-ZSKDK8           PIC S9(13)  COMP-3.
                    05 WKB-ZNKDK8           PIC S9(13)  COMP-3.
                    05 WKB-ZSYNB8           PIC S9(09)  COMP-3.
                    05 WKB-ZKRDK8           PIC S9(13)  COMP-3.
                 04 WKB-TOGTU8.
                    05 WKB-TKIDK8           PIC S9(13)  COMP-3.
                    05 WKB-TSKDK8           PIC S9(13)  COMP-3.
                    05 WKB-MSKSK8           PIC S9(13)  COMP-3.
                    05 WKB-TURZN8           PIC S9(13)  COMP-3.
                 04 WKB-SHZEI8              PIC S9(13)  COMP-3.
                 04 WKB-TKAIG8              PIC S9(13)  COMP-3.
                 04 WKB-TNMJO8              PIC  X(40).
                 04 WKB-TNMGE8              PIC  X(40).
                 04 WKB-NOSKA8              PIC  X(30).
                 04 WKB-HIFLG8              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-YUSCD                 PIC  X(07).
       01  EDIT-AREA.
           02 WED2-YUSCD.
              03 WED2-YUSCD1                PIC  X(01).
              03 FILLER                     PIC  X(06).
           02 WED3-KANJI.
              05 WED3-TNMJO4                PIC  X(40).
              05 WED3-TNMGE4                PIC  X(40).
              05 WED3-NOSKA4                PIC  X(40).
       PROCEDURE DIVISION.
      *-------------------------------------------------------------*
      *    M0 : CONTROL                                             *
      *-------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *-------------------------------------------------------------*
      *    M1 : INIT                                                *
      *-------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                       TMSFILE
                       SFMFILE
                       NSMFILE
                OUTPUT WKBFILE.
       M1-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKA-YUSCD   TO WIN1-YUSCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           IF WKA-RKB04 NOT = '04'
             PERFORM M21-EDIT1-WKB THRU M21-EXIT
           ELSE
             PERFORM M22-EDIT2-WKB THRU M22-EXIT.
       M2-L01.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.1 : EDIT1 WKBFILE (WKA-RKB04 NOT = '04')              *
      *-------------------------------------------------------------*
       M21-EDIT1-WKB.
           MOVE WKA-R       TO WKB-R.
           WRITE WKB-REC  FROM WKB-R.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2 : EDIT2 WKBFILE (WKA-RKB04 = '04')                  *
      *-------------------------------------------------------------*
       M22-EDIT2-WKB.
           MOVE WKA-YUSCD   TO WED2-YUSCD.
           PERFORM M221-READ-TMS THRU M221-EXIT.
           IF TMS-INKAN NOT = '1'
             PERFORM M222-READ-SFM THRU M222-EXIT
             IF WKA-YUNCD   NOT = SPACE AND
                WED2-YUSCD1 NOT = '8'
               PERFORM M223-READ-NSM THRU M223-EXIT.
       M22-L01.
           MOVE SPACE       TO WKB-R.
           INITIALIZE          WKB-R.
           MOVE WKA-R       TO WKB-R.
           IF TMS-INKAN = '1'   OR
              WKA-YUNCD    = SPACE OR
              WED2-YUSCD1  = '8'
             MOVE SPACE       TO WKB-TNMJO4
           ELSE
             MOVE WED3-TNMJO4 TO WKB-TNMJO4.
           IF TMS-INKAN = '1'
             MOVE SPACE       TO WKB-TNMGE4
           ELSE
             IF WKB-TNMJO4 = WED3-TNMJO4
               MOVE WED3-TNMGE4 TO WKB-TNMGE4
             ELSE
               MOVE WED3-TNMJO4 TO WKB-TNMGE4.
           IF TMS-INKAN = '1'
             MOVE SPACE       TO WKB-NOSKA4
           ELSE
             IF WKB-TNMGE4 = WED3-TNMJO4
               MOVE WED3-TNMGE4 TO WKB-NOSKA4
             ELSE
               IF WKA-YUNCD NOT = SPACE AND
                  WKA-YUNCD NOT = '00'
                 MOVE WED3-NOSKA4 TO WKB-NOSKA4
               ELSE
                 MOVE SPACE       TO WKB-NOSKA4.
           WRITE WKB-REC  FROM WKB-R.
       M22-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2.1 : READ TMSFILE                                    *
      *-------------------------------------------------------------*
       M221-READ-TMS.
           MOVE WKA-YUSCD TO KEY-TMS.
           MOVE 'Z '      TO S-CODE.
           READ TMSFILE INTO TMS-R
                INVALID KEY
                        MOVE '1'   TO TMS-INKAN
                        GO TO M221-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA5060 TMSFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO TMS-R.
       M221-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2.2 : READ SFMFILE                                    *
      *-------------------------------------------------------------*
       M222-READ-SFM.
           MOVE WKA-YUSCD TO KEY-SFM.
           MOVE 'Z '      TO S-CODE.
           READ SFMFILE INTO SFM-R
                INVALID KEY
                        MOVE SPACE TO WED3-TNMJO4
                        MOVE SPACE TO WED3-TNMGE4
                        GO TO M222-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA5060 SFMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO SFM-R.
           MOVE SFM-TMSJU TO WED3-TNMJO4.
           MOVE SFM-TMSJL TO WED3-TNMGE4.
       M222-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2.2.3 : READ NSMFILE                                    *
      *-------------------------------------------------------------*
       M223-READ-NSM.
           MOVE WKA-YUSCD TO KEY-NSM1.
           MOVE WKA-YUNCD TO KEY-NSM2.
           MOVE 'Z '      TO S-CODE.
           READ NSMFILE INTO NSM-R
                INVALID KEY
                        MOVE SPACE TO WED3-NOSKA4
                        GO TO M223-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA5060 NSMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO NSM-R.
           MOVE NSM-NOSKA TO WED3-NOSKA4.
       M223-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 TMSFILE
                 SFMFILE
                 NSMFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA5060  END                                          *
      *-------------------------------------------------------------*
