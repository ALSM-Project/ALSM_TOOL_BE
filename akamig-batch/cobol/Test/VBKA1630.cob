./ ADD NAME=VBKA1630  01.04 20150805 20151204 141248 00747 00719 MCSK004
       ID  DIVISION.
       PROGRAM-ID. 'VBKA1630'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : KAISYUU    SYSTEM             *
      *              <> PG.OUTLINE     : CS YOU SEIKYUSYO FILE NO      *
      *                                  FURIWAKE                      *
      *                 * INPUT FILE   : CS YOU SEIKYUSYO DATA (DWKC)  *
      *                 * OUTPUT FILE  : TYOKUSOU BUN CS YOU           *
      *                                  SEIKYUSYO DATA        (DWKB)  *
      *                 * OUTPUT FILE  : TORITOME BUN CS YOU           *
      *                                  SEIKYUSYO DATA        (DWKD)  *
      *                 * OUTPUT FILE  : SEIKYUSYO FILE HIKIWATASHIHYO *
      *                                                        (PRT1)  *
      *              <> AUTHOR         : CSK-K.OTSUKA                  *
      *              <> DATE           : 86/08/30                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) CONTROL KEY        ÍÝº³        : 86/10/31  CSK-K.OTSUKA   *
      * (02) OUTFILE LENGTH     ÍÝº³        : 87/01/27  CSK-H.NARUSE   *
      * (03) SYOHIZEI TUIKA     Â²¶         : 89/03/16  CSK-S.MUNEISHI *
      * (04) MEISAI SYOHIZEI    Â²¶         : 95/06/07  CSK-H.MIYATA   *
      * (05) K-261              ÍÝº³        : 97/09/25  CSK-H.ESAKI    *
      * (06) K-358              ÍÝº³        : 99/03/09  CSK-R.TAKAYANAG*
      * (07) K-388              ÍÝº³        : 99/09/03  CSK-K.FUKAYA   *
      * (08) K-407              ÍÝº³        : 99/12/17  CSK-C.FURUTA   *
      * (09) H-010 (O-2907)     ÍÝº³        : 00/09/05  CSK-K.ITO      *
      * (10) K-568              ÍÝº³        : 03/08/18  CSK-S.OKA      *
      * (11) C-258              ÍÝº³        : 05/10/19  CSK-Y.MATSUOKA *
      * (12) K-1555             ±½Ã¨½Â²¶    : 14/06/10  CSK-K.KOYAMA   *
      * (13) K-1780             ØºÝÊß²Ù     : 15/08/05  CSK-Y.YOSHIGA  *
      * (14) K-1759             ÍÝº³        : 15/08/31  CSK-N.EGUCHI   *
      * (15) K-1771             ÍÝº³        : 15/12/04  CSK-N.EGUCHI   *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS SKPCH01.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT KDPFILE ASSIGN    TO S-PRT1.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(250).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(250).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(250).
       FD  KDPFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD OMITTED.
       01  KDP-REC.
           02 FILLER                        PIC  X(160).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKC-R.
           COPY VFILSKA SUPPRESS.
       01  WKB-R.
           COPY VFILSKA SUPPRESS.
       01  WKD-R.
           COPY VFILSKA SUPPRESS.
       01  LPH-HEADER.
           02 LPH-HEAD0.
              03 FILLER                PIC  X(160) VALUE SPACE.
           02 LPH-HEAD1.
              03 FILLER                PIC  X(115) VALUE SPACE.
              03 LPH-YMDKJ             PIC  X(04).
              03 FILLER                PIC  X(01)  VALUE SPACE.
              03 LPH-YY                PIC  9(02).
              03 FILLER                PIC  X(01)  VALUE '/'.
              03 LPH-MM                PIC  9(02).
              03 FILLER                PIC  X(01)  VALUE '/'.
              03 LPH-DD                PIC  9(02).
           02 LPH-HEAD2.
              03 FILLER                PIC  X(26) VALUE SPACE.
              03 LPH-MDSKJ             PIC  X(90).
           02 LPH-HEAD3.
              03 FILLER                PIC  X(05) VALUE SPACE.
              03 LPH-KBNKJ             PIC  X(06).
              03 LPH-YUSOK             PIC  X(10).
              03 FILLER                PIC  X(02) VALUE SPACE.
              03 LPH-MISKJ             PIC  X(12).
              03 LPH-PAGE1             PIC  X(10).
              03 FILLER                PIC  X(70) VALUE SPACE.
              03 LPH-PAGEN             PIC  ZZ9.
              03 FILLER                PIC  X(06) VALUE ' Íß-¼Þ'.
           02 LPH-HEAD4.
              03 FILLER                PIC  X(16) VALUE SPACE.
              03 LPH-SOKKJ1            PIC  X(06).
              03 FILLER                PIC  X(10) VALUE SPACE.
              03 LPH-KSUKJ1            PIC  X(04).
              03 FILLER                PIC  X(01) VALUE SPACE.
              03 LPH-RECKJ1            PIC  X(10).
              03 FILLER                PIC  X(08) VALUE SPACE.
              03 LPH-SOKKJ2            PIC  X(06).
              03 FILLER                PIC  X(10) VALUE SPACE.
              03 LPH-KSUKJ2            PIC  X(04).
              03 FILLER                PIC  X(01) VALUE SPACE.
              03 LPH-RECKJ2            PIC  X(10).
              03 FILLER                PIC  X(08) VALUE SPACE.
              03 LPH-SOKKJ3            PIC  X(06).
              03 FILLER                PIC  X(10) VALUE SPACE.
              03 LPH-KSUKJ3            PIC  X(04).
              03 FILLER                PIC  X(01) VALUE SPACE.
              03 LPH-RECKJ3            PIC  X(10).
       01  LPD-DETAIL.
           02 LPD-DTL1.
              03 FILLER                PIC  X(10).
              03 LPD-MEISAI OCCURS 3.
                 04 LPD-SOKME          PIC  X(18).
                 04 FILLER             PIC  X(01).
                 04 LPD-KENS1          PIC  ZZZ,ZZ9.
                 04 FILLER             PIC  X(03).
                 04 LPD-KENS2          PIC  ZZZZ,ZZ9.
                 04 FILLER             PIC  X(02).
       01  LPT-TOTAL.
           02 LPT-TTL1.
              03 FILLER                PIC  X(88).
              03 LPT-GOKEI             PIC  X(18).
              03 FILLER                PIC  X(02).
              03 LPT-TOTL1             PIC  ZZZ,ZZ9.
              03 FILLER                PIC  X(01).
              03 LPT-TOTL2             PIC  ZZZZ,ZZ9.
       01  TABLE-AREA.
           02  WTB1-TABLE  OCCURS 25.
               03  WTB1-MEISAI  OCCURS 3.
                   04  WTB1-SOKME      PIC  X(18).
                   04  WTB1-KENSU1     PIC  9(06) COMP-3.
                   04  WTB1-KENSU2     PIC  9(07) COMP-3.
           02  WTB1-TITLE.
               03  WTB1-TITLE1  OCCURS 10.
                   04  WTB1-PARM       PIC  X(02).
                   04  WTB1-MDSKJ      PIC  X(90).
      *
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEYB.
                 04 WIN1-KEYA.
                    05 WIN1-KEY1.
                       06 WIN1-KAINO   PIC  X(02).
                       06 WIN1-YTKBN   PIC  X(01).
                       06 WIN1-PGMSU   PIC  X(02).
                    05 WIN1-YUSBNO     PIC  X(03).
                    05 WIN1-YUSNO      PIC  X(03).
                 04 WIN1-YUSCKK        PIC  X(05).
                 04 WIN1-YUSCD         PIC  X(07).
                 04 WIN1-YUNCD         PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEYB.
                 04 WSV1-KEYA.
                    05 WSV1-KEY1.
                       06 WSV1-KAINO   PIC  X(02).
                       06 WSV1-YTKBN   PIC  X(01).
                       06 WSV1-PGMSU   PIC  X(02).
                    05 WSV1-YUSBNO     PIC  X(03).
                    05 WSV1-YUSNO      PIC  X(03).
                 04 WSV1-YUSCKK        PIC  X(05).
                 04 WSV1-YUSCD         PIC  X(07).
                 04 WSV1-YUNCD         PIC  X(02).
       01  TOTAL-AREA.
           02 WTT1-KENSU1              PIC S9(06) COMP-3.
           02 WTT1-KENSU2              PIC S9(07) COMP-3.
           02 WTT1-GOKEI1              PIC S9(06) COMP-3.
           02 WTT1-GOKEI2              PIC S9(07) COMP-3.
       01  EDIT-AREA.
           02 WED1-KNJCD.
              03 WED1-KNJCD1.
                 04 FILLER             PIC X(08) VALUE '459445BA'.
              03 WED1-KNJCD11.
                 04 FILLER             PIC X(64) VALUE '40404040425C425C
      -      '40404040446D4040438E404043C740404389404043BD4040'.
                 04 FILLER             PIC X(116) VALUE    '4AE8404049A0
      -      '4040479D4040439F4040434740404382404043AF4040449A404046DA40
      -      '4045D2404046C040404040425C425C4040404040404040'.
              03 WED1-KNJCD12.
                 04 FILLER             PIC X(64) VALUE '40404040425C425C
      -      '404040404396404043864040439A404046E1404045D04040'.
                 04 FILLER             PIC X(116) VALUE    '446D40404AE8
      -      '404049A04040479D4040439F4040434740404382404043AF4040449A40
      -      '4046DA404045D2404046C040404040425C425C40404040'.
              03 WED1-KNJCD13.
                 04 FILLER             PIC X(64) VALUE '4040425C425C4040
      -      '4040446D4040438E404043C740404389404043BD404047FD'.
                 04 FILLER             PIC X(116) VALUE    '404048594040
      -      '4AE8404049A04040479D4040439F4040434740404382404043AF404044
      -      '9A404046DA404045D2404046C040404040425C425C4040'.
              03 WED1-KNJCD14.
                 04 FILLER             PIC X(64) VALUE '4040425C425C4040
      -      '4040446D4040438E404043C740404389404043BD404045BF'.
                 04 FILLER             PIC X(116) VALUE    '404045CB4040
      -      '4AE8404049A04040479D4040439F4040434740404382404043AF404044
      -      '9A404046DA404045D2404046C040404040425C425C4040'.
              03 WED1-KNJCD15.
                 04 FILLER             PIC X(64) VALUE '4040404040404040
      -      '425C425C40404040446D4040438C404043BD404043874040'.
                 04 FILLER             PIC X(116) VALUE    '4AE8404049A0
      -      '4040479D4040439F4040434740404382404043AF4040449A404046DA40
      -      '4045D2404046C040404040425C425C4040404040404040'.
              03 WED1-KNJCD16.
                 04 FILLER             PIC X(64) VALUE '40404040425C425C
      -      '40404040446D404043814040438E40404394404043484040'.
                 04 FILLER             PIC X(116) VALUE    '438E40404AE8
      -      '404049A04040479D4040439F4040434740404382404043AF4040449A40
      -      '4046DA404045D2404046C040404040425C425C40404040'.
              03 WED1-KNJCD17.
                 04 FILLER             PIC X(64) VALUE '425C425C40404040
      -      '446D4040438E404043C740404389404043BD404045C64040'.
                 04 FILLER             PIC X(116) VALUE    '459940404CAA
      -      '40404AE8404049A04040479D4040439F4040434740404382404043AF40
      -      '40449A404046DA404045D2404046C040404040425C425C'.
              03 WED1-KNJCD18.
                 04 FILLER             PIC X(64) VALUE '4040404040404040
      -      '4040425C425C40404040446D404054F9404046E140404AE8'.
                 04 FILLER             PIC X(116) VALUE    '404049A04040
      -      '479D4040439F4040434740404382404043AF4040449A404046DA404045
      -      'D2404046C040404040425C425C40404040404040404040'.
              03 WED1-KNJCD2.
                 04 FILLER             PIC X(48) VALUE '455445B4427A4AE8
      -      '49A0479D49D946CC427A477B485445F3'.
                 04 FILLER             PIC X(60) VALUE '45E346CC43BA438A
      -      '435843CE46CC4040424D404045A0404045C94040425D'.
           02 WED1-KNJMD.
              03 WED1-KNJMD1.
                 04 WED1-YMDKJ         PIC  X(04).
              03 WED1-KANJI11.
                 04 WED1-MDSKJ1        PIC  X(90).
              03 WED1-KANJI12.
                 04 WED1-MDSKJ2        PIC  X(90).
              03 WED1-KANJI13.
                 04 WED1-MDSKJ3        PIC  X(90).
              03 WED1-KANJI14.
                 04 WED1-MDSKJ4        PIC  X(90).
              03 WED1-KANJI15.
                 04 WED1-MDSKJ5        PIC  X(90).
              03 WED1-KANJI16.
                 04 WED1-MDSKJ6        PIC  X(90).
              03 WED1-KANJI17.
                 04 WED1-MDSKJ7        PIC  X(90).
              03 WED1-KANJI18.
                 04 WED1-MDSKJ8        PIC  X(90).
              03 WED1-KNJMD2.
                 04 WED1-KBNKJ         PIC  X(06).
                 04 WED1-MISKJ         PIC  X(12).
                 04 WED1-SOKKJ         PIC  X(06).
                 04 WED1-KSUKJ         PIC  X(04).
                 04 WED1-RECKJ         PIC  X(10).
                 04 WED1-GOKEI         PIC  X(16).
           02 WED1-SOKME               PIC  X(18).
       01  COUNT-AREA.
           02 I                        PIC S9(4) COMP.
           02 WCT1-PAGE                PIC S9(4) COMP.
           02 WCT1-LINE                PIC S9(4) COMP.
           02 WK-I                     PIC S9(04) COMP-3.
           02 WK-N                     PIC S9(04) COMP-3.
           02 WSW1-STNCD               PIC  X(01).
       01  P02-PARM.
               COPY    VPARB02 SUPPRESS.
       01  P07-PARM.
               COPY    VPARB07 SUPPRESS.
       01  P41-PARM.
               COPY    VPARB41 SUPPRESS.
       01  P43-PARM.
               COPY    VPARB43 SUPPRESS.
       01  FLAG-AREA.
           02 WFL1-FOUND               PIC  X(01).
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *    LINKAGE SECTION                                             *
      *----------------------------------------------------------------*
       01  PARM-AREA.
           02 FILLER                   PIC S9(04) COMP.
           02 PARM-KUBUN               PIC  X(02).
       PROCEDURE DIVISION USING PARM-AREA.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
              UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           PERFORM M10-PARM     THRU M10-EXIT.
           OPEN INPUT  WKCFILE
                OUTPUT WKBFILE
                       WKDFILE
                       KDPFILE.
       M1-L01.
           MOVE '3'         TO P41-HNBCD.
           MOVE WED1-KNJCD1 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA    TO WED1-KNJMD1.
           MOVE WED1-KNJCD2 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA    TO WED1-KNJMD2.
           MOVE WED1-YMDKJ  TO LPH-YMDKJ.
           MOVE WED1-KBNKJ  TO LPH-KBNKJ.
           MOVE WED1-MISKJ  TO LPH-MISKJ.
           MOVE WED1-SOKKJ  TO LPH-SOKKJ1 LPH-SOKKJ2 LPH-SOKKJ3.
           MOVE WED1-KSUKJ  TO LPH-KSUKJ1 LPH-KSUKJ2 LPH-KSUKJ3.
           MOVE WED1-RECKJ  TO LPH-RECKJ1 LPH-RECKJ2 LPH-RECKJ3.
           CALL 'VSBA0070' USING P07-PARM.
           MOVE P07-DATE    TO P02-DATE.
           CALL 'VSBA0020' USING P02-PARM.
           MOVE P02-YY      TO LPH-YY.
           MOVE P02-MM      TO LPH-MM.
           MOVE P02-DD      TO LPH-DD.
       M1-L02.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : PARM CHECK                                           *
      *----------------------------------------------------------------*
       M10-PARM.
           MOVE '3'          TO P41-HNBCD.
           MOVE WED1-KNJCD11 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI11.
           MOVE WED1-KNJCD12 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI12.
           MOVE WED1-KNJCD13 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI13.
           MOVE WED1-KNJCD14 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI14.
           MOVE WED1-KNJCD15 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI15.
           MOVE WED1-KNJCD16 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI16.
           MOVE WED1-KNJCD17 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI17.
           MOVE WED1-KNJCD18 TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA     TO WED1-KANJI18.
       M10-L01.
      *------ KAISYA-KOYU
           MOVE '0'          TO WFL1-FOUND.
           MOVE HIGH-VALUE   TO WTB1-TITLE.
           MOVE '01'         TO WTB1-PARM(1).
           MOVE WED1-KANJI11 TO WTB1-MDSKJ(1).
           MOVE '02'         TO WTB1-PARM(2).
           MOVE WED1-KANJI12 TO WTB1-MDSKJ(2).
           MOVE '03'         TO WTB1-PARM(3).
           MOVE WED1-KANJI13 TO WTB1-MDSKJ(3).
           MOVE '04'         TO WTB1-PARM(4).
           MOVE WED1-KANJI14 TO WTB1-MDSKJ(4).
           MOVE '05'         TO WTB1-PARM(5).
           MOVE WED1-KANJI15 TO WTB1-MDSKJ(5).
           MOVE '06'         TO WTB1-PARM(6).
           MOVE WED1-KANJI16 TO WTB1-MDSKJ(6).
           MOVE '07'         TO WTB1-PARM(7).
           MOVE WED1-KANJI17 TO WTB1-MDSKJ(7).
           MOVE '08'         TO WTB1-PARM(8).
           MOVE WED1-KANJI18 TO WTB1-MDSKJ(8).
       M10-L02.
           PERFORM VARYING I FROM 1 BY 1
              UNTIL ( I > 10 ) OR ( WTB1-PARM(I) = HIGH-VALUE ) OR
                    ( WFL1-FOUND = '1' )
              IF PARM-KUBUN = WTB1-PARM(I)
                 MOVE WTB1-MDSKJ(I)  TO LPH-MDSKJ
                 MOVE '1'            TO WFL1-FOUND
              END-IF
           END-PERFORM.
       M10-L03.
           IF WFL1-FOUND = '0'
              DISPLAY 'R VBKA1630 PARM ERROR ! '
              UPON CONSOLE
              CALL 'VSBABEND'.
       M10-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKCFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKC.
           READ WKCFILE INTO WKC-R
                AT END MOVE HIGH-VALUE TO WIN1-KEY
                       GO  TO  M11-EXIT.
       M11-L01.
           IF SKA-YTKBN OF WKC-R = '0'
              WRITE WKB-REC FROM WKC-R.
           IF SKA-YTKBN OF WKC-R = '1'
              WRITE WKD-REC FROM WKC-R.
           MOVE SKA-KAINO  OF WKC-R TO WIN1-KAINO.
           MOVE SKA-YTKBN  OF WKC-R TO WIN1-YTKBN.
           MOVE SKA-PGMSU  OF WKC-R TO WIN1-PGMSU.
           MOVE SKA-YUSBNO OF WKC-R TO WIN1-YUSBNO.
           MOVE SKA-YUSNO  OF WKC-R TO WIN1-YUSNO.
           MOVE SKA-YUSCKK OF WKC-R TO WIN1-YUSCKK.
           MOVE SKA-YUSCD  OF WKC-R TO WIN1-YUSCD.
           MOVE SKA-YUNCD  OF WKC-R TO WIN1-YUNCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    1.2   :     TABLE   CLEAR                                   *
      *----------------------------------------------------------------*
       M12-TBLCLA.
           MOVE 1           TO  WK-I.
       M12-L01.
           IF WK-I > 25
              GO  TO  M12-EXIT.
           MOVE SPACE       TO WTB1-SOKME  (WK-I 1).
           MOVE ZERO        TO WTB1-KENSU1 (WK-I 1).
           MOVE ZERO        TO WTB1-KENSU2 (WK-I 1).
           MOVE SPACE       TO WTB1-SOKME  (WK-I 2).
           MOVE ZERO        TO WTB1-KENSU1 (WK-I 2).
           MOVE ZERO        TO WTB1-KENSU2 (WK-I 2).
           MOVE SPACE       TO WTB1-SOKME  (WK-I 3).
           MOVE ZERO        TO WTB1-KENSU1 (WK-I 3).
           MOVE ZERO        TO WTB1-KENSU2 (WK-I 3).
           ADD 1            TO WK-I.
           GO  TO  M12-L01.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KAINO TO WSV1-KAINO.
           MOVE WIN1-YTKBN TO WSV1-YTKBN.
           MOVE WIN1-PGMSU TO WSV1-PGMSU.
           MOVE SPACE      TO LPT-TTL1.
           MOVE ZERO       TO WTT1-GOKEI1.
           MOVE ZERO       TO WTT1-GOKEI2.
           MOVE ZERO        TO WCT1-PAGE.
           PERFORM M12-TBLCLA THRU M12-EXIT.
           MOVE '0'         TO WSW1-STNCD.
           MOVE ZERO        TO WK-N.
           MOVE 1           TO WK-I.
       M2-L01.
           PERFORM M21-CNTRL-BREAK THRU M21-EXIT
              UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
       M2-L02.
           PERFORM M22-CNTRL-TOTAL THRU M22-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : CONTROL BREAK                                        *
      *----------------------------------------------------------------*
       M21-CNTRL-BREAK.
           MOVE WIN1-YUSBNO TO WSV1-YUSBNO.
           MOVE WIN1-YUSNO  TO WSV1-YUSNO.
           MOVE ZERO       TO WTT1-KENSU1.
           MOVE ZERO       TO WTT1-KENSU2.
           MOVE SPACE      TO WED1-SOKME.
       M21-L01.
           PERFORM M211-COUNT-UP THRU M211-EXIT
              UNTIL ( WIN1-KEYA NOT = WSV1-KEYA ).
       M21-L02.
           PERFORM M212-TABLE-WRITE THRU M212-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : CONTROL + ETC.                                     *
      *----------------------------------------------------------------*
       M211-COUNT-UP.
           MOVE WIN1-YUSCKK TO WSV1-YUSCKK.
           MOVE WIN1-YUSCD  TO WSV1-YUSCD.
           MOVE WIN1-YUNCD  TO WSV1-YUNCD.
       M211-L01.
           PERFORM M2111-COUNT-UP2 THRU M2111-EXIT
              UNTIL ( WIN1-KEYB NOT = WSV1-KEYB ).
       M211-L02.
           PERFORM M2112-COUNT-UP1 THRU M2112-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : COUNT UP  (KENSU2)                               *
      *----------------------------------------------------------------*
       M2111-COUNT-UP2.
           IF SKA-RECKB OF WKC-R = '40'
              MOVE SKA-STNMJ4 OF WKC-R TO WED1-SOKME.
           ADD 1        TO WTT1-KENSU2.
           ADD 1        TO WTT1-GOKEI2.
       M2111-L01.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.2 : COUNT UP  (KENSU1)                               *
      *----------------------------------------------------------------*
       M2112-COUNT-UP1.
           ADD 1        TO WTT1-KENSU1.
           ADD 1        TO WTT1-GOKEI1.
       M2112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : TABLE WRITE                                        *
      *----------------------------------------------------------------*
       M212-TABLE-WRITE.
           ADD 1           TO WK-N.
           IF WK-N > 3
               MOVE 1           TO WK-N
               ADD 1            TO WK-I.
           IF WSW1-STNCD = '1'
              MOVE '0'      TO WSW1-STNCD
              IF WK-N > 1
                 MOVE 1     TO WK-N
                 ADD 1      TO WK-I.
           IF WK-I > 25
              PERFORM M2121-TABLE-MAX THRU M2121-EXIT.
           MOVE WED1-SOKME  TO WTB1-SOKME  (WK-I WK-N).
           MOVE WTT1-KENSU1 TO WTB1-KENSU1 (WK-I WK-N).
           MOVE WTT1-KENSU2 TO WTB1-KENSU2 (WK-I WK-N).
           IF WIN1-YUSBNO NOT = WSV1-YUSBNO
              MOVE '1'      TO WSW1-STNCD.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1 :  TABLE MAX                                       *
      *----------------------------------------------------------------*
       M2121-TABLE-MAX.
           PERFORM M221-HEADER THRU M221-EXIT.
           MOVE SPACE     TO LPT-TTL1.
           PERFORM M223-MEISAI THRU M223-EXIT.
           PERFORM M12-TBLCLA  THRU M12-EXIT.
           MOVE 1         TO WK-I.
           MOVE 1         TO WK-N.
       M2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : CONTROL TOTAL                                        *
      *----------------------------------------------------------------*
       M22-CNTRL-TOTAL.
           PERFORM M221-HEADER THRU M221-EXIT.
           PERFORM M222-GOKEI  THRU M222-EXIT.
           PERFORM M223-MEISAI THRU M223-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1.  : HEADER                                           *
      *----------------------------------------------------------------*
       M221-HEADER.
           WRITE KDP-REC  FROM  LPH-HEAD0 AFTER SKPCH01.
           MOVE '1'        TO P43-HNBCD (1).
           MOVE 116        TO P43-START (1).
           MOVE 04         TO P43-LENGH (1).
           MOVE '*'        TO P43-HNBCD (2).
           MOVE LPH-HEAD1  TO P43-LPDAT.
           MOVE 128        TO P43-DATAL.
           CALL 'VSBA0430' USING P43-PARM.
           WRITE KDP-REC  FROM  P43-DATAM AFTER 1.
           MOVE '1'        TO P43-HNBCD (1).
           MOVE 27         TO P43-START (1).
           MOVE 90         TO P43-LENGH (1).
           MOVE '*'        TO P43-HNBCD (2).
           MOVE LPH-HEAD2  TO P43-LPDAT.
           MOVE 116        TO P43-DATAL.
           CALL 'VSBA0430' USING P43-PARM.
           WRITE KDP-REC  FROM  P43-DATAM AFTER 2.
           MOVE SPACE      TO LPH-YUSOK.
           IF WSV1-YTKBN = '0'
              MOVE '4674474447A240404040' TO P41-DATA
              CALL 'VSBA0410' USING P41-PARM
              MOVE P41-DATA TO LPH-YUSOK.
           IF WSV1-YTKBN = '1'
              MOVE '46A644AE48D844A747A2' TO P41-DATA
              CALL 'VSBA0410' USING P41-PARM
              MOVE P41-DATA TO LPH-YUSOK.
           MOVE SPACE      TO LPH-PAGE1.
           IF WSV1-PGMSU = 01
              MOVE '42F149D947A2' TO P41-DATA
              CALL 'VSBA0410' USING P41-PARM
              MOVE P41-DATA TO LPH-PAGE1.
           IF WSV1-PGMSU = 02
              MOVE '42F249D947A2' TO P41-DATA
              CALL 'VSBA0410' USING P41-PARM
              MOVE P41-DATA TO LPH-PAGE1.
           IF WSV1-PGMSU = 03
              MOVE '42F349D947A2' TO P41-DATA
              CALL 'VSBA0410' USING P41-PARM
              MOVE P41-DATA TO LPH-PAGE1.
           IF WSV1-PGMSU > 03
              MOVE '42F449D946F6455E47A2' TO P41-DATA
              CALL 'VSBA0410' USING P41-PARM
              MOVE P41-DATA TO LPH-PAGE1.
           MOVE '1'        TO P43-HNBCD (1).
           MOVE 6          TO P43-START (1).
           MOVE 16         TO P43-LENGH (1).
           MOVE '1'        TO P43-HNBCD (2).
           MOVE 24         TO P43-START (2).
           MOVE 22         TO P43-LENGH (2).
           MOVE '*'        TO P43-HNBCD (3).
           ADD 1           TO WCT1-PAGE.
           MOVE WCT1-PAGE  TO LPH-PAGEN.
           MOVE LPH-HEAD3  TO P43-LPDAT.
           MOVE 124        TO P43-DATAL.
           CALL 'VSBA0430' USING P43-PARM.
           WRITE KDP-REC  FROM  P43-DATAM AFTER 2.
           MOVE '1'        TO P43-HNBCD (1).
           MOVE 17         TO P43-START (1).
           MOVE 06         TO P43-LENGH (1).
           MOVE '1'        TO P43-HNBCD (2).
           MOVE 33         TO P43-START (2).
           MOVE 04         TO P43-LENGH (2).
           MOVE '1'        TO P43-HNBCD (3).
           MOVE 38         TO P43-START (3).
           MOVE 10         TO P43-LENGH (3).
           MOVE '1'        TO P43-HNBCD (4).
           MOVE 56         TO P43-START (4).
           MOVE 06         TO P43-LENGH (4).
           MOVE '1'        TO P43-HNBCD (5).
           MOVE 72         TO P43-START (5).
           MOVE 04         TO P43-LENGH (5).
           MOVE '1'        TO P43-HNBCD (6).
           MOVE 77         TO P43-START (6).
           MOVE 10         TO P43-LENGH (6).
           MOVE '1'        TO P43-HNBCD (7).
           MOVE 95         TO P43-START (7).
           MOVE 06         TO P43-LENGH (7).
           MOVE '1'        TO P43-HNBCD (8).
           MOVE 111        TO P43-START (8).
           MOVE 04         TO P43-LENGH (8).
           MOVE '1'        TO P43-HNBCD (9).
           MOVE 116        TO P43-START (9).
           MOVE 10         TO P43-LENGH (9).
           MOVE '*'        TO P43-HNBCD (10).
           MOVE LPH-HEAD4  TO P43-LPDAT.
           MOVE 125        TO P43-DATAL.
           CALL 'VSBA0430' USING P43-PARM.
           WRITE KDP-REC  FROM  P43-DATAM AFTER 2.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    2.2.2 :     GOKEI GYO                                       *
      *----------------------------------------------------------------*
       M222-GOKEI.
           IF (WK-I = 25) AND (WK-N = 3)
              GO  TO  M222-L01.
           IF WK-N = 3
              MOVE 1    TO WK-N
              ADD 1     TO WK-I
           ELSE
              ADD 1     TO WK-N.
           MOVE WED1-GOKEI  TO WTB1-SOKME  (WK-I WK-N).
           MOVE WTT1-GOKEI1 TO WTB1-KENSU1 (WK-I WK-N).
           MOVE WTT1-GOKEI2 TO WTB1-KENSU2 (WK-I WK-N).
           GO  TO  M222-EXIT.
       M222-L01.
           MOVE SPACE       TO LPT-TTL1.
           MOVE WED1-GOKEI  TO LPT-GOKEI.
           MOVE WTT1-GOKEI1 TO LPT-TOTL1.
           MOVE WTT1-GOKEI2 TO LPT-TOTL2.
       M222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.3.  : MEISAI                                           *
      *----------------------------------------------------------------*
       M223-MEISAI.
           MOVE 1       TO WK-I.
       M223-L01.
           IF WK-I > 25
              PERFORM M2231-TOTAL THRU M2231-EXIT
              GO TO M223-EXIT.
           MOVE SPACE   TO LPD-DTL1.
           IF (WTB1-SOKME  (WK-I 1) = SPACE) AND
              (WTB1-KENSU1 (WK-I 1) = ZERO)  AND
              (WTB1-KENSU2 (WK-I 1) = ZERO)
              GO TO M223-EXIT.
           MOVE '1'                  TO P43-HNBCD (1).
           MOVE 11                   TO P43-START (1).
           MOVE 18                   TO P43-LENGH (1).
           MOVE '*'                  TO P43-HNBCD (2).
           MOVE WTB1-SOKME  (WK-I 1) TO LPD-SOKME (1).
           MOVE WTB1-KENSU1 (WK-I 1) TO LPD-KENS1 (1).
           MOVE WTB1-KENSU2 (WK-I 1) TO LPD-KENS2 (1).
           IF (WTB1-SOKME  (WK-I 2) = SPACE) AND
              (WTB1-KENSU1 (WK-I 2) = ZERO)  AND
              (WTB1-KENSU2 (WK-I 2) = ZERO)
              GO  TO  M223-L03.
           MOVE '1'                  TO P43-HNBCD (2).
           MOVE 50                   TO P43-START (2).
           MOVE 18                   TO P43-LENGH (2).
           MOVE '*'                  TO P43-HNBCD (3).
           MOVE WTB1-SOKME  (WK-I 2) TO LPD-SOKME (2).
           MOVE WTB1-KENSU1 (WK-I 2) TO LPD-KENS1 (2).
           MOVE WTB1-KENSU2 (WK-I 2) TO LPD-KENS2 (2).
           IF (WTB1-SOKME  (WK-I 3) = SPACE) AND
              (WTB1-KENSU1 (WK-I 3) = ZERO)  AND
              (WTB1-KENSU2 (WK-I 3) = ZERO)
              GO TO M223-L03.
           MOVE '1'                  TO P43-HNBCD (3).
           MOVE 89                   TO P43-START (3).
           MOVE 18                   TO P43-LENGH (3).
           MOVE '*'                  TO P43-HNBCD (4).
           MOVE WTB1-SOKME  (WK-I 3) TO LPD-SOKME (3).
           MOVE WTB1-KENSU1 (WK-I 3) TO LPD-KENS1 (3).
           MOVE WTB1-KENSU2 (WK-I 3) TO LPD-KENS2 (3).
       M223-L03.
           MOVE LPD-DTL1  TO P43-LPDAT.
           MOVE 125       TO P43-DATAL.
           CALL 'VSBA0430' USING P43-PARM.
           WRITE KDP-REC  FROM  P43-DATAM AFTER 2.
           ADD 1          TO WK-I.
           GO  TO  M223-L01.
       M223-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.3.1  :  MAX 26                                         *
      *----------------------------------------------------------------*
       M2231-TOTAL.
           IF LPT-TTL1 = SPACE
              GO TO M2231-EXIT.
           MOVE '1'       TO P43-HNBCD (1).
           MOVE 89        TO P43-START (1).
           MOVE 18        TO P43-LENGH (1).
           MOVE '*'       TO P43-HNBCD (2).
           MOVE LPT-TTL1  TO P43-LPDAT.
           MOVE 124       TO P43-DATAL.
           CALL 'VSBA0430' USING P43-PARM.
           WRITE KDP-REC  FROM  P43-DATAM AFTER 2.
       M2231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKBFILE
                 WKDFILE
                 KDPFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA1630  END                                          *
      *-------------------------------------------------------------*
