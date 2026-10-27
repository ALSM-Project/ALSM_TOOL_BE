./ ADD NAME=VBKA4460  01.16 20030820 20190726 115806 00343 00341 MCSK013
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4460'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K                             *
      *              <> PG.OUTLINE     : BU SYUCYOUJYO JYUSYO HONYAKU  *
      *                                                                *
      *                 * INPUT FILE   : SEIKYUSYO DATA                *
      *                                  (SEIKYUU KAGAMI)       (DWKB) *
      *                 * OUTPUT FILE  : SEIKYUSYO DATA                *
      *                                  (SEIKYUU KAGAMI)       (DWKC) *
      *                                                                *
      *              <> AUTHOR         : CSK-R.TAKAYANAGI              *
      *              <> DATE           : 99/03/08                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-568              ÍÝº³   : 03/08/20  CSK-C.YAMAGATA      *
      * (02) K-02163            ÍÝº³   : 19/06/26  SCSK-A.ISIKAWA      *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  P34-PARM.                                                    VBS01370
           COPY VPARB34 SUPPRESS.                                       VBS01380
       01  P35-PARM.                                                    VBS01370
           COPY VPARB35 SUPPRESS.                                       VBS01380
       01  PA8-PARM.                                                    VBS01370
           COPY VPARBA8 SUPPRESS.                                       VBS01380
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
                 04 WKB-STNMJ               PIC  X(18).
                 04 WKB-STYBN               PIC  X(07).
                 04 WKB-STTEL               PIC  X(13).
                 04 WKB-NSTJJ.
                    05 WKB-NSTJ1            PIC  X(36).
                    05 WKB-NSTJ2            PIC  X(36).
                 04 WKB-HKJBG               PIC  X(14).
                 04 FILLER                  PIC  X(91).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
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
              03 FILLER  REDEFINES  WKC-MESAI3.
                 04 WKC-HJPTN2              PIC  X(01).
                 04 WKC-HYOJI1              PIC  X(22).
                 04 WKC-HYOJI2              PIC  X(16).
                 04 WKC-HYOJI3              PIC  X(26).
                 04 WKC-HYOJI4              PIC  X(20).
                 04 FILLER                  PIC  X(130).
              03 FILLER  REDEFINES  WKC-MESAI3.
                 04 WKC-HJPTN3              PIC  X(01).
                 04 FILLER                  PIC  X(214).
           02 WKC-DATA40  REDEFINES  WKC-LPDAT.
              03 WKC-GNO40                  PIC  9(02).
              03 WKC-RKB40                  PIC  X(02).
              03 WKC-MESAI4.
                 04 WKC-STNMJ               PIC  X(18).
                 04 WKC-STYBN               PIC  X(07).
                 04 WKC-STTEL               PIC  X(13).
                 04 WKC-NSTJJ.
                    05 WKC-NSTJ1            PIC  X(36).
                    05 WKC-NSTJ2            PIC  X(36).
                 04 WKC-HKJBG               PIC  X(14).
                 04 FILLER                  PIC  X(91).
           02 WKC-PGMSU                     PIC  X(02).
           02 WKC-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-STNCD              PIC  X(03).
                 04 WIN1-SOKNO              PIC  X(03).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-STNCD              PIC  X(03).
                 04 WIN2-SOKNO              PIC  X(03).
                 04 WIN2-CKUKA              PIC  X(03).
       01  EDIT-AREA.
           02 WED1-PARM.
              03 WED1-KEYS                 PIC  X(09).
              03 WED1-DATA                 PIC  X(233).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-STNCD                PIC  X(03).
              03 WSV1-SOKNO                PIC  X(03).
           02 WSV2-KEY1.
              03 WSV2-STNCD                PIC  X(03).
              03 WSV2-SOKNO                PIC  X(03).
              03 WSV2-CKUKA                PIC  X(03).
           02 WSV3-FLAGX.
              03 WSV3-FLAG1                PIC  X(01).
              03 WSV3-FLAG2                PIC  X(01).
              03 WSV3-FLAG3                PIC  X(01).
      *
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY1 = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                OUTPUT WKCFILE.
           CALL 'VSBA0340'  USING P34-PARM.
           MOVE HIGH-VALUE TO WSV1-KEY1.
           MOVE SPACE      TO WSV2-KEY1.
           PERFORM M11-READ-WKB THRU M11-EXIT.
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
           MOVE WKB-YSBNO TO WIN1-STNCD.
           MOVE WKB-YSSTN TO WIN1-SOKNO.
           MOVE WKB-YSCKG TO WIN2-CKUKA.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (H O N Y A K U)                            *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF ( WKB-RKB40 = '40'                       )  OR
              ( WKB-RKB30 = '30' AND WKB-HJPTN1 = SPACE )
              IF WIN1-KEY1 NOT = WSV1-KEY1
                 PERFORM M21-HONYAKU THRU M21-EXIT
              END-IF
              IF WIN2-KEY1 NOT = WSV2-KEY1
                 PERFORM M22-HONYAKU THRU M22-EXIT
              END-IF
           END-IF.
           PERFORM M23-WKCOUT THRU M23-EXIT.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : HONYAKU SHORI (STNCD + SOKNO)                        *
      *----------------------------------------------------------------*
       M21-HONYAKU.
           MOVE WIN1-KEY1   TO WSV1-KEY1.
           MOVE '5'         TO P35-HNBCD.
           MOVE WIN1-STNCD  TO P35-STNNO.
           MOVE WIN1-SOKNO  TO P35-SOKON.
           CALL 'VSBA0350' USING P35-PARM P34-PARM.
           IF P35-RTNCD = ZERO
              MOVE P35-HSTNO  TO WIN2-STNCD
              MOVE P35-SOKON  TO WIN2-SOKNO
           ELSE
              MOVE WIN1-STNCD TO WIN2-STNCD
              MOVE WIN1-SOKNO TO WIN2-SOKNO.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : HONYAKU SHORI (STNCD + SOKNO OR (BOTCD) + SYKNO)     *
      *----------------------------------------------------------------*
       M22-HONYAKU.
           MOVE WIN2-KEY1  TO WSV2-KEY1.
           MOVE WIN2-STNCD TO PA8-STNCD.
           MOVE WIN2-SOKNO TO PA8-BOTCD.
           MOVE WIN2-CKUKA TO PA8-SYKNO.
           CALL 'VSBA1080' USING PA8-PARM.
           MOVE PA8-PARM   TO WED1-PARM.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKCOUT                                               *
      *----------------------------------------------------------------*
       M23-WKCOUT.
           IF WKB-RKB30 = '30' AND WKB-HJPTN1 = SPACE
              PERFORM M231-OUT30  THRU M231-EXIT
           ELSE
              IF WKB-RKB40 = '40'
                 PERFORM M232-OUT40  THRU M232-EXIT
              ELSE
                 MOVE WKB-R      TO WKC-R
                 WRITE WKC-REC FROM WKC-R.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : WKCWRITE (KUBUN=30)                                *
      *----------------------------------------------------------------*
       M231-OUT30.
           MOVE WKB-R      TO WKC-R.
           MOVE SPACE      TO WKC-MESAI3.
           MOVE WKB-MIDAS  TO WKC-MIDAS.
           MOVE WKB-HJPTN1 TO WKC-HJPTN1.
           IF WED1-DATA = SPACE
              MOVE 0       TO WKC-BNKSU
              GO TO M231-L01.
           MOVE PA8-BANK1  TO WKC-TRBNK (1).
           MOVE PA8-BANK2  TO WKC-TRBNK (2).
           MOVE PA8-BANK3  TO WKC-TRBNK (3).
           MOVE SPACE      TO WSV3-FLAGX.
           IF PA8-BANK1  = SPACE
              MOVE '*'     TO WSV3-FLAG1.
           IF PA8-BANK2  = SPACE
              MOVE '*'     TO WSV3-FLAG2.
           IF PA8-BANK3  = SPACE
              MOVE '*'     TO WSV3-FLAG3.
           IF WSV3-FLAGX = '***'
              MOVE 0       TO WKC-BNKSU
              GO TO M231-L01.
           IF WSV3-FLAGX = ' **'
              MOVE 1       TO WKC-BNKSU
              GO TO M231-L01.
           IF WSV3-FLAGX = '  *'
              MOVE 2       TO WKC-BNKSU
           ELSE
              MOVE 3       TO WKC-BNKSU.
       M231-L01.
           WRITE WKC-REC FROM WKC-R.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.2 : WKCWRITE (KUBUN=40)                                *
      *----------------------------------------------------------------*
       M232-OUT40.
           MOVE WKB-R      TO WKC-R.
           MOVE SPACE      TO WKC-MESAI4.
           IF WED1-DATA  = SPACE
              GO TO M232-L01.
           MOVE PA8-STNME  TO WKC-STNMJ.
           MOVE PA8-YUBNO  TO WKC-STYBN.
           MOVE PA8-TELNO  TO WKC-STTEL.
           MOVE PA8-ADRES  TO WKC-NSTJJ.
       M232-L01.
           WRITE WKC-REC FROM WKC-R.
       M232-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *                VBKA4460   END                                  *
      *----------------------------------------------------------------*
