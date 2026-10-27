./ ADD NAME=VBKA5070  01.04 20150827 20190710 093047 00446 00423 MCSK013
       ID  DIVISION.
       PROGRAM-ID.     'VBKA5070'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : KA                            *
      *              <> PG.OUTLINE     : SEIKKYU GOUKEI HYO            *
      *                                  YUSOSAKI SITEN JOHO HONYAKU   *
      *                                                                *
      *                 * INPUT FILE   : SEIKYU GOUKEI HYO DATA (DWKA) *
      *                 * OUTPUT FILE  : SEIKYU GOUKEI HYO DATA (DWKB) *
      *                                                                *
      *              <> AUTHOR         : CSK-Y.MORIO                   *
      *              <> DATE           : 03/08/06                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1780             Õ›∫≥   : 15/08/27  CSK-Y.YOSHIGA       *
      * (02) K-02163            Õ›∫≥   : 19/07/10  SCSK-A.ISIKAWA      *
      * (03) YYYYYY YYYYYY       ¬≤∂   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  P34-PARM.
           COPY VPARB34 SUPPRESS.
       01  P35-PARM.
           COPY VPARB35 SUPPRESS.
       01  PA8-PARM.
           COPY VPARBA8 SUPPRESS.
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
                 04 WKA-SHZSK7              PIC  X(01).
                 04 FILLER                  PIC  X(146).
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
           02 WKB-DATA05  REDEFINES  WKB-LPDAT.
              03 WKB-GNO05                  PIC  9(02).
              03 WKB-RKB05                  PIC  X(02).
              03 WKB-MESAI5.
                 04 WKB-HJPTN5              PIC  X(01).
                 04 WKB-MIDAK5              PIC  X(08).
                 04 WKB-TGNME5  OCCURS  3.
                    05 WKB-GSKAJ5           PIC  X(30).
                    05 WKB-KOZKJ5           PIC  X(04).
                    05 WKB-KZABG5           PIC  X(07).
                 04 WKB-BNKSU5              PIC  9(01).
                 04 FILLER                  PIC  X(82).
           02 WKB-DATA06  REDEFINES  WKB-LPDAT.
              03 WKB-GNO06                  PIC  9(02).
              03 WKB-RKB06                  PIC  X(02).
              03 WKB-MESAI6.
                 04 WKB-STNMJ6              PIC  X(18).
                 04 WKB-STYBN6              PIC  X(07).
                 04 WKB-STTEL6              PIC  X(13).
                 04 WKB-NSTJJ6.
                    05 WKB-NSTJ16           PIC  X(36).
                    05 WKB-NSTJ26           PIC  X(36).
                 04 FILLER                  PIC  X(105).
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
                 04 WKB-SHZSK7              PIC  X(01).
                 04 FILLER                  PIC  X(146).
           02 WKB-DATA08  REDEFINES  WKB-LPDAT.
              03 WKB-GNO08                  PIC  9(02).
              03 WKB-RKB08                  PIC  X(02).
              03 WKB-ZBEGK.
                 04 WKB-MIDS1               PIC  X(22).
                 04 WKB-ZOKDK               PIC S9(13)   COMP-3.
                 04 WKB-MIDS2               PIC  X(08).
                 04 WKB-ZSHZI               PIC S9(13)   COMP-3.
                 04 WKB-ZEIRT               PIC S9(02)V9 COMP-3.
                 04 WKB-KGZRT               PIC  X(01).
                 04 FILLER                  PIC X(168).
           02 WKB-DATA09  REDEFINES  WKB-LPDAT.
              03 WKB-GNO09                  PIC  9(02).
              03 WKB-RKB09                  PIC  X(02).
              03 WKB-MESAI9.
                 04 WKB-ZENGT9.
                    05 WKB-ZSKDK9           PIC S9(13)  COMP-3.
                    05 WKB-ZNKDK9           PIC S9(13)  COMP-3.
                    05 WKB-ZSYNB9           PIC S9(09)  COMP-3.
                    05 WKB-ZKRDK9           PIC S9(13)  COMP-3.
                 04 WKB-TOGTU9.
                    05 WKB-TKIDK9           PIC S9(13)  COMP-3.
                    05 WKB-TSKDK9           PIC S9(13)  COMP-3.
                    05 WKB-MSKSK9           PIC S9(13)  COMP-3.
                    05 WKB-TURZN9           PIC S9(13)  COMP-3.
                 04 WKB-SHZEI9              PIC S9(13)  COMP-3.
                 04 WKB-TKAIG9              PIC S9(13)  COMP-3.
                 04 WKB-TNMJO9              PIC  X(40).
                 04 WKB-TNMGE9              PIC  X(40).
                 04 WKB-NOSKA9              PIC  X(30).
                 04 WKB-HIFLG9              PIC  X(01).
                 04 FILLER                  PIC  X(36).
           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILLER                        PIC  X(03).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-YUSBNO                PIC  X(03).
              03 WIN1-YUSNO                 PIC  X(03).
           02 WIN2-KEY.
              03 WIN2-YUSBNO                PIC  X(03).
              03 WIN2-YUSNO                 PIC  X(03).
              03 WIN2-SYKNO                 PIC  X(03).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-YUSBNO                PIC  X(03).
              03 WSV1-YUSNO                 PIC  X(03).
           02 WSV2-KEY.
              03 WSV2-YUSBNO                PIC  X(03).
              03 WSV2-YUSNO                 PIC  X(03).
              03 WSV2-SYKNO                 PIC  X(03).
       01  EDIT-AREA.
           02 WED1-SORTJ.
              03 WED1-SORTJ1                PIC  X(07).
              03 WED1-SORTJ2                PIC  X(02).
           02 WED2-YUSCKK.
              03 WED2-YUSCK1                PIC  X(03).
              03 FILLER                     PIC  X(02).
       01  KANJI-AREA.
           02 WKJ1-KANJI.
              03 WKJ1-KAN                   PIC  X(10)
                                            VALUE 'éÊà¯ã‚çs'.
           02 WKJ2-KANJI  REDEFINES  WKJ1-KANJI.
              03 FILLER                     PIC  X(01).
              03 WKJ2-KAN                   PIC  X(08).
              03 FILLER                     PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                OUTPUT WKBFILE.
           CALL 'VSBA0340'  USING P34-PARM.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKA-YUSBNO      TO WIN1-YUSBNO.
           MOVE WKA-YUSNO       TO WIN1-YUSNO.
           MOVE WKA-YUSCKK      TO WED2-YUSCKK.
           MOVE WED2-YUSCK1     TO WIN2-SYKNO.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WKA-YUSCD  TO WED1-SORTJ1.
           MOVE WKA-YUNCD  TO WED1-SORTJ2.
           IF WKA-RKB03 = '03'
              IF WIN1-KEY NOT = WSV1-KEY
                PERFORM M21-HONYAKU THRU M21-EXIT
              END-IF
              IF WIN2-KEY NOT = WSV2-KEY
                PERFORM M22-HONYAKU THRU M22-EXIT
              END-IF
              PERFORM M23-EDIT1-WKB THRU M23-EXIT
           ELSE
              PERFORM M24-EDIT2-WKB THRU M24-EXIT.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : HONYAKU SHORI (YUSBNO + YUSNO)                       *
      *----------------------------------------------------------------*
       M21-HONYAKU.
           MOVE WIN1-KEY    TO WSV1-KEY.
           MOVE '5'         TO P35-HNBCD.
           MOVE WIN1-YUSBNO TO P35-STNNO.
           MOVE WIN1-YUSNO  TO P35-SOKON.
           CALL 'VSBA0350' USING P35-PARM P34-PARM.
           IF P35-RTNCD = ZERO
              MOVE P35-HSTNO   TO WIN2-YUSBNO
              MOVE P35-SOKON   TO WIN2-YUSNO
           ELSE
              MOVE WIN1-YUSBNO TO WIN2-YUSBNO
              MOVE WIN1-YUSNO  TO WIN2-YUSNO.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : HONYAKU SHORI (YUSBNO + YUSNOO + SYKNO)              *
      *----------------------------------------------------------------*
       M22-HONYAKU.
           MOVE WIN2-KEY    TO WSV2-KEY.
           MOVE WIN2-YUSBNO TO PA8-STNCD.
           MOVE WIN2-YUSNO  TO PA8-BOTCD.
           MOVE WIN2-SYKNO  TO PA8-SYKNO.
           CALL 'VSBA1080'  USING PA8-PARM.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : EDIT1 WKBFILE (REKO-DO KUBUN = '03')                 *
      *----------------------------------------------------------------*
       M23-EDIT1-WKB.
           MOVE SPACE      TO WKB-R.
           INITIALIZE         WKB-R.
           MOVE WKA-R      TO WKB-R.
           MOVE WED1-SORTJ TO WKB-SORTJ.
           WRITE WKB-REC  FROM WKB-R.
       M23-L01.
           MOVE SPACE      TO WKB-R.
           INITIALIZE         WKB-R.
           MOVE WKA-CNTKEY TO WKB-CNTKEY.
           MOVE ZERO       TO WKB-GNO05.
           MOVE '05'       TO WKB-RKB05.
           MOVE SPACE      TO WKB-HJPTN5.
           MOVE WKJ2-KAN   TO WKB-MIDAK5.
           MOVE PA8-BANK1  TO WKB-TGNME5 (1).
           MOVE PA8-BANK2  TO WKB-TGNME5 (2).
           MOVE PA8-BANK3  TO WKB-TGNME5 (3).
           IF PA8-BANK3 = SPACE
             IF PA8-BANK2 = SPACE
               IF PA8-BANK1 = SPACE
                 MOVE ZERO TO WKB-BNKSU5
               ELSE
                 MOVE 1    TO WKB-BNKSU5
               END-IF
             ELSE
               MOVE 2      TO WKB-BNKSU5
             END-IF
           ELSE
             MOVE 3        TO WKB-BNKSU5.
           MOVE WKA-PGMSU  TO WKB-PGMSU.
           MOVE WED1-SORTJ TO WKB-SORTJ.
           WRITE WKB-REC  FROM WKB-R.
       M23-L02.
           MOVE SPACE      TO WKB-R.
           INITIALIZE         WKB-R.
           MOVE WKA-CNTKEY TO WKB-CNTKEY.
           MOVE ZERO       TO WKB-GNO06.
           MOVE '06'       TO WKB-RKB06.
           MOVE PA8-STNME  TO WKB-STNMJ6.
           MOVE PA8-YUBNO  TO WKB-STYBN6.
           MOVE PA8-TELNO  TO WKB-STTEL6.
           MOVE PA8-ADRES  TO WKB-NSTJJ6.
           MOVE WKA-PGMSU  TO WKB-PGMSU.
           MOVE WED1-SORTJ TO WKB-SORTJ.
           WRITE WKB-REC  FROM WKB-R.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4 : EDIT2 WKBFILE (REKO-DO KUBUN NOT = '03')             *
      *----------------------------------------------------------------*
       M24-EDIT2-WKB.
           MOVE SPACE      TO WKB-R.
           INITIALIZE         WKB-R.
           MOVE WKA-R      TO WKB-R.
           MOVE WED1-SORTJ TO WKB-SORTJ.
           WRITE WKB-REC  FROM WKB-R.
       M24-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *         VBKA5070 END                                           *
      *----------------------------------------------------------------*
