./ ADD NAME=VBKA1210  01.02 19980427 20140206 152727 00616 00615 MCSK004
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1210'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : 20 SIME SEIKYU SHORI          *
      *              <> PG.OUTLINE     : SHIMEBI HENKOUSAKI            *
      *                                  URIAGE GOUKEI DATA SAKUSEI    *
      *                                                                *
      *                 * INPUT FILE   : SHIMEBI HENKOUSAKI TORIHIKI   *
      *                                  MEISAI DATA      (DWKB)       *
      *                 * OUTPUT FILE  : SHIMEBI HENKOUSAKI            *
      *                                  URIAGEDAKAKEI DATA            *
      *                                                   (DWKA)       *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-H.NIIKURA             *
      *              <> DATE           : 86/08/07                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) SYOHIZEI TUIKA     ÍÝº³   : 89/03/20  CSK-M.AOYAMA        *
      * (02) K-275C             ÍÝº³   : 98/04/27  CSK-R.TAKAYANAGI    *
      * (03) K-1549             ÍÝº³   : 14/02/06  CSK-N.EGUCHI        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN TO S-DWKB.
           SELECT WKAFILE ASSIGN TO S-DWKA.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(250).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(380).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKB-R.
           02 WKB-SITEK                     PIC  X(01).
           02 WKB-SOKOK                     PIC  X(01).
           02 WKB-DNPNO.
              03 WKB-DNSYK                  PIC  X(01).
              03 WKB-DNREN                  PIC  9(06).
           02 WKB-TORIK.
              03 WKB-TURIK                  PIC  X(01).
              03 WKB-THAIK                  PIC  X(01).
           02 WKB-CKUKG.
              03 WKB-CKUKA                  PIC  X(03).
              03 WKB-CKUCK                  PIC  X(02).
           02 WKB-TOKCD.
              03 WKB-TKCD1                  PIC  X(04).
              03 WKB-TKCD2                  PIC  X(03).
           02 WKB-SSOKO                     PIC  X(01).
           02 WKB-SCKUK                     PIC  X(05).
           02 WKB-SKYCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-HAYMD.
              03 WKB-HAYY                   PIC  9(04).
              03 WKB-HAMM                   PIC  9(02).
              03 WKB-HADD                   PIC  9(02).
           02 WKB-LINNO                     PIC  9(01).
           02 WKB-SYOCD.
              03 WKB-SYCD1                  PIC  X(03).
              03 WKB-SYCD2                  PIC  X(06).
           02 WKB-SURYO                     PIC S9(05) COMP-3.
           02 WKB-TANKA                     PIC S9(08) COMP-3.
           02 WKB-NTANK                     PIC S9(08) COMP-3.
           02 WKB-HDNPN                     PIC  X(07).
           02 WKB-HDNPL                     PIC  9(01).
           02 WKB-HDNPD.
              03 WKB-HDNYY                  PIC  9(04).
              03 WKB-HDNMM                  PIC  9(02).
              03 WKB-HDNDD                  PIC  9(02).
           02 WKB-HDNPT                     PIC  X(02).
           02 WKB-URIDA                     PIC  9(08).
           02 WKB-ZHYOS                     PIC  X(01).
           02 WKB-HKHYO                     PIC  X(01).
           02 WKB-GIGYD.
              03 WKB-GIYY                   PIC  9(04).
              03 WKB-GIMM                   PIC  9(02).
              03 WKB-GIDD                   PIC  9(02).
           02 WKB-SBMKG                     PIC  X(02).
           02 WKB-SYOME.
              03 WKB-SYOMH                  PIC  X(25).
              03 WKB-SYOMK                  PIC  X(10).
              03 WKB-SYOMY                  PIC  X(10).
           02 WKB-SHIMC                     PIC  9(04).
           02 WKB-SHIMM                     PIC  X(08).
           02 WKB-TSIME                     PIC  9(02).
           02 WKB-SEKMM                     PIC  9(02).
           02 WKB-TSYCD.
              03 WKB-TSCD1                  PIC  X(03).
              03 WKB-TSCD2                  PIC  X(06).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-TSFKB                     PIC  X(01).
           02 WKB-THAKB                     PIC  X(01).
           02 WKB-ADNPN                     PIC  X(07).
           02 WKB-ADNPL                     PIC  9(01).
           02 WKB-ADNPD                     PIC  9(08).
           02 WKB-ADNPT                     PIC  X(02).
           02 WKB-TOKME                     PIC  X(25).
           02 WKB-TKSYM.
              03 WKB-TKSYY                  PIC  9(04).
              03 WKB-TKSMM                  PIC  9(02).
           02 WKB-OSOKO                     PIC  X(01).
           02 WKB-ZHAKB                     PIC  X(01).
           02 WKB-SSIMD                     PIC  9(08).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-SHZEI                     PIC S9(11) COMP-3.
           02 WKB-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 FILLER                        PIC  X(01).
       01  WKA-R.
           02 WKA-SITEK                     PIC  X(01).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-SIMEB                     PIC  9(02).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-TOHAT.
              03 WKA-TOET1                  PIC S9(11) COMP-3.
              03 WKA-TONE1                  PIC S9(11) COMP-3.
           02 WKA-TOHAZ.
              03 WKA-TOET2                  PIC S9(11) COMP-3.
              03 WKA-TONE2                  PIC S9(11) COMP-3.
           02 WKA-ZEHAZ.
              03 WKA-ZEET1                  PIC S9(11) COMP-3.
              03 WKA-ZENE1                  PIC S9(11) COMP-3.
           02 WKA-ZETRN.
              03 WKA-ZENMM                  OCCURS 12.
                 04 WKA-ZENME               PIC S9(11) COMP-3.
                 04 WKA-ZENMN               PIC S9(11) COMP-3.
           02 WKA-SHZEI.
              03 WKA-STOAT.
                 04 WKA-STOE1               PIC S9(11) COMP-3.
                 04 WKA-STON1               PIC S9(11) COMP-3.
              03 WKA-STOAZ.
                 04 WKA-STOE2               PIC S9(11) COMP-3.
                 04 WKA-STON2               PIC S9(11) COMP-3.
              03 WKA-SZEAZ.
                 04 WKA-SZET1               PIC S9(11) COMP-3.
                 04 WKA-SZEN1               PIC S9(11) COMP-3.
              03 WKA-SZTRN.
                 04 WKA-SZNMM               OCCURS 12.
                    05 WKA-SZNME            PIC S9(11) COMP-3.
                    05 WKA-SZNMN            PIC S9(11) COMP-3.
           02 WKA-SSIMD                     PIC  9(08).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEYB.
                 04 WIN1-KEYA.
                    05 WIN1-TOKCD           PIC  X(07).
                 04 WIN1-NOSCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEYB.
                 04 WSV1-KEYA.
                    05 WSV1-TOKCD           PIC  X(07).
                 04 WSV1-NOSCD              PIC  X(02).
       01  WORK-AREA.
           02 I                             PIC  9(02).
           02 WK-KINGAKU                    PIC S9(11) COMP-3.
           02 WK-ZEKIN                      PIC S9(11) COMP-3.
           02 DECI-FLG1                     PIC  X(02).
           02 WK-SSIMD.
              03 WK-SSIYM.
                 04 WK-SSIYY                PIC  9(04).
                 04 WK-SSIMM                PIC  9(02).
              03 FILLER                     PIC  9(02).
           02 DECI-FLG2                     PIC  X(01).
           02 DECI-FLG3                     PIC  X(01).
           02 WK-TSYCD.
              03 FILLER                     PIC  9(04).
              03 WK-TSYCD59                 PIC  9(05).
       PROCEDURE DIVISION.
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
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKBFILE
                OUTPUT WKAFILE.
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
           MOVE WKB-TOKCD TO WIN1-TOKCD.
           MOVE WKB-NOSCD TO WIN1-NOSCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           PERFORM M21-INIT THRU M21-EXIT.
       M2-L01.
           PERFORM M22-TOTAL THRU M22-EXIT
                   UNTIL ( WIN1-KEY NOT = WSV1-KEY ).
       M2-L02.
           PERFORM M23-WKA-WRITE THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : INITIAL                                              *
      *----------------------------------------------------------------*
       M21-INIT.
           MOVE WIN1-KEY  TO WSV1-KEY.
           MOVE SPACES    TO WKA-R.
           MOVE WKB-SITEK TO WKA-SITEK.
           MOVE WKB-TOKCD TO WKA-TOKCD.
           MOVE WKB-TSIME TO WKA-SIMEB.
           MOVE WKB-NOSCD TO WKA-NOSCD.
           MOVE ZERO      TO WKA-TOET1.
           MOVE ZERO      TO WKA-TONE1.
           MOVE ZERO      TO WKA-TOET2.
           MOVE ZERO      TO WKA-TONE2.
           MOVE ZERO      TO WKA-ZEET1.
           MOVE ZERO      TO WKA-ZENE1.
           PERFORM M21-L01
                   VARYING I FROM 1 BY 1 UNTIL I = 13
                   GO TO M21-L02.
       M21-L01.
           MOVE ZERO      TO WKA-ZENME (I).
           MOVE ZERO      TO WKA-ZENMN (I).
       M21-L02.
           MOVE ZERO      TO WKA-STOE1.
           MOVE ZERO      TO WKA-STON1.
           MOVE ZERO      TO WKA-STOE2.
           MOVE ZERO      TO WKA-STON2.
           MOVE ZERO      TO WKA-SZET1.
           MOVE ZERO      TO WKA-SZEN1.
           PERFORM M21-L03
                   VARYING I FROM 1 BY 1 UNTIL I = 13
                   GO TO M21-L04.
       M21-L03.
           MOVE ZERO      TO WKA-SZNME (I).
           MOVE ZERO      TO WKA-SZNMN (I).
       M21-L04.
           MOVE WKB-SSIMD TO WKA-SSIMD.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : TOTAL                                                *
      *----------------------------------------------------------------*
       M22-TOTAL.
           PERFORM M221-INIT THRU M221-EXIT.
           PERFORM M222-DECI THRU M222-EXIT.
           PERFORM M223-DECI THRU M223-EXIT.
           PERFORM M224-DECI THRU M224-EXIT.
           PERFORM M225-KIN  THRU M225-EXIT.
           PERFORM M226-TOT  THRU M226-EXIT.
           PERFORM M227-ZEI  THRU M227-EXIT.
       M22-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : INITIAL                                            *
      *----------------------------------------------------------------*
       M221-INIT.
           MOVE SPACES    TO DECI-FLG1.
           MOVE WKB-SSIMD TO WK-SSIMD.
           MOVE SPACES    TO DECI-FLG2.
           MOVE WKB-TSYCD TO WK-TSYCD.
           MOVE ZERO      TO I.
           MOVE ZERO      TO WK-KINGAKU.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.2 : DECI                                               *
      *----------------------------------------------------------------*
       M222-DECI.
           IF ( NOT ( WKB-ZHAKB = '1' ) ) AND
              (     ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              ( NOT ( WKB-TURIK = '8' ) )
              MOVE '15' TO DECI-FLG1
              GO TO M222-EXIT.
           IF ( NOT ( WKB-ZHAKB = '1' ) ) AND
              (     ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              (     ( WKB-TURIK = '8' ) )
              MOVE '26' TO DECI-FLG1
              GO TO M222-EXIT.
           IF ( NOT ( WKB-ZHAKB = '1' ) ) AND
              ( NOT ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              ( NOT ( WKB-TURIK = '8' ) )
              MOVE '35' TO DECI-FLG1
              GO TO M222-EXIT.
           IF ( NOT ( WKB-ZHAKB = '1' ) ) AND
              ( NOT ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              (     ( WKB-TURIK = '8' ) )
              MOVE '26' TO DECI-FLG1
              GO TO M222-EXIT.
           IF (     ( WKB-ZHAKB = '1' ) ) AND
              (     ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              ( NOT ( WKB-TURIK = '8' ) )
              MOVE '45' TO DECI-FLG1
              GO TO M222-EXIT.
           IF (     ( WKB-ZHAKB = '1' ) ) AND
              (     ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              (     ( WKB-TURIK = '8' ) )
              MOVE '26' TO DECI-FLG1
              GO TO M222-EXIT.
           IF (     ( WKB-ZHAKB = '1' ) ) AND
              ( NOT ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              ( NOT ( WKB-TURIK = '8' ) )
              MOVE '45' TO DECI-FLG1
              GO TO M222-EXIT.
           IF ( NOT ( WKB-ZHAKB = '1' ) ) AND
              (     ( NOT ( WKB-TKSYM > WK-SSIYM ) ) ) AND
              ( NOT ( WKB-TURIK = '8' ) )
              MOVE '26' TO DECI-FLG1
              GO TO M222-EXIT.
       M222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.3 : DECI GET WK-KINGAKU KEISANSIKI                     *
      *----------------------------------------------------------------*
       M223-DECI.
           IF ( WKB-TORIK = '11' OR '12' OR '21' OR '14' )
              MOVE '1' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TORIK = '15' OR '16' OR '17' OR '18' )
              MOVE '5' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TORIK = '31' OR '32' OR '34' )
              MOVE '2' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TORIK = '35' OR '36' OR '37' OR '38' )
              MOVE '6' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TURIK = '4' OR '6' ) AND
              ( WK-TSYCD59 = '88888' OR '88889' )
              MOVE '5' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TURIK = '4' OR '6' ) AND
              ( NOT ( WK-TSYCD59 = '88888' OR '88889' ) )
              MOVE '3' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TURIK = '5' OR '7' ) AND
              ( WK-TSYCD59 = '88888' OR '88889' )
              MOVE '6' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TURIK = '5' OR '7' ) AND
              ( NOT ( WK-TSYCD59 = '88888' OR '88889' ) )
              MOVE '4' TO DECI-FLG2
              GO TO M223-EXIT.
           IF ( WKB-TURIK = '8' )
              MOVE '6' TO DECI-FLG2
              GO TO M223-EXIT.
       M223-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.4 : DECI SET ICHIZUKE I                                *
      *----------------------------------------------------------------*
       M224-DECI.
           IF WKB-TKSYY = WK-SSIYY
              COMPUTE I = WKB-TKSMM - WK-SSIMM + 1
           ELSE
              COMPUTE I = 12 - WK-SSIMM + WKB-TKSMM + 1.
       M224-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.5 : GET WK-KINGAKU                                     *
      *----------------------------------------------------------------*
       M225-KIN.
           IF DECI-FLG2 = '1'
              PERFORM M2251-KIN THRU M2251-EXIT
           ELSE
              IF DECI-FLG2 = '2'
                 PERFORM M2252-KIN THRU M2252-EXIT
              ELSE
                 IF DECI-FLG2 = '3'
                    PERFORM M2253-KIN THRU M2253-EXIT
                 ELSE
                    IF DECI-FLG2 = '4'
                       PERFORM M2254-KIN THRU M2254-EXIT
                    ELSE
                       IF DECI-FLG2 = '5'
                          PERFORM M2255-KIN THRU M2255-EXIT
                       ELSE
                          IF DECI-FLG2 = '6'
                             PERFORM M2256-KIN THRU M2256-EXIT.
       M225-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.5.1 : DECI GET WK-KINGAKU 1                            *
      *----------------------------------------------------------------*
       M2251-KIN.
           COMPUTE WK-KINGAKU = WKB-TANKA * WKB-SURYO.
       M2251-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.5.2 : DECI GET WK-KINGAKU 2                            *
      *----------------------------------------------------------------*
       M2252-KIN.
           COMPUTE WK-KINGAKU = WKB-TANKA * WKB-SURYO * (-1).
       M2252-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.5.3 : DECI GET WK-KINGAKU 3                            *
      *----------------------------------------------------------------*
       M2253-KIN.
           COMPUTE WK-KINGAKU = WKB-NTANK * WKB-SURYO.
       M2253-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.5.4 : DECI GET WK-KINGAKU 4                            *
      *----------------------------------------------------------------*
       M2254-KIN.
           COMPUTE WK-KINGAKU = WKB-NTANK * WKB-SURYO * (-1).
       M2254-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.5.5 : DECI GET WK-KINGAKU 5                            *
      *----------------------------------------------------------------*
       M2255-KIN.
           COMPUTE WK-KINGAKU = WKB-NTANK.
       M2255-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.3.6 : DECI GET WK-KINGAKU 6                            *
      *----------------------------------------------------------------*
       M2256-KIN.
           COMPUTE WK-KINGAKU = WKB-NTANK * (-1).
       M2256-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.6 : TOTAL SHORI                                        *
      *----------------------------------------------------------------*
       M226-TOT.
           IF DECI-FLG1 = '15'
              PERFORM M2261-TOT THRU M2261-EXIT
              PERFORM M2265-TOT THRU M2265-EXIT
           ELSE
              IF DECI-FLG1 = '26'
                 PERFORM M2262-TOT THRU M2262-EXIT
                 PERFORM M2266-TOT THRU M2266-EXIT
              ELSE
                 IF DECI-FLG1 = '35'
                    PERFORM M2263-TOT THRU M2263-EXIT
                    PERFORM M2265-TOT THRU M2265-EXIT
                 ELSE
                    IF DECI-FLG1 = '45'
                       PERFORM M2264-TOT THRU M2264-EXIT
                       PERFORM M2265-TOT THRU M2265-EXIT.
       M226-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.6.1 : TOT DECI-FLG1 KOUMOKU-1                          *
      *----------------------------------------------------------------*
       M2261-TOT.
           COMPUTE WKA-TOET1 = WKA-TOET1 + WK-KINGAKU.
       M2261-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.6.2 : TOT DECI-FLG1 KOUMOKU-2                          *
      *----------------------------------------------------------------*
       M2262-TOT.
           COMPUTE WKA-TONE1 = WKA-TONE1 + WK-KINGAKU.
       M2262-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.6.3 : TOT DECI-FLG1 KOUMOKU-3                          *
      *----------------------------------------------------------------*
       M2263-TOT.
           COMPUTE WKA-TOET2 = WKA-TOET2 + WK-KINGAKU.
       M2263-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.6.4 : TOT DECI-FLG1 KOUMOKU-4                          *
      *----------------------------------------------------------------*
       M2264-TOT.
           COMPUTE WKA-ZEET1 = WKA-ZEET1 + WK-KINGAKU.
       M2264-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.4.5 : TOT DECI-FLG1 KOUMOKU-5                          *
      *----------------------------------------------------------------*
       M2265-TOT.
           COMPUTE WKA-ZENME (I) = WKA-ZENME (I) + WK-KINGAKU.
       M2265-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.6.6 : TOT DECI-FLG1 KOUMOKU-6                          *
      *----------------------------------------------------------------*
       M2266-TOT.
           COMPUTE WKA-ZENMN (I) = WKA-ZENMN (I) + WK-KINGAKU.
       M2266-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7 : ZEI                                                *
      *----------------------------------------------------------------*
       M227-ZEI.
           IF ( WKB-ZHAKB NOT = '1' )      AND
              ( WKB-TKSYM NOT > WK-SSIYM ) AND
              ( WKB-TURIK NOT = '8' )
              PERFORM M2271-STOE1 THRU M2271-EXIT
              PERFORM M2275-SZNME THRU M2275-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB NOT = '1' )      AND
              ( WKB-TKSYM NOT > WK-SSIYM ) AND
              ( WKB-TURIK = '8' )
              PERFORM M2272-STON1 THRU M2272-EXIT
              PERFORM M2276-SZNMN THRU M2276-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB NOT = '1' )  AND
              ( WKB-TKSYM > WK-SSIYM ) AND
              ( WKB-TURIK NOT = '8' )
              PERFORM M2273-STOE2 THRU M2273-EXIT
              PERFORM M2275-SZNME THRU M2275-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB NOT = '1' )  AND
              ( WKB-TKSYM > WK-SSIYM ) AND
              ( WKB-TURIK = '8' )
              PERFORM M2272-STON1 THRU M2272-EXIT
              PERFORM M2276-SZNMN THRU M2276-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB = '1' )          AND
              ( WKB-TKSYM NOT > WK-SSIYM ) AND
              ( WKB-TURIK NOT = '8' )
              PERFORM M2274-SZET1 THRU M2274-EXIT
              PERFORM M2275-SZNME THRU M2275-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB = '1' )          AND
              ( WKB-TKSYM NOT > WK-SSIYM ) AND
              ( WKB-TURIK = '8' )
              PERFORM M2272-STON1 THRU M2272-EXIT
              PERFORM M2276-SZNMN THRU M2276-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB = '1' )      AND
              ( WKB-TKSYM > WK-SSIYM ) AND
              ( WKB-TURIK NOT = '8' )
              PERFORM M2274-SZET1 THRU M2274-EXIT
              PERFORM M2275-SZNME THRU M2275-EXIT
              GO TO M227-EXIT.
           IF ( WKB-ZHAKB = '1' )      AND
              ( WKB-TKSYM > WK-SSIYM ) AND
              ( WKB-TURIK = '8' )
              PERFORM M2272-STON1 THRU M2272-EXIT
              PERFORM M2276-SZNMN THRU M2276-EXIT
              GO TO M227-EXIT.
       M227-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7.1 : STOE1                                            *
      *----------------------------------------------------------------*
       M2271-STOE1.
           COMPUTE WKA-STOE1 = WKA-STOE1 + WKB-SHZEI.
       M2271-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7.2 : STON1                                            *
      *----------------------------------------------------------------*
       M2272-STON1.
           COMPUTE WKA-STON1 = WKA-STON1 + WKB-SHZEI.
       M2272-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7.3 : STOE2                                            *
      *----------------------------------------------------------------*
       M2273-STOE2.
           COMPUTE WKA-STOE2 = WKA-STOE2 + WKB-SHZEI.
       M2273-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7.4 : SZET1                                            *
      *----------------------------------------------------------------*
       M2274-SZET1.
           COMPUTE WKA-SZET1 = WKA-SZET1 + WKB-SHZEI.
       M2274-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7.5 : SZNME (I)                                        *
      *----------------------------------------------------------------*
       M2275-SZNME.
           COMPUTE WKA-SZNME(I) = WKA-SZNME(I) + WKB-SHZEI.
       M2275-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.7.6 : SZNMN (I)                                        *
      *----------------------------------------------------------------*
       M2276-SZNMN.
           COMPUTE WKA-SZNMN(I) = WKA-SZNMN(I) + WKB-SHZEI.
       M2276-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE WRITE                                        *
      *----------------------------------------------------------------*
       M23-WKA-WRITE.
           WRITE WKA-REC FROM WKA-R.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKAFILE.
       M3-EXIT.
           EXIT.
