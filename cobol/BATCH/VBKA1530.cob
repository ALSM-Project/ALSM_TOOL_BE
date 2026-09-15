./ ADD NAME=VBKA1530  01.02 20180601 20190624 095209 00605 00600 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1530'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : KAKAXX  SYSTEM                *
      *              <> PG.OUTLINE     : MATCHING ( 1 : 1 )            *
      *                 * INPUT FILE   : SYOUHIN MASTER   (SMSFILE)    *
      *                                : SEIKYUU HANBAI   (DWKA)       *
      *                 * OUTPUT FILE  : SEIKYUU MEISAI   (DWKB)       *
      *              <> AUTHOR         : CSK-K.OTSUKA                  *
      *              <> DATE           : 86/08/19                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) ¼®³ËÝÒ² ÍÝ¼­³¼Þ®³¹Ý Â²¶   : 86/10/22  SUZ-A.Y             *
      * (02) INPUT & OUTPUT FILE NO                                    *
      *      LAYOUT & LENGTH     ÍÝº³                                  *
      *      SYOHIN MEI NO ÍÝ¼Õ³ ÍÝº³  : 86/11/13  CSK-H.NARUSE        *
      * (03) KAISYU-45                 : 89/03/10  CSK-A.ONITSUKA      *
      * (04) I-2201 YOUKEN HENKOU      : 92/06/26  SUZ-T.OKUDA         *
      * (05) K-258               ÍÝº³  : 97/09/24  CSK-Y.FUJITA        *
      * (06) K-275C              ÍÝº³  : 98/04/28  CSK-R.TAKAYANAGI    *
      * (07) H-010  T-6336       ÍÝº³  : 07/09/10  CSK-Y.YAMADA        *
      * (08) K-1624              ÍÝº³  : 14/08/13  CSK-Y.MIO           *
      * (09) K-1780              ÍÝº³  : 15/07/31  CSK-Y.YOSHIGA       *
      * (10) K-2066           ØºÝÊß²Ù  : 18/06/01  CSK-T.KINOSHITA     *
      * (11) K-2163              ÍÝº³  : 19/06/24  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT MMSFILE ASSIGN    TO MMSFILE
                          ORGANIZATION INDEXED
                          ACCESS MODE   RANDOM
                          RECORD KEY    KEY-MMS
                          FILE  STATUS  S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(151).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(242).
       FD  MMSFILE
           LABEL RECORD STANDARD.
       01  MMS-REC.
           02 KEY-MMS.
              03 KEY-MMS1.
                 04 KEY-MMS11               PIC  X(01).
                 04 KEY-MMS12               PIC  X(03).
              03 KEY-MMS2                   PIC  X(01).
           02 FILLER                        PIC  X(95).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  P28-PARM.
           COPY VPARB28 SUPPRESS.
       01  P49-PARM.
           COPY VPARB49 SUPPRESS.
       01  P56-PARM.
           COPY VPARB56 SUPPRESS.
       01  P41-PARM.
           COPY VPARB41 SUPPRESS.
       01  WKA-R.
           02 WKA-STNCD                     PIC  X(03).
           02 WKA-SKUCD                     PIC  X(07).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-SOKOK                     PIC  X(01).
           02 WKA-CKUKG                     PIC  X(05).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-NSKBN                     PIC  X(02).
           02 WKA-DNPH1.
              03 WKA-DNYY1                  PIC  9(04).
              03 WKA-DNMM1                  PIC  9(02).
              03 WKA-DNDD1                  PIC  9(02).
           02 WKA-DNPN1.
              03 WKA-DNPN11                 PIC  X(01).
              03 FILLER                     PIC  X(06).
           02 WKA-DNPNL                     PIC  9(01).
           02 WKA-TORIK.
              03 WKA-TURIK                  PIC  X(01).
              03 WKA-THAIK                  PIC  X(01).
           02 WKA-SYOCD.
              03 WKA-SYCD1.
                 04 WKA-SYCD11              PIC  X(01).
                 04 FILLER                  PIC  X(02).
              03 WKA-SYCD2                  PIC  X(06).
           02 WKA-SYOCD-R REDEFINES WKA-SYOCD.
              03 WKA-SYCD4                  PIC  X(04).
              03 WKA-SYCD5                  PIC  X(05).
           02 WKA-SYOME.
              03 WKA-SYOMH                  PIC  X(25).
              03 WKA-SYOMKY                 PIC  X(20).
           02 WKA-SURYO                     PIC S9(05) COMP-3.
           02 WKA-TANKA                     PIC S9(08) COMP-3.
           02 WKA-KINGK                     PIC S9(09) COMP-3.
           02 WKA-DNPH2                     PIC  9(08).
           02 WKA-DNPN2                     PIC  X(07).
           02 WKA-THKBN                     PIC  X(01).
           02 WKA-BNKKB                     PIC  X(01).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-SHIMM                     PIC  X(08).
           02 WKA-SHZEI                     PIC S9(09) COMP-3.
           02 WKA-SHIMC                     PIC  9(04).
           02 WKA-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKA-DSHZFG                    PIC  X(01).
           02 WKA-KGZKB                     PIC  X(01).
       01  MMS-R.
           COPY VFILMMS SUPPRESS.
       01  WKB-R.
           02 WKB-STNCD                     PIC  X(03).
           02 WKB-SKUCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-SOKOK                     PIC  X(01).
           02 WKB-CKUKG                     PIC  X(05).
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NSKBN                     PIC  X(02).
           02 WKB-DNPH1.
              03 WKB-DNYY1                  PIC  9(04).
              03 WKB-DNMM1                  PIC  9(02).
              03 WKB-DNDD1                  PIC  9(02).
           02 WKB-DNPN1                     PIC  X(07).
           02 WKB-DNPNL                     PIC  9(01).
           02 WKB-TORIK.
              03 WKB-TURIK                  PIC  X(01).
              03 WKB-THAIK                  PIC  X(01).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-SYOME.
              03 WKB-SYOMH                  PIC  X(50).
              03 WKB-SYOMKY                 PIC  X(40).
           02 WKB-SURYO                     PIC S9(05) COMP-3.
           02 WKB-TANKA                     PIC S9(08) COMP-3.
           02 WKB-KINGK                     PIC S9(09) COMP-3.
           02 WKB-DNKIN                     PIC S9(09) COMP-3.
           02 WKB-DNPH2                     PIC  9(08).
           02 WKB-DNPN2                     PIC  X(07).
           02 WKB-THKBN                     PIC  X(01).
           02 WKB-BNKKB                     PIC  X(01).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-TORKJ                     PIC  X(08).
           02 WKB-SYOME-X.
              03 WKB-SYOMH-X                PIC  X(25).
              03 WKB-SYOMKY-X               PIC  X(20).
           02 WKB-SHZEI                     PIC S9(09) COMP-3.
           02 WKB-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKB-DSHZFG                    PIC  X(01).
           02 WKB-KGZKB                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY1.
              03 WIN1-SYOCD                 PIC  X(09).
           02 WIN2-KEY1.
              03 WIN2-SYOCD                 PIC  X(09).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SYOCD                 PIC  X(09).
           02 WSV2-KEY1.
              03 WSV2-SYOCD                 PIC  X(09).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN2-KEY1 = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                       MMSFILE
                OUTPUT WKBFILE.
           MOVE '10S'     TO P28-TYPE.
           MOVE 'O'       TO P28-OPCLK.
           CALL 'VSBA0280' USING P28-PARM.
           PERFORM M11-READ-SMS THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ SMSFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-SMS.
           CALL 'VSBA0280' USING P28-PARM.
           IF P28-RTNCD NOT = ZERO
              MOVE HIGH-VALUE TO WIN1-KEY1
              GO TO M11-EXIT.
           MOVE SMS-HSYCD TO WIN1-SYOCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY1
                   GO TO M12-EXIT.
           MOVE WKA-SYOCD TO WIN2-SYOCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M22-SMSONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY1  TO WSV2-KEY1.
           MOVE '3'        TO P49-HNBCD.
           MOVE SMS-SYHKK  TO P49-SYHKK.
           MOVE SMS-SYKYKK TO P49-SYKYKK.
           CALL 'VSBA0490' USING P49-PARM.
           PERFORM M211-WKB-WRITE1 THRU M211-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 :   WKBFILE  WRITE                                   *
      *----------------------------------------------------------------*
       M211-WKB-WRITE1.
           MOVE SPACE      TO WKB-R.
           MOVE WKA-STNCD  TO WKB-STNCD.
           MOVE WKA-SKUCD  TO WKB-SKUCD.
           MOVE WKA-SNSCD  TO WKB-SNSCD.
           MOVE WKA-SOKOK  TO WKB-SOKOK.
           MOVE WKA-CKUKG  TO WKB-CKUKG.
           MOVE WKA-TOKCD  TO WKB-TOKCD.
           MOVE WKA-NSKBN  TO WKB-NSKBN.
           MOVE WKA-DNPH1  TO WKB-DNPH1.
           MOVE WKA-DNPN1  TO WKB-DNPN1.
           MOVE WKA-DNPNL  TO WKB-DNPNL.
           MOVE WKA-TORIK  TO WKB-TORIK.
           MOVE WKA-SYOCD  TO WKB-SYOCD.
           MOVE P49-SYHKJ  TO WKB-SYOMH.
           MOVE P49-SYKYKJ TO WKB-SYOMKY.
           MOVE WKA-SURYO  TO WKB-SURYO.
           MOVE WKA-TANKA  TO WKB-TANKA.
           MOVE WKA-KINGK  TO WKB-KINGK.
           MOVE ZERO       TO WKB-DNKIN.
           MOVE WKA-DNPH2  TO WKB-DNPH2.
           MOVE WKA-DNPN2  TO WKB-DNPN2.
           MOVE WKA-THKBN  TO WKB-THKBN.
           MOVE WKA-BNKKB  TO WKB-BNKKB.
           MOVE WKA-KETKB  TO WKB-KETKB.
           MOVE SPACE      TO WKB-TORKJ.
           MOVE SMS-SYMKK  TO WKB-SYOME-X.
           MOVE WKA-SHZEI  TO WKB-SHZEI.
           MOVE WKA-ZEIRT  TO WKB-ZEIRT.
           MOVE WKA-DSHZFG TO WKB-DSHZFG.
           MOVE WKA-KGZKB  TO WKB-KGZKB.
           WRITE WKB-REC FROM WKB-R.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : SMSFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-SMSONL.
           PERFORM M11-READ-SMS THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M231-WKB-WRITE2 THRU M231-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 :   WKBFILE  WRITE                                   *
      *----------------------------------------------------------------*
       M231-WKB-WRITE2.
           MOVE SPACE      TO WKB-R.
           MOVE WKA-STNCD  TO WKB-STNCD.
           MOVE WKA-SKUCD  TO WKB-SKUCD.
           MOVE WKA-SNSCD  TO WKB-SNSCD.
           MOVE WKA-SOKOK  TO WKB-SOKOK.
           MOVE WKA-CKUKG  TO WKB-CKUKG.
           MOVE WKA-TOKCD  TO WKB-TOKCD.
           MOVE WKA-NSKBN  TO WKB-NSKBN.
           MOVE WKA-DNPH1  TO WKB-DNPH1.
           MOVE WKA-DNPN1  TO WKB-DNPN1.
           MOVE WKA-DNPNL  TO WKB-DNPNL.
           MOVE WKA-TORIK  TO WKB-TORIK.
           MOVE WKA-SYOCD  TO WKB-SYOCD.
           MOVE WKA-SURYO  TO WKB-SURYO.
           MOVE WKA-TANKA  TO WKB-TANKA.
           MOVE WKA-KINGK  TO WKB-KINGK.
           MOVE ZERO       TO WKB-DNKIN.
           MOVE WKA-DNPH2  TO WKB-DNPH2.
           MOVE WKA-DNPN2  TO WKB-DNPN2.
           MOVE WKA-THKBN  TO WKB-THKBN.
           MOVE WKA-BNKKB  TO WKB-BNKKB.
           MOVE WKA-KETKB  TO WKB-KETKB.
           MOVE SPACE      TO WKB-TORKJ.
           PERFORM M2311-WKB-EDIT THRU M2311-EXIT.
           PERFORM M2313-WKB-EDIT THRU M2313-EXIT.
           MOVE WKA-SHZEI  TO WKB-SHZEI.
           MOVE WKA-ZEIRT  TO WKB-ZEIRT.
           MOVE WKA-DSHZFG TO WKB-DSHZFG.
           MOVE WKA-KGZKB  TO WKB-KGZKB.
           WRITE WKB-REC FROM WKB-R.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1 : SYOME EDIT                                       *
      *----------------------------------------------------------------*
       M2311-WKB-EDIT.
           MOVE SPACE   TO WKB-SYOMH.
           MOVE SPACE   TO WKB-SYOMKY.
           IF WKA-SYCD4 NOT = '9999'
              MOVE 'A'  TO KEY-MMS2
              MOVE WKA-SHIMC TO KEY-MMS1
              PERFORM M23112-READ-MMS THRU M23112-EXIT.
           IF WKA-TURIK = '4' OR '5'
              GO TO M2311-L03.
           IF WKA-TURIK = '6' OR '7'
              GO TO M2311-L04.
           GO TO M2311-L05.
       M2311-L03.
           IF WKA-SYCD5 = '88888'
              IF WKA-SYCD4 = '9999'
                 PERFORM M23114-SYMKY-EDIT1 THRU M23114-EXIT
              ELSE
                 IF MMS-R = SPACE
                    PERFORM M23114-SYMKY-EDIT1 THRU M23114-EXIT
                 ELSE
                    PERFORM M23115-SYMKY-EDIT2 THRU M23115-EXIT
           ELSE
              IF WKA-SYCD5 = '88889'
                 IF WKA-SYCD4 = '9999'
                    PERFORM M23116-SYMKY-EDIT3 THRU M23116-EXIT
                 ELSE
                    IF MMS-R = SPACE
                       PERFORM M23116-SYMKY-EDIT3 THRU M23116-EXIT
                    ELSE
                       PERFORM M23117-SYMKY-EDIT4 THRU M23117-EXIT
              ELSE
                 PERFORM M23113-SYOME-EDIT THRU M23113-EXIT.
           GO TO M2311-EXIT.
       M2311-L04.
           IF WKA-SYCD5 = '88888'
              IF WKA-SYCD4 = '9999'
                 PERFORM M23116-SYMKY-EDIT3 THRU M23116-EXIT
              ELSE
                 IF MMS-R = SPACE
                    PERFORM M23116-SYMKY-EDIT3 THRU M23116-EXIT
                 ELSE
                    PERFORM M23117-SYMKY-EDIT4 THRU M23117-EXIT
           ELSE
              PERFORM M23113-SYOME-EDIT THRU M23113-EXIT.
           GO TO M2311-EXIT.
       M2311-L05.
           PERFORM M23113-SYOME-EDIT THRU M23113-EXIT.
       M2311-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1.2 : MMSFILE READ                                   *
      *----------------------------------------------------------------*
       M23112-READ-MMS.
           READ MMSFILE INTO MMS-R
                   INVALID MOVE SPACE  TO MMS-R.
       M23112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1.3 : HINMEI KIKAKU YOURYOU  EDIT                    *
      *----------------------------------------------------------------*
       M23113-SYOME-EDIT.
           MOVE '3'        TO P49-HNBCD.
           MOVE WKA-SYOMH  TO P49-SYHKK.
           MOVE WKA-SYOMKY TO P49-SYKYKK.
           CALL 'VSBA0490' USING P49-PARM.
           MOVE P49-SYHKJ  TO WKB-SYOMH.
           MOVE P49-SYKYKJ TO WKB-SYOMKY.
       M23113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1.4 : KIKAKU YOURYOU EDIT (1)                        *
      *----------------------------------------------------------------*
       M23114-SYMKY-EDIT1.
           MOVE '3'      TO P41-HNBCD.
           IF WKA-TURIK = '4'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D46DA' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D464D' TO P41-DATA.
           IF WKA-TURIK = '5'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D464D' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D46DA' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA TO WKB-SYOMKY.
       M23114-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1.5 : KIKAKU YOURYOU EDIT (2)                        *
      *----------------------------------------------------------------*
       M23115-SYMKY-EDIT2.
           MOVE '3'      TO P41-HNBCD.
           IF WKA-TURIK = '4'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D46DA' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D464D' TO P41-DATA.
           IF WKA-TURIK = '5'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D464D' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D46DA' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE MMS-SHIKJ TO P56-KUMKA.
           MOVE P41-DATA  TO P56-KUMKB.
           MOVE ZERO      TO P56-SPACE.
           CALL 'VSBA0560' USING P56-PARM.
           MOVE P56-KUMKR TO WKB-SYOMKY.
       M23115-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1.6 : KIKAKU YOURYOU EDIT (3)                        *
      *----------------------------------------------------------------*
       M23116-SYMKY-EDIT3.
           MOVE '3'        TO P41-HNBCD.
           MOVE '45414FC5' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA   TO WKB-SYOMKY.
       M23116-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1.7 : KIKAKU YOURYOU EDIT (4)                        *
      *----------------------------------------------------------------*
       M23117-SYMKY-EDIT4.
           MOVE '3'        TO P41-HNBCD.
           MOVE '45414FC5' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE MMS-SHIKJ TO P56-KUMKA.
           MOVE P41-DATA  TO P56-KUMKB.
           MOVE ZERO      TO P56-SPACE.
           CALL 'VSBA0560' USING P56-PARM.
           MOVE P56-KUMKR TO WKB-SYOMKY.
       M23117-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3 : SYOME EDIT                                       *
      *----------------------------------------------------------------*
       M2313-WKB-EDIT.
           MOVE SPACE   TO WKB-SYOMH-X.
           MOVE SPACE   TO WKB-SYOMKY-X.
           IF WKA-SYCD4 NOT = '9999'
              MOVE 'A'  TO KEY-MMS2
              MOVE WKA-SHIMC TO KEY-MMS1
              PERFORM M23112-READ-MMS THRU M23112-EXIT.
           IF WKA-TURIK = '4' OR '5'
              GO TO M2313-L03.
           IF WKA-TURIK = '6' OR '7'
              GO TO M2313-L04.
           GO TO M2313-L05.
       M2313-L03.
           IF WKA-SYCD5 = '88888'
              IF WKA-SYCD4 = '9999'
                 PERFORM M23134-SYOMH-EDIT1 THRU M23134-EXIT
              ELSE
                 IF MMS-R = SPACE
                    PERFORM M23134-SYOMH-EDIT1 THRU M23134-EXIT
                 ELSE
                    PERFORM M23135-SYOMH-EDIT2 THRU M23135-EXIT
           ELSE
              IF WKA-SYCD5 = '88889'
                 IF WKA-SYCD4 = '9999'
                    PERFORM M23136-SYOMH-EDIT3 THRU M23136-EXIT
                 ELSE
                    IF MMS-R = SPACE
                       PERFORM M23136-SYOMH-EDIT3 THRU M23136-EXIT
                    ELSE
                       PERFORM M23137-SYOMH-EDIT4 THRU M23137-EXIT
              ELSE
                 PERFORM M23133-SYOME-EDIT THRU M23133-EXIT.
           GO TO M2313-EXIT.
       M2313-L04.
           IF WKA-SYCD5 = '88888'
              IF WKA-SYCD4 = '9999'
                 PERFORM M23136-SYOMH-EDIT3 THRU M23136-EXIT
              ELSE
                 IF MMS-R = SPACE
                    PERFORM M23136-SYOMH-EDIT3 THRU M23136-EXIT
                 ELSE
                    PERFORM M23137-SYOMH-EDIT4 THRU M23137-EXIT
           ELSE
              PERFORM M23133-SYOME-EDIT THRU M23133-EXIT.
           GO TO M2313-EXIT.
       M2313-L05.
           PERFORM M23133-SYOME-EDIT THRU M23133-EXIT.
       M2313-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3.3 : HINMEI KIKAKU YOURYOU  EDIT                    *
      *----------------------------------------------------------------*
       M23133-SYOME-EDIT.
           MOVE WKA-SYOMH  TO WKB-SYOMH-X.
           MOVE WKA-SYOMKY TO WKB-SYOMKY-X.
       M23133-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3.4 : KIKAKU YOURYOU EDIT (1)                        *
      *----------------------------------------------------------------*
       M23134-SYOMH-EDIT1.
           MOVE '2'      TO P41-HNBCD.
           IF WKA-TURIK = '4'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D46DA' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D464D' TO P41-DATA.
           IF WKA-TURIK = '5'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D464D' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D46DA' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA TO WKB-SYOMH-X.
       M23134-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3.5 : KIKAKU YOURYOU EDIT (2)                        *
      *----------------------------------------------------------------*
       M23135-SYOMH-EDIT2.
           MOVE '3'      TO P41-HNBCD.
           IF WKA-TURIK = '4'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D46DA' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D464D' TO P41-DATA.
           IF WKA-TURIK = '5'
              IF WKA-THKBN = 'A'
                 MOVE '457B45D0487D464D' TO P41-DATA
              ELSE
                 MOVE '457B45D0487D46DA' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE MMS-SHIKJ TO P56-KUMKA.
           MOVE P41-DATA  TO P56-KUMKB.
           MOVE ZERO      TO P56-SPACE.
           CALL 'VSBA0560' USING P56-PARM.
           MOVE '1'       TO P41-HNBCD.
           MOVE P56-KUMKR TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA  TO WKB-SYOMH-X.
       M23135-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3.6 : KIKAKU YOURYOU EDIT (3)                        *
      *----------------------------------------------------------------*
       M23136-SYOMH-EDIT3.
           MOVE '2'        TO P41-HNBCD.
           MOVE '45414FC5' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA   TO WKB-SYOMH-X.
       M23136-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.3.7 : KIKAKU YOURYOU EDIT (4)                        *
      *----------------------------------------------------------------*
       M23137-SYOMH-EDIT4.
           MOVE '3'        TO P41-HNBCD.
           MOVE '45414FC5' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE MMS-SHIKJ  TO P56-KUMKA.
           MOVE P41-DATA   TO P56-KUMKB.
           MOVE ZERO       TO P56-SPACE.
           CALL 'VSBA0560' USING P56-PARM.
           MOVE '1'        TO P41-HNBCD.
           MOVE P56-KUMKR  TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA   TO WKB-SYOMH-X.
       M23137-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           MOVE 'C'   TO P28-OPCLK.
           CALL 'VSBA0280' USING P28-PARM.
           CLOSE WKAFILE
                 MMSFILE
                 WKBFILE.
       M3-EXIT.
           EXIT.
