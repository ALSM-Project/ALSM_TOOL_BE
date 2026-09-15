./ ADD NAME=VBKA1520  01.04 20150731 20190624 145426 00606 00597 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1520'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : KAKAXX  SYSTEM                *
      *              <> PG.OUTLINE     : MATCHING ( 1 : 1 )            *
      *                 * INPUT FILE   : SEIKYUU TORIKESI (DWKA)       *
      *                 * INPUT FILE   : SEIKYUU HANBAI   (DWKB)       *
      *                 * OUTPUT FILE  : SEIKYUU MEISAI   (DWKC)       *
      *              <> AUTHOR         : CSK-K.OTSUKA                  *
      *              <> DATE           : 86/08/11                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) WKDFILE             Â²¶   : 86/09/08  CSK-K.OTSUKA        *
      * (02) FILE LAYOUT & LENGTH                                      *
      *                         ÍÝº³   : 86/11/12  CSK-H.NARUSE        *
      * (03) SYOHIZEI TUIKA     ÍÝº³   : 89/03/09  CSK-M.AOYAMA        *
      * (04) K-275C             ÍÝº³   : 98/04/28  CSK-R.TAKAYANAGI    *
      * (05) H-010  T-6336      ÍÝº³   : 07/09/10  CSK-Y.YAMADA        *
      * (06) K-1549             ÍÝº³   : 14/02/10  CSK-N.EGUCHI        *
      * (07) K-1624             ÍÝº³   : 14/08/12  CSK-Y.MIO           *
      * (08) K-1780             ÍÝº³   : 15/07/31  CSK-Y.YOSHIGA       *
      * (09) K-2163             ÍÝº³   : 19/06/24  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(180).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(180).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(151).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(147).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  WKA-R.
           02 WKA-STNCD                     PIC  X(03).
           02 WKA-SKUCD                     PIC  X(07).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-SOKOK                     PIC  X(01).
           02 WKA-CKUKG                     PIC  X(05).
           02 WKA-DNPNO                     PIC  X(07).
           02 WKA-TORIK.
              03 WKA-TURIK                  PIC  X(01).
              03 WKA-THAIK                  PIC  X(01).
           02 WKA-TOKCD                     PIC  X(07).
           02 WKA-LINNO                     PIC  9(01).
           02 WKA-SYOCD                     PIC  X(09).
           02 WKA-SYOCD-R  REDEFINES WKA-SYOCD.
              03 FILLER                     PIC  X(04).
              03 WKA-SYCD5                  PIC  X(05).
           02 WKA-SURYO                     PIC S9(05) COMP-3.
           02 WKA-TANKA                     PIC S9(08) COMP-3.
           02 WKA-NETAN                     PIC S9(08) COMP-3.
           02 WKA-HYMD                      PIC  9(08).
           02 WKA-SYOME                     PIC  X(45).
           02 WKA-HDNPN                     PIC  X(07).
           02 WKA-HDNPL                     PIC  9(01).
           02 WKA-HDNPD                     PIC  9(08).
           02 WKA-HDNPT                     PIC  X(02).
           02 WKA-NSKBN                     PIC  X(02).
           02 WKA-TSFKB                     PIC  X(01).
           02 WKA-THKBN                     PIC  X(01).
           02 WKA-ADNPN                     PIC  X(07).
           02 WKA-ADNPL                     PIC  9(01).
           02 WKA-ADNPD                     PIC  9(08).
           02 WKA-ADNPT                     PIC  X(02).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-URYMD                     PIC  9(08).
           02 WKA-SHIMM                     PIC  X(08).
           02 WKA-SHZEI                     PIC S9(09) COMP-3.
           02 WKA-SHIMC                     PIC  9(04).
           02 WKA-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKA-DSHZFG                    PIC  X(01).
           02 WKA-KGZKB                     PIC  X(01).
       01  WKB-R.
           02 WKB-STNCD                     PIC  X(03).
           02 WKB-SKUCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-SOKOK                     PIC  X(01).
           02 WKB-CKUKG                     PIC  X(05).
           02 WKB-DNPNO                     PIC  X(07).
           02 WKB-TORIK.
              03 WKB-TURIK                  PIC  X(01).
              03 WKB-THAIK                  PIC  X(01).
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-LINNO                     PIC  9(01).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-SYOCD-R  REDEFINES WKB-SYOCD.
              03 FILLER                     PIC  X(04).
              03 WKB-SYCD5                  PIC  X(05).
           02 WKB-SURYO                     PIC S9(05) COMP-3.
           02 WKB-TANKA                     PIC S9(08) COMP-3.
           02 WKB-NETAN                     PIC S9(08) COMP-3.
           02 WKB-HYMD                      PIC  9(08).
           02 WKB-SYOME                     PIC  X(45).
           02 WKB-HDNPN                     PIC  X(07).
           02 WKB-HDNPL                     PIC  9(01).
           02 WKB-HDNPD                     PIC  9(08).
           02 WKB-HDNPT                     PIC  X(02).
           02 WKB-NSKBN                     PIC  X(02).
           02 WKB-TSFKB                     PIC  X(01).
           02 WKB-THKBN                     PIC  X(01).
           02 WKB-ADNPN                     PIC  X(07).
           02 WKB-ADNPL                     PIC  9(01).
           02 WKB-ADNPD                     PIC  9(08).
           02 WKB-ADNPT                     PIC  X(02).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-URYMD                     PIC  9(08).
           02 WKB-SHIMM                     PIC  X(08).
           02 WKB-SHZEI                     PIC S9(09) COMP-3.
           02 WKB-SHIMC                     PIC  9(04).
           02 WKB-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKB-DSHZFG                    PIC  X(01).
           02 WKB-KGZKB                     PIC  X(01).
       01  WKC-R.
           02 WKC-STNCD                     PIC  X(03).
           02 WKC-SKUCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-SOKOK                     PIC  X(01).
           02 WKC-CKUKG                     PIC  X(05).
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-NSKBN                     PIC  X(02).
           02 WKC-DNPH1.
              03 WKC-DNYY1                  PIC  9(04).
              03 WKC-DNMM1                  PIC  9(02).
              03 WKC-DNDD1                  PIC  9(02).
           02 WKC-DNPN1                     PIC  X(07).
           02 WKC-DNPNL                     PIC  9(01).
           02 WKC-TORIK.
              03 WKC-TURIK                  PIC  X(01).
              03 WKC-THAIK                  PIC  X(01).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-SYOME                     PIC  X(45).
           02 WKC-SURYO                     PIC S9(05) COMP-3.
           02 WKC-TANKA                     PIC S9(08) COMP-3.
           02 WKC-KINGK                     PIC S9(09) COMP-3.
           02 WKC-DNPH2                     PIC  9(08).
           02 WKC-DNPN2                     PIC  X(07).
           02 WKC-THKBN                     PIC  X(01).
           02 WKC-BNKKB                     PIC  X(01).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-SHIMM                     PIC  X(08).
           02 WKC-SHZEI                     PIC S9(09) COMP-3.
           02 WKC-SHIMC                     PIC  9(04).
           02 WKC-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKC-DSHZFG                    PIC  X(01).
           02 WKC-KGZKB                     PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY1.
              03 WIN1-SKUCD                 PIC  X(07).
              03 WIN1-SNSCD                 PIC  X(02).
              03 WIN1-TOKCD                 PIC  X(07).
              03 WIN1-ADNPN                 PIC  X(07).
              03 WIN1-ADNPL                 PIC  9(01).
           02 WIN2-KEY1.
              03 WIN2-SKUCD                 PIC  X(07).
              03 WIN2-SNSCD                 PIC  X(02).
              03 WIN2-TOKCD                 PIC  X(07).
              03 WIN2-ADNPN                 PIC  X(07).
              03 WIN2-ADNPL                 PIC  9(01).
       01  SAVE-AREA.
           02 WSV1-KEY1.
              03 WSV1-SKUCD                 PIC  X(07).
              03 WSV1-SNSCD                 PIC  X(02).
              03 WSV1-TOKCD                 PIC  X(07).
              03 WSV1-ADNPN                 PIC  X(07).
              03 WSV1-ADNPL                 PIC  9(01).
           02 WSV2-KEY1.
              03 WSV2-SKUCD                 PIC  X(07).
              03 WSV2-SNSCD                 PIC  X(02).
              03 WSV2-TOKCD                 PIC  X(07).
              03 WSV2-ADNPN                 PIC  X(07).
              03 WSV2-ADNPL                 PIC  9(01).
       01  EDIT-AREA.
           02 WED1-WKD.
              03 WED1-WKD-R                 PIC  X(143).
              03 WED1-ZEIRT                 PIC S9(02)V9 COMP-3.
              03 WED1-DSHZFG                PIC  X(01).
              03 WED1-KGZKB                 PIC  X(01).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY1 = HIGH-VALUE AND
                          WIN2-KEY1 = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKAFILE
                       WKBFILE
                OUTPUT WKCFILE
                       WKDFILE.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY1
                   GO TO M11-EXIT.
           MOVE WKA-SKUCD TO WIN1-SKUCD.
           MOVE WKA-SNSCD TO WIN1-SNSCD.
           MOVE WKA-TOKCD TO WIN1-TOKCD.
           MOVE WKA-ADNPN TO WIN1-ADNPN.
           MOVE WKA-ADNPL TO WIN1-ADNPL.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKBFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKB.
           READ WKBFILE INTO WKB-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY1
                   GO TO M12-EXIT.
           MOVE WKB-SKUCD TO WIN2-SKUCD.
           MOVE WKB-SNSCD TO WIN2-SNSCD.
           MOVE WKB-TOKCD TO WIN2-TOKCD.
           MOVE WKB-DNPNO TO WIN2-ADNPN.
           MOVE WKB-LINNO TO WIN2-ADNPL.
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
                 PERFORM M22-WKAONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKBONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M11-READ-WKA THRU M11-EXIT.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKAONL.
           MOVE WIN1-KEY1 TO WSV1-KEY1.
           PERFORM M221-WKC-WRITE1 THRU M221-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 :   WKCFILE  WRITE                                   *
      *----------------------------------------------------------------*
       M221-WKC-WRITE1.
           MOVE SPACE      TO WKC-R.
           MOVE ZERO       TO WKC-SHZEI.
           MOVE WKA-STNCD  TO WKC-STNCD.
           MOVE WKA-SKUCD  TO WKC-SKUCD.
           MOVE WKA-SNSCD  TO WKC-SNSCD.
           MOVE WKA-SOKOK  TO WKC-SOKOK.
           MOVE WKA-CKUKG  TO WKC-CKUKG.
           MOVE WKA-TOKCD  TO WKC-TOKCD.
           MOVE WKA-NSKBN  TO WKC-NSKBN.
           MOVE WKA-HYMD   TO WKC-DNPH1.
           MOVE WKA-DNPNO  TO WKC-DNPN1.
           MOVE WKA-LINNO  TO WKC-DNPNL.
           MOVE WKA-TORIK  TO WKC-TORIK.
           MOVE WKA-SYOCD  TO WKC-SYOCD.
           MOVE WKA-SYOME  TO WKC-SYOME.
           MOVE WKA-THKBN  TO WKC-THKBN.
           MOVE '0'        TO WKC-BNKKB.
           IF WKA-THAIK = '5' OR '6' OR '7' OR '8'
              MOVE '1'        TO WKC-BNKKB.
           MOVE WKA-KETKB  TO WKC-KETKB.
           MOVE WKA-SHIMM  TO WKC-SHIMM.
           PERFORM M2211-WKA-EDIT THRU M2211-EXIT.
           MOVE WKA-SHZEI  TO WKC-SHZEI.
           MOVE WKC-R      TO WED1-WKD.
           MOVE WKA-ZEIRT  TO WED1-ZEIRT.
           MOVE WKA-DSHZFG TO WED1-DSHZFG.
           MOVE WKA-KGZKB  TO WED1-KGZKB.
           MOVE WKA-SHIMC  TO WKC-SHIMC.
           MOVE WKA-ZEIRT  TO WKC-ZEIRT.
           MOVE WKA-DSHZFG TO WKC-DSHZFG.
           MOVE WKA-KGZKB  TO WKC-KGZKB.
           IF WKA-TURIK = '8'
              GO TO M221-L10.
           IF WKA-THAIK = '4'
              WRITE WKD-REC FROM WED1-WKD
           ELSE
              WRITE WKC-REC FROM WKC-R.
       M221-L10.
           PERFORM M11-READ-WKA THRU M11-EXIT.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1.1 :  SURYO TANKA KINGK DNPH DNPN  EDIT               *
      *----------------------------------------------------------------*
       M2211-WKA-EDIT.
           MOVE ZERO      TO WKC-SURYO.
           MOVE ZERO      TO WKC-TANKA.
           MOVE ZERO      TO WKC-KINGK.
           MOVE ZERO      TO WKC-DNPH2.
           MOVE SPACE     TO WKC-DNPN2.
           IF WKA-THKBN = 'A'
              MOVE WKA-ADNPD TO WKC-DNPH2
              MOVE WKA-ADNPN TO WKC-DNPN2.
           IF WKA-TORIK = '11' OR '12' OR '21' OR '14'
              MOVE WKA-SURYO TO WKC-SURYO
              MOVE WKA-TANKA TO WKC-TANKA
              COMPUTE WKC-KINGK = WKA-TANKA * WKA-SURYO
              GO TO M2211-EXIT.
           IF WKA-TORIK = '15' OR '17'
              MOVE WKA-SURYO TO WKC-SURYO
              MOVE WKA-NETAN TO WKC-KINGK
              GO TO M2211-EXIT.
           IF WKA-TORIK = '16' OR '18'
              MOVE WKA-NETAN TO WKC-KINGK
              GO TO M2211-EXIT.
           IF WKA-TORIK = '31' OR '32' OR '34'
              COMPUTE WKC-SURYO = WKA-SURYO * (-1)
              MOVE WKA-TANKA TO WKC-TANKA
              COMPUTE WKC-KINGK = WKA-TANKA * WKC-SURYO
              IF WKA-THKBN NOT = 'A'
                 MOVE WKA-URYMD TO WKC-DNPH2
                 GO TO M2211-EXIT
              ELSE
                 GO TO M2211-EXIT.
           IF WKA-TORIK = '35' OR '37'
              COMPUTE WKC-SURYO = WKA-SURYO * (-1)
              COMPUTE WKC-KINGK = WKA-NETAN * (-1)
              IF WKA-THKBN NOT = 'A'
                 MOVE WKA-URYMD TO WKC-DNPH2
                 GO TO M2211-EXIT
              ELSE
                 GO TO M2211-EXIT.
           IF WKA-TORIK = '36' OR '38'
              COMPUTE WKC-KINGK = WKA-NETAN * (-1)
              IF WKA-THKBN NOT = 'A'
                 MOVE WKA-URYMD TO WKC-DNPH2
                 GO TO M2211-EXIT
              ELSE
                 GO TO M2211-EXIT.
           IF WKA-SYCD5 = '88888'
              IF WKA-TURIK = '4'
                 MOVE WKA-NETAN TO WKC-KINGK
                 GO TO M2211-EXIT.
           IF WKA-SYCD5 = '88888'
              IF WKA-TURIK = '5'
                 COMPUTE WKC-KINGK = WKA-NETAN * (-1)
                 GO TO M2211-EXIT.
           IF WKA-SYCD5 = '88888'
              IF WKA-TURIK = '6'
                 MOVE WKA-NETAN TO WKC-KINGK
                 GO TO M2211-EXIT.
           IF WKA-SYCD5 = '88888'
              IF WKA-TURIK = '7'
                 COMPUTE WKC-KINGK = WKA-NETAN * (-1)
                 GO TO M2211-EXIT.
           IF WKA-SYCD5 = '88889'
              IF WKA-TURIK = '4'
                 MOVE WKA-NETAN TO WKC-KINGK
                 GO TO M2211-EXIT.
           IF WKA-SYCD5 = '88889'
              IF WKA-TURIK = '5'
                 COMPUTE WKC-KINGK = WKA-NETAN * (-1)
                 GO TO M2211-EXIT.
           IF WKA-TURIK = '4'
              MOVE WKA-SURYO TO WKC-SURYO
              MOVE WKA-NETAN TO WKC-TANKA
              COMPUTE WKC-KINGK = WKA-NETAN * WKA-SURYO
              IF WKA-THKBN NOT = 'A'
                 MOVE WKA-HDNPD TO WKC-DNPH2
                 MOVE WKA-HDNPN TO WKC-DNPN2
                 GO TO M2211-EXIT
              ELSE
                 GO TO M2211-EXIT.
           IF WKA-TURIK = '5'
              MOVE WKA-SURYO TO WKC-SURYO
              COMPUTE WKC-TANKA = WKA-NETAN * (-1)
              COMPUTE WKC-KINGK = WKC-TANKA * WKA-SURYO
              IF WKA-THKBN NOT = 'A'
                 MOVE WKA-HDNPD TO WKC-DNPH2
                 MOVE WKA-HDNPN TO WKC-DNPN2
                 GO TO M2211-EXIT
              ELSE
                 GO TO M2211-EXIT.
           IF WKA-TURIK = '6'
              MOVE WKA-SURYO TO WKC-SURYO
              MOVE WKA-NETAN TO WKC-TANKA
              COMPUTE WKC-KINGK = WKA-NETAN * WKA-SURYO
              IF WKA-THKBN NOT = 'A'
                 MOVE WKA-HDNPD TO WKC-DNPH2
                 MOVE WKA-HDNPN TO WKC-DNPN2
                 GO TO M2211-EXIT
              ELSE
                 GO TO M2211-EXIT.
           MOVE WKA-SURYO TO WKC-SURYO.
           COMPUTE WKC-TANKA = WKA-NETAN * (-1).
           COMPUTE WKC-KINGK = WKC-TANKA * WKA-SURYO.
           IF WKA-THKBN NOT = 'A'
              MOVE WKA-HDNPD TO WKC-DNPH2
              MOVE WKA-HDNPN TO WKC-DNPN2.
       M2211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKBFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKBONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M231-WKC-WRITE2 THRU M231-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 :   WKCFILE  WRITE                                   *
      *----------------------------------------------------------------*
       M231-WKC-WRITE2.
           MOVE SPACE      TO WKC-R.
           MOVE ZERO       TO WKC-SHZEI.
           MOVE WKB-STNCD  TO WKC-STNCD.
           MOVE WKB-SKUCD  TO WKC-SKUCD.
           MOVE WKB-SNSCD  TO WKC-SNSCD.
           MOVE WKB-SOKOK  TO WKC-SOKOK.
           MOVE WKB-CKUKG  TO WKC-CKUKG.
           MOVE WKB-TOKCD  TO WKC-TOKCD.
           MOVE WKB-NSKBN  TO WKC-NSKBN.
           MOVE WKB-HYMD   TO WKC-DNPH1.
           MOVE WKB-DNPNO  TO WKC-DNPN1.
           MOVE WKB-LINNO  TO WKC-DNPNL.
           MOVE WKB-TORIK  TO WKC-TORIK.
           MOVE WKB-SYOCD  TO WKC-SYOCD.
           MOVE WKB-SYOME  TO WKC-SYOME.
           MOVE WKB-THKBN  TO WKC-THKBN.
           MOVE '0'        TO WKC-BNKKB.
           IF WKB-THAIK = '5' OR '6' OR '7' OR '8'
              MOVE '1'        TO WKC-BNKKB.
           MOVE WKB-KETKB  TO WKC-KETKB.
           MOVE WKB-SHIMM  TO WKC-SHIMM.
           PERFORM M2311-WKB-EDIT THRU M2311-EXIT.
           MOVE WKB-SHZEI  TO WKC-SHZEI.
           MOVE WKC-R      TO WED1-WKD.
           MOVE WKB-ZEIRT  TO WED1-ZEIRT.
           MOVE WKB-DSHZFG TO WED1-DSHZFG.
           MOVE WKB-KGZKB  TO WED1-KGZKB.
           MOVE WKB-SHIMC  TO WKC-SHIMC.
           MOVE WKB-ZEIRT  TO WKC-ZEIRT.
           MOVE WKB-DSHZFG TO WKC-DSHZFG.
           MOVE WKB-KGZKB  TO WKC-KGZKB.
           IF WKB-TURIK = '8'
              GO TO M231-L10.
           IF WKB-THAIK = '4'
              WRITE WKD-REC FROM WED1-WKD
           ELSE
              WRITE WKC-REC FROM WKC-R.
       M231-L10.
           PERFORM M12-READ-WKB THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1 :  SURYO TANKA KINGK DNPH DNPN  EDIT               *
      *----------------------------------------------------------------*
       M2311-WKB-EDIT.
           MOVE ZERO      TO WKC-SURYO.
           MOVE ZERO      TO WKC-TANKA.
           MOVE ZERO      TO WKC-KINGK.
           MOVE ZERO      TO WKC-DNPH2.
           MOVE SPACE     TO WKC-DNPN2.
           IF WKB-THKBN = 'A'
              MOVE WKB-ADNPD TO WKC-DNPH2
              MOVE WKB-ADNPN TO WKC-DNPN2.
           IF WKB-TORIK = '11' OR '12' OR '21' OR '14'
              MOVE WKB-SURYO TO WKC-SURYO
              MOVE WKB-TANKA TO WKC-TANKA
              COMPUTE WKC-KINGK = WKB-TANKA * WKB-SURYO
              GO TO M2311-EXIT.
           IF WKB-TORIK = '15' OR '17'
              MOVE WKB-SURYO TO WKC-SURYO
              MOVE WKB-NETAN TO WKC-KINGK
              GO TO M2311-EXIT.
           IF WKB-TORIK = '16' OR '18'
              MOVE WKB-NETAN TO WKC-KINGK
              GO TO M2311-EXIT.
           IF WKB-TORIK = '31' OR '32' OR '34'
              COMPUTE WKC-SURYO = WKB-SURYO * (-1)
              MOVE WKB-TANKA TO WKC-TANKA
              COMPUTE WKC-KINGK = WKB-TANKA * WKC-SURYO
              IF WKB-THKBN NOT = 'A'
                 MOVE WKB-URYMD TO WKC-DNPH2
                 GO TO M2311-EXIT
              ELSE
                 GO TO M2311-EXIT.
           IF WKB-TORIK = '35' OR '37'
              COMPUTE WKC-SURYO = WKB-SURYO * (-1)
              COMPUTE WKC-KINGK = WKB-NETAN * (-1)
              IF WKB-THKBN NOT = 'A'
                 MOVE WKB-URYMD TO WKC-DNPH2
                 GO TO M2311-EXIT
              ELSE
                 GO TO M2311-EXIT.
           IF WKB-TORIK = '36' OR '38'
              COMPUTE WKC-KINGK = WKB-NETAN * (-1)
              IF WKB-THKBN NOT = 'A'
                 MOVE WKB-URYMD TO WKC-DNPH2
                 GO TO M2311-EXIT
              ELSE
                 GO TO M2311-EXIT.
           IF WKB-SYCD5 = '88888'
              IF WKB-TURIK = '4'
                 MOVE WKB-NETAN TO WKC-KINGK
                 GO TO M2311-EXIT.
           IF WKB-SYCD5 = '88888'
              IF WKB-TURIK = '5'
                 COMPUTE WKC-KINGK = WKB-NETAN * (-1)
                 GO TO M2311-EXIT.
           IF WKB-SYCD5 = '88888'
              IF WKB-TURIK = '6'
                 MOVE WKB-NETAN TO WKC-KINGK
                 GO TO M2311-EXIT.
           IF WKB-SYCD5 = '88888'
              IF WKB-TURIK = '7'
                 COMPUTE WKC-KINGK = WKB-NETAN * (-1)
                 GO TO M2311-EXIT.
           IF WKB-SYCD5 = '88889'
              IF WKB-TURIK = '4'
                 MOVE WKB-NETAN TO WKC-KINGK
                 GO TO M2311-EXIT.
           IF WKB-SYCD5 = '88889'
              IF WKB-TURIK = '5'
                 COMPUTE WKC-KINGK = WKB-NETAN * (-1)
                 GO TO M2311-EXIT.
           IF WKB-TURIK = '4'
              MOVE WKB-SURYO TO WKC-SURYO
              MOVE WKB-NETAN TO WKC-TANKA
              COMPUTE WKC-KINGK = WKB-NETAN * WKB-SURYO
              IF WKB-THKBN NOT = 'A'
                 MOVE WKB-HDNPD TO WKC-DNPH2
                 MOVE WKB-HDNPN TO WKC-DNPN2
                 GO TO M2311-EXIT
              ELSE
                 GO TO M2311-EXIT.
           IF WKB-TURIK = '5'
              MOVE WKB-SURYO TO WKC-SURYO
              COMPUTE WKC-TANKA = WKB-NETAN * (-1)
              COMPUTE WKC-KINGK = WKC-TANKA * WKB-SURYO
              IF WKB-THKBN NOT = 'A'
                 MOVE WKB-HDNPD TO WKC-DNPH2
                 MOVE WKB-HDNPN TO WKC-DNPN2
                 GO TO M2311-EXIT
              ELSE
                 GO TO M2311-EXIT.
           IF WKB-TURIK = '6'
              MOVE WKB-SURYO TO WKC-SURYO
              MOVE WKB-NETAN TO WKC-TANKA
              COMPUTE WKC-KINGK = WKB-NETAN * WKB-SURYO
              IF WKB-THKBN NOT = 'A'
                 MOVE WKB-HDNPD TO WKC-DNPH2
                 MOVE WKB-HDNPN TO WKC-DNPN2
                 GO TO M2311-EXIT
              ELSE
                 GO TO M2311-EXIT.
           MOVE WKB-SURYO TO WKC-SURYO.
           COMPUTE WKC-TANKA = WKB-NETAN * (-1).
           COMPUTE WKC-KINGK = WKC-TANKA * WKB-SURYO.
           IF WKB-THKBN NOT = 'A'
              MOVE WKB-HDNPD TO WKC-DNPH2
              MOVE WKB-HDNPN TO WKC-DNPN2.
       M2311-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
