./ ADD NAME=VBKA1550  01.01 20150731 20190625 132733 00513 00509 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1550'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : KAKAXX  SYSTEM                *
      *              <> PG.OUTLINE     : TOUGOU TOUKUISAKI NO          *
      *                                  SYOUKEI GYO SAKUSEI           *
      *                 * INPUT FILE   : TOUGETU SEIKYU TORIHIKI       *
      *                                  MEISAI DATA      (DWKC)       *
      *                 * INPUT FILE   : TOKUISAKI MASTER (DWKA)       *
      *                 * INPUT FILE   : NOUSEI MASTER    (NSMFILE)    *
      *                 * INPUT FILE   : TOKUISAKI MEISYOU FILE        *
      *                                                   (SFMFILE)    *
      *                 * OUTPUT FILE  : SEIKYU SYO MEISAI GYO DATA    *
      *                                                   (DWKD)       *
      *              <> AUTHOR         : CSK-K.OTSUKA                  *
      *              <> DATE           : 86/08/25                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) INPUTFILE RECKB     Â²¶   : 86/09/09  CSK-K.OTSUKA        *
      * (02) SYOHIZEI TUIKA      Â²¶   : 89/03/13  CSK-S.MUNEISHI      *
      * (03) KAKUJYU 105               : 89/04/26  CSK-A.ONITSUKA      *
      * (04) YOUKEN  K-263             : 90/11/01  SUZ-N.MARUYAMA      *
      * (05) YOUKEN HENKOU K-062       : 94/05/26  SUZ-Y.GOTOH         *
      * (06) K-275C              ÍÝº³  : 98/04/28  CSK-R.TAKAYANAGI    *
      * (07) K-407               ÍÝº³  : 99/12/16  CSK-Y.DOYAMA        *
      * (08) K-568               ÍÝº³  : 03/07/18  CSK-M.YOSHIDA       *
      * (09) K-1228              ÍÝº³  : 10/06/23  CSK-M.KODAMA        *
      * (10) K-1549              ÍÝº³  : 14/02/12  CSK-N.EGUCHI        *
      * (11) K-1624              ÍÝº³  : 14/08/13  CSK-Y.MIO           *
      * (12) K-1780              ÍÝº³  : 15/07/31  CSK-Y.YOSHIGA       *
      * (13) K-2163              ÍÝº³  : 19/06/25  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT NSMFILE ASSIGN    TO NSMFILE
                          ORGANIZATION INDEXED
                          ACCESS MODE   RANDOM
                          RECORD KEY    KEY-NSM
                          FILE  STATUS  S-CODE.
           SELECT SFMFILE ASSIGN    TO SFMFILE
                          ORGANIZATION INDEXED
                          ACCESS MODE   RANDOM
                          RECORD KEY    KEY-SFM
                          FILE  STATUS  S-CODE.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(252).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(300).
       FD  NSMFILE
           LABEL RECORD STANDARD.
       01  NSM-REC.
           02 KEY-NSM.
              03 KEY-NSM1                   PIC  X(07).
              03 KEY-NSM2                   PIC  X(02).
           02 FILLER                        PIC  X(91).
       FD  SFMFILE
           LABEL RECORD STANDARD.
       01  SFM-REC.
           02 KEY-SFM                       PIC  X(07).
           02 FILLER                        PIC  X(203).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(250).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  P56-PARM.
           COPY VPARB56 SUPPRESS.
       01  P41-PARM.
           COPY VPARB41 SUPPRESS.
       01  NSM-R.
           COPY VFILNSM SUPPRESS.
       01  SFM-R.
           COPY VFILSFM SUPPRESS.
       01  WKA-R.
           02  WKA-KEY.
               03  WKA-TOKCD.
                   04  WKA-TKCD1  PIC  X(4).
                   04  WKA-TKCD2  PIC  X(3).
           02  WKA-TOKME       PIC  X(25).
           02  WKA-STNCD.
               03  WKA-STCD1   PIC  X(1).
               03  WKA-STCD2   PIC  X(2).
           02  WKA-CKUKG.
               03  WKA-CKUNO   PIC  X(3).
               03  WKA-CKUKA   PIC  X(1).
               03  WKA-CKUCK   PIC  X(1).
           02  WKA-CKUR1     REDEFINES  WKA-CKUKG.
               03  WKA-CKR11   PIC  X(4).
               03  FILLER      PIC  X(1).
           02  WKA-CKUR2     REDEFINES  WKA-CKUR1.
               03  FILLER      PIC  X(3).
               03  WKA-CKR22   PIC  X(2).
           02  WKA-SNRKM.
               03  WKA-SRKM    PIC  X(1)     OCCURS 35.
           02  WKA-SEYOS       PIC  9(1).
           02  WKA-SEBUS       PIC  9(1).
           02  WKA-YUTOS       PIC  9(1).
           02  WKA-NOSEK       PIC  9(1).
           02  WKA-SIHENG      PIC  9(8).
           02  WKA-LSIMEB      PIC  X(2).
           02  WKA-BOGIE       PIC S9(7)     COMP-3.
           02  WKA-CYUSI       PIC  9(1).
           02  WKA-BRIC        PIC  X(5).
           02  WKA-LET         PIC  X(1).
           02  WKA-MANG        PIC  X(2).
           02  WKA-SKNRK       PIC  X(1).
           02  WKA-SIMEB       PIC  9(2).
           02  WKA-HNRKB       PIC  X(1).
           02  WKA-SKUCD.
               03  WKA-SUCD1   PIC  X(4).
               03  WKA-SUCD2   PIC  X(3).
           02  WKA-LSEBUS      PIC  X(1).
           02  WKA-MOTCP       PIC  9(1).
           02  WKA-INKAN       PIC  9(1).
           02  WKA-KEIYK       PIC  9(1).
           02  WKA-TKONU       PIC S9(6)     COMP-3.
           02  WKA-UMPFL       PIC  X(1).
           02  WKA-MKHNO.
               03  WKA-MAKHN   PIC  X(1)     OCCURS 100.
           02  WKA-HENYM       PIC  9(6).
           02  WKA-QCKUK.
               03  WKA-QCKNO   PIC  X(3).
               03  WKA-QCK22.
                   04  WKA-QCKKA   PIC  X(1).
                   04  WKA-QCKCK   PIC  X(1).
           02  WKA-TOKKA       PIC  X(30).
           02  WKA-QSTCD       PIC  X(3).
           02  WKA-BED         PIC  X(5).
           02  WKA-ZASOS       PIC  X(1).
           02  WKA-OYAKO       PIC  X(1).
           02  WKA-OUMSC       PIC  X(01).
           02  WKA-OUMSE       PIC  X(01).
           02  WKA-OGHPF       PIC  X(01).
           02  WKA-OGKKS       PIC  X(01).
           02  WKA-OGKKM       PIC  X(01).
           02  FILLER          PIC  X(22).
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
           02 WKC-RECKB                     PIC  X(02).
           02 WKC-DNPN1                     PIC  X(07).
           02 WKC-DNPNL                     PIC  9(01).
           02 WKC-TORIK.
              03 WKC-TURIK                  PIC  X(01).
              03 WKC-THAIK                  PIC  X(01).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-SYOME.
              03 WKC-SYOMH                  PIC  X(50).
              03 WKC-SYOMKY                 PIC  X(40).
           02 WKC-SURYO                     PIC S9(05) COMP-3.
           02 WKC-TANKA                     PIC S9(08) COMP-3.
           02 WKC-KINGK                     PIC S9(09) COMP-3.
           02 WKC-DNKIN                     PIC S9(11) COMP-3.
           02 WKC-DNPH2                     PIC  9(08).
           02 WKC-DNPN2                     PIC  X(07).
           02 WKC-THKBN                     PIC  X(01).
           02 WKC-BNKKB                     PIC  X(01).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-TORKJ                     PIC  X(08).
           02 WKC-SYOME-X.
              03 WKC-SYOMH-X                PIC  X(25).
              03 WKC-SYOMKY-X               PIC  X(20).
           02 WKC-SHZEI                     PIC S9(11) COMP-3.
           02 WKC-ZEGAK                     PIC S9(09) COMP-3.
           02 WKC-DNGYO                     PIC  9(01).
           02 WKC-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKC-DSHZFG                    PIC  X(01).
           02 WKC-KGZKB                     PIC  X(01).
       01  WKD-R.
           02 WKD-STNCD                     PIC  X(03).
           02 WKD-SKUCD                     PIC  X(07).
           02 WKD-SNSCD                     PIC  X(02).
           02 WKD-TOKCD                     PIC  X(07).
           02 WKD-NSKBN                     PIC  X(02).
           02 WKD-DATA-X                    PIC  X(229).
           02 WKD-DATA1 REDEFINES WKD-DATA-X.
              03 WKD-RECK1                  PIC  X(02).
              03 WKD-DNPH1.
                 04 WKD-DNYY1               PIC  9(04).
                 04 WKD-DNMM1               PIC  9(02).
                 04 WKD-DNDD1               PIC  9(02).
              03 WKD-DNPN1                  PIC  X(07).
              03 WKD-DNPNL                  PIC  9(01).
              03 WKD-TORIK                  PIC  X(02).
              03 WKD-SYOCD                  PIC  X(09).
              03 WKD-SYOME.
                 04 WKD-SYOMH               PIC  X(50).
                 04 WKD-SYOMKY              PIC  X(40).
              03 WKD-SURYO                  PIC S9(05) COMP-3.
              03 WKD-TANKA                  PIC S9(08) COMP-3.
              03 WKD-KINGK                  PIC S9(09) COMP-3.
              03 WKD-DNKIN                  PIC S9(11) COMP-3.
              03 WKD-DNPH2                  PIC  9(08).
              03 WKD-DNPN2                  PIC  X(07).
              03 WKD-THKBN                  PIC  X(01).
              03 WKD-BNKKB                  PIC  X(01).
              03 WKD-TORKJ                  PIC  X(08).
              03 WKD-SYOME-X.
                 04 WKD-SYOMH-X             PIC  X(25).
                 04 WKD-SYOMKY-X            PIC  X(20).
              03 WKD-SHZEI                  PIC S9(11) COMP-3.
              03 WKD-ZEGAK                  PIC S9(09) COMP-3.
              03 WKD-DNGYO                  PIC  9(01).
              03 WKD-ZEIRT                  PIC S9(02)V9 COMP-3.
              03 WKD-DSHZFG                 PIC  X(01).
              03 WKD-KGZKB                  PIC  X(01).
              03 FILLER                     PIC  X(05).
           02 WKD-DATA2 REDEFINES WKD-DATA-X.
              03 WKD-RECK2                  PIC  X(02).
              03 WKD-STTLA                  PIC  X(50).
              03 WKD-STTLB                  PIC  X(50).
              03 WKD-TNTLA                  PIC S9(09) COMP-3.
              03 WKD-TNTLB                  PIC S9(09) COMP-3.
              03 FILLER                     PIC  X(117).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEYA.
                 04 WIN1-SKUCD              PIC  X(07).
              03 WIN1-SNSCD                 PIC  X(02).
              03 WIN1-STNCD                 PIC  X(03).
              03 WIN1-TOKCD                 PIC  X(07).
              03 WIN1-NSKBN                 PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-KEYA.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEYA.
                 04 WSV1-SKUCD              PIC  X(07).
              03 WSV1-SNSCD                 PIC  X(02).
              03 WSV1-STNCD                 PIC  X(03).
              03 WSV1-TOKCD.
                 04 WSV1-TKCD1              PIC  X(04).
                 04 WSV1-TKCD2              PIC  X(03).
              03 WSV1-NSKBN                 PIC  X(02).
       01  EDIT-AREA.
           02 WED1-MIDSI.
              03 WED1-MID04                 PIC  X(04).
              03 WED1-MID46                 PIC  X(46).
           02 WED1-SANKAK                   PIC  X(04).
           02 WED1-KEI                      PIC  X(06).
       01  TOTAL-AREA.
           02 WTT1-KINGK                    PIC S9(11)  COMP-3.
           02 WTT2-KINGK                    PIC S9(11)  COMP-3.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL ( WIN1-KEY = HIGH-VALUE ).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKCFILE
                       WKAFILE
                       NSMFILE
                       SFMFILE
                OUTPUT WKDFILE.
           MOVE '3'        TO P41-HNBCD.
           MOVE '6AB14040' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA   TO WED1-SANKAK.
           MOVE '424D45C9425D' TO P41-DATA.
           CALL 'VSBA0410' USING P41-PARM.
           MOVE P41-DATA   TO WED1-KEI.
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
           MOVE WKC-SKUCD TO WIN1-SKUCD.
           MOVE WKC-SNSCD TO WIN1-SNSCD.
           MOVE WKC-STNCD TO WIN1-STNCD.
           MOVE WKC-TOKCD TO WIN1-TOKCD.
           MOVE WKC-NSKBN TO WIN1-NSKBN.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEYA
                   GO TO M12-EXIT.
           MOVE WKA-TOKCD TO WIN2-TOKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (BREAK)                                    *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-SKUCD TO WSV1-SKUCD.
           MOVE WIN1-SNSCD TO WSV1-SNSCD.
           MOVE WIN1-STNCD TO WSV1-STNCD.
           MOVE WIN1-TOKCD TO WSV1-TOKCD.
           MOVE WIN1-NSKBN TO WSV1-NSKBN.
           MOVE ZERO       TO WTT1-KINGK.
           MOVE ZERO       TO WTT2-KINGK.
       M2-L01.
           PERFORM M21-KEY-BREAK THRU M21-EXIT
                   UNTIL ( WIN1-KEY NOT = WSV1-KEY ).
       M2-L02.
           PERFORM M22-GOKEI-WRITE THRU M22-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1   : CONTROL-KEY BREAK                                  *
      *----------------------------------------------------------------*
       M21-KEY-BREAK.
           MOVE SPACE      TO WKD-R.
           MOVE WKC-STNCD  TO WKD-STNCD.
           MOVE WKC-SKUCD  TO WKD-SKUCD.
           MOVE WKC-SNSCD  TO WKD-SNSCD.
           MOVE WKC-TOKCD  TO WKD-TOKCD.
           MOVE WKC-NSKBN  TO WKD-NSKBN.
           MOVE WKC-RECKB  TO WKD-RECK1.
           MOVE WKC-DNPH1  TO WKD-DNPH1.
           MOVE WKC-DNPN1  TO WKD-DNPN1.
           MOVE WKC-DNPNL  TO WKD-DNPNL.
           MOVE WKC-TORIK  TO WKD-TORIK.
           MOVE WKC-SYOCD  TO WKD-SYOCD.
           MOVE WKC-SYOME  TO WKD-SYOME.
           MOVE WKC-SURYO  TO WKD-SURYO.
           MOVE WKC-TANKA  TO WKD-TANKA.
           MOVE WKC-KINGK  TO WKD-KINGK.
           MOVE WKC-DNKIN  TO WKD-DNKIN.
           MOVE WKC-DNPH2  TO WKD-DNPH2.
           MOVE WKC-DNPN2  TO WKD-DNPN2.
           MOVE WKC-THKBN  TO WKD-THKBN.
           MOVE WKC-BNKKB  TO WKD-BNKKB.
           MOVE WKC-TORKJ  TO WKD-TORKJ.
           MOVE WKC-SYOME-X TO WKD-SYOME-X.
           MOVE WKC-SHZEI  TO WKD-SHZEI.
           MOVE WKC-ZEGAK  TO WKD-ZEGAK.
           MOVE WKC-DNGYO  TO WKD-DNGYO.
           MOVE WKC-ZEIRT  TO WKD-ZEIRT.
           MOVE WKC-DSHZFG TO WKD-DSHZFG.
           MOVE WKC-KGZKB  TO WKD-KGZKB.
           WRITE WKD-REC FROM WKD-R.
           COMPUTE WTT1-KINGK = WKC-DNKIN + WTT1-KINGK.
           COMPUTE WTT2-KINGK = WKC-SHZEI + WTT2-KINGK.
       M21-L01.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : GOKEI WRITE                                          *
      *----------------------------------------------------------------*
       M22-GOKEI-WRITE.
           IF WSV1-KEYA = WIN2-KEYA
              PERFORM M221-MATCH THRU M221-EXIT
           ELSE
              IF WSV1-KEYA < WIN2-KEYA
                 NEXT SENTENCE
              ELSE
                 PERFORM M222-WKAONL THRU M222-EXIT
                 GO TO M22-GOKEI-WRITE.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : MATCHING                                           *
      *----------------------------------------------------------------*
       M221-MATCH.
           MOVE SPACE      TO WKD-DATA-X.
           MOVE '65'       TO WKD-RECK1.
           IF WKA-OYAKO NOT = '0' OR WKA-NOSEK = '1'
              IF WKA-SEBUS = '0'
                 IF WSV1-NSKBN = SPACE OR '00'
                    PERFORM M2211-WKD-EDIT1 THRU M2211-EXIT
                 ELSE
                    PERFORM M2212-WKD-EDIT2 THRU M2212-EXIT
                 END-IF
                 MOVE WTT1-KINGK TO WKD-TNTLA
                 MOVE WTT2-KINGK TO WKD-TNTLB
                 WRITE WKD-REC FROM WKD-R
              END-IF
           END-IF.
       M221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1.1 : SYOUKEI A B EDIT                                 *
      *----------------------------------------------------------------*
       M2211-WKD-EDIT1.
           PERFORM M22111-READ-SFM THRU M22111-EXIT.
           IF SFM-R = SPACE
              GO TO  M2211-EXIT.
       M2211-L01.
           MOVE SPACE  TO WED1-MIDSI.
           IF SFM-TMSJL = SPACE
              MOVE WED1-SANKAK   TO WED1-MID04
              MOVE SFM-TMSJU     TO P56-KUMKA
              MOVE WED1-KEI      TO P56-KUMKB
              MOVE ZERO          TO P56-SPACE
              CALL 'VSBA0560' USING P56-PARM
              MOVE P56-KUMKR     TO WED1-MID46
              MOVE WED1-MIDSI    TO WKD-STTLB
           ELSE
              MOVE WED1-SANKAK   TO WED1-MID04
              MOVE SFM-TMSJU     TO WED1-MID46
              MOVE WED1-MIDSI    TO WKD-STTLA
              MOVE SPACE         TO WED1-MIDSI
              MOVE SFM-TMSJL     TO P56-KUMKA
              MOVE WED1-KEI      TO P56-KUMKB
              MOVE ZERO          TO P56-SPACE
              CALL 'VSBA0560' USING P56-PARM
              MOVE P56-KUMKR     TO WED1-MID46
              MOVE WED1-MIDSI    TO WKD-STTLB
           END-IF.
       M2211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1.1.1 : READ SFMFILE                                   *
      *----------------------------------------------------------------*
       M22111-READ-SFM.
           MOVE WSV1-TOKCD TO KEY-SFM.
           MOVE 'Z '      TO S-CODE.
           READ SFMFILE INTO SFM-R
                INVALID KEY
                        MOVE SPACE TO SFM-R
                        GO TO M22111-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA1550 SFMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO SFM-R.
       M22111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1.2 : SYOUKEI A B EDIT                                 *
      *----------------------------------------------------------------*
       M2212-WKD-EDIT2.
           PERFORM M22121-READ-NSM THRU M22121-EXIT.
           IF NSM-R = SPACE
              GO TO  M2212-EXIT.
       M2212-L01.
           MOVE SPACE       TO WED1-MIDSI.
           MOVE WED1-SANKAK TO WED1-MID04.
           MOVE WED1-KEI    TO P56-KUMKB.
           MOVE ZERO        TO P56-SPACE.
           CALL 'VSBA0560'  USING P56-PARM.
           MOVE P56-KUMKR   TO WED1-MID46.
           MOVE WED1-MIDSI  TO WKD-STTLB.
       M2212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1.2.1 : NSMFILE READ                                   *
      *----------------------------------------------------------------*
       M22121-READ-NSM.
           MOVE WSV1-TOKCD TO KEY-NSM1.
           MOVE WSV1-NSKBN TO KEY-NSM2.
           READ NSMFILE INTO NSM-R
                INVALID KEY
                   MOVE SPACE TO NSM-R.
           MOVE NSM-NOSKA TO P56-KUMKA.
       M22121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.2 : WKAFILE ONLY                                       *
      *----------------------------------------------------------------*
       M222-WKAONL.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M222-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKCFILE
                 WKAFILE
                 NSMFILE
                 SFMFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    VBKA1550  END                                               *
      *----------------------------------------------------------------*
