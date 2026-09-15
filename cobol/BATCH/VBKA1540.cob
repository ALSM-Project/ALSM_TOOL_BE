./ ADD NAME=VBKA1540  01.01 20150731 20190624 101152 00448 00443 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1540'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      :                               *
      *              <> PG.OUTLINE     : DENPYO KINGAKU NO KEISAN      *
      *                                                                *
      *                 * INPUT FILE   : HONYAKUGO SEIKYU MEISAI DATA  *
      *                                                     (DWKA)     *
      *                 * OUTPUT FILE  : TOUGETU SEIKYU TORIHIKI       *
      *                                   MEISAI DATA       (DWKB)     *
      *                                                                *
      *              <> AUTHOR         : SUZ-A.YAMAMOTO                *
      *              <> DATE           : 86/08/19                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) Ä¸²»·Ò² ÍÝ¼­³¼Þ®³¹Ý Â²¶   : 86/10/15  SUZ-A.Y             *
      * (02) KAISYU-45          ÍÝº³   : 89/03/10  CSK-A.ONITSUKA      *
      * (03) K-062              ÍÝº³   : 94/05/26  SUZ-M.YOSHITA       *
      * (04) K-195              ÍÝº³   : 96/09/12  CSK-K.WATANABE      *
      * (05) K-275C             ÍÝº³   : 98/04/28  CSK-R.TAKAYANAGI    *
      * (06) K-407              ÍÝº³   : 99/12/16  CSK-Y.DOYAMA        *
      * (07) K-557              ÍÝº³   : 02/09/14  CSK-H.YOSHIMOTO     *
      * (08) K-557              ÍÝº³   : 02/09/19  CSK-M.YOSHIDA       *
      * (09) K-1624             ÍÝº³   : 14/08/13  CSK-Y.MIO           *
      * (10) K-1780             ÍÝº³   : 15/07/31  CSK-Y.YOSHIGA       *
      * (10) K-2163             ÍÝº³   : 19/06/24  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                     PIC  X(242).
       FD  WKBFILE RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                     PIC  X(252).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  P41-PARM.
               COPY  VPARB41  SUPPRESS.
       01  WKA-R.
           02 WKA-SITEK                  PIC  X(03).
           02 WKA-SKUCD                  PIC  X(07).
           02 WKA-SKNCD                  PIC  X(02).
           02 WKA-SOKOK                  PIC  X(01).
           02 WKA-CKUKG                  PIC  X(05).
           02 WKA-TOKCD                  PIC  X(07).
           02 WKA-NSKBN                  PIC  X(02).
           02 WKA-DEND1.
              03 WKA-DENYY               PIC  9(04).
              03 WKA-DENMM               PIC  9(02).
              03 WKA-DENDD               PIC  9(02).
           02 WKA-DENN1                  PIC  X(07).
           02 WKA-DENPL                  PIC  9(01).
           02 WKA-TORIK                  PIC  X(02).
           02 WKA-SYOCD                  PIC  X(09).
           02 WKA-SYOME.
              03 WKA-HINME               PIC  X(50).
              03 WKA-KIKYO               PIC  X(40).
           02 WKA-SURYO                  PIC S9(05) COMP-3.
           02 WKA-TANKA                  PIC S9(08) COMP-3.
           02 WKA-KINGK                  PIC S9(09) COMP-3.
           02 WKA-DENPK                  PIC S9(09) COMP-3.
           02 WKA-DEND2                  PIC  9(08).
           02 WKA-DENN2                  PIC  X(07).
           02 WKA-THKBN                  PIC  X(01).
           02 WKA-BUKBN                  PIC  X(01).
           02 WKA-KETKB                  PIC  X(01).
           02 WKA-TOKAN                  PIC  X(08).
           02 WKA-SYOME-X.
              03 WKA-SYHKK               PIC  X(25).
              03 WKA-SYKYKK              PIC  X(20).
           02 WKA-SHZEI                  PIC S9(09) COMP-3.
           02 WKA-ZEIRT                  PIC S9(02)V9 COMP-3.
           02 WKA-DSHZFG                 PIC  X(01).
           02 WKA-KGZKB                  PIC  X(01).
       01  WKB-R.
           02 WKB-SITEK                  PIC  X(03).
           02 WKB-SKUCD                  PIC  X(07).
           02 WKB-SKNCD                  PIC  X(02).
           02 WKB-SOKOK                  PIC  X(01).
           02 WKB-CKUKG                  PIC  X(05).
           02 WKB-TOKCD                  PIC  X(07).
           02 WKB-NSKBN                  PIC  X(02).
           02 WKB-DEND1.
              03 WKB-DENYY               PIC  9(04).
              03 WKB-DENMM               PIC  9(02).
              03 WKB-DENDD               PIC  9(02).
           02 WKB-REKBN                  PIC  X(02).
           02 WKB-DENN1                  PIC  X(07).
           02 WKB-DENPL                  PIC  9(01).
           02 WKB-TORIK                  PIC  X(02).
           02 WKB-SYOCD                  PIC  X(09).
           02 WKB-SYOME.
              03 WKB-HINME               PIC  X(50).
              03 WKB-KIKYO               PIC  X(40).
           02 WKB-SURYO                  PIC S9(05) COMP-3.
           02 WKB-TANKA                  PIC S9(08) COMP-3.
           02 WKB-KINGK                  PIC S9(09) COMP-3.
           02 WKB-DENPK                  PIC S9(11) COMP-3.
           02 WKB-DEND2                  PIC  9(08).
           02 WKB-DENN2                  PIC  X(07).
           02 WKB-THKBN                  PIC  X(01).
           02 WKB-BUKBN                  PIC  X(01).
           02 WKB-KETKB                  PIC  X(01).
           02 WKB-TOKAN                  PIC  X(08).
           02 WKB-SYOME-X.
              03 WKB-SYHKK               PIC  X(25).
              03 WKB-SYKYKK              PIC  X(20).
           02 WKB-SHZEI                  PIC S9(11) COMP-3.
           02 WKB-ZEGAK                  PIC S9(09) COMP-3.
           02 WKB-DNGYO                  PIC  9(01).
           02 WKB-ZEIRT                  PIC S9(02)V9 COMP-3.
           02 WKB-DSHZFG                 PIC  X(01).
           02 WKB-KGZKB                  PIC  X(01).
       01  TABLE-AREA.
           02 TBL-R1                     OCCURS 20.
              03 TBL-SITEK               PIC  X(03).
              03 TBL-SKUCD               PIC  X(07).
              03 TBL-SKNCD               PIC  X(02).
              03 TBL-SOKOK               PIC  X(01).
              03 TBL-CKUKG               PIC  X(05).
              03 TBL-TOKCD               PIC  X(07).
              03 TBL-NSKBN               PIC  X(02).
              03 TBL-DEND1.
                 04 TBL-DENYY            PIC  9(04).
                 04 TBL-DENMM            PIC  9(02).
                 04 TBL-DENDD            PIC  9(02).
              03 TBL-DENN1               PIC  X(07).
              03 TBL-DENPL               PIC  9(01).
              03 TBL-TORIK.
                 04 TBL-TORI1            PIC  X(01).
                 04 TBL-TORI2            PIC  X(01).
              03 TBL-SYOCD.
                 04 TBL-SYOC1            PIC  X(04).
                 04 TBL-SYOC2            PIC  X(05).
              03 TBL-SYOME.
                 04 TBL-HINME            PIC  X(50).
                 04 TBL-KIKYO            PIC  X(40).
              03 TBL-SURYO               PIC S9(05) COMP-3.
              03 TBL-TANKA               PIC S9(08) COMP-3.
              03 TBL-KINGK               PIC S9(09) COMP-3.
              03 TBL-DENPK               PIC S9(09) COMP-3.
              03 TBL-DEND2               PIC  9(08).
              03 TBL-DENN2               PIC  X(07).
              03 TBL-THKBN               PIC  X(01).
              03 TBL-BUKBN               PIC  X(01).
              03 TBL-KETKB               PIC  X(01).
              03 TBL-TOKAN               PIC  X(08).
              03 TBL-SYOME-X.
                 04 TBL-SYHKK            PIC  X(25).
                 04 TBL-SYKYKK           PIC  X(20).
              03 TBL-SHZEI               PIC S9(09) COMP-3.
              03 TBL-ZEIRT               PIC S9(02)V9 COMP-3.
              03 TBL-DSHZFG              PIC  X(01).
              03 TBL-KGZKB               PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-SKUCD              PIC  X(07).
              03 WIN1-SKNCD              PIC  X(02).
              03 WIN1-TOKCD              PIC  X(07).
              03 WIN1-NSKBN              PIC  X(02).
              03 WIN1-DEND1              PIC  9(08).
              03 WIN1-DENN1              PIC  X(07).
              03 WIN1-TORIK              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-SKUCD              PIC  X(07).
              03 WSV1-SKNCD              PIC  X(02).
              03 WSV1-TOKCD              PIC  X(07).
              03 WSV1-NSKBN              PIC  X(02).
              03 WSV1-DEND1              PIC  9(08).
              03 WSV1-DENN1              PIC  X(07).
              03 WSV1-TORIK              PIC  X(02).
           02 WSV1-SW1                   PIC  9(01).
       01  SAVE-KANJI.
           02 WSVK-INDATA.
              03 WSVK-INKJ01             PIC  X(08).
              03 WSVK-INKJ02             PIC  X(08).
              03 WSVK-INKJ03             PIC  X(08).
              03 WSVK-INKJ04             PIC  X(08).
              03 WSVK-INKJ05             PIC  X(08).
              03 WSVK-INKJ06             PIC  X(08).
              03 WSVK-INKJ07             PIC  X(08).
       01  COUNT-AREA.
           02 WCT1-I                     PIC  9(02).
           02 WCT1-J                     PIC  9(02).
           02 WCT1-K                     PIC  9(02).
           02 WCT1-DNGYO                 PIC  9(01).
       01  TOTAL-AREA.
           02 WTT1-ALTOT                 PIC S9(11) COMP-3.
           02 WTT1-SHZEI                 PIC S9(11) COMP-3.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL WIN1-KEY = HIGH-VALUE.
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INIT                                                   *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT   WKAFILE
                OUTPUT  WKBFILE.
       M1-L01.
           MOVE '3'                TO P41-HNBCD.
           MOVE '44854974455E44C3' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ01.
           MOVE '44854974455E44C3' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ02.
           MOVE '404048D645D04040' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ03.
           MOVE '404048D645D04040' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ04.
           MOVE '457B45D0487D464D' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ05.
           MOVE '457B45D0487D46DA' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ06.
           MOVE '475A47DE4FC44577' TO P41-DATA.
           CALL 'VSBA0410'         USING P41-PARM.
           MOVE P41-DATA           TO WSVK-INKJ07.
       M1-L02.
           PERFORM M11-WKA THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : WKAFILE READ                                         *
      *----------------------------------------------------------------*
       M11-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
       M11-L01.
           MOVE WKA-SKUCD TO WIN1-SKUCD.
           MOVE WKA-SKNCD TO WIN1-SKNCD.
           MOVE WKA-TOKCD TO WIN1-TOKCD.
           MOVE WKA-NSKBN TO WIN1-NSKBN.
           MOVE WKA-DEND1 TO WIN1-DEND1.
           MOVE WKA-DENN1 TO WIN1-DENN1.
           MOVE WKA-TORIK TO WIN1-TORIK.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KEY TO WSV1-KEY.
           PERFORM M21-CLR THRU M21-EXIT.
           MOVE ZERO     TO WTT1-ALTOT.
           MOVE ZERO     TO WTT1-SHZEI.
           MOVE ZERO     TO WCT1-DNGYO.
           MOVE ZERO     TO WCT1-I.
       M2-L01.
           PERFORM M22-STORE  THRU M22-EXIT
                   UNTIL WIN1-KEY NOT = WSV1-KEY.
       M2-L02.
           PERFORM M23-HENSHU THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : TABLE-AREA CLEAR                                     *
      *----------------------------------------------------------------*
       M21-CLR.
           MOVE ZERO TO WCT1-J.
       M21-L01.
           ADD 1 TO WCT1-J.
           IF WCT1-J > 20
              GO TO M21-EXIT.
           MOVE SPACE TO TBL-R1 (WCT1-J).
           GO TO M21-L01.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKAFILE-STORE                                        *
      *----------------------------------------------------------------*
       M22-STORE.
           ADD 1      TO WCT1-I.
           IF WCT1-I > 20
              PERFORM M3-FINL THRU M3-EXIT
              DISPLAY 'R VBKA1540 TABLE OVER !' UPON CONSOLE
              CALL 'VSBABEND'.
           MOVE WKA-R TO TBL-R1 (WCT1-I).
           COMPUTE WTT1-ALTOT = WTT1-ALTOT + WKA-KINGK.
           COMPUTE WTT1-SHZEI = WTT1-SHZEI + WKA-SHZEI.
           ADD 1      TO WCT1-DNGYO.
       M22-L01.
           PERFORM M11-WKA THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE-HENSHU-WRITE                                 *
      *----------------------------------------------------------------*
       M23-HENSHU.
           MOVE ZERO  TO WSV1-SW1.
           MOVE ZERO  TO WCT1-K.
       M23-L01.
           ADD 1 TO WCT1-K.
           IF WCT1-K > 20 OR
              TBL-R1 (WCT1-K) = SPACE
              GO TO M23-EXIT.
           MOVE SPACE TO WKB-R.
           MOVE TBL-SITEK (WCT1-K) TO WKB-SITEK.
           MOVE TBL-SKUCD (WCT1-K) TO WKB-SKUCD.
           MOVE TBL-SKNCD (WCT1-K) TO WKB-SKNCD.
           MOVE TBL-SOKOK (WCT1-K) TO WKB-SOKOK.
           MOVE TBL-CKUKG (WCT1-K) TO WKB-CKUKG.
           MOVE TBL-TOKCD (WCT1-K) TO WKB-TOKCD.
           MOVE TBL-NSKBN (WCT1-K) TO WKB-NSKBN.
           MOVE TBL-DEND1 (WCT1-K) TO WKB-DEND1.
           MOVE '60'               TO WKB-REKBN.
           MOVE TBL-DENN1 (WCT1-K) TO WKB-DENN1.
           MOVE TBL-DENPL (WCT1-K) TO WKB-DENPL.
           MOVE TBL-TORIK (WCT1-K) TO WKB-TORIK.
           MOVE TBL-SYOCD (WCT1-K) TO WKB-SYOCD.
           MOVE TBL-SYOME (WCT1-K) TO WKB-SYOME.
           MOVE TBL-SURYO (WCT1-K) TO WKB-SURYO.
           MOVE TBL-TANKA (WCT1-K) TO WKB-TANKA.
           MOVE TBL-KINGK (WCT1-K) TO WKB-KINGK.
           MOVE TBL-DEND2 (WCT1-K) TO WKB-DEND2.
           MOVE TBL-DENN2 (WCT1-K) TO WKB-DENN2.
           MOVE TBL-THKBN (WCT1-K) TO WKB-THKBN.
           MOVE TBL-BUKBN (WCT1-K) TO WKB-BUKBN.
           MOVE TBL-KETKB (WCT1-K) TO WKB-KETKB.
           MOVE TBL-SYOME-X (WCT1-K) TO WKB-SYOME-X.
           IF TBL-DSHZFG(WCT1-K) = '1'
              MOVE ZERO               TO WKB-ZEGAK
           ELSE
              MOVE TBL-SHZEI (WCT1-K) TO WKB-ZEGAK.
           MOVE TBL-ZEIRT (WCT1-K) TO WKB-ZEIRT.
           MOVE TBL-DSHZFG(WCT1-K) TO WKB-DSHZFG.
           MOVE TBL-KGZKB (WCT1-K) TO WKB-KGZKB.
       M23-L02.
           IF WSV1-SW1 = ZERO
              MOVE WTT1-ALTOT TO WKB-DENPK
              MOVE WTT1-SHZEI TO WKB-SHZEI
              MOVE WCT1-DNGYO TO WKB-DNGYO
           ELSE
              MOVE ZERO       TO WKB-DENPK
              MOVE ZERO       TO WKB-SHZEI
              MOVE ZERO       TO WKB-DNGYO.
       M23-L03.
           MOVE 1             TO WSV1-SW1.
           MOVE WSVK-INKJ07   TO WKB-TOKAN.
       M23-L04.
           IF TBL-THKBN (WCT1-K) NOT = 'A'
              PERFORM M231-TOKAN-HENSHU-1 THRU M231-EXIT
           ELSE
              PERFORM M232-TOKAN-HENSHU-2 THRU M232-EXIT.
       M23-L05.
           WRITE WKB-REC FROM WKB-R.
           GO TO M23-L01.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : TOKAN-HENSHU-1 ( THKBN NOT = 'A' )                 *
      *----------------------------------------------------------------*
       M231-TOKAN-HENSHU-1.
           IF TBL-TORI1 (WCT1-K)  = '1'
              IF TBL-TORI2 (WCT1-K)  = '2'
                 MOVE WSVK-INKJ01 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ02 TO WKB-TOKAN.
       M231-L01.
           IF TBL-TORI1 (WCT1-K)  = '2'
              MOVE WSVK-INKJ02 TO WKB-TOKAN.
       M231-L02.
           IF TBL-TORI1 (WCT1-K)  = '3'
              IF TBL-TORI2 (WCT1-K)  = '2'
                 MOVE WSVK-INKJ03 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ04 TO WKB-TOKAN.
       M231-L03.
           IF TBL-TORI1 (WCT1-K)  = '4'
              IF TBL-SYOC2 (WCT1-K)  = '88888'
                 MOVE WSVK-INKJ05 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ07 TO WKB-TOKAN.
       M231-L04.
           IF TBL-TORI1 (WCT1-K)  = '5'
              IF TBL-SYOC2 (WCT1-K)  = '88888'
                 MOVE WSVK-INKJ06 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ07 TO WKB-TOKAN.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.2 : TOKAN-HENSHU-2 ( THKBN  = 'A' )                    *
      *----------------------------------------------------------------*
       M232-TOKAN-HENSHU-2.
           IF TBL-TORI1 (WCT1-K)  = '1'
              IF TBL-TORI2 (WCT1-K)  = '2'
                 MOVE WSVK-INKJ03 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ04 TO WKB-TOKAN.
       M232-L01.
           IF TBL-TORI1 (WCT1-K)  = '2'
              MOVE WSVK-INKJ04 TO WKB-TOKAN.
       M232-L02.
           IF TBL-TORI1 (WCT1-K)  = '3'
              IF TBL-TORI2 (WCT1-K)  = '2'
                 MOVE WSVK-INKJ01 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ02 TO WKB-TOKAN.
       M232-L03.
           IF TBL-TORI1 (WCT1-K)  = '4'
              IF TBL-SYOC2 (WCT1-K)  = '88888'
                 MOVE WSVK-INKJ06 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ07 TO WKB-TOKAN.
       M232-L04.
           IF TBL-TORI1 (WCT1-K)  = '5'
              IF TBL-SYOC2 (WCT1-K)  = '88888'
                 MOVE WSVK-INKJ05 TO WKB-TOKAN
              ELSE
                 MOVE WSVK-INKJ07 TO WKB-TOKAN.
       M232-EXIT.
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
      *         VBKA1540 END                                           *
      *----------------------------------------------------------------*
