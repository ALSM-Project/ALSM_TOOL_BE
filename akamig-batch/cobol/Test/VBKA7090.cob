./ ADD NAME=VBKA7090  01.23 20190626 20190801 090115 00347 00331 MCSK015
       ID  DIVISION.
       PROGRAM-ID. 'VBKA7090'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN    SYSTEM             *
      *              <> SUBSYSTEM      : 20SIME SEIKYUSYO              *
      *              <> PG.OUTLINE     : ZEIRITSUBETSU GOUKEI DATA     *
      *                                  SAKUSEI                       *
      *                                                                *
      *                 * INPUT FILE   : TOUGETSU SEIKYU TORIHIKI      *
      *                                  MEISAI DATA          (DWKB)   *
      *                 * SYSIN        : SHIMEBI                       *
      *                 * OUTPUT FILE  : ZEIRITSUBETSU GOUKEI DATA     *
      *                                                       (DWKC)   *
      *                                                                *
      *              <> AUTHOR         : CSK-M.MATSUSHIMA              *
      *              <> DATE           : 19/06/26                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (02) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
       DATA DIVISION.
       FILE SECTION.
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(252).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(250).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       01  P48-PARM.
           COPY VPARB48  SUPPRESS.
       01  PC0-PARM.
           COPY VPARBC0  SUPPRESS.
       01  P197-PARM.
           COPY VPARB197 SUPPRESS.
       01  P205-PARM.
           COPY VPARB205 SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  WKB-R.
           02 WKB-STNCD                     PIC  X(03).
           02 WKB-SKUCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-SOKOK                     PIC  X(01).
           02 WKB-CKUKG                     PIC  X(05).
           02 WKB-TOKCD                     PIC  X(07).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-DNPH1                     PIC  9(08).
           02 WKB-REKBN                     PIC  X(02).
           02 WKB-DNPN1                     PIC  X(07).
           02 WKB-DNPNL                     PIC  9(01).
           02 WKB-TORIK                     PIC  X(02).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-SYOME                     PIC  X(90).
           02 WKB-SURYO                     PIC S9(05) COMP-3.
           02 WKB-TANKA                     PIC S9(08) COMP-3.
           02 WKB-KINGK                     PIC S9(09) COMP-3.
           02 WKB-DNKIN                     PIC S9(11) COMP-3.
           02 WKB-DNPH2                     PIC  9(08).
           02 WKB-DNPN2                     PIC  X(07).
           02 WKB-THKBN                     PIC  X(01).
           02 WKB-BNKKB                     PIC  X(01).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-TORKJ                     PIC  X(08).
           02 WKB-SYOME-X.
              03 WKB-SYOMH-X                PIC  X(25).
              03 WKB-SYOMKY-X               PIC  X(20).
           02 WKB-SHZEI                     PIC S9(11) COMP-3.
           02 WKB-ZEGAK                     PIC S9(09) COMP-3.
           02 WKB-DNGYO                     PIC  9(01).
           02 WKB-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKB-DSHZFG                    PIC  X(01).
           02 WKB-KGZKB                     PIC  X(01).
       01  WKC-R.
           02 WKC-STNCD                     PIC  X(03).
           02 WKC-SKUCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-TOKCD                     PIC  X(07).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-DATAB.
              03 WKC-RCKBN                  PIC  X(02).
              03 WKC-MIDAA                  PIC  X(50).
              03 WKC-MIDBB                  PIC  X(50).
              03 WKC-ZRKGT                  PIC S9(11)   COMP-3.
              03 WKC-ZRSZT                  PIC S9(11)   COMP-3.
              03 WKC-ZEIRT                  PIC S9(02)V9 COMP-3.
              03 WKC-KGZKB                  PIC  X(01).
              03 FILLER                     PIC  X(112).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SKUCD              PIC  X(07).
                 04 WIN1-SNSCD              PIC  X(02).
                 04 WIN1-ZEIRT              PIC S9(02)V9 COMP-3.
                 04 WIN1-KGZKB              PIC  X(01).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-SKUCD              PIC  X(07).
                 04 WSV1-SNSCD              PIC  X(02).
                 04 WSV1-ZEIRT              PIC S9(02)V9 COMP-3.
                 04 WSV1-KGZKB              PIC  X(01).
           02 WSV2-DAT.
              03 WSV2-STNCD                 PIC  X(03).
              03 WSV2-SKUCD                 PIC  X(07).
              03 WSV2-SNSCD                 PIC  X(02).
              03 WSV2-TOKCD                 PIC  X(07).
              03 WSV2-NOSCD                 PIC  X(02).
              03 WSV2-ZEIRT                 PIC S9(02)V9 COMP-3.
              03 WSV2-KGZKB                 PIC  X(01).
      *       <Å—¦>
           02 WSV3-MID1.
              03 FILLER                     PIC  X(04) VALUE
              X'47C44783'.
      *       <ŒyŒ¸Å—¦>
           02 WSV3-MID2.
              03 FILLER                     PIC  X(08) VALUE
              X'48A6478A47C44783'.
      *       <‹ŒÅ—¦>
           02 WSV3-MID3.
              03 FILLER                     PIC  X(06) VALUE
              X'4C7B47C44783'.
      *       <”ñ‰ÛÅ‘ÎÛ>
           02 WSV3-MID4.
              03 FILLER                     PIC  X(10) VALUE
              X'48E149D647C446564994'.
      *       <“‘ÎÛ>
           02 WSV3-MID5.
              03 FILLER                     PIC  X(06) VALUE
              X'426C46564994'.
       01  TOTAL-AREA.
           02 WTT1-DNKIN                    PIC S9(13) COMP-3.
           02 WTT1-SHZEI                    PIC S9(13) COMP-3.
       01  EDIT-AREA.
           02 WED1-DATE.
              03 WED1-YY                    PIC  9(04).
              03 WED1-MM                    PIC  9(02).
              03 WED1-DD                    PIC  9(02).
           02 WED1-ZEIRT                    PIC  99.9.
           02 WED1-ZEIRTX REDEFINES WED1-ZEIRT.
              03 WED1-ZEI1                  PIC  X(01).
              03 WED1-ZEI2                  PIC  X(01).
              03 FILLER                     PIC  X(01).
              03 WED1-ZEI3                  PIC  X(01).
           02 WED1-MIDBB                    PIC  X(60).
       01  SYSIN-AREA.
           02 SYS-CARD1.
              03 SYS-SIMEB                  PIC  9(02).
              03 FILLER                     PIC  X(78).
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
           OPEN INPUT  WKBFILE
                OUTPUT WKCFILE.
           CALL 'VSBA2280' USING P228-PARM.
           PERFORM M10-SYSIN1   THRU M10-EXIT.
       M1-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : SYSIN READ                                           *
      *----------------------------------------------------------------*
       M10-SYSIN1.
           MOVE LOW-VALUE   TO SYS-CARD1.
           ACCEPT SYS-CARD1 FROM SYSIN.
           IF SYS-CARD1 = LOW-VALUE
              CALL 'VSBABEND'.
           IF SYS-SIMEB NOT = '20' AND '31' AND '15'
              CALL 'VSBABEND'.
       M10-L01.
           MOVE P228-DATEX        TO WED1-DATE.
           IF SYS-SIMEB = '31'
              IF P228-DD NOT < 25
                 MOVE P228-DATEX  TO PC0-RYMDS8
              ELSE
                 IF P228-MM = 1
                    COMPUTE WED1-YY = WED1-YY - 1
                    MOVE 12       TO WED1-MM
                 ELSE
                    COMPUTE WED1-MM = WED1-MM - 1
                 END-IF
                 MOVE WED1-DATE   TO PC0-RYMDS8
              END-IF
              CALL 'VSBA1200' USING PC0-PARM
              MOVE PC0-RYMDS8     TO WED1-DATE
           ELSE
              MOVE SYS-SIMEB      TO WED1-DD.
           MOVE WED1-DATE         TO P205-YMDS8-X.
           MOVE '0'               TO P205-KGZKB.
           CALL 'VSBA2050' USING P205-PARM.
       M10-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKBFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKB.
           READ WKBFILE INTO WKB-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKB-SKUCD TO WIN1-SKUCD.
           MOVE WKB-SNSCD TO WIN1-SNSCD.
           MOVE WKB-ZEIRT TO WIN1-ZEIRT.
           MOVE WKB-KGZKB TO WIN1-KGZKB.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-SKUCD TO WSV1-SKUCD.
           MOVE WIN1-SNSCD TO WSV1-SNSCD.
           MOVE WIN1-ZEIRT TO WSV1-ZEIRT.
           MOVE WIN1-KGZKB TO WSV1-KGZKB.
           MOVE ZERO       TO WTT1-DNKIN.
           MOVE ZERO       TO WTT1-SHZEI.
           MOVE WKB-STNCD  TO WSV2-STNCD.
           MOVE WKB-SKUCD  TO WSV2-SKUCD.
           MOVE WKB-SNSCD  TO WSV2-SNSCD.
           MOVE WKB-TOKCD  TO WSV2-TOKCD.
           MOVE WKB-NOSCD  TO WSV2-NOSCD.
           MOVE WKB-ZEIRT  TO WSV2-ZEIRT.
           MOVE WKB-KGZKB  TO WSV2-KGZKB.
       M2-L01.
           PERFORM M21-WKC-EDIT THRU M21-EXIT
               UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
       M2-L02.
           PERFORM M22-WKC-WRITE THRU M22-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1 : WKC EDIT                                           *
      *-------------------------------------------------------------*
       M21-WKC-EDIT.
           COMPUTE WTT1-DNKIN = WTT1-DNKIN + WKB-DNKIN.
           COMPUTE WTT1-SHZEI = WTT1-SHZEI + WKB-SHZEI.
       M21-L01.
           PERFORM M11-READ-WKB THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.2 : WKC WRITE                                          *
      *-------------------------------------------------------------*
       M22-WKC-WRITE.
           MOVE SPACE      TO WKC-R.
           MOVE WSV2-STNCD TO WKC-STNCD.
           MOVE WSV2-SKUCD TO WKC-SKUCD.
           MOVE WSV2-SNSCD TO WKC-SNSCD.
           MOVE WSV2-TOKCD TO WKC-TOKCD.
           MOVE WSV2-NOSCD TO WKC-NOSCD.
           MOVE '66'       TO WKC-RCKBN.
           MOVE SPACE      TO WKC-MIDAA.
           PERFORM M221-MIDBB-PROC THRU M221-EXIT.
           MOVE WTT1-DNKIN TO WKC-ZRKGT.
           MOVE WTT1-SHZEI TO WKC-ZRSZT.
           MOVE WSV2-ZEIRT TO WKC-ZEIRT.
           MOVE WSV2-KGZKB TO WKC-KGZKB.
           WRITE WKC-REC FROM WKC-R.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2.1 : MIDASI GYO B                                       *
      *----------------------------------------------------------------*
       M221-MIDBB-PROC.
           MOVE WSV2-ZEIRT TO WED1-ZEIRT.
           IF WED1-ZEI1 = ZERO
              IF WED1-ZEI3 = ZERO
                 STRING WED1-ZEI2
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 1       TO P48-INLEN
              ELSE
                 STRING WED1-ZEI2 '.' WED1-ZEI3
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 3       TO P48-INLEN
              END-IF
           ELSE
              IF WED1-ZEI3 = ZERO
                 STRING WED1-ZEI1 WED1-ZEI2
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 2       TO P48-INLEN
              ELSE
                 STRING WED1-ZEI1 WED1-ZEI2 '.' WED1-ZEI3
                    DELIMITED BY SIZE INTO P48-INDAT-X
                 MOVE 4       TO P48-INLEN
              END-IF
           END-IF.
           CALL 'VSBA0480' USING P48-PARM.
       M221-L01.
           MOVE SPACE TO WED1-MIDBB.
           IF WSV2-ZEIRT = P205-SHZRT
              STRING WSV3-MID1 P48-OTDAT-X WSV3-MID5
                 DELIMITED BY SIZE INTO WED1-MIDBB
           ELSE
              IF WSV2-KGZKB = '1'
                 STRING WSV3-MID2 P48-OTDAT-X WSV3-MID5
                    DELIMITED BY SIZE INTO WED1-MIDBB
              ELSE
                 IF WSV2-ZEIRT = ZERO
                    MOVE WSV3-MID4 TO WED1-MIDBB
                 ELSE
                    STRING WSV3-MID3 P48-OTDAT-X WSV3-MID5
                       DELIMITED BY SIZE INTO WED1-MIDBB
                 END-IF
              END-IF
           END-IF.
           MOVE '4'        TO P197-SORKB.
           MOVE 60         TO P197-EDKET.
           MOVE WED1-MIDBB TO P197-EDKOU.
           CALL 'VSBA1970' USING P197-PARM.
           STRING '                ' P197-EDKOU(1:50)
              DELIMITED BY SIZE INTO WKC-MIDBB.
       M221-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKBFILE
                 WKCFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA7090  END                                          *
      *-------------------------------------------------------------*
