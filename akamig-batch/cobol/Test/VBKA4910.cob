./ ADD NAME=VBKA4910  01.20 20030710 20100629 182612 00278 00233 MCSK020
       ID  DIVISION.
       PROGRAM-ID. 'VBKA4910'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : SEIKYUUSYO YUUSOUSAKI CODE    *
      *                                  SETTEISAKI CYUUSYUTU          *
      *                                                                *
      *                 * INPUT FILE   : TOKUISAKI MASTER      (SI.TMS)*
      *                 * INPUT FILE   : SEIKYUU JYOUHOU DATA    (DWKC)*
      *                 * INPUT FILE   : TOKUISAKI JYUUSYO MASTER      *
      *                                                      (TJMFILE) *
      *                 * OUTPUT FILE  : YUUSOUSAKI JYOUHOU DATA (DWKD)*
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/07/10                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-568  A-2199      ÍÝº³   : 03/09/29  CSK-Y.MORIO         *
      * (02) K-1228             ÍÝº³   : 10/06/29  CSK-M.KODAMA        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TJMFILE ASSIGN    TO TJMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-TJM
                          FILE  STATUS S-CODE.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  TJMFILE
           LABEL RECORD STANDARD.
       01  TJM-REC.
           02 KEY-TJM                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(80).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(80).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE VDCLTMS END-EXEC.
       01  TMS REDEFINES DCLTMS.
           COPY VTABTMS SUPPRESS.
       01  TJM-R.
           COPY VFILTJM SUPPRESS.
       01  P89-PARM.
           COPY VPARB89 SUPPRESS.
       01  PB2-PARM.
           COPY VPARBB2 SUPPRESS.
       01  WKC-R.
           02 WKC-SKYCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-SYUCD                     PIC  X(07).
           02 WKC-SYNCD                     PIC  X(02).
           02 WKC-SYHHK                     PIC  X(01).
           02 WKC-SYUSO.
              03 WKC-SIMBI                  PIC  X(02).
              03 WKC-KAINO                  PIC  X(02).
              03 WKC-BUNO                   PIC  X(03).
              03 WKC-SOKNO                  PIC  X(03).
              03 WKC-TIKCD                  PIC  X(05).
              03 WKC-STRIYU.
                 04 WKC-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKC-STKBN                  PIC  X(01).
              03 WKC-YUBIN                  PIC  X(07).
           02 WKC-SKSIM                     PIC  9(08).
           02 WKC-ICYOK                     PIC  X(01).
           02 WKC-URKZD                     PIC S9(12)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  WKD-R.
           02 WKD-SKYCD                     PIC  X(07).
           02 WKD-SNSCD                     PIC  X(02).
           02 WKD-SYUCD                     PIC  X(07).
           02 WKD-SYNCD                     PIC  X(02).
           02 WKD-SYHHK                     PIC  X(01).
           02 WKD-SYUSO.
              03 WKD-SIMBI                  PIC  X(02).
              03 WKD-KAINO                  PIC  X(02).
              03 WKD-BUNO                   PIC  X(03).
              03 WKD-SOKNO                  PIC  X(03).
              03 WKD-CKUKG                  PIC  X(05).
              03 WKD-STRIYU.
                 04 WKD-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKD-STKBN                  PIC  X(01).
              03 WKD-YUBIN                  PIC  X(07).
           02 WKD-SKSIM                     PIC  9(08).
           02 WKD-ICYOK                     PIC  X(01).
           02 WKD-URKZD                     PIC S9(12)  COMP-3.
           02 FILLER                        PIC  X(07).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-SYUCD              PIC  X(07).
                 04 WIN1-SYNCD              PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-SYUCD              PIC  X(07).
                 04 WSV1-SYNCD              PIC  X(02).
       01  EDIT-AREA.
           02 WED1-SQLCD                    PIC  ---9.
           02 WED1-TOKCD.
              03 WED1-TOKCD1                PIC  X(01).
              03 FILLER                     PIC  X(06).
           02 WED1-ASTA                     PIC  X(01) VALUE '*'.
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
           PERFORM M10-LOCK-DB2 THRU M10-EXIT.
           OPEN INPUT  WKCFILE
                       TJMFILE
                OUTPUT WKDFILE.
       M1-L01.
           PERFORM M11-READ-WKC THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.0 : LOCK-DB2                                             *
      *----------------------------------------------------------------*
       M10-LOCK-DB2.
           MOVE '   TMS'   TO P89-TBLID(1).
           MOVE 'S'        TO P89-LTYPE(1).
           MOVE ALL '*'    TO P89-TABLE(2).
           CALL 'VSBA0890' USING P89-PARM.
       M10-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKCFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKC.
           READ WKCFILE INTO WKC-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
           IF WKC-SKYCD = WKC-SYUCD AND WKC-SNSCD = WKC-SYNCD
              GO TO M11-READ-WKC.
       M11-L01.
           MOVE WKC-SYUCD TO WIN1-SYUCD.
           MOVE WKC-SYNCD TO WIN1-SYNCD.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           MOVE WIN1-KEY1  TO WSV1-KEY1.
           PERFORM M21-WRITE-WKD  THRU M21-EXIT.
       M2-L01.
           PERFORM M11-READ-WKC THRU M11-EXIT
               UNTIL ( WIN1-KEY1 NOT = WSV1-KEY1 ).
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1 : WRITE WKD                                          *
      *-------------------------------------------------------------*
       M21-WRITE-WKD.
           PERFORM M211-SELECT-TMS THRU M211-EXIT.
           PERFORM M212-READ-TJM THRU M212-EXIT.
       M21-L01.
           MOVE SPACE        TO WKD-R.
           INITIALIZE           WKD-R.
           MOVE WKC-SYUCD    TO WKD-SKYCD.
           MOVE WKC-SYNCD    TO WKD-SNSCD.
           MOVE WKC-SYUCD    TO WKD-SYUCD.
           MOVE WKC-SYNCD    TO WKD-SYNCD.
           MOVE TSIME OF TMS TO WKD-SIMBI.
           MOVE STNCD OF TMS TO WKD-BUNO.
           MOVE CKUKG OF TMS TO WKD-CKUKG.
           MOVE TJM-YUBNO    TO WKD-YUBIN.
           MOVE '1'          TO PB2-HNBCD.
           MOVE STNCD OF TMS TO PB2-BUNO.
           CALL 'VSBA1120' USING PB2-PARM.
           MOVE PB2-KAINO    TO WKD-KAINO.
           PERFORM M213-SET-TRIYU THRU M213-EXIT.
           WRITE WKD-REC  FROM WKD-R.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : SELECT-TMS                                         *
      *----------------------------------------------------------------*
       M211-SELECT-TMS.
           MOVE WKC-SYUCD TO TOKCD OF TMS.
           EXEC SQL WHENEVER NOT FOUND  GO TO M211-L01 END-EXEC.
           EXEC SQL WHENEVER SQLERROR   GO TO M211-ERR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING GO TO M211-ERR END-EXEC.
           EXEC SQL SELECT A0,A2,A3,A4,A5,A9,B9,C3
              INTO :DCLTMS.A0,:DCLTMS.A2,:DCLTMS.A3,
                   :DCLTMS.A4,:DCLTMS.A5,:DCLTMS.A9,
                   :DCLTMS.B9,:DCLTMS.C3
              FROM    TMS
              WHERE A0=:DCLTMS.A0
           END-EXEC.
           GO TO M211-EXIT.
       M211-L01.
           DISPLAY 'R VBKA4910 SI.TMS NOT FOUND KEY = ' WKC-SYUCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-ERR.
           DISPLAY 'R VBKA4910 TABLE(SI.TMS) SELECT ERROR !'
           UPON CONSOLE.
           MOVE SQLCODE TO WED1-SQLCD.
           DISPLAY '  SQLCODE = ' WED1-SQLCD
           UPON CONSOLE.
           PERFORM M3-FINL THRU M3-EXIT.
           CALL 'VSBABEND'.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : READ TJMFILE                                       *
      *----------------------------------------------------------------*
       M212-READ-TJM.
           MOVE WKC-SYUCD TO KEY-TJM.
           MOVE 'Z '      TO S-CODE.
           READ TJMFILE INTO TJM-R
                INVALID KEY
                        MOVE SPACE TO TJM-R
                        GO TO M212-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VBKA4910 TJMFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE SPACE TO TJM-R.
       M212-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1.3 : SEIKYUUSYO TORIYAME RIYU  SET                    *
      *-------------------------------------------------------------*
       M213-SET-TRIYU.
           MOVE WKC-STRIYU TO WKD-STRIYU.
           IF YTFLG OF TMS =  '1'
              MOVE WED1-ASTA TO WKD-TRIYU(1).
           MOVE TOKCD OF TMS TO WED1-TOKCD.
           IF WED1-TOKCD1 = '8'
              MOVE WED1-ASTA TO WKD-TRIYU(4).
           IF INKAN OF TMS = '1'
              MOVE WED1-ASTA TO WKD-TRIYU(5).
           IF TJM-ADRES = SPACE
              MOVE WED1-ASTA TO WKD-TRIYU(11).
       M213-EXIT.
            EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE TJMFILE
                 WKCFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA4910  END                                          *
      *-------------------------------------------------------------*
