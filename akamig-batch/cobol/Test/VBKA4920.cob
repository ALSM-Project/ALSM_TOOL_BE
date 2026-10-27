./ ADD NAME=VBKA4920  01.09 20030710 20150709 104439 00170 00252 MCSK082
       ID  DIVISION.
       PROGRAM-ID. 'VBKA4920'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SITEN SYSTEM                  *
      *              <> SUBSYSTEM      : K A                           *
      *              <> PG.OUTLINE     : YUUSOUSAKI JYOUHOU SOSIKI     *
      *                                  HONYAKU                       *
      *                                                                *
      *                 * INPUT FILE   : YUUSOUSAKI JYOUHOU DATA (DWKE)*
      *                 * OUTPUT FILE  : YUUSOUSAKI JYOUHOU DATA (DWKD)*
      *                                                                *
      *              <> AUTHOR         : CSK-C.YAMAGATA                *
      *              <> DATE           : 03/07/09                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1758           ØºÝÊß²Ù  : 15/07/09  CSK-K.KOYAMA        *
      * (02) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (03) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
       DATA DIVISION.
       FILE SECTION.
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(80).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(80).
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                  *
      *-------------------------------------------------------------*
       01  P228-PARM.
           COPY VPARB228  SUPPRESS.
       01  P237-PARM.
           COPY VPARB237  SUPPRESS.
       01  P238-PARM.
           COPY VPARB238  SUPPRESS.
       01  WKE-R.
           02 WKE-SKYCD                     PIC  X(07).
           02 WKE-SNSCD                     PIC  X(02).
           02 WKE-SYUCD                     PIC  X(07).
           02 WKE-SYNCD                     PIC  X(02).
           02 WKE-SYHHK                     PIC  X(01).
           02 WKE-SYUSO.
              03 WKE-SIMBI                  PIC  X(02).
              03 WKE-KAINO                  PIC  X(02).
              03 WKE-BUNO                   PIC  X(03).
              03 WKE-SOKNO                  PIC  X(03).
              03 WKE-CKUKG                  PIC  X(05).
              03 WKE-STRIYU.
                 04 WKE-TRIYU               OCCURS 15
                                            PIC  X(01).
              03 WKE-STKBN                  PIC  X(01).
              03 WKE-YUBIN                  PIC  X(07).
           02 WKE-SKSIM                     PIC  9(08).
           02 WKE-ICYOK                     PIC  X(01).
           02 WKE-URKZD                     PIC S9(12)  COMP-3.
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
                 04 WIN1-BUNO            PIC  X(03).
                 04 WIN1-CKUKG1          PIC  X(03).
       01  SAVE-AREA.
           02 WSV1-KEY.
              03 WSV1-KEY1.
                 04 WSV1-BUNO            PIC  X(03).
                 04 WSV1-CKUKG1          PIC  X(03).
       01  EDIT-AREA.
           02 WED1-CKUKG.
              03 WED1-CKUKG1             PIC  X(03).
              03 FILLER                  PIC  X(02).
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
           OPEN INPUT  WKEFILE
                OUTPUT WKDFILE.
       M1-L01.
           PERFORM M11-READ-WKE THRU M11-EXIT.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-DATE     TO P237-KIJUN.
           CALL 'VSBA2370' USING P237-PARM.
       M1-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M1.1 : READ WKEFILE                                      *
      *-------------------------------------------------------------*
       M11-READ-WKE.
           READ WKEFILE INTO WKE-R
                 AT END
                    MOVE HIGH-VALUE TO WIN1-KEY
                    GO TO M11-EXIT.
       M11-L01.
           MOVE WKE-BUNO    TO WIN1-BUNO.
           MOVE WKE-CKUKG   TO WED1-CKUKG.
           MOVE WED1-CKUKG1 TO WIN1-CKUKG1.
       M11-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *    M2 : MAIN                                                *
      *-------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY1   NOT =   WSV1-KEY1
              PERFORM M21-SEARCH-SOSHIKI THRU M21-EXIT.
           MOVE WKE-R         TO  WKD-R.
           MOVE P238-SOKNO    TO  WKD-SOKNO.
           WRITE WKD-REC    FROM  WKD-R.
       M2-L01.
           PERFORM M11-READ-WKE THRU M11-EXIT.
       M2-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M2.1 : SEARCH-SOSHIKI                                     *
      *-------------------------------------------------------------*
       M21-SEARCH-SOSHIKI.
           MOVE WIN1-KEY1   TO WSV1-KEY1.
           MOVE 'A'         TO P238-HNBCD.
           MOVE WKE-BUNO    TO P238-STNNO.
           MOVE WED1-CKUKG1 TO P238-SYKNO.
           CALL 'VSBA2380' USING P238-PARM P237-PARM.
       M21-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *   M3 : FINAL                                                *
      *-------------------------------------------------------------*
       M3-FINL.
           CLOSE WKEFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA4920  END                                          *
      *-------------------------------------------------------------*
