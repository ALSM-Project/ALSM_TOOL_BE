./ ADD NAME=VSBA2370  01.01 20150623 20150901 174217 00216 00216 MCSK082
       ID  DIVISION.
       PROGRAM-ID.      'VSBA2370'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : LOAD SOSIKI TABLE                *
      *                                                                *
      *                 *  OUTPUT.PARM  : SOSHIKI TABLE ENTRY          *
      *                                                                *
      *                                                                *
      *              <> AUTHOR      : CSK-S.WATANABE                   *
      *              <> DATE        : 98/04/10                         *
      *--------------< MAINTENANCE  HISTORY >--------------------------*
      * (01) C-073            ÍÝº³  : 99/02/01   CSK-Y.FUJITA          *
      * (02) C-110            ÍÝº³  : 99/07/06   CSK-T.NAKAMURA        *
      * (03) H-010            ÍÝº³  : 00/05/31   CSK-I.HARADA          *
      * (04) K-394            ÍÝº³  : 00/08/01   CSK-S.KOZAWA          *
      * (05) K-455            ÍÝº³  : 00/11/15   CSK-H.SATO            *
      * (06) K-1758           ÍÝº³  : 15/06/23   CSK-H.TAKAKUWA        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TBLFILE   ASSIGN    TO TBLFILE
                            ORGANIZATION INDEXED
                            ACCESS MODE SEQUENTIAL
                            RECORD KEY KEY-TSZ
                            FILE STATUS S-CODE.
           SELECT ORGFILE   ASSIGN    TO ORGFILE
                            ORGANIZATION INDEXED
                            ACCESS MODE RANDOM
                            RECORD KEY KEY-ORG
                            FILE STATUS S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  TBLFILE
           LABEL RECORD     STANDARD.
       01  TSZ-REC.
           02 KEY-TSZ       PIC X(06).
           02 FILLER        PIC X(74).
       FD  ORGFILE
           LABEL RECORD     STANDARD.
       01  ORG-REC.
           02 KEY-ORG       PIC X(17).
           02 FILLER        PIC X(153).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       77  S-CODE           PIC  X(02).
       01  TSZ-R.
           COPY VFILTSZ     SUPPRESS.
       01  ORG-R.
           COPY VFILORG     SUPPRESS.
       01  P34-PARM.
           COPY VPARB34     SUPPRESS.
       01  P35-PARM.
           COPY VPARB35     SUPPRESS.
       01  P132-PARM.
           COPY VPARB132    SUPPRESS.
       01  COUNT-AREA.
           02 WCT1-J        PIC S9(04)  COMP.
           02 WCT2-J        PIC S9(04)  COMP.
           02 WCT3-J        PIC S9(04)  COMP.
           02 WCT4-J        PIC S9(04)  COMP.
           02 WCT5-J        PIC S9(04)  COMP.
           02 I             PIC S9(04)  COMP.
           02 J             PIC S9(04)  COMP.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINCAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  P237-PARM.
           COPY VPARB237    SUPPRESS.
       PROCEDURE DIVISION   USING P237-PARM.
      *----------------------------------------------------------------*
      *   M1 : MAIN CONTROL                                            *
      *----------------------------------------------------------------*
       M1-MAIN-CONTROL.
           OPEN INPUT       TBLFILE
                            ORGFILE.
           MOVE ZERO        TO WCT1-J.
           MOVE ZERO        TO WCT2-J.
           MOVE ZERO        TO WCT3-J.
           MOVE ZERO        TO WCT4-J.
           MOVE ZERO        TO WCT5-J.
           CALL 'VSBA0340'  USING P34-PARM.
           MOVE P237-KIJUN  TO P132-DATE.
           CALL 'VSBA1320'  USING P132-PARM.
           MOVE ALL '*'     TO P237-SOTBL.
           MOVE LOW-VALUE   TO TSZ-KEY.
           MOVE 'TZ'        TO TSZ-RECID.
           MOVE TSZ-KEY     TO KEY-TSZ.
           START TBLFILE    KEY > KEY-TSZ.
       M1-L01.
           READ  TBLFILE INTO TSZ-R
                 AT END
                    GO TO M1-L02.
           IF TSZ-RECID NOT = 'TZ'
              GO TO M1-L02.
           IF TSZ-SYOKAKG = '*'
              GO TO M1-L01.
           IF TSZ-SITENKG = 'A' OR '´' OR 'O' OR 'M' OR 'P' OR '¸' OR
                            '½' OR '»' OR 'G' OR 'Ä' OR 'È' OR 'Ë' OR
                            'Å' OR 'H' OR 'J' OR 'N'
              ADD  1  TO WCT1-J
              MOVE 1  TO I
              IF WCT1-J > 700
                 GO TO M1-L01
              ELSE
                 MOVE WCT1-J TO J
                 PERFORM M2-DATA-SET THRU M2-EXIT
                 GO TO M1-L01.
           IF TSZ-SITENKG = 'Q' OR 'S' OR 'U' OR '¶' OR '·' OR 'Ã' OR
                            'Æ' OR 'Ç' OR 'Ê' OR 'C' OR 'K' OR 'L' OR
                            'T' OR '±'
              ADD  1  TO WCT2-J
              MOVE 2  TO I
              IF WCT2-J > 700
                 GO TO M1-L01
              ELSE
                 MOVE WCT2-J TO J
                 PERFORM M2-DATA-SET THRU M2-EXIT
                GO TO M1-L01.
           IF TSZ-SITENKG = '³' OR 'µ' OR '¿' OR 'À' OR 'Â' OR 'É' OR
                            'D' OR 'E' OR 'F' OR 'I' OR 'R' OR '²' OR
                            '¹'
              ADD  1  TO WCT3-J
              MOVE 3  TO I
              IF WCT3-J > 700
                 GO TO M1-L01
              ELSE
                 MOVE WCT3-J TO J
                 PERFORM M2-DATA-SET THRU M2-EXIT
                GO TO M1-L01.
           IF TSZ-SITENKG = '0' OR '1' OR '2' OR '3' OR '4' OR '5' OR
                            '6' OR '7' OR '8' OR '9' OR 'Y' OR 'Z'
              ADD  1  TO WCT4-J
              MOVE 4  TO I
              IF WCT4-J > 700
                 GO TO M1-L01
              ELSE
                 MOVE WCT4-J TO J
                 PERFORM M2-DATA-SET THRU M2-EXIT
                GO TO M1-L01.
           ADD  1  TO WCT5-J.
           MOVE 5  TO I.
           IF WCT5-J > 700
              GO TO M1-L01
           ELSE
              MOVE WCT5-J TO J
              PERFORM M2-DATA-SET THRU M2-EXIT
              GO TO M1-L01.
       M1-L02.
           CLOSE TBLFILE
                 ORGFILE.
       M1-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *   M2 : DATA SET                                                *
      *----------------------------------------------------------------*
       M2-DATA-SET.
           MOVE TSZ-SYSNO   TO P237-SYSNO( I J ).
           MOVE TSZ-SITENKG TO P237-STNKG( I J ).
           MOVE TSZ-SOKOKG  TO P237-SOKKG( I J ).
           MOVE TSZ-SYOKAKG TO P237-SYKKG( I J ).
           MOVE TSZ-SITENNO TO P237-STNNO( I J ).
           MOVE TSZ-SOKONO  TO P237-SOKNO( I J ).
           MOVE TSZ-SYOKANO TO P237-SYKNO( I J ).
           MOVE TSZ-SITEN   TO P237-STNME( I J ).
           MOVE TSZ-SOKME   TO P237-SOKME( I J ).
           MOVE TSZ-SYOKA   TO P237-SYKME( I J ).
      *
           IF TSZ-SITENNO = '988'
              MOVE TSZ-SITENNO TO P35-STNNO
              MOVE TSZ-SYOKANO TO P35-KANO
              MOVE '2'         TO P35-HNBCD
              CALL 'VSBA0350'  USING P35-PARM P34-PARM
              MOVE P35-HSTNO   TO ORG-STNCD
           ELSE
              MOVE TSZ-SITENNO TO ORG-STNCD.
      *
           MOVE P132-GATUDOX TO ORG-DNEN.
           MOVE '00'         TO ORG-BLKCD.
           MOVE TSZ-SOKONO   TO ORG-TENCD.
           MOVE TSZ-SYOKANO  TO ORG-SKANO.
           PERFORM M21-READ-ORG THRU M21-EXIT.
           MOVE ORG-STKNJ   TO P237-STNMJ( I J ).
           MOVE ORG-KAKNJ   TO P237-SYKMJ( I J ).
           MOVE ORG-TNKNJ   TO P237-SOKMJ( I J ).
           MOVE ORG-OTSEQ   TO P237-STJCD( I J ).
           MOVE ORG-SGIGY   TO P237-STNGK( I J ).
           MOVE ORG-KGIGY   TO P237-KAGKB( I J ).
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *   M2.1 : READ ORGFILE ( RANDOM READ )                          *
      *----------------------------------------------------------------*
       M21-READ-ORG.
           MOVE ORG-KEY TO KEY-ORG.
           MOVE 'Z '    TO S-CODE.
           READ ORGFILE INTO ORG-R
                KEY IS  KEY-ORG
                INVALID KEY
                        MOVE SPACE TO ORG-R
                        GO TO M21-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'R VSBA2370 ORGFILE READ ERROR !'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              CLOSE TBLFILE
                    ORGFILE
              CALL 'VSBABEND'.
       M21-EXIT.
           EXIT.
