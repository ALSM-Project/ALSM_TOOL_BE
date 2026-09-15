./ ADD NAME=PILOTPRT  10.02 19840907 20011225 143600 00091 00091 TSO62
010010 IDENTIFICATION  DIVISION.                                        PILOTPRT
010020 PROGRAM-ID. PILOTPRT                                             PILOTPRT
010030 DATE-WRITTEN.   04/30/74.                                        PILOTPRT
010040 DATE-COMPILED.  04/30/74.                                        PILOTPRT
010050*                                                                 PILOTPRT
010060*                                                                 PILOTPRT
010070*                                                                 PILOTPRT
010080*                                                                 PILOTPRT
020010 ENVIRONMENT DIVISION.                                            PILOTPRT
020020 CONFIGURATION   SECTION.                                         PILOTPRT
020030 SOURCE-COMPUTER.    IBM-370.                                     PILOTPRT
020040 OBJECT-COMPUTER.    IBM-370.                                     PILOTPRT
020050*                                                                 PILOTPRT
020060 SPECIAL-NAMES.                                                   PILOTPRT
020070     C01     IS  SKPCH01.                                         PILOTPRT
020080*                                                                 PILOTPRT
020090*                                                                 PILOTPRT
020100 INPUT-OUTPUT    SECTION.                                         PILOTPRT
020110 FILE-CONTROL.                                                    PILOTPRT
020120     SELECT  PRTFILE ASSIGN  UT-S-PRT1.                           PILOTPRT
020130*                                                                 PILOTPRT
020140*                                                                 PILOTPRT
020150*                                                                 PILOTPRT
030010 DATA    DIVISION.                                                PILOTPRT
030020 FILE    SECTION.                                                 PILOTPRT
030030*                                                                 PILOTPRT
030040 FD  PRTFILE BLOCK CONTAINS 0 RECORDS                             PILOTPRT
030050     RECORDING   MODE    IS  F                                    PILOTPRT
030060     LABEL   RECORD      IS  OMITTED                              PILOTPRT
030070     DATA    RECORD      IS  PRTREC.                              PILOTPRT
030080*                                                                 PILOTPRT
030090 01  PRTREC.                                                      PILOTPRT
030110     02  PRTDATA         PICTURE X(132).                          PILOTPRT
030120*                                                                 PILOTPRT
030130*                                                                 PILOTPRT
030140*                                                                 PILOTPRT
100010 WORKING-STORAGE SECTION.                                         PILOTPRT
100020*                                                                 PILOTPRT
100030 01  DATAPRT.                                                     PILOTPRT
100040     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100050     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100060     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100070     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100080     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100090     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100100     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100110     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100120     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100130     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100140     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100150     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100160     02  FILLER          PICTURE X(10)   VALUE   '1234567890'.    PILOTPRT
100170     02  FILLER          PICTURE X(2)    VALUE   '12'.            PILOTPRT
100180*                                                                 PILOTPRT
100190*                                                                 PILOTPRT
100200*                                                                 PILOTPRT
110010 01  DISPA   PICTURE X(16)   VALUE   'M PILOTPRT BEGIN'.          PILOTPRT
110020 01  DISPB   PICTURE X(16)   VALUE   'M PILOTPRT END  '.          PILOTPRT
110030*                                                                 PILOTPRT
110040 01  PRINTPAGE           PICTURE 9(3)    VALUE   2.               PILOTPRT
110050 01  PRINTLINES          PICTURE 9(3)    VALUE   54.              PILOTPRT
110060 01  PRINTLINESW         PICTURE 9(3)    VALUE   0.               PILOTPRT
110070*                                                                 PILOTPRT
200010 PROCEDURE   DIVISION.                                            PILOTPRT
200020 STARTX.                                                          PILOTPRT
200030     DISPLAY DISPA   UPON    CONSOLE.                             PILOTPRT
200040     OPEN    OUTPUT  PRTFILE.                                     PILOTPRT
200050     MOVE    SPACE   TO  PRTREC.                                  PILOTPRT
200060     PERFORM PRTSUBA THRU    PRTSUBAEXIT PRINTPAGE   TIMES.       PILOTPRT
200070     CLOSE   PRTFILE.                                             PILOTPRT
200080     DISPLAY     DISPB   UPON    CONSOLE.                         PILOTPRT
200090     STOP    RUN.                                                 PILOTPRT
200100*                                                                 PILOTPRT
200110*                                                                 PILOTPRT
200120*                                                                 PILOTPRT
200130 PRTSUBA.                                                         PILOTPRT
200140     MOVE    DATAPRT     TO  PRTDATA.                             PILOTPRT
200150     WRITE   PRTREC  AFTER   SKPCH01.                             PILOTPRT
200160     COMPUTE PRINTLINESW =   PRINTLINES  -   1.                   PILOTPRT
200170     PERFORM PRTSUBB THRU    PRTSUBBEXIT PRINTLINESW TIMES.       PILOTPRT
200180 PRTSUBAEXIT.                                                     PILOTPRT
200190     EXIT.                                                        PILOTPRT
200200*                                                                 PILOTPRT
210010 PRTSUBB.                                                         PILOTPRT
210020     MOVE    DATAPRT     TO  PRTDATA                              PILOTPRT
210030     WRITE   PRTREC  AFTER   1   LINES.                           PILOTPRT
210040 PRTSUBBEXIT.                                                     PILOTPRT
210050     EXIT.                                                        PILOTPRT
210060*                                                                 PILOTPRT
210070*                                                                 PILOTPRT
210080*                                                                 PILOTPRT
