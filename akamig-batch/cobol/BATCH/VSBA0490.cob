./ ADD NAME=VSBA0490  01.02 19870105 20140925 103430 00541 00523 MCSK082
       ID  DIVISION.                                                    VSB00020
       PROGRAM-ID.      'VSBA0490'.                                     VSB00030
      *----------------------------------------------------------------*VSB00040
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00050
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00060
      *              <> PG.OUTLINE  : SYOUHIMEI KANJI HENKAN           *VSB00070
      *                 *  INPUT.PARM :                                *VSB00090
      *                 *  OUTPUT.PARM :                               *VSB00110
      *              <> AUTHOR      : CSK-A.ONITSUKA                   *VSB00130
      *              <> DATE        : 86/06/05                         *VSB00140
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-1616           ØºÝÊß²Ù  : 14/09/25  CSK-K.KOYAMA        *
      * (02) XXXXXX XXXXXX      ÍÝº³   : YY/MM/DD  CSK-X.XXXXXX        *
      * (03) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSB00150
       ENVIRONMENT DIVISION.                                            VSB00160
       DATA DIVISION.                                                   VSB00170
       WORKING-STORAGE SECTION.                                         VSB00180
      *----------------------------------------------------------------*VSB00190
      *   WORKING-STORAGE SECTION                                      *VSB00200
      *----------------------------------------------------------------*VSB00210
       01  WK-OAREA.
           02 WK-OAREA1                PIC  X(1)
                                       OCCURS  1  TO  50
                                       DEPENDING  I.
       01  FILLER.
           02 WK-0E-DEC                PIC S9(04) COMP VALUE +014.
           02 FILLER  REDEFINES WK-0E-DEC.
              03 FILLER                PIC  X(01).
              03 WK-0E                 PIC  X(01).
           02 WK-0F-DEC                PIC S9(04) COMP VALUE +015.
           02 FILLER  REDEFINES WK-0F-DEC.
              03 FILLER                PIC  X(01).
              03 WK-0F                 PIC  X(01).
           02 WK-40-DEC                PIC S9(04) COMP VALUE +064.
           02 FILLER  REDEFINES WK-40-DEC.
              03 FILLER                PIC  X(01).
              03 WK-40                 PIC  X(01).
           02 WK-41-DEC                PIC S9(04) COMP VALUE +065.
           02 FILLER  REDEFINES WK-41-DEC.
              03 FILLER                PIC  X(01).
              03 WK-41                 PIC  X(01).
           02 WK-42-DEC                PIC S9(04) COMP VALUE +066.
           02 FILLER  REDEFINES WK-42-DEC.
              03 FILLER                PIC  X(01).
              03 WK-42                 PIC  X(01).
           02 WK-43-DEC                PIC S9(04) COMP VALUE +067.
           02 FILLER  REDEFINES WK-43-DEC.
              03 FILLER                PIC  X(01).
              03 WK-43                 PIC  X(01).
           02 WK-49-DEC                PIC S9(04) COMP VALUE +073.
           02 FILLER  REDEFINES WK-49-DEC.
              03 FILLER                PIC  X(01).
              03 WK-49                 PIC  X(01).
           02 WK-4A-DEC                PIC S9(04) COMP VALUE +074.
           02 FILLER  REDEFINES WK-4A-DEC.
              03 FILLER                PIC  X(01).
              03 WK-4A                 PIC  X(01).
           02 WK-50-DEC                PIC S9(04) COMP VALUE +080.
           02 FILLER  REDEFINES WK-50-DEC.
              03 FILLER                PIC  X(01).
              03 WK-50                 PIC  X(01).
           02 WK-51-DEC                PIC S9(04) COMP VALUE +081.
           02 FILLER  REDEFINES WK-51-DEC.
              03 FILLER                PIC  X(01).
              03 WK-51                 PIC  X(01).
           02 WK-56-DEC                PIC S9(04) COMP VALUE +086.
           02 FILLER  REDEFINES WK-56-DEC.
              03 FILLER                PIC  X(01).
              03 WK-56                 PIC  X(01).
           02 WK-58-DEC                PIC S9(04) COMP VALUE +088.
           02 FILLER  REDEFINES WK-58-DEC.
              03 FILLER                PIC  X(01).
              03 WK-58                 PIC  X(01).
           02 WK-5A-DEC                PIC S9(04) COMP VALUE +090.
           02 FILLER  REDEFINES WK-5A-DEC.
              03 FILLER                PIC  X(01).
              03 WK-5A                 PIC  X(01).
           02 WK-61-DEC                PIC S9(04) COMP VALUE +097.
           02 FILLER  REDEFINES WK-61-DEC.
              03 FILLER                PIC  X(01).
              03 WK-61                 PIC  X(01).
           02 WK-6A-DEC                PIC S9(04) COMP VALUE +106.
           02 FILLER  REDEFINES WK-6A-DEC.
              03 FILLER                PIC  X(01).
              03 WK-6A                 PIC  X(01).
           02 WK-6F-DEC                PIC S9(04) COMP VALUE +111.
           02 FILLER  REDEFINES WK-6F-DEC.
              03 FILLER                PIC  X(01).
              03 WK-6F                 PIC  X(01).
           02 WK-79-DEC                PIC S9(04) COMP VALUE +121.
           02 FILLER  REDEFINES WK-79-DEC.
              03 FILLER                PIC  X(01).
              03 WK-79                 PIC  X(01).
           02 WK-7F-DEC                PIC S9(04) COMP VALUE +127.
           02 FILLER  REDEFINES WK-7F-DEC.
              03 FILLER                PIC  X(01).
              03 WK-7F                 PIC  X(01).
           02 WK-81-DEC                PIC S9(04) COMP VALUE +129.
           02 FILLER  REDEFINES WK-81-DEC.
              03 FILLER                PIC  X(01).
              03 WK-81                 PIC  X(01).
           02 WK-83-DEC                PIC S9(04) COMP VALUE +131.
           02 FILLER  REDEFINES WK-83-DEC.
              03 FILLER                PIC  X(01).
              03 WK-83                 PIC  X(01).
           02 WK-86-DEC                PIC S9(04) COMP VALUE +134.
           02 FILLER  REDEFINES WK-86-DEC.
              03 FILLER                PIC  X(01).
              03 WK-86                 PIC  X(01).
           02 WK-8A-DEC                PIC S9(04) COMP VALUE +138.
           02 FILLER  REDEFINES WK-8A-DEC.
              03 FILLER                PIC  X(01).
              03 WK-8A                 PIC  X(01).
           02 WK-8C-DEC                PIC S9(04) COMP VALUE +140.
           02 FILLER  REDEFINES WK-8C-DEC.
              03 FILLER                PIC  X(01).
              03 WK-8C                 PIC  X(01).
           02 WK-95-DEC                PIC S9(04) COMP VALUE +149.
           02 FILLER  REDEFINES WK-95-DEC.
              03 FILLER                PIC  X(01).
              03 WK-95                 PIC  X(01).
           02 WK-9A-DEC                PIC S9(04) COMP VALUE +154.
           02 FILLER  REDEFINES WK-9A-DEC.
              03 FILLER                PIC  X(01).
              03 WK-9A                 PIC  X(01).
           02 WK-9D-DEC                PIC S9(04) COMP VALUE +157.
           02 FILLER  REDEFINES WK-9D-DEC.
              03 FILLER                PIC  X(01).
              03 WK-9D                 PIC  X(01).
           02 WK-9F-DEC                PIC S9(04) COMP VALUE +159.
           02 FILLER  REDEFINES WK-9F-DEC.
              03 FILLER                PIC  X(01).
              03 WK-9F                 PIC  X(01).
           02 WK-A2-DEC                PIC S9(04) COMP VALUE +162.
           02 FILLER  REDEFINES WK-A2-DEC.
              03 FILLER                PIC  X(01).
              03 WK-A2                 PIC  X(01).
           02 WK-A3-DEC                PIC S9(04) COMP VALUE +163.
           02 FILLER  REDEFINES WK-A3-DEC.
              03 FILLER                PIC  X(01).
              03 WK-A3                 PIC  X(01).
           02 WK-AA-DEC                PIC S9(04) COMP VALUE +170.
           02 FILLER  REDEFINES WK-AA-DEC.
              03 FILLER                PIC  X(01).
              03 WK-AA                 PIC  X(01).
           02 WK-AC-DEC                PIC S9(04) COMP VALUE +172.
           02 FILLER  REDEFINES WK-AC-DEC.
              03 FILLER                PIC  X(01).
              03 WK-AC                 PIC  X(01).
           02 WK-AF-DEC                PIC S9(04) COMP VALUE +175.
           02 FILLER  REDEFINES WK-AF-DEC.
              03 FILLER                PIC  X(01).
              03 WK-AF                 PIC  X(01).
           02 WK-BA-DEC                PIC S9(04) COMP VALUE +186.
           02 FILLER  REDEFINES WK-BA-DEC.
              03 FILLER                PIC  X(01).
              03 WK-BA                 PIC  X(01).
           02 WK-BD-DEC                PIC S9(04) COMP VALUE +189.
           02 FILLER  REDEFINES WK-BD-DEC.
              03 FILLER                PIC  X(01).
              03 WK-BD                 PIC  X(01).
           02 WK-BE-DEC                PIC S9(04) COMP VALUE +190.
           02 FILLER  REDEFINES WK-BE-DEC.
              03 FILLER                PIC  X(01).
              03 WK-BE                 PIC  X(01).
           02 WK-BF-DEC                PIC S9(04) COMP VALUE +191.
           02 FILLER  REDEFINES WK-BF-DEC.
              03 FILLER                PIC  X(01).
              03 WK-BF                 PIC  X(01).
           02 WK-C1-DEC                PIC S9(04) COMP VALUE +193.
           02 FILLER  REDEFINES WK-C1-DEC.
              03 FILLER                PIC  X(01).
              03 WK-C1                 PIC  X(01).
           02 WK-C9-DEC                PIC S9(04) COMP VALUE +201.
           02 FILLER  REDEFINES WK-C9-DEC.
              03 FILLER                PIC  X(01).
              03 WK-C9                 PIC  X(01).
           02 WK-D1-DEC                PIC S9(04) COMP VALUE +209.
           02 FILLER  REDEFINES WK-D1-DEC.
              03 FILLER                PIC  X(01).
              03 WK-D1                 PIC  X(01).
           02 WK-D9-DEC                PIC S9(04) COMP VALUE +217.
           02 FILLER  REDEFINES WK-D9-DEC.
              03 FILLER                PIC  X(01).
              03 WK-D9                 PIC  X(01).
           02 WK-E0-DEC                PIC S9(04) COMP VALUE +224.
           02 FILLER  REDEFINES WK-E0-DEC.
              03 FILLER                PIC  X(01).
              03 WK-E0                 PIC  X(01).
           02 WK-E2-DEC                PIC S9(04) COMP VALUE +226.
           02 FILLER  REDEFINES WK-E2-DEC.
              03 FILLER                PIC  X(01).
              03 WK-E2                 PIC  X(01).
           02 WK-E9-DEC                PIC S9(04) COMP VALUE +233.
           02 FILLER  REDEFINES WK-E9-DEC.
              03 FILLER                PIC  X(01).
              03 WK-E9                 PIC  X(01).
           02 WK-F0-DEC                PIC S9(04) COMP VALUE +240.
           02 FILLER  REDEFINES WK-F0-DEC.
              03 FILLER                PIC  X(01).
              03 WK-F0                 PIC  X(01).
           02 WK-F9-DEC                PIC S9(04) COMP VALUE +249.
           02 FILLER  REDEFINES WK-F9-DEC.
              03 FILLER                PIC  X(01).
              03 WK-F9                 PIC  X(01).
           02 WK-DAKU-TBL.
              03 FILLER                PIC  X(2) VALUE      '¶Þ'.
              03 FILLER                PIC  X(2) VALUE      '·Þ'.
              03 FILLER                PIC  X(2) VALUE      '¸Þ'.
              03 FILLER                PIC  X(2) VALUE      '¹Þ'.
              03 FILLER                PIC  X(2) VALUE      'ºÞ'.
              03 FILLER                PIC  X(2) VALUE      '»Þ'.
              03 FILLER                PIC  X(2) VALUE      '¼Þ'.
              03 FILLER                PIC  X(2) VALUE      '½Þ'.
              03 FILLER                PIC  X(2) VALUE      '¾Þ'.
              03 FILLER                PIC  X(2) VALUE      '¿Þ'.
              03 FILLER                PIC  X(2) VALUE      'ÀÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÁÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÂÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÃÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÄÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÊÞ'.
              03 FILLER                PIC  X(2) VALUE      'ËÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÌÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÍÞ'.
              03 FILLER                PIC  X(2) VALUE      'ÎÞ'.
              03 FILLER                PIC  X(2) VALUE      '³Þ'.
              03 FILLER                PIC  X(2) VALUE      'Êß'.
              03 FILLER                PIC  X(2) VALUE      'Ëß'.
              03 FILLER                PIC  X(2) VALUE      'Ìß'.
              03 FILLER                PIC  X(2) VALUE      'Íß'.
              03 FILLER                PIC  X(2) VALUE      'Îß'.
           02 FILLER  REDEFINES WK-DAKU-TBL.
              03 WK-DAKU               PIC  X(2) OCCURS  26.
           02 WK-IAREA.
              03 WK-IAREA1             PIC  X(1)
                                       OCCURS 25.
           02 WK-TAREA.
              03 WK-TAREA1             PIC  X(1).
              03 WK-TAREA2             PIC  X(1).
           02 I                        PIC S9(4) COMP.
           02 J                        PIC S9(4) COMP.
           02 K                        PIC S9(4) COMP.
           02 L                        PIC S9(4) COMP.
           02 L-X   REDEFINES L        PIC  X(2).
           02 WK-SFT                   PIC  X(1).
           02 WK-SYKYKJ                PIC  X(40) JUST RIGHT.
       LINKAGE SECTION.                                                 VSB00250
      *----------------------------------------------------------------*VSB00260
      *   LINKAGE SECTION                                              *VSB00270
      *----------------------------------------------------------------*VSB00280
       01  PARAM.                                                       VSB00290
           COPY VPARB49  SUPPRESS.                                      VSB00300
       PROCEDURE DIVISION USING PARAM.                                  VSB00310
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           IF P49-HNBCD = '1' OR '3'
              PERFORM M1-SYHKK  THRU M1-EXIT.
           IF P49-HNBCD = '2' OR '3'
              PERFORM M2-SYKYKK THRU M2-EXIT.
           GOBACK.                                                      00610000
      *----------------------------------------------------------------*
      *    M1 : SYOHIMEI HENKAN                                        *
      *----------------------------------------------------------------*
       M1-SYHKK.
           MOVE ZERO  TO WK-SFT.
           MOVE P49-SYHKK TO WK-IAREA.
           MOVE 50    TO I.
           MOVE ZERO  TO J.
           MOVE ZERO  TO K.
       M1-1.
           ADD 1 TO J.
           IF J > 25 GO TO M1-9.
           IF WK-IAREA1 (J) = WK-0E
              MOVE '1' TO WK-SFT
              GO TO M1-1.
           IF WK-IAREA1 (J) = WK-0F
              MOVE ZERO TO WK-SFT
              GO TO M1-1.
           IF WK-SFT = '1'
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              ADD 1 TO J
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              GO TO M1-1.
           IF WK-IAREA1 (J) NOT < WK-9D AND
              WK-IAREA1 (J) NOT > WK-9F  OR
              WK-IAREA1 (J) NOT < WK-A2 AND
              WK-IAREA1 (J) NOT > WK-A3
              COMPUTE L = J + 1
              IF L > 25
                 GO TO M1-2
              ELSE
                 IF WK-IAREA1 (L) = WK-BE OR WK-BF
                    GO TO M1-3
                 ELSE
                    GO TO M1-2.
           IF WK-IAREA1 (J) NOT < WK-86 AND
              WK-IAREA1 (J) NOT > WK-8A  OR
              WK-IAREA1 (J) NOT < WK-8C AND
              WK-IAREA1 (J) NOT > WK-95  OR
              WK-IAREA1 (J) =  WK-83
              COMPUTE L = J + 1
              IF L > 25
                 GO TO M1-2
              ELSE
                 IF WK-IAREA1 (L) = WK-BE
                    GO TO M1-3
                 ELSE
                    GO TO M1-2.
       M1-2.
           IF WK-IAREA1 (J) = WK-40
              ADD 1 TO K
              MOVE WK-40 TO WK-OAREA1 (K)
              ADD 1 TO K
              MOVE WK-40 TO WK-OAREA1 (K)
              GO TO M1-1.
           IF WK-IAREA1 (J) NOT < WK-4A AND
              WK-IAREA1 (J) NOT > WK-50  OR
              WK-IAREA1 (J) NOT < WK-5A AND
              WK-IAREA1 (J) NOT > WK-61  OR
              WK-IAREA1 (J) NOT < WK-6A AND
              WK-IAREA1 (J) NOT > WK-6F  OR
              WK-IAREA1 (J) NOT < WK-79 AND
              WK-IAREA1 (J) NOT > WK-7F  OR
              WK-IAREA1 (J) NOT < WK-C1 AND
              WK-IAREA1 (J) NOT > WK-C9  OR
              WK-IAREA1 (J) NOT < WK-D1 AND
              WK-IAREA1 (J) NOT > WK-D9  OR
              WK-IAREA1 (J) NOT < WK-E2 AND
              WK-IAREA1 (J) NOT > WK-E9  OR
              WK-IAREA1 (J) NOT < WK-F0 AND
              WK-IAREA1 (J) NOT > WK-F9  OR
              WK-IAREA1 (J) = WK-E0
              ADD 1 TO K
              MOVE WK-42 TO WK-OAREA1 (K)
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              GO TO M1-1.
           IF WK-IAREA1 (J) NOT < WK-41 AND
              WK-IAREA1 (J) NOT > WK-49  OR
              WK-IAREA1 (J) NOT < WK-51 AND
              WK-IAREA1 (J) NOT > WK-56  OR
              WK-IAREA1 (J) NOT < WK-81 AND
              WK-IAREA1 (J) NOT > WK-8A  OR
              WK-IAREA1 (J) NOT < WK-8C AND
              WK-IAREA1 (J) NOT > WK-9A  OR
              WK-IAREA1 (J) NOT < WK-9D AND
              WK-IAREA1 (J) NOT > WK-9F  OR
              WK-IAREA1 (J) NOT < WK-A2 AND
              WK-IAREA1 (J) NOT > WK-AA  OR
              WK-IAREA1 (J) NOT < WK-AC AND
              WK-IAREA1 (J) NOT > WK-AF  OR
              WK-IAREA1 (J) NOT < WK-BA AND
              WK-IAREA1 (J) NOT > WK-BF  OR
              WK-IAREA1 (J) = WK-58
              ADD 1 TO K
              MOVE WK-43 TO WK-OAREA1 (K)
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              GO TO M1-1.
           DISPLAY 'C VSBA0490 HENKAN-ERROR HINMEI=' P49-SYHKK
               UPON CONSOLE.
           ADD 1 TO K.
           MOVE WK-40 TO WK-OAREA1 (K).
           ADD 1 TO K.
           MOVE WK-40 TO WK-OAREA1 (K).
           GO TO M1-1.
       M1-3.
           MOVE WK-IAREA1 (J) TO WK-TAREA1.
           MOVE WK-IAREA1 (L) TO WK-TAREA2.
           MOVE ZERO TO L.
       M1-4.
           ADD 1 TO L.
           IF L > 26
              DISPLAY 'C VSBA0490 HENKAN-ERROR HINMEI=' P49-SYHKK
                  UPON CONSOLE
               ADD 1 TO K
               MOVE WK-40 TO WK-OAREA1 (K)
               ADD 1 TO K
               MOVE WK-40 TO WK-OAREA1 (K)
               ADD 1 TO J
               GO TO M1-1.
           IF WK-DAKU (L) NOT = WK-TAREA
              GO TO M1-4.
           ADD 191 TO L.
           MOVE L-X TO WK-TAREA.
           ADD 1 TO K.
           MOVE WK-43 TO WK-OAREA1 (K).
           ADD 1 TO K.
           MOVE WK-TAREA2 TO WK-OAREA1 (K).
           ADD 1 TO J.
           GO TO M1-1.
       M1-9.
           MOVE K TO I.
           MOVE WK-OAREA TO P49-SYHKJ.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : KIKAKU YOURYOU HENKAN                                  *
      *----------------------------------------------------------------*
       M2-SYKYKK.
           MOVE ZERO  TO WK-SFT.
           MOVE P49-SYKYKK TO WK-IAREA.
           MOVE 40    TO I.
           MOVE ZERO  TO J.
           INSPECT P49-SYKYKK TALLYING J FOR LEADING SPACE.
           MOVE ZERO  TO K.
       M2-1.
           ADD 1 TO J.
           IF J > 20 GO TO M2-9.
           IF WK-IAREA1 (J) = WK-0E
              MOVE '1' TO WK-SFT
              GO TO M2-1.
           IF WK-IAREA1 (J) = WK-0F
              MOVE ZERO TO WK-SFT
              GO TO M2-1.
           IF WK-SFT = '1'
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              ADD 1 TO J
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              GO TO M2-1.
           IF WK-IAREA1 (J) NOT < WK-9D AND
              WK-IAREA1 (J) NOT > WK-9F  OR
              WK-IAREA1 (J) NOT < WK-A2 AND
              WK-IAREA1 (J) NOT > WK-A3
              COMPUTE L = J + 1
              IF L > 20
                 GO TO M2-2
              ELSE
                 IF WK-IAREA1 (L) = WK-BE OR WK-BF
                    GO TO M2-3
                 ELSE
                    GO TO M2-2.
           IF WK-IAREA1 (J) NOT < WK-86 AND
              WK-IAREA1 (J) NOT > WK-8A  OR
              WK-IAREA1 (J) NOT < WK-8C AND
              WK-IAREA1 (J) NOT > WK-95  OR
              WK-IAREA1 (J) = WK-83
              COMPUTE L = J + 1
              IF L > 20
                 GO TO M2-2
              ELSE
                 IF WK-IAREA1 (L) = WK-BE
                    GO TO M2-3
                 ELSE
                    GO TO M2-2.
       M2-2.
           IF WK-IAREA1 (J) = WK-40
              ADD 1 TO K
              MOVE WK-40 TO WK-OAREA1 (K)
              ADD 1 TO K
              MOVE WK-40 TO WK-OAREA1 (K)
              GO TO M2-1.
           IF WK-IAREA1 (J) NOT < WK-4A AND
              WK-IAREA1 (J) NOT > WK-50  OR
              WK-IAREA1 (J) NOT < WK-5A AND
              WK-IAREA1 (J) NOT > WK-61  OR
              WK-IAREA1 (J) NOT < WK-6A AND
              WK-IAREA1 (J) NOT > WK-6F  OR
              WK-IAREA1 (J) NOT < WK-79 AND
              WK-IAREA1 (J) NOT > WK-7F  OR
              WK-IAREA1 (J) NOT < WK-C1 AND
              WK-IAREA1 (J) NOT > WK-C9  OR
              WK-IAREA1 (J) NOT < WK-D1 AND
              WK-IAREA1 (J) NOT > WK-D9  OR
              WK-IAREA1 (J) NOT < WK-E2 AND
              WK-IAREA1 (J) NOT > WK-E9  OR
              WK-IAREA1 (J) NOT < WK-F0 AND
              WK-IAREA1 (J) NOT > WK-F9  OR
              WK-IAREA1 (J) = WK-E0
              ADD 1 TO K
              MOVE WK-42 TO WK-OAREA1 (K)
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              GO TO M2-1.
           IF WK-IAREA1 (J) NOT < WK-41 AND
              WK-IAREA1 (J) NOT > WK-49  OR
              WK-IAREA1 (J) NOT < WK-51 AND
              WK-IAREA1 (J) NOT > WK-56  OR
              WK-IAREA1 (J) NOT < WK-81 AND
              WK-IAREA1 (J) NOT > WK-8A  OR
              WK-IAREA1 (J) NOT < WK-8C AND
              WK-IAREA1 (J) NOT > WK-9A  OR
              WK-IAREA1 (J) NOT < WK-9D AND
              WK-IAREA1 (J) NOT > WK-9F  OR
              WK-IAREA1 (J) NOT < WK-A2 AND
              WK-IAREA1 (J) NOT > WK-AA  OR
              WK-IAREA1 (J) NOT < WK-AC AND
              WK-IAREA1 (J) NOT > WK-AF  OR
              WK-IAREA1 (J) NOT < WK-BA AND
              WK-IAREA1 (J) NOT > WK-BF  OR
              WK-IAREA1 (J) = WK-58
              ADD 1 TO K
              MOVE WK-43 TO WK-OAREA1 (K)
              ADD 1 TO K
              MOVE WK-IAREA1 (J) TO WK-OAREA1 (K)
              GO TO M2-1.
           DISPLAY 'C VSBA0490 HENKAN-ERROR KIKAKU/YORYO=' P49-SYKYKK
               UPON CONSOLE.
           ADD 1 TO K.
           MOVE WK-40 TO WK-OAREA1 (K).
           ADD 1 TO K.
           MOVE WK-40 TO WK-OAREA1 (K).
           GO TO M2-1.
       M2-3.
           MOVE WK-IAREA1 (J) TO WK-TAREA1.
           MOVE WK-IAREA1 (L) TO WK-TAREA2.
           MOVE ZERO TO L.
       M2-4.
           ADD 1 TO L.
           IF L > 26
               DISPLAY 'C VSBA0490 HENKAN-ERROR KIKAKU/YORYO='
                  P49-SYKYKK UPON CONSOLE
               ADD 1 TO K
               MOVE WK-40 TO WK-OAREA1 (K)
               ADD 1 TO K
               MOVE WK-40 TO WK-OAREA1 (K)
               ADD 1 TO J
               GO TO M2-1.
           IF WK-DAKU (L) NOT = WK-TAREA
              GO TO M2-4.
           ADD 191 TO L.
           MOVE L-X TO WK-TAREA.
           ADD 1 TO K.
           MOVE WK-43 TO WK-OAREA1 (K).
           ADD 1 TO K.
           MOVE WK-TAREA2 TO WK-OAREA1 (K).
           ADD 1 TO J.
           GO TO M2-1.
       M2-9.
           MOVE K TO I.
           MOVE WK-OAREA  TO WK-SYKYKJ.
           MOVE WK-SYKYKJ TO P49-SYKYKJ.
       M2-EXIT.
           EXIT.
