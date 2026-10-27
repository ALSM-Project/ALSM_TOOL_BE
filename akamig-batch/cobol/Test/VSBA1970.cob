./ ADD NAME=VSBA1970  01.12 19990413 20050506 134627 00260 00169 MCSK115
       ID  DIVISION.
       PROGRAM-ID.      'VSBA1970'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN SHITEN SYSTEM            *
      *              <> SUBSYSTEM   : SUBROUTINE                       *
      *              <> PG.OUTLINE  : MOJI HENSYU                      *
      *                                                                *
      *               *  INPUT.PARM : P197-SORKB,P197-EDKET,P197-EDKOU *
      *               *  OUTPUT.PARM: P197-EDKOU,P197-RTNCD            *
      *                                                                *
      *              <> AUTHOR      : CSK-M.KUBOTA                     *
      *              <> DATE        : 95/10/23                         *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-148  S-0298      ÍÝº³   : 95/12/27  CSK-M.KUBOTA        *
      * (02) H-010  O-0913      ÍÝº³   : 96/12/05  CSK-K.WATANABE      *
      * (03) K-363              ÍÝº³   : 99/04/13  CSK-K.WATANABE      *
      * (04) K-686              ÍÝº³   : 05/04/11  CSK-T.HATTORI       *
      * (05) XXXXXX XXXXXX       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       01  COUNT-AREA.
           02 I                      PIC 9(03).
           02 J                      PIC 9(03).
           02 K                      PIC 9(03).
           02 L                      PIC 9(03).
           02 I-STA                  PIC 9(03).
           02 I-END                  PIC 9(03).
       01  TABLE-AREA.
           02 WTB1-MOJIX.
              03 FILLER              PIC X(01) VALUE X'5A'.
              03 FILLER              PIC X(01) VALUE X'7F'.
              03 FILLER              PIC X(01) VALUE X'E0'.
              03 FILLER              PIC X(01) VALUE X'6C'.
              03 FILLER              PIC X(01) VALUE X'50'.
              03 FILLER              PIC X(01) VALUE X'7D'.
              03 FILLER              PIC X(01) VALUE X'5C'.
              03 FILLER              PIC X(01) VALUE X'4E'.
              03 FILLER              PIC X(01) VALUE X'6B'.
              03 FILLER              PIC X(01) VALUE X'7A'.
              03 FILLER              PIC X(01) VALUE X'5E'.
              03 FILLER              PIC X(01) VALUE X'4C'.
              03 FILLER              PIC X(01) VALUE X'7E'.
              03 FILLER              PIC X(01) VALUE X'6E'.
              03 FILLER              PIC X(01) VALUE X'6F'.
              03 FILLER              PIC X(01) VALUE X'7C'.
              03 FILLER              PIC X(01) VALUE X'70'.
              03 FILLER              PIC X(01) VALUE X'5B'.
              03 FILLER              PIC X(01) VALUE X'80'.
              03 FILLER              PIC X(01) VALUE X'B0'.
              03 FILLER              PIC X(01) VALUE X'6D'.
              03 FILLER              PIC X(01) VALUE X'79'.
              03 FILLER              PIC X(01) VALUE X'C0'.
              03 FILLER              PIC X(01) VALUE X'4F'.
              03 FILLER              PIC X(01) VALUE X'D0'.
              03 FILLER              PIC X(01) VALUE X'A0'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
              03 FILLER              PIC X(01) VALUE X'40'.
           02 FILLER            REDEFINES WTB1-MOJIX.
              03 WTB1-MOJI      OCCURS  50  PIC  X(01).
       01  FLAG-AREA.
           02 WFL1-SW                PIC X(01).
       01  EDIT-AREA.
           02 WED1-IN-DATA           PIC X(200).
           02 WED1-IN-DATAX          REDEFINES WED1-IN-DATA.
              03 WED1-IN             PIC X(01)   OCCURS 200.
           02 WED1-OUT-DATA          PIC X(200).
           02 WED1-OUT-DATAX         REDEFINES WED1-OUT-DATA.
              03 WED1-OUT            PIC X(01)   OCCURS 200.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PARM.
           COPY VPARB197.
       PROCEDURE DIVISION USING PARM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1.0 : INITIAL                                              *
      *----------------------------------------------------------------*
       M1-INIT.
           MOVE ZERO  TO P197-RTNCD.
           MOVE SPACE TO EDIT-AREA.
           INITIALIZE COUNT-AREA.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.0 : MAIN CONTROL                                         *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF P197-SORKB NOT = '1' AND '2' AND '3' AND '4' AND '5'
              MOVE  '1' TO P197-RTNCD
              GO TO M2-EXIT.
           IF P197-EDKET = 1
              GO TO M2-EXIT
           ELSE
              IF P197-EDKET < 1  OR  P197-EDKET > 200
                 MOVE  '1' TO P197-RTNCD
                 GO TO M2-EXIT.
       M2-L01.
           MOVE P197-EDKOU TO WED1-IN-DATA.
           PERFORM  M21-SPACE-CHECK THRU M21-EXIT
                    VARYING I FROM 1 BY 1
                    UNTIL (I > P197-EDKET).
           IF I-STA = ZERO
              IF P197-SORKB = '1'
                 PERFORM  M22-ZERO-SET THRU M22-EXIT
                          VARYING  J FROM 1 BY 1
                          UNTIL   (J > P197-EDKET)
                 GO TO M2-L03
              ELSE
                 GO TO M2-EXIT.
       M2-L02.
           IF P197-SORKB = '1' OR '2' OR '3'
              PERFORM  M23-EDIT-123 THRU M23-EXIT
           ELSE
              IF P197-SORKB = '4'
                 MOVE  1  TO  J
                 PERFORM  M24-EDIT-4 THRU M24-EXIT
                          VARYING  I FROM I-STA BY 1
                          UNTIL   (I > I-END)
              ELSE
                 PERFORM M25-EDIT-5 THRU M25-EXIT
              END-IF
           END-IF.
       M2-L03.
           MOVE WED1-OUT-DATA TO P197-EDKOU.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : SPACE CHECK (SYORI='1' OR '2' OR '3' OR '4')         *
      *----------------------------------------------------------------*
       M21-SPACE-CHECK.
           IF WED1-IN(I) NOT = SPACE
              IF I-STA = ZERO
                 MOVE I TO I-STA
                 MOVE I TO I-END
              ELSE
                 MOVE I TO I-END.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : ZERO SET (SYORI='1')                                 *
      *----------------------------------------------------------------*
       M22-ZERO-SET.
           MOVE '0' TO WED1-OUT(J).
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : EDIT (SYORI='1' OR '2' OR '3')                       *
      *----------------------------------------------------------------*
       M23-EDIT-123.
           COMPUTE J = I-END - I-STA + 1.
           COMPUTE I = P197-EDKET - J.
           IF P197-SORKB = '1'
              PERFORM  M22-ZERO-SET THRU M22-EXIT
                       VARYING  J FROM 1 BY 1
                       UNTIL   (J > I).
           MOVE 1 TO K.
           IF P197-SORKB = '1' OR '2'
              PERFORM  M231-SET-12 THRU M231-EXIT
                       VARYING  J FROM I-STA BY 1
                       UNTIL   (J > I-END)
           ELSE
              PERFORM  M232-SET-3  THRU M232-EXIT
                       VARYING  J FROM I-STA BY 1
                       UNTIL   (J > I-END).
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1: SET      (SYORI='1' OR '2')                         *
      *----------------------------------------------------------------*
       M231-SET-12.
           COMPUTE L = I + K.
           MOVE WED1-IN(J) TO WED1-OUT(L)
           ADD  1  TO  K.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.2: SET      (SYORI='3')                                *
      *----------------------------------------------------------------*
       M232-SET-3.
           MOVE WED1-IN(J) TO WED1-OUT(K)
           ADD  1  TO  K.
       M232-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4 : EDIT SYORI = '4'                                     *
      *----------------------------------------------------------------*
       M24-EDIT-4.
           IF WED1-IN(I) NOT =  SPACE
              MOVE WED1-IN(I) TO WED1-OUT(J)
              ADD  1  TO  J.
       M24-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.5 : EDIT SYORI = '5'                                     *
      *----------------------------------------------------------------*
       M25-EDIT-5.
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL  (I            > 50   ) OR
                          (WTB1-MOJI(I) = X'40')
             INSPECT WED1-IN-DATA REPLACING
                     ALL WTB1-MOJI(I) BY SPACE
           END-PERFORM.
           IF WED1-IN-DATA NOT = SPACE
              MOVE SPACE TO WED1-OUT-DATA
              MOVE 1     TO I
              MOVE 1     TO J
              MOVE SPACE TO WFL1-SW
              PERFORM UNTIL I > 200
                IF (WED1-IN(I) = SPACE) AND
                   (WFL1-SW    = SPACE)
                   COMPUTE I = I + 1
                ELSE
                   MOVE WED1-IN(I) TO WED1-OUT(J)
                   COMPUTE I = I + 1
                   COMPUTE J = J + 1
                   MOVE 1 TO WFL1-SW
                 END-IF
              END-PERFORM
           END-IF.
       M25-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    VSBA1970 END                                                *
      *----------------------------------------------------------------*
