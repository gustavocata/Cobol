       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEP010.
      *******************************
      * CONSULTA DE CENTRO DE CUSTO *
      *******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT CADCEP   ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CEP
               FILE STATUS  IS ST-ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
      *COPY CADCEP.BOK.
      *
      *****************************************************************
      * ARQUIVO : CADCEP     - CADASTRO DE CENTROS DE CUSTOS          *
      * SISTEMA PRINCIPAL :                                           *
      * TAMANHO :  256  BYTES       FATOR DE BLOCO : 1                *
      * ORGANIZACAO : INDEXADO                                        *
      * CHAVE 1     : CEP  -  CODIGO DO CENTRO DE CUSTO          *
      * CHAVE 2     : LOGRADOURO  -  NOME DO CENTRO DE CUSTO            *
      * ANOCISTA RESPONSAVEL : ANGELO LOTIERSO FILHO                  *
      * DATA DEFINICAO : 03/05/95    DATA ULTIMA MANUT. : 03/05/95    *
      *****************************************************************
      *
       FD CADCEP
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS W-CADCEP.
       01 CADCEP.
           03 CEP.
              05 CEP1          PIC 9(08).
              05 BAIRRO        PIC X(25).
              05 CIDADE        PIC X(25).
              05 NUM           PIC 9(08).
              05 UF            PIC X(02). 
              05 SITUAÇAO      PIC X(01).
           03 LOGRADOURO.
              05 LOGRADOURO1   PIC X(35).
           03 FILLER           PIC X(21).
      *
      *****************************************************************
      *SEQ.*  NOME        *   DENOMINACAO              *TIPO * TAMANHO*
      *****************************************************************
      * 01 * CEP1    * CODIGO DO CENTO DE CUSTO   *  N  *   03   *
      * 02 * LOGRADOURO1     * DENOMINACAO                *  X  *   40   *
      * 13 * FILLER       *                            *  X  *   21   *
      *****************************************************************
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 W-CADCEP     PIC X(40) VALUE "CADCEP.DAT                    ".
       01 CONLIN       PIC 9(03) VALUE 001.
       01 RCEP         PIC 9(08) VALUE ZEROS.
       01 RLOGRADOURO  PIC X(25) VALUE SPACES.
      *-----------------------------------------------------------------
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DET2.
          03 FILLER    PIC X(16) VALUE SPACES.
          03 DET21     PIC 9(03) VALUE ZEROS.
          03 FILLER    PIC X(04) VALUE SPACES.
          03 DET22     PIC X(40) VALUE SPACES.
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELACEP010.
           5  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 01  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05  LINE 02  COLUMN 01
               VALUE  "º                      *** CONSULTA DE C".
           05  LINE 02  COLUMN 41
               VALUE  "ENTROS DE CUSTOS ***                   º".
           05  LINE 03  COLUMN 01
               VALUE  "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶".
           05  LINE 04  COLUMN 01
               VALUE  "º              CODIGO                 DE".
           05  LINE 04  COLUMN 41
               VALUE  "NOMINACAO                              º".
           05  LINE 05  COLUMN 01
               VALUE  "º              ------   ----------------".
           05  LINE 05  COLUMN 41
               VALUE  "------------------------               º".
           05  LINE 06  COLUMN 01
               VALUE  "º".
           05  LINE 06  COLUMN 41
               VALUE  "                                       º".
           05  LINE 07  COLUMN 01
               VALUE  "º".
           05  LINE 07  COLUMN 41
               VALUE  "                                       º".
           05  LINE 08  COLUMN 01
               VALUE  "º".
           05  LINE 08  COLUMN 41
               VALUE  "                                       º".
           05  LINE 09  COLUMN 01
               VALUE  "º".
           05  LINE 09  COLUMN 41
               VALUE  "                                       º".
           05  LINE 10  COLUMN 01
               VALUE  "º".
           05  LINE 10  COLUMN 41
               VALUE  "                                       º".
           05  LINE 11  COLUMN 01
               VALUE  "º".
           05  LINE 11  COLUMN 41
               VALUE  "                                       º".
           05  LINE 12  COLUMN 01
               VALUE  "º".
           05  LINE 12  COLUMN 41
               VALUE  "                                       º".
           05  LINE 13  COLUMN 01
               VALUE  "º".
           05  LINE 13  COLUMN 41
               VALUE  "                                       º".
           05  LINE 14  COLUMN 01
               VALUE  "º".
           05  LINE 14  COLUMN 41
               VALUE  "                                       º".
           05  LINE 15  COLUMN 01
               VALUE  "º".
           05  LINE 15  COLUMN 41
               VALUE  "                                       º".
           05  LINE 16  COLUMN 01
               VALUE  "º".
           05  LINE 16  COLUMN 41
               VALUE  "                                       º".
           05  LINE 17  COLUMN 01
               VALUE  "º".
           05  LINE 17  COLUMN 41
               VALUE  "                                       º".
           05  LINE 18  COLUMN 01
               VALUE  "º".
           05  LINE 18  COLUMN 41
               VALUE  "                                       º".
           05  LINE 19  COLUMN 01
               VALUE  "º".
           05  LINE 19  COLUMN 41
               VALUE  "                                       º".
           05  LINE 20  COLUMN 01
               VALUE  "º".
           05  LINE 20  COLUMN 41
               VALUE  "                                       º".
           05  LINE 21  COLUMN 01
               VALUE  "º".
           05  LINE 21  COLUMN 41
               VALUE  "                                       º".
           05  LINE 22  COLUMN 01
               VALUE  "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 22  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¶".
           05  LINE 23  COLUMN 01
               VALUE  "ºMENSAGEM :".
           05  LINE 23  COLUMN 41
               VALUE  "                              ³ OCP038 º".
           05  LINE 24  COLUMN 01
               VALUE  "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 24  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍ¼".
           05  T-EMPRESA
               LINE 02  COLUMN 02  PIC X(20)
               USING  W-EMPRESA
               HIGHLIGHT.
           05  DATELA
               LINE 02  COLUMN 69  PIC XX/XX/XX
               USING  DATA-EQ
               HIGHLIGHT.
       01 OCT038T.
           05  OCT03801
               LINE 06  COLUMN 18  PIC 9(03)
               USING  OCW03801.
           05  OCT03802
               LINE 06  COLUMN 25  PIC X(40)
               USING  OCW03802.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           ACCEPT DATA-EQ FROM DATE.
      *
       ROT-INC.
           MOVE   DATA-EQ TO DATA-INV.
           PERFORM INVNOR.
           MOVE DATA-N TO DATA-EQ.
       INC-OP1.
           OPEN I-O   CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CENTROS DE CUSTOS NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO  " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
           ELSE
               NEXT SENTENCE.
      *
       INC-001.
           DISPLAY  TELACEP010.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/DENOMIN."
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCEP
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE RCEP TO CEP1
           START CADCEP KEY IS NOT LESS CEP INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/CODIGO "
           ACCEPT TLOGRADOURO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCEP
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE RLOGRADOURO TO LOGRADOURO1
           START CADCEP KEY IS NOT LESS LOGRADOURO INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADCEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE CEP1 TO DET21
           MOVE LOGRADOURO1 TO DET22
           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 02) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACEP010
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  MOVE ZEROS  TO RCEP
                  MOVE SPACES TO RLOGRADOURO
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADCEP.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
    
      *---------------------*** FIM DE PROGRAMA ***--------------------*