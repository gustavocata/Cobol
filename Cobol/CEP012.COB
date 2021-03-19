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
          03 CEP              PIC 9(03).
          03 LOGRADOURO       PIC X(40).
          05 BAIRRO           PIC X(25).
          05 CIDADE           PIC X(25).
          05 NUM              PIC 9(08).
          05 UF               PIC X(02).
          05 SITUAÇAO         PIC X(01).
          03 FILLER           PIC X(21).
      *
      *****************************************************************
      *SEQ.*  NOME        *   DENOMINACAO              *TIPO * TAMANHO*
      *****************************************************************
      * 01 * CEP     * CODIGO DO CENTO DE CUSTO   *  N  *   03   *
      * 02 * LOGRADOURO     * DENOMINACAO                *  X  *   40   *
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
       01 OCW03801     PIC 9(06) VALUE ZEROS.
       01 OCW03802     PIC X(35) VALUE SPACES.
       01 OCW03803     PIC X(12) VALUE SPACES.
       01 OCW03810     PIC 9(15) VALUE ZEROS.
       01 W-EMPRESA    PIC X(20) VALUE "FATEC ZL".
      *-----------------------------------------------------------------
      *              *** CAMPOS PARA TRATAMENTO DE DATA ***
       01 DATA-EQ.
          03 ANO-EQ    PIC 99.
          03 MES-EQ    PIC 99.
          03 DIA-EQ    PIC 99.
       01 DATA-N.
          03 DIA       PIC 99.
          03 MES       PIC 99.
          03 ANO       PIC 99.
       01 DATA-INV.
          03 ANO-INV   PIC 99.
          03 MES-INV   PIC 99.
          03 DIA-INV   PIC 99.
       01 DATA-FOR.
          03 DIA-FOR   PIC 99.
          03 BR1-FOR   PIC X(01) VALUE "/".
          03 MES-FOR   PIC 99.
          03 BR2-FOR   PIC X(01) VALUE "/".
          03 ANO-FOR   PIC 99.
       01 W-01         PIC 9(04).
       01 FLAG1        PIC 9(01).
      *-----------------------------------------------------------------
      *
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DET2.
          03 FILLER    PIC X(16) VALUE SPACES.
          03 DET21     PIC 9(03) VALUE ZEROS.
          03 FILLER    PIC X(04) VALUE SPACES.
          03 DET22     PIC X(40) VALUE SPACES.
          03 FILLER    PIC X(14) VALUE SPACES.
          03 DET23     PIC X(25) VALUE SPACES.
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELACEP12.
           05  LINE 01  COLUMN 01 
               VALUE  "                               CONSULTA".
           05  LINE 01  COLUMN 41 
               VALUE  "CEP".
           05  LINE 02  COLUMN 01 
               VALUE  "========================================".
           05  LINE 02  COLUMN 41 
               VALUE  "========================================".
           05  LINE 03  COLUMN 01 
               VALUE  "    CEP:         LOGRADOURO:".
           05  LINE 03  COLUMN 41 
               VALUE  " BAIRRO:        UF:       SITUACAO:".
           05  TCEP
               LINE 04  COLUMN 01  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 04  COLUMN 11  PIC X(20)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 04  COLUMN 33  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TUF
               LINE 04  COLUMN 55  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TSITUACAO
               LINE 04  COLUMN 64  PIC X(08)
               USING  SITUACAO
               HIGHLIGHT.
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
           DISPLAY  TELACEP12.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/DENOMIN."
           ACCEPT TELACEP1201
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
           MOVE OCW03801 TO CEP
           START CADCEP KEY IS NOT LESS CEP INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/CODIGO "
           ACCEPT TELACEP1202
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
           MOVE OCW03802 TO LOGRADOURO
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
           MOVE CEP TO DET21
           MOVE LOGRADOURO TO DET22
           MOVE BAIRRO TO DET23
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
                  DISPLAY TELACEP12
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
                  MOVE ZEROS  TO OCW03801
                  MOVE SPACES TO OCW03802
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
      *
      ************************************
      *      *** ROTINA DE DATA ***      *
      ************************************
      *
      *
      *
       FORNOR.
           MOVE DIA-FOR TO DIA
           MOVE MES-FOR TO MES
           MOVE ANO-FOR TO ANO.
      *
      * FUNCAO : CONVERTE DATA FORMATADA EM NORMAL
      *
       NORINV.
           MOVE DIA     TO DIA-INV DIA-FOR
           MOVE MES     TO MES-INV MES-FOR
           MOVE ANO     TO ANO-INV ANO-FOR.
      *
      * FUNCAO : CONVERTE DATA NORMAL P/INVERTIDA E FORMATADA
      *
       INVNOR.
           MOVE DIA-INV TO DIA DIA-FOR
           MOVE MES-INV TO MES MES-FOR
           MOVE ANO-INV TO ANO ANO-FOR.
      *
      * FUNCAO : CONVERTE DATA INVERTIDA PARA NORMA
      *
       VALDATA.
           MOVE 0 TO FLAG1.
           GO TO D31 D28 D31 D30 D31 D30 D31 D31 D30 D31 D30 D31
                                          DEPENDING ON MES
           MOVE 1 TO FLAG1
           GO TO VALDATA-FIM.
       D31.
           IF DIA > 31
              MOVE 2 TO FLAG1
              GO TO VALDATA-FIM.
       D30.
           IF DIA > 30
              MOVE 2 TO FLAG1
              GO TO VALDATA-FIM.
       D28.
           COMPUTE W-01 = ANO / 4
           COMPUTE W-01 = W-01 * 4
           COMPUTE W-01 = ANO - W-01
           IF W-01 = ZEROS
                IF DIA > 29
                  MOVE 2 TO FLAG1
                  GO TO VALDATA-FIM
                ELSE
                  IF DIA > 28
                    MOVE 2 TO FLAG1
                    GO TO VALDATA-FIM.
       VALDATA-FIM.
      *
      *---------------------*** FIM DE PROGRAMA ***--------------------*