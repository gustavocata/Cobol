       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLAPROD.
       AUTHOR. GABRIEL ASSIS DE MORAES.
      **************************************
      * MANUTENCAO DO CADASTRO DE PRODUTO  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT PRODUTO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = DESCRICAO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD PRODUTO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "PRODUTO.DAT".
       01 REGPRO.
                03 CODIGO          PIC 9(03).
                03 DESCRICAO       PIC X(25).
                03 TIPO            PIC 9(01).
                03 IPI             PIC 99V99.
                03 ICMS            PIC 99V99.
                03 LUCRO           PIC 99v99.
                03 SITUACAO        PIC X(01).
      * 
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 TIPOPROD     PIC X(20) VALUE SPACES.
       01 TIPO1. 
                   05 FILLER PIC X(20) VALUE "ALIMENTAÇÃO".
                   05 FILLER PIC X(20) VALUE "HIGIENE".
                   05 FILLER PIC X(20) VALUE "SAÚDE".
                   05 FILLER PIC X(20) VALUE "VESTUARIO".
                   05 FILLER PIC X(20) VALUE "ARTEFATOS COZINHA".
                   05 FILLER PIC X(20) VALUE "ANIMAL".
                   05 FILLER PIC X(20) VALUE "LIMPEZA".
                   05 FILLER PIC X(20) VALUE "BEBIDA".
                   05 FILLER PIC X(20) VALUE "JARDINAGEM".
                   05 FILLER PIC X(20) VALUE "OUTROS".
       01 TABPRODUTO REDEFINES TIPO1.
                   05 TPROD PIC X(20) OCCURS 9 TIMES.
                   
      *-----------
       SCREEN SECTION.
       
        01  TELAPRODUTO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                        CLASSIFICA€AO DO".
           05  LINE 02  COLUMN 41 
               VALUE  "S PRODUTOS".
           05  LINE 03  COLUMN 01 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 05  COLUMN 01 
               VALUE  " CODIGO:                      DESCRICAOICAO:".
           05  LINE 09  COLUMN 01 
               VALUE  " IPI:                         TIPO:".
           05  LINE 13  COLUMN 01 
               VALUE  " ICMS:                        SITUA€AO:".
           05  LINE 17  COLUMN 01 
               VALUE  " LUCRO:".
           05  TCODIGO
               LINE 05  COLUMN 09  PIC 9(03)
               USING  CODIGO
               HIGHLIGHT.
           05  TDESCRICAOICAO
               LINE 05  COLUMN 41  PIC X(25)
               USING  DESCRICAOICAO
               HIGHLIGHT.
           05  TIPI
               LINE 09  COLUMN 06  PIC 99,99
               USING  IPI
               HIGHLIGHT.
           05  TTIPOPROD
               LINE 09  COLUMN 36  PIC 9(01)
               USING  TIPO
               HIGHLIGHT.
           05  TICMS
               LINE 13  COLUMN 07  PIC 99,99
               USING  ICMS
               HIGHLIGHT.
           05  TSITUACAO
               LINE 13  COLUMN 40  PIC X(01)
               USING  SITUACAO
               HIGHLIGHT.
           05  TLUCRO
               LINE 17  COLUMN 08  PIC 99,99
               USING  LUCRO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O PRODUTO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT PRODUTO
                      CLOSE PRODUTO
                      MOVE "*** ARQUIVO PRODUTO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO PRODTUO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CODIGO IPI ICMS LUCRO TIPO
                MOVE SPACES TO DESCRICAO SITUACAO
                DISPLAY TELAPRODUTO.
       INC-002.
                ACCEPT TCODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE PRODUTO
                   GO TO ROT-FIM.
                IF CODIGO  = 0
                   MOVE "*** PRODUTO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ PRODUTO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                   MOVE TPROD( TIPO + 1 ) TO TIPOPROD
                      DISPLAY TELAPRODUTO
                      MOVE "*** PRODUTO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO PRODUTO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                      ELSE
                        NEXT SENTENCE.
       INC-003.
                ACCEPT TDESCRICAOICAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TTIPO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                MOVE TPRODGO(TIPO + 1) TO TIPOPROD
                DISPLAY TELAPRODUTO.
                
       INC-005.
                ACCEPT TIPI
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TICMS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TLUCRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                
       INC-008.
                ACCEPT TSITUACAO 
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-007.
       
               
      *
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** PRODUTO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE PRODUTO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 30000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
