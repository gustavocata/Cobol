       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLACEP.
       AUTHOR. GABRIEL ASSIS DE MORAES.
      **************************************
      * MANUTENCAO DO CADASTRO DE CEP  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS NUMERO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = LOGRADOURO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CEP.DAT".
       01 CADCEP.
                03 NUMERO          PIC 9(08).
                03 LOGRADOURO      PIC X(35).
                03 BAIRRO          PIC X(25).
                03 CIDADE          PIC X(25).
                03 UF              PIC X(02).
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
       01 TISITU       PIC X(10) VALUE SPACES.
       01 TABUF. 
                   05 FILLER PIC X(02) VALUE "AC".
                   05 FILLER PIC X(02) VALUE "AL".
                   05 FILLER PIC X(02) VALUE "AM".
                   05 FILLER PIC X(02) VALUE "AP".
                   05 FILLER PIC X(02) VALUE "BA".
                   05 FILLER PIC X(02) VALUE "CE".
                   05 FILLER PIC X(02) VALUE "DF".
                   05 FILLER PIC X(02) VALUE "ES".
                   05 FILLER PIC X(02) VALUE "GO".
                   05 FILLER PIC X(02) VALUE "MA".
                   05 FILLER PIC X(02) VALUE "MT".
                   05 FILLER PIC X(02) VALUE "MS".
                   05 FILLER PIC X(02) VALUE "MG".
                   05 FILLER PIC X(02) VALUE "PA".
                   05 FILLER PIC X(02) VALUE "PB".
                   05 FILLER PIC X(02) VALUE "PR".
                   05 FILLER PIC X(02) VALUE "PE".
                   05 FILLER PIC X(02) VALUE "PI".
                   05 FILLER PIC X(02) VALUE "RJ".
                   05 FILLER PIC X(02) VALUE "RN".
                   05 FILLER PIC X(02) VALUE "RS".
                   05 FILLER PIC X(02) VALUE "RO".
                   05 FILLER PIC X(02) VALUE "SP".
                   05 FILLER PIC X(02) VALUE "SE".
                   05 FILLER PIC X(02) VALUE "TO".
 
       01 TUF REDEFINES TABUF.
                   05 T-UF PIC X(02) OCCURS 26 TIMES.
       01 IND PIC 9(02) VALUE ZEROS.
       
       01 TSITUACAO1. 
                   05 FILLER PIC X(10) VALUE "ATIVO".
                   05 FILLER PIC X(10) VALUE "INATIVO".
                   05 FILLER PIC X(10) VALUE "CANCELADO".
                   05 FILLER PIC X(10) VALUE "EXCLUIDO".
       01 TABSITU REDEFINES TSITUACAO1.
                   05 T-SITU PIC X(01) OCCURS 4 TIMES.
      *-----------
       SCREEN SECTION.
       
       01  TEL002.
           05  LINE 02  COLUMN 01 
               VALUE  "                               CADASTRO".
           05  LINE 02  COLUMN 41 
               VALUE  "CEP".
           05  LINE 03  COLUMN 01 
               VALUE  "========================================".
           05  LINE 03  COLUMN 41 
               VALUE  "========================================".
           05  LINE 04  COLUMN 01 
               VALUE  "NUMERO:".
           05  LINE 07  COLUMN 01 
               VALUE  "LOGRADOURO:".
           05  LINE 10  COLUMN 01 
               VALUE  "BAIRRO:".
           05  LINE 13  COLUMN 01 
               VALUE  "CIDADE:".
           05  LINE 16  COLUMN 01 
               VALUE  "UF:".
           05  LINE 19  COLUMN 01 
               VALUE  "SITUA€AO:".
           05  TNUMERO
               LINE 04  COLUMN 08  PIC 99999.999
               USING  NUMERO
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 07  COLUMN 12  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 10  COLUMN 08  PIC X(25)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 13  COLUMN 08  PIC X(25)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 16  COLUMN 04  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TSITUACAO
               LINE 19  COLUMN 10  PIC X(01)
               USING  SITUACAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CEP
                      CLOSE CEP
                      MOVE "*** ARQUIVO CEP SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO PRODTUO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO TNUMERO
                MOVE SPACES TO TLOGRADOURO TBAIRRO TCIDADE TUF TSITUACAO
                DISPLAY TEL002.
       INC-002.
                ACCEPT TNUMERO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CEP
                   GO TO ROT-FIM.
                IF CODIGO  = 0
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CEP01.
                MOVE 0 TO W-SEL
                READ CEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                   IF SITUACAO1 = "a" or "A"
                   MOVE T-SITU (1) TO TISITU
                   IF SITUACAO1 = "i" or "I"
                   MOVE T-SITU (2) TO TISITU
                   IF SITUACAO1 = "c" or "C"
                   MOVE T-SITU (3) TO TISITU 
                   IF SITUACAO1 = "e" or "E"
                   MOVE T-SITU (4) TO TISITU.
                    DISPLAY TEL002
                      MOVE "*** CEP JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                      ELSE
                        NEXT SENTENCE.
       INC-003.
                ACCEPT TLOGRADOURO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                
       INC-005.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TSITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                
       INC-008.
                ACCEPT (1, C) UF
                MOVE 1 TO IND.          
       INC-008-A 
                IF UF NOT = T-UF(IND)
                IF IND = 26
                  MENSAGEM "ERRO DE UF"
                  GO TO INC-008
                ELSE 
                  ADD 1 TO IND
                  GO TO INC-008-A
                ELSE 
                  UF CORRETO.               
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
                WRITE CADCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CEP"
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
                DELETE CEP RECORD
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
                REWRITE CADCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CEP"   TO MENS
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
