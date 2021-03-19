
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
               LINE 04  COLUMN 08  PIC 9(08)
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
