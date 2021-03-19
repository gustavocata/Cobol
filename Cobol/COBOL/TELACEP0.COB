
       01  TELACEP010.
           05  LINE 01  COLUMN 01 
               VALUE  "                             CONSULTA CE".
           05  LINE 01  COLUMN 41 
               VALUE  "P".
           05  LINE 02  COLUMN 01 
               VALUE  "========================================".
           05  LINE 02  COLUMN 41 
               VALUE  "========================================".
           05  LINE 03  COLUMN 01 
               VALUE  "CEP:".
           05  LINE 05  COLUMN 01 
               VALUE  "LOGRADOURO:".
           05  LINE 07  COLUMN 01 
               VALUE  "NUMERO:".
           05  LINE 09  COLUMN 01 
               VALUE  "COMPLEMENTO:".
           05  LINE 11  COLUMN 01 
               VALUE  "BAIRRO:".
           05  LINE 13  COLUMN 01 
               VALUE  "CIDADE:".
           05  LINE 15  COLUMN 01 
               VALUE  "UF:".
           05  TELACEP
               LINE 03  COLUMN 05  PIC 99999.999
               USING  CEP1
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 05  COLUMN 12  PIC X(35)
               USING  LOGRADOURO1
               HIGHLIGHT.
