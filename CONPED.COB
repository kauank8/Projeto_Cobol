       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSCLI.
      *AUTHOR. Kauan Paulino Farias.
    
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY IS CHAVE
                    FILE STATUS  IS ST-ERRO.
                    
       DATA DIVISION.
       FILE SECTION.
       FD ARQPED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPED.DAT".
       01 REGPED.
          03 CHAVE.
            05 NUMPEDIDO       PIC 9(6).
            05 TIPEDIDO        PIC 9(1). 
          03 CODPROD         PIC 9(6). 
          03 CLIENTE         PIC 9(14).
          03 FORNECEDOR      PIC 9(14).
          03 QUANTIDADE      PIC 9(06).  
          03 PRECOUN         PIC 9(6)V99.
          03 TOTAL           PIC 9(8)V99.
          

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT         PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN          PIC 9(03) VALUE 001.
       01 CON-NUMPED       PIC 9(06) VALUE ZEROS.
       01 CON-TPEDIDO       PIC 9(01) VALUE ZEROS.
   
       
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DET2.
          03 FILLER    PIC X(02) VALUE SPACES.
          03 DETNUMPED   PIC 9(06) VALUE ZEROS.
          03 FILLER    PIC X(08) VALUE SPACES.
          03 DETPEDIDO  PIC 9(01) VALUE ZEROS.
          03 FILLER    PIC X(12) VALUE SPACES.
          03 DETPRODUTO    PIC 9(06) VALUE ZEROS.
          03 FILLER    PIC X(09) VALUE SPACES.
          03 DETQTD    PIC 9(06) VALUE ZEROS.
          03 FILLER    PIC X(09) VALUE SPACES.
          03 DETVALOR   PIC 9(08) VALUE ZEROS.

       SCREEN SECTION.
       01  TCONPED.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             Consulta de".
           05  LINE 02  COLUMN 41 
               VALUE  " PEDIDO".
           05  LINE 04  COLUMN 03 
               VALUE  "Num.Ped       Tp.PEDIDO".
           05  LINE 04  COLUMN 30 
               VALUE  "Cod.Produto".
           05  LINE 04  COLUMN 45 
               VALUE  "Quantidade". 
           05  LINE 04  COLUMN 60 
               VALUE  "Valor Total".               
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM :".      
           05  TCONNUMPED
               LINE 06  COLUMN 03  PIC 9(06)
               USING  CON-NUMPED
               HIGHLIGHT.
           05  TCONTP
               LINE 06  COLUMN 17  PIC 9(01)
               USING  CON-TPEDIDO
               HIGHLIGHT.
               
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  ARQPED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADPED NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQPED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
                 
        INC-001.
           MOVE ZEROS TO CON-NUMPED
           MOVE ZEROS TO CON-TPEDIDO
           DISPLAY  TCONPED.
           
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA ."
           ACCEPT TCONNUMPED
           ACCEPT TCONTP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQPED
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CON-NUMPED TO NUMPEDIDO
           MOVE CON-TPEDIDO TO TIPEDIDO
           START ARQPED KEY IS NOT LESS CHAVE INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQPED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQPED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
              
       ROT-MONTAR.
           MOVE NUMPEDIDO TO DETNUMPED
           MOVE TIPEDIDO   TO DETPEDIDO
           MOVE CODPROD   TO DETPRODUTO
           MOVE QUANTIDADE TO DETQTD
           MOVE TOTAL TO DETVALOR
           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.
    
        ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TCONPED
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
           CLOSE ARQPED.
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