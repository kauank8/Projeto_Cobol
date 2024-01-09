       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSCLI.
      *AUTHOR. Kauan Paulino Farias.
    
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY IS CODIGO
                    FILE STATUS  IS ST-ERRO.
                    
       DATA DIVISION.
       FILE SECTION.
       FD ARQPRO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPRO.DAT".
       01 REGPRO.
          03 CODIGO          PIC 9(06).
          03 DESCRICAO       PIC X(30).           
          03 UNIDADE         PIC X(02).
          03 TPPROD          PIC 9(01).
          03 PRECO           PIC 9(06)V99.
          03 DATACP          PIC 9(8).
          03 ESTOQUE         PIC 9(06).
          03 CPFCNPJ         PIC 9(14).
          

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
       01 CON-CODIGO       PIC 9(06) VALUE ZEROS.
   
       
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DET2.
          03 FILLER    PIC X(02) VALUE SPACES.
          03 DETCODIGO    PIC 9(06) VALUE ZEROS.
          03 FILLER    PIC X(04) VALUE SPACES.
          03 DETDESC  PIC X(30) VALUE SPACES.
          03 FILLER    PIC X(02) VALUE SPACES.
          03 DETESTOQUE    PIC 9(06) VALUE ZEROS.
		  03 FILLER    PIC X(15) VALUE SPACES.
          03 DETPRECO   PIC 9(06) VALUE ZEROS.

       SCREEN SECTION.
       01  TCONPRO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             Consulta de".
           05  LINE 02  COLUMN 41 
               VALUE  " Produtos".
           05  LINE 04  COLUMN 01 
               VALUE  "  Produtos     Descriscao".
           05  LINE 04  COLUMN 44
               VALUE  " Qtd.Estoque".
		   05  LINE 04  COLUMN 65
               VALUE  " Preco".   
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM :".      
           05  TCODIGO
               LINE 06  COLUMN 03  PIC 9(6)
               USING  CON-CODIGO
               HIGHLIGHT.
               
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  ARQPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCLI NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ArqPRO"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
                 
        INC-001.
           MOVE ZEROS TO CON-CODIGO
           DISPLAY  TCONPRO.
           
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA ."
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQPRO
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CON-CODIGO TO CODIGO
           START ARQPRO KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQPRO NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQPRO"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
              
       ROT-MONTAR.
           MOVE CODIGO TO DETCODIGO
           MOVE DESCRICAO   TO DETDESC
           MOVE ESTOQUE   TO DETESTOQUE
		   MOVE PRECO TO DETPRECO
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
                  DISPLAY TCONPRO
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
           CLOSE ARQPRO.
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