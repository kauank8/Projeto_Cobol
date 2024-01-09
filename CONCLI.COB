       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSCLI.
      *AUTHOR. Kauan Paulino Farias.
    
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY IS CPFCNPJ
                    FILE STATUS  IS ST-ERRO.
                    
       DATA DIVISION.
       FILE SECTION.
       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
          03 PESSOA      PIC X(1).
          03 CPFCNPJ     PIC 9(14).
          03 RAZAO       PIC X(30). 
          03 APELIDO     PIC X(30). 
          03 CEP         PIC 9(8).
          03 NUMERO      PIC 9(05).  
          03 COMPLEMENTO PIC X(10).
          03 TEL         PIC 9(11). 
          03 EMAIL       PIC X(30). 
          

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
       01 CON-CPF        PIC 9(14) VALUE ZEROS.
   
       
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DET2.
          03 FILLER    PIC X(02) VALUE SPACES.
          03 DETCPFCNPJ    PIC 9(14) VALUE ZEROS.
          03 FILLER    PIC X(04) VALUE SPACES.
          03 DETNOME  PIC X(30) VALUE SPACES.
          03 FILLER    PIC X(02) VALUE SPACES.
          03 DETTEL    PIC 9(11) VALUE ZEROS.

       SCREEN SECTION.
       01  TCONCLI.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             Consulta de".
           05  LINE 02  COLUMN 41 
               VALUE  " Clientes".
           05  LINE 04  COLUMN 01 
               VALUE  "  Cpf/Cnpj           Nome/RazaoSocial".
           05  LINE 04  COLUMN 41 
               VALUE  "              Telefone".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM :".      
           05  TCONCPF
               LINE 06  COLUMN 03  PIC X(14)
               USING  CON-CPF
               HIGHLIGHT.
               
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  ARQCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCLI NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCLI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
                 
        INC-001.
           MOVE ZEROS TO CON-CPF
           DISPLAY  TCONCLI.
           
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA ."
           ACCEPT TCONCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQCLI
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CON-CPF TO CPFCNPJ
           START ARQCLI KEY IS NOT LESS CPFCNPJ INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQCLI NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQCLI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
              
       ROT-MONTAR.
           MOVE CPFCNPJ TO DETCPFCNPJ
           MOVE RAZAO   TO DETNOME
           MOVE TEL   TO DETTEL
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
                  DISPLAY TCONCLI
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
           CLOSE ARQCLI.
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