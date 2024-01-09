       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE002.
      *AUTHOR. Kauan Paulino Farias.
    
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQFOR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY IS FOR-CPFCNPJ
                    FILE STATUS  IS ST-ERRO.
                    
           SELECT ARQCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES.             
      
       DATA DIVISION.
       FILE SECTION.
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".
       01 REGFOR.
          03 FOR-CPFCNPJ     PIC 9(14).
          03 FOR-RAZAO       PIC X(30). 
          03 FOR-APELIDO     PIC X(30). 
          03 FOR-CEP         PIC 9(8).
          03 FOR-NUMERO      PIC 9(05).  
          03 FOR-COMP PIC X(10).
          03 FOR-TEL         PIC 9(11). 
          03 FOR-EMAIL       PIC X(30). 

      *-----------------------------------------------------------------
       FD ARQCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP        PIC 9(08).
                03 CEP-LOGRA         PIC X(30).
                03 CEP-BAIRRO        PIC X(20).
                03 CEP-CIDADE        PIC X(20).
                03 CEP-UF            PIC X(02).          
      *-----------------------------------------------------------------  
       WORKING-STORAGE SECTION.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 MENS          PIC X(50) VALUE SPACES.
       01 TPREGISTRO    PIC X(05) VALUE SPACES.
       01 TXTRAZAO      PIC X(13) VALUE SPACES.
       01 TXTAP         PIC X(10) VALUE SPACES. 
       01 DPESSOA       PIC X(10) VALUE SPACES.
       01 DADO          PIC 9(14) VALUE ZEROS.
       01 CONT          PIC 9(02) VALUE ZEROS.
       01 DADOS         PIC X(01) VALUE SPACES.
       01 LOUG          PIC X(40) VALUE SPACES.
       01 BAIRRO        PIC X(25) VALUE SPACES.
       01 CIDADE        PIC X(25) VALUE SPACES.
       01 UF            PIC X(02) VALUE SPACES.
	   01 FOR-PESSOA    PIC X(01) VALUE SPACES.
	   
     
       
       
       
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  TELACLI.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "     ------------------- CADASTRO DE FOR".
           05  LINE 02  COLUMN 41 
               VALUE  "NECEDOR ---------------------------".
           05  LINE 04  COLUMN 01 
               VALUE  "                                       T".
           05  LINE 04  COLUMN 41 
               VALUE  "ipo Pessoa:".
           05  LINE 09  COLUMN 01 
               VALUE  "     -----------------------------------".
           05  LINE 09  COLUMN 41 
               VALUE  "----------------------------------".
           05  LINE 10  COLUMN 01 
               VALUE  "                                  ENDERE".
           05  LINE 10  COLUMN 41 
               VALUE  "CO".
           05  LINE 11  COLUMN 01 
               VALUE  "     CEP:".
           05  LINE 13  COLUMN 01 
               VALUE  "     NUMERO:         LOGRADOURO:".
           05  LINE 15  COLUMN 01 
               VALUE  "     COMPLEMENTO:               BAIRRO:".
           05  LINE 17  COLUMN 01 
               VALUE  "     CIDADE:".
           05  LINE 17  COLUMN 41 
               VALUE  "  UF:".
           05  LINE 18  COLUMN 01 
               VALUE  "     -----------------------------------".
           05  LINE 18  COLUMN 41 
               VALUE  "----------------------------------".
           05  LINE 19  COLUMN 01 
               VALUE  "     TELEFONE:                     EMAIL".
           05  LINE 19  COLUMN 41 
               VALUE  ":".
           05  LINE 20  COLUMN 01 
               VALUE  "     -----------------------------------".
           05  LINE 20  COLUMN 41 
               VALUE  "----------------------------------".
           05  LINE 21  COLUMN 01 
               VALUE  "     MENSAGEM:".
           05  TTPREGISTRO
               LINE 04  COLUMN 06  PIC X(05)
               USING  TPREGISTRO
               HIGHLIGHT.
           05  TCPFCNPJ
               LINE 04  COLUMN 12  PIC 9(14)
               USING  FOR-CPFCNPJ
               HIGHLIGHT.
           05  TPESSOA
               LINE 04  COLUMN 53  PIC X(01)
               USING  FOR-PESSOA
               HIGHLIGHT.
           05  TDPESSOA
               LINE 04  COLUMN 55  PIC X(10)
               USING  DPESSOA
               HIGHLIGHT.
           05  TTXTRAZAO
               LINE 06  COLUMN 06  PIC X(13)
               USING  TXTRAZAO
               HIGHLIGHT.
           05  TRAZAO
               LINE 06  COLUMN 20  PIC X(30)
               USING  FOR-RAZAO
               HIGHLIGHT.
           05  TTXTAP
               LINE 08  COLUMN 06  PIC X(10)
               USING  TXTAP
               HIGHLIGHT.
           05  TAPELIDO
               LINE 08  COLUMN 17  PIC X(30)
               USING  FOR-APELIDO
               HIGHLIGHT.
           05  TCEP
               LINE 11  COLUMN 11  PIC 9(08)
               USING  FOR-CEP
               HIGHLIGHT.
           05  TNUMERO
               LINE 13  COLUMN 14  PIC 9(05)
               USING  FOR-NUMERO
               HIGHLIGHT.
           05  TLOUG
               LINE 13  COLUMN 34  PIC X(40)
               USING  LOUG
               HIGHLIGHT.
           05  TCOMP
               LINE 15  COLUMN 19  PIC X(10)
               USING  FOR-COMP
               HIGHLIGHT.
           05  TBAIRRO
               LINE 15  COLUMN 41  PIC X(25)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 17  COLUMN 14  PIC X(25)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 17  COLUMN 47  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TTELEFONE
               LINE 19  COLUMN 16  PIC 9(11)
               USING  FOR-TEL
               HIGHLIGHT.
           05  TEMAIL
               LINE 19  COLUMN 43  PIC X(30)
               USING  FOR-EMAIL
               HIGHLIGHT. 
                       
      
  
       PROCEDURE DIVISION.
       INICIO.
  
       ABRE-ARQUIVO.
           OPEN I-O ARQFOR
            IF ST-ERRO NOT = "00"
            IF ST-ERRO = "30"
              OPEN OUTPUT ARQFOR
              MOVE "CRIANDO ARQUIVO CADASTRO DE FORNECEDOR " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQFOR
              GO TO INICIO
            ELSE
              IF ST-ERRO = "95"
                MOVE "ISAM NAO CARREGADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO CADASTRO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
                
        ABRE-CEP.
           OPEN INPUT ARQCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
                
       R1.
           MOVE SPACES TO TPREGISTRO FOR-APELIDO FOR-RAZAO TXTAP 
		   TXTRAZAO 
           LOUG FOR-COMP BAIRRO CIDADE UF FOR-EMAIL CEP-LOGRA 
           CEP-BAIRRO CEP-CIDADE CEP-UF
           MOVE ZEROS TO FOR-CEP DADO  FOR-NUMERO FOR-TEL CONT 
		   FOR-CPFCNPJ W-SEL 
           CEP-NUMCEP . 
       
       RTELA.
           DISPLAY TELACLI.       
       
       R2.
           ACCEPT TPESSOA.        
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             CLOSE ARQFOR
             STOP RUN.
       R2A.
           IF FOR-PESSOA = "F" OR "f"
             MOVE "FISICA" TO DPESSOA
             MOVE "F" TO FOR-PESSOA
             DISPLAY TDPESSOA
             GO TO RCPF
           ELSE 
             IF FOR-PESSOA = "J" OR "j"
             MOVE "JURIDICA" TO DPESSOA 
             DISPLAY TDPESSOA
             GO TO RCNPJ
           ELSE 
             MOVE "OPCAO INVALIDA: F-FISICA J-JURIDICA" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R2.
           
           
       RCPF.
           MOVE "CPF:" TO TPREGISTRO
           MOVE "NOME INICIAL:" TO TXTRAZAO
           MOVE "SOBRENOME:" TO TXTAP
           DISPLAY TELACLI.
       RCPFA.
           MOVE 0 TO CONT FOR-CPFCNPJ
           MOVE ZEROS TO DADO
           DISPLAY TELACLI
           ACCEPT TCPFCNPJ
           MOVE FOR-CPFCNPJ TO DADO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF FOR-CPFCNPJ = ZEROS
              MOVE "CPF VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO RCPFA.
       RCPFB.
           IF DADO > 0 
              DIVIDE 10 INTO DADO
              ADD 1 TO CONT
              GO TO RCPFB.
       RCPFC.
           IF CONT = 11
             MOVE "CPF VALIDO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO LER-CAD
           ELSE 
             MOVE "CPF INVALIDO, CONFIRA O NUMERO DE DIGITOS" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM       
             GO TO RCPFA.
          
       RCNPJ.
           MOVE "CNPJ:" TO TPREGISTRO
           MOVE "RAZAO SOCIAL:" TO TXTRAZAO
           MOVE "APELIDO:" TO TXTAP
           DISPLAY TELACLI.
       RCNPJA.
           MOVE 0 TO CONT FOR-CPFCNPJ
           MOVE ZEROS TO DADO
           DISPLAY TELACLI
           ACCEPT TCPFCNPJ
           MOVE FOR-CPFCNPJ TO DADO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF FOR-CPFCNPJ = ZEROS
              MOVE "CNPJ VAZIO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO RCNPJA.
       RCNPJB.
           IF DADO > 0 
              DIVIDE 10 INTO DADO
              ADD 1 TO CONT
              GO TO RCNPJB.
       RCNPJC.
           IF CONT = 14
             MOVE "CNPJ VALIDO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO LER-CAD
           ELSE 
             MOVE "CNPJ INVALIDO, CONFIRA O NUMERO DE DIGITOS" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM  
             GO TO RCNPJA.
             
       LER-CAD.
           READ ARQFOR
           IF ST-ERRO NOT = "23"
           IF ST-ERRO = "00"
		     PERFORM R5A
             DISPLAY TELACLI        
             MOVE "*** FORNECEDOR JA CADASTRADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ACE-001
           ELSE
               MOVE "ERRO NA LEITURA ARQUIVO FORNECEDOR" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM              
           ELSE 
              NEXT SENTENCE.

       R3.        
           ACCEPT TRAZAO.   
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R1.
           IF FOR-RAZAO = SPACES
              IF FOR-PESSOA = "F"
              MOVE "NOME EM BRANCO" TO MENS           
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3
              ELSE
              MOVE "RAZAO SOCIAL EM BRANCO" TO MENS           
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       
       R4.
           ACCEPT TAPELIDO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF FOR-APELIDO = SPACES
              IF FOR-PESSOA = "F"
              MOVE "SOBRENOME EM BRANCO" TO MENS           
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4
              ELSE
              MOVE "APELIDO EM BRANCO" TO MENS           
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
           IF FOR-CEP = ZEROS
              MOVE "CEP VAZIO, DIGITE O CEP" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
             
        R5A.
           MOVE FOR-CEP TO CEP-NUMCEP.
           READ ARQCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE CEP-BAIRRO TO BAIRRO
                MOVE CEP-CIDADE TO CIDADE
                MOVE CEP-LOGRA TO LOUG
                MOVE CEP-UF TO UF
                DISPLAY TELACLI.           
            
       R6.
           ACCEPT TNUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R5.
           IF FOR-NUMERO = ZEROS
              MOVE "NUMERO VAZIO, DIGITE O NUMERO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6. 
       
       R7.
           ACCEPT TCOMP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R6.
           IF FOR-COMP = SPACES
              MOVE "COMPLEMENTO VAZIO, DIGITE O COMPL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.

       R8.
           ACCEPT TTELEFONE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R7.
           IF FOR-TEL = ZEROS
              MOVE "TELEFONE VAZIO, DIGITE O TELEFONE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.    
       
       R9.
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R8.
           IF FOR-EMAIL = SPACES
              MOVE "EMAIL VAZIO, DIGITE O EMAIL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.
              
       RALT.
             IF W-SEL = 1 
             GO TO ALT-OPC.       
    
       R10.
           DISPLAY (21, 20) "DESEJA GRAVAR? S-SIM/N-NAO"    
           ACCEPT (21, 50) DADOS.
           IF DADOS = "S" OR "s"
              MOVE "DADOS GRAVADOS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-WR1 
           ELSE
             IF DADOS = "N" OR "n" 
               MOVE "DADOS APAGADOS E NAO SALVOS" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R1
           ELSE
               MOVE "DIGITE UMA OPERACAO VALIDA (S/N)" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R10.
               
       INC-WR1. 
           WRITE REGFOR.
           GO TO R1.

       ACE-001.
           DISPLAY (21, 16) "N = NOVO REGISTRO | A = ALTERAR |" 
           " E = EXCLUIR | S = SAIR"  
           ACCEPT (22, 12) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              GO TO R1.
           IF W-OPCAO = "A" OR "a" 
              MOVE 1 TO W-SEL
              GO TO R3.
           IF W-OPCAO = "S" OR "s"
              STOP RUN.
           IF W-OPCAO NOT = "E" AND "e"
              GO TO ACE-001.

        EXC-OPC.
           DISPLAY TELACLI.
           DISPLAY (21, 20) "EXCLUIR   (S/N) : ".
           ACCEPT (21, 50) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO EXC-OPC.

        EXC-DL1.
           DELETE ARQFOR RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO FORNECEDOR EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.

       ALT-OPC.
           DISPLAY TELACLI.
           DISPLAY (21, 20) "ALTERAR (S/N) : ".
           ACCEPT (21, 50) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO R9.
           IF W-OPCAO = "N" OR "n"
              MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "DIGITE APENAS S = SIM e N = NAO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ALT-OPC.

       ALT-RW1.
           REWRITE REGFOR
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA ALTERACAO DO REGISTRO FORNECEDOR"  TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.             
              
       ROT-FIM.
           CLOSE ARQFOR ARQCEP.
		   
           STOP RUN.    

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (21, 18) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (21, 18) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.           