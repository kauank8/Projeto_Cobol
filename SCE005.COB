       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE005.
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
                    
           SELECT ARQPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO.

           SELECT ARQCLI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY IS CLI-CPFCNPJ
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
        
       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
          03 PESSOA      PIC X(1).
          03 CLI-CPFCNPJ PIC 9(14).
          03 RAZAO       PIC X(30). 
          03 APELIDO     PIC X(30). 
          03 CEP         PIC 9(8).
          03 NUMERO      PIC 9(05).  
          03 COMPLEMENTO PIC X(10).
          03 TEL         PIC 9(11). 
          03 EMAIL       PIC X(30).
      *-----------------------------------------------------------------    
       WORKING-STORAGE SECTION.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 MENS          PIC X(50) VALUE SPACES.
       01 DTPEDIDO      PIC X(06) VALUE SPACES.
       01 PRODESC       PIC X(30) VALUE SPACES.
	   01 DADOS         PIC X(01) VALUE SPACES.
       
      *-----------------------------------------------------------------  
       SCREEN SECTION.
           01  TELAPED.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                                  Pedido".
           05  LINE 01  COLUMN 41 
               VALUE  "s".
           05  LINE 03  COLUMN 01 
               VALUE  " Num Pedido:            Tipo Pedido:".
           05  LINE 05  COLUMN 01 
               VALUE  " Codigo Produto:           Descricao:".
           05  LINE 07  COLUMN 01 
               VALUE  " Codigo Cliente:".
           05  LINE 09  COLUMN 01 
               VALUE  " Codigo Fornecedor:".
           05  LINE 11  COLUMN 01 
               VALUE  " Quantidade:".
           05  LINE 13  COLUMN 01 
               VALUE  " Preco Unitario:".
           05  LINE 15  COLUMN 01 
               VALUE  " Valor do Pedido:".
           05  LINE 21  COLUMN 01 
               VALUE  " Mensagem:".
           05  TPEDIDO
               LINE 03  COLUMN 14  PIC 9(06)
               USING  NUMPEDIDO
               HIGHLIGHT.
           05  TPPEDIDO
               LINE 03  COLUMN 38  PIC X(01)
               USING  TIPEDIDO
               HIGHLIGHT.
           05  TDTPEDIDO
               LINE 03  COLUMN 40  PIC X(06)
               USING  DTPEDIDO
               HIGHLIGHT.
           05  TPRODUTO
               LINE 05  COLUMN 18  PIC 9(06)
               USING  CODPROD
               HIGHLIGHT.
           05  TPRODESC
               LINE 05  COLUMN 39  PIC X(30)
               USING  PRODESC
               HIGHLIGHT.
           05  TCLIENTE
               LINE 07  COLUMN 18  PIC 9(14)
               USING  CLIENTE
               HIGHLIGHT.
           05  TFORNECEDOR
               LINE 09  COLUMN 21  PIC 9(14)
               USING  FORNECEDOR
               HIGHLIGHT.
           05  TQUANTIDADE
               LINE 11  COLUMN 14  PIC 9(06)
               USING  QUANTIDADE
               HIGHLIGHT.
           05  TPRECOUN
               LINE 13  COLUMN 18  PIC 9(06)
               USING  PRECOUN
               HIGHLIGHT.
           05  TTOTAL
               LINE 15  COLUMN 19  PIC 9(08)
               USING  TOTAL
               HIGHLIGHT.
			   
      *-----------------------------------------------------------------  
       PROCEDURE DIVISION.
       INICIO.
  
       ABRE-ARQUIVO.
	       OPEN I-O ARQPED
           IF ST-ERRO NOT = "00"
            IF ST-ERRO = "30"
              OPEN OUTPUT ARQPED
              MOVE "CRIANDO ARQUIVO CADASTRO DE PEDIDO " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPED
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
          
       ABRE-PRO.
           OPEN INPUT ARQPRO
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE PRODUTO NAO ENCONTRADO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO PRODUTO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 		  
				 
	
         ABRE-CLI.
           OPEN INPUT ARQCLI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CLIENTE NAO ENCONTRADO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CLIENTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 		  
				 	
       R1.
           MOVE SPACES TO DTPEDIDO PRODESC  TIPEDIDO
           MOVE ZEROS TO NUMPEDIDO CODIGO W-SEL
           CODPROD PRECOUN TOTAL FORNECEDOR PRECO
           CPFCNPJ CLI-CPFCNPJ CLIENTE QUANTIDADE.	

       RTELA.
           DISPLAY TELAPED.
		   
       R2.
           ACCEPT TPEDIDO.
 		   ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             CLOSE ARQPED
             STOP RUN.
		   IF NUMPEDIDO = ZEROS
             MOVE "NUMERO DO PEDIDO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R2.
			    

       R3.	   
	       ACCEPT TPPEDIDO
		   ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R2.
		   IF TIPEDIDO = ZEROS
             MOVE "TIPO DO PEDIDO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R3. 
		   IF TIPEDIDO > 2
		     MOVE "OPCAO INVALIDA --> 1-COMPRA / 2-VENDA " 
			 TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
			 GO TO R3
		   ELSE
             NEXT SENTENCE.
 
       R3A.
           IF TIPEDIDO = 1
             MOVE "COMPRA" TO DTPEDIDO
           ELSE
             MOVE "VENDA" TO DTPEDIDO.
           DISPLAY TELAPED.			 
             
       LER-CAD.
           READ ARQPED
           IF ST-ERRO NOT = "23"
           IF ST-ERRO = "00"
             PERFORM R3A
             PERFORM R4A
			 PERFORM R5A
             DISPLAY TELAPED      
             MOVE "*** PEDIDO JA CADASTRADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ACE-001
           ELSE
               MOVE "ERRO NA LEITURA ARQUIVO CADPED" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM              
           ELSE 
              NEXT SENTENCE. 
        
       R4.
           ACCEPT TPRODUTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R3.
           IF CODPROD = ZEROS
             MOVE "CODIGO DO PRODUTO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM 
             GO TO R4.   
    
       R4A.
           MOVE CODPROD TO CODIGO
           READ ARQPRO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "PRODUTO DIGITADO NAO ENCONTRADO " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO CPFCNPJ
                   GO TO R4
               ELSE
                  MOVE "ERRO LEITURA ARQUIVO CADASTRO PRODUTO" 
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE DESCRICAO TO PRODESC
                MOVE PRECO TO PRECOUN
                MOVE CPFCNPJ TO FORNECEDOR
                DISPLAY TELAPED.

       R5.
	       IF TIPEDIDO = 1
		      GO TO R6
		   ELSE
             ACCEPT TCLIENTE
             ACCEPT W-ACT FROM ESCAPE KEY
             IF W-ACT = 01    
               GO TO R4.
		     IF CLIENTE = ZEROS
               MOVE "CLIENTE EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM	
               GO TO R5.
        	 
      
       R5A.
		   IF TIPEDIDO = 2
           MOVE CLIENTE TO CLI-CPFCNPJ
           READ ARQCLI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "CLIENTE DIGITADO NAO ENCONTRADO " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO CPFCNPJ
                   GO TO R5
               ELSE
                  MOVE "ERRO LEITURA ARQUIVO CADASTRO CLIENTE" 
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAPED.
		
       R6.
           ACCEPT TQUANTIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
             IF W-ACT = 01    
               GO TO R5.
           IF QUANTIDADE = ZEROS
               MOVE "QUANTIDADE EM BRANCO" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM          
               GO TO R6.
       R7.
           MULTIPLY PRECOUN BY QUANTIDADE GIVING TOTAL
           DISPLAY TELAPED.
		   
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
           WRITE REGPED.
           GO TO R1.
           
       ACE-001.
           DISPLAY (21, 16) "N = NOVO REGISTRO | A = ALTERAR |" 
           " E = EXCLUIR | S = SAIR"  
           ACCEPT (22, 12) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              GO TO R1.
           IF W-OPCAO = "A" OR "a" 
              MOVE 1 TO W-SEL
              GO TO R4.
           IF W-OPCAO = "S" OR "s"
              STOP RUN.
           IF W-OPCAO NOT = "E" AND "e"
              GO TO ACE-001. 

       EXC-OPC.
           DISPLAY TELAPED.
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
           DELETE ARQPED RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
           
       ALT-OPC.
           DISPLAY TELAPED.
           DISPLAY (21, 20) "ALTERAR (S/N) : ".
           ACCEPT (21, 50) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO R7.
           IF W-OPCAO = "N" OR "n"
              MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "DIGITE APENAS S = SIM e N = NAO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ALT-OPC.

       ALT-RW1.
           REWRITE REGPED
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA ALTERACAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.      

    
        
       ROT-FIM.
           CLOSE ARQPED ARQPRO ARQCLI.
           STOP RUN.             

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (21, 15) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (22, 18) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.          
                         