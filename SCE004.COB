       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE004.
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
                    
           SELECT ARQFOR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS FOR-CPFCNPJ
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
      *-----------------------------------------------
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".
       01 REGFOR.
	      03 FOR-CPFCNPJ     PIC 9(14).
          03 FOR-RAZAO       PIC X(30). 
          03 FOR-APELIDO     PIC X(30). 
          03 FOR-CEP         PIC 9(8).
          03 FOR-NUMERO      PIC 9(05).  
          03 FOR-COMP        PIC X(10).
          03 FOR-TEL         PIC 9(11). 
          03 FOR-EMAIL       PIC X(30).       
    
      *-----------------------------------------------------------------  
       WORKING-STORAGE SECTION.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 MENS          PIC X(50) VALUE SPACES. 
       01 DUNIDADE      PIC X(12) VALUE SPACES.
       01 DTPPRODUTO    PIC X(25) VALUE SPACES.
       01 APELIDO       PIC X(30) VALUE SPACES.
       01 NOME          PIC X(30) VALUE SPACES.
	   01 IND           PIC 9(02) VALUE ZEROS.
	   01 DADOS         PIC X(01) VALUE SPACES.
	   01 TABUNIDADE.
	     03 TBUNIDADE   PIC X(16) OCCURS 7 TIMES.
	   01 TABPRODUTO.
	     03 TBPRODUTO  PIC X(15) OCCURS 8 TIMES.	 
	   01 TXTUNIDADE.
         03 UN-MAISCULO PIC X(02) VALUE SPACES.
         03 UN-MINUSCULO PIC X(02) VALUE SPACES.
         03 UN-DESCRICAO PIC X(12) VALUE SPACES.
       		 
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  TELAPRO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "--------------------------- Cadastro de".
           05  LINE 01  COLUMN 41 
               VALUE  "Produto --------------------------------".
           05  LINE 02  COLUMN 01 
               VALUE  " Codigo:".
           05  LINE 04  COLUMN 01 
               VALUE  " Descricao:".
           05  LINE 06  COLUMN 01 
               VALUE  " Unidade:".
           05  LINE 08  COLUMN 01 
               VALUE  " Tipo Produto:".
           05  LINE 10  COLUMN 01 
               VALUE  " Ultimo Preco Compra:".
           05  LINE 12  COLUMN 01 
               VALUE  " Data Ultima Compra:".
           05  LINE 14  COLUMN 01 
               VALUE  " Quantidade em Estoque:".
           05  LINE 15  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 16  COLUMN 01 
               VALUE  " Cpf/Cnpj do Fornecedor:".
           05  LINE 18  COLUMN 01 
               VALUE  " Nome/Razao:".
           05  LINE 20  COLUMN 01 
               VALUE  " Apelido/Sobrenome:".
           05  LINE 21  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 22  COLUMN 01 
               VALUE  " Mensagem:".
           05  TCOD
               LINE 02  COLUMN 10  PIC 9(06)
               USING  CODIGO
               HIGHLIGHT.
           05  TDESCRICAO
               LINE 04  COLUMN 13  PIC X(30)
               USING  DESCRICAO
               HIGHLIGHT.
           05  TUNIDADE
               LINE 06  COLUMN 11  PIC X(02)
               USING  UNIDADE
               HIGHLIGHT.
           05  TDUNIDADE
               LINE 06  COLUMN 14  PIC X(12)
               USING  DUNIDADE
               HIGHLIGHT.
           05  TTPPROD
               LINE 08  COLUMN 16  PIC 9(01)
               USING  TPPROD
               HIGHLIGHT.
           05  TDTPPRODUTO
               LINE 08  COLUMN 18  PIC X(25)
               USING  DTPPRODUTO
               HIGHLIGHT.
           05  TPRECO
               LINE 10  COLUMN 23  PIC 9(06)V99
               USING  PRECO
               HIGHLIGHT.
           05  TDATA
               LINE 12  COLUMN 22  PIC 99/99/9999
               USING  DATACP
               HIGHLIGHT.
           05  TESTOQUE
               LINE 14  COLUMN 25  PIC 9(06)
               USING  ESTOQUE
               HIGHLIGHT.
           05  TCPFCNPJ
               LINE 16  COLUMN 26  PIC 9(14)
               USING  CPFCNPJ
               HIGHLIGHT.
           05  TNOME
               LINE 18  COLUMN 14  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TAPELIDO
               LINE 20  COLUMN 21  PIC X(30)
               USING  APELIDO
               HIGHLIGHT.

         01 TELAUN.
          05  LINE 5  COLUMN 50 
               VALUE  "        PC-PECA".
           05  LINE 6  COLUMN 50
               VALUE  "        KL-KILO".
           05  LINE 7  COLUMN 50 
               VALUE  "        LT-LITRO".
           05  LINE 8  COLUMN 50 
               VALUE  "        TN-TONELADA".
           05  LINE 9  COLUMN 50 
               VALUE  "        GR-GRAMAS".
           05  LINE 10  COLUMN 50 
               VALUE  "        MG-MILIGRAMA".
           05  LINE 11  COLUMN 50 
               VALUE  "        MT-METROS".
			   
	    01 TELATP.
          05  LINE 5  COLUMN 50 
               VALUE  "        01-SAUDE".
           05  LINE 6  COLUMN 50
               VALUE  "        02-ALIMENTO".
           05  LINE 7  COLUMN 50 
               VALUE  "        03-VESTUARIO".
           05  LINE 8  COLUMN 50 
               VALUE  "        04-AGROPECUARIO".
           05  LINE 9  COLUMN 50 
               VALUE  "        05-PET".
           05  LINE 10  COLUMN 50 
               VALUE  "        06-ESTETICA".
           05  LINE 11  COLUMN 50 
               VALUE  "        07-CASA".
		   05  LINE 12  COLUMN 50 
               VALUE  "        08-AUTOMOVEL".	   
			   
       PROCEDURE DIVISION.
       INICIO.
  
       ABRE-ARQUIVO.
           OPEN I-O ARQPRO
           IF ST-ERRO NOT = "00"
            IF ST-ERRO = "30"
              OPEN OUTPUT ARQPRO
              MOVE "CRIANDO ARQUIVO CADASTRO DE PRODUTOS " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPRO
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
                
        ABRE-FOR.
           OPEN INPUT ARQFOR
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE FORNECEDOR NAO ENCONTRADO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO FORNECEDOR" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
				 
		
       R0.
           MOVE "PCpcPECA" TO TBUNIDADE(1)     
           MOVE "KLklKILO" TO TBUNIDADE(2)
           MOVE "LTltLITRO" TO TBUNIDADE(3)
           MOVE "TNtnTONELADA" TO TBUNIDADE(4)  
           MOVE "GRgrGRAMA" TO TBUNIDADE(5)
           MOVE "MGmgMILIGRAMA" TO TBUNIDADE(6)
           MOVE "MTmtMETRO" TO TBUNIDADE(7).
       R0A.
           MOVE "SAUDE" TO TBPRODUTO(1)     
           MOVE "ALIMENTO" TO TBPRODUTO(2)
           MOVE "VESTUARIO" TO TBPRODUTO(3) 
           MOVE "AGROPECUARIO" TO TBPRODUTO(4) 
           MOVE "PET" TO TBPRODUTO(5) 
           MOVE "ESTETICA" TO TBPRODUTO(6)
           MOVE "CASA" TO TBPRODUTO(7)
           MOVE "AUTOMOVEL" TO TBPRODUTO(8).   		   
                
       R1.
           MOVE SPACES TO FOR-APELIDO FOR-RAZAO FOR-COMP FOR-EMAIL
            DESCRICAO UNIDADE DUNIDADE  DTPPRODUTO
           APELIDO NOME MENS
           MOVE ZEROS TO FOR-CEP  FOR-NUMERO FOR-TEL CODIGO
           FOR-CPFCNPJ W-SEL TPPROD DATACP
           PRECO ESTOQUE CPFCNPJ .
    
       RTELA.
           DISPLAY TELAPRO.

       R2.
           ACCEPT TCOD.       
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             CLOSE ARQPRO
             STOP RUN.
		   IF CODIGO = ZEROS
             MOVE "CODIGO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R2.
			 
       LER-CAD.
           READ ARQPRO
           IF ST-ERRO NOT = "23"
           IF ST-ERRO = "00"
		     PERFORM R4A
			 PERFORM R5A
			 PERFORM R9A
             DISPLAY TELAPRO       
             MOVE "*** PRODUTO JA CADASTRADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ACE-001
           ELSE
               MOVE "ERRO NA LEITURA ARQUIVO CADFUNC" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM              
           ELSE 
              NEXT SENTENCE.		 

       R3.
           ACCEPT TDESCRICAO.        
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R2.
		   IF DESCRICAO = SPACES
             MOVE "DESCRICAO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R3.	   

       R4. 
	       DISPLAY TELAUN.
		   ACCEPT TUNIDADE.
		   MOVE 1 TO IND.
		   ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R3.
		   IF UNIDADE = SPACES
             MOVE "UNIDADE EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R4.
       R4A. 
           MOVE TBUNIDADE(IND) TO TXTUNIDADE.
           IF UN-MAISCULO NOT = UNIDADE AND  
		   UN-MINUSCULO NOT = UNIDADE
             ADD 1 TO IND
               IF IND < 9
               GO TO R4A
           ELSE 
             MOVE "UNIDADE INVALIDA CONSULTE A TABELA" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R4
           ELSE 
             MOVE UN-DESCRICAO TO DUNIDADE.
		   DISPLAY TDUNIDADE.	 
         
       R5.
           DISPLAY TELAPRO.
           DISPLAY TELATP.
           ACCEPT TTPPROD.		  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R4.
           IF TPPROD = ZEROS
             MOVE "TIPO DE PRODUTO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R5.
       R5A.
           IF TPPROD > 8
		     MOVE "TIPO DE PRODUTO INVALIDO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM	
             GO TO R5.
           MOVE TBPRODUTO(TPPROD) TO DTPPRODUTO.
           DISPLAY TELAPRO.
      
       R6.
           ACCEPT TPRECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R5.
           IF PRECO = ZEROS          
             MOVE "PRECO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM 
             GO TO R6.
             
       R7.  
           ACCEPT TDATA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R6.
           IF DATACP = ZEROS             
             MOVE "DATA EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM 
             GO TO R7.
             
       R8.       
           ACCEPT TESTOQUE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R7.
           IF ESTOQUE = ZEROS             
             MOVE "PRECO EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM 
             GO TO R8.

       R9.
           ACCEPT TCPFCNPJ.	   
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01    
             GO TO R8.
           IF CPFCNPJ = ZEROS             
             MOVE "Cpf/Cnpj EM BRANCO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM 
             GO TO R9.
       R9A.
           MOVE CPFCNPJ TO FOR-CPFCNPJ.
           READ ARQFOR
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "FORNECEDOR DIGITADO NAO ENCONTRADO " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
				   MOVE ZEROS TO CPFCNPJ
                   GO TO R9
               ELSE
                  MOVE "ERRO LEITURA ARQUIVO CADASTRO FORNECEDOR" 
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE FOR-RAZAO TO NOME
                MOVE FOR-APELIDO TO APELIDO
                DISPLAY TELAPRO.                
      
       RALT.
             IF W-SEL = 1 
             GO TO ALT-OPC.
             
       R10.
           DISPLAY (22, 20) "DESEJA GRAVAR? S-SIM/N-NAO"    
           ACCEPT (22, 50) DADOS.
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
           WRITE REGPRO.
           GO TO R1.
		   
       ACE-001.
           DISPLAY (22, 16) "N = NOVO REGISTRO | A = ALTERAR |" 
           " E = EXCLUIR | S = SAIR"  
           ACCEPT (23, 12) W-OPCAO
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
           DISPLAY TELAPRO.
           DISPLAY (22, 20) "EXCLUIR   (S/N) : ".
           ACCEPT (22, 50) W-OPCAO
           IF W-OPCAO = "N" OR "n"
              MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO EXC-OPC.

        EXC-DL1.
           DELETE ARQPRO RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
		   
       ALT-OPC.
           DISPLAY TELAPRO.
           DISPLAY (22, 20) "ALTERAR (S/N) : ".
           ACCEPT (22, 50) W-OPCAO
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
           REWRITE REGPRO
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           MOVE "ERRO NA ALTERACAO DO REGISTRO PRODUTO"   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.             
              	   
			  
               



       ROT-FIM.
           CLOSE ARQPRO ARQFOR.
           STOP RUN.    

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (22, 18) MENS.
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
                 