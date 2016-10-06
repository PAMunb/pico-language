*********************************************************************************
*			Autor: Leonardo Henrique Moreira - leohmoreira@gmail.com			*
*			Disciplina: Análise Estática e Transformação de Programas			*
*			Professor: Rodrigo Bonifácio										*
*			Instruções para utilizar o BNFC com haskell							*
*********************************************************************************

1. Introdução
	No desenvolvimento deste parser para linguagem PICO foi utilizado a ferramenta
BNFC (http://bnfc.digitalgrammars.com/) aliada com a linguagem de programaçao haskell.
	Foi utilizado o sistema operacional Ubuntu 16.04 64bit para desenvolvimento e testes.

2. Requisitos
	Para instalar as ferramentas, digite:
	sudo apt-get install bnfc

3. Gramática Concreta
	A gramática concreta da linguagem PICO, utilizada para construção do parser, 
está apresentada no aruivo "pico.cf".

4. Execução do Parser
	Para gerar o parser, execute os comandos:
		bnfc -m -haskell "arquivo da gramatica" (no caso pico.cf)
		Observação:
			Antes de executar o make é preciso editar os arquivos LexPico.x e TestPico.hs.
			No arquivo LexPico.x, adicione "import Data.Char (ord)" na linha 9.
			No arquivo TestPico.hs, adicione "import Data.Char (ord)" na linha 17.
		make
5. Verificação
	Para verificar o funcionamento do parser, execute:
	./"executavel Gerado" arquivo_fonte.pico
	(no caso, o executável será TestPico. O comando será ./TestPico soma.pico)

6. TODO
	1. Ajustar as palavras chaves da gramática com a turma. Por exemplo, estou usando 
"integer" no lugar de "natural"
	2. Verificar com o professor se o parser precisar testar se uma variável foi declarada 
antes de ter atribuição;
