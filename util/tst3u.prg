CLEAR
DO UTIL
SET COLOR TO W/B,GR+/BG,,,W+/BG
OPENBOX(04,08,20,68," PLANILHA DE LANCAMENTOS ")
SET CURSOR ON
@ 08,13 TO 17,13
@ 08,20 TO 17,20
@ 08,30 TO 17,30
@ 08,46 TO 17,46
@ 08,50 TO 17,50
@ 08,09 SAY "DIA "
@ 08,14 SAY "No SEQ"
@ 08,21 SAY "No CHEQUE"
@ 08,31 SAY " VALOR DO LCTO "
@ 08,47 SAY "C/D"
@ 08,51 SAY "     BALANCO     "
@ 07,09 SAY REPL(CHR(196),4)+CHR(194)+REPL(CHR(196),6)+CHR(194)+;
  REPL(CHR(196),9)+CHR(194)+REPL(CHR(196),15)+CHR(194)+REPL(CHR(196),3)+;
  CHR(194)+REPL(CHR(196),17)
@ 09,09 SAY REPL(CHR(196),4)+CHR(197)+REPL(CHR(196),6)+CHR(197)+;
  REPL(CHR(196),9)+CHR(197)+REPL(CHR(196),15)+CHR(197)+REPL(CHR(196),3)+;
  CHR(197)+REPL(CHR(196),17)
@ 17,09 SAY REPL(CHR(196),4)+CHR(193)+REPL(CHR(196),6)+CHR(193)+;
  REPL(CHR(196),9)+CHR(193)+REPL(CHR(196),15)+CHR(193)+REPL(CHR(196),3)+;
   CHR(193)+REPL(CHR(196),17)
@ 18,10 SAY "HISTORICO:"
SET DELETED ON
USE CAFF030
APPEND BLANK
LOADVARS()
SAVEVARS()
INDEX ON STR(BANCO,3)+CHEQUE TO CAFF0301
SET INDEX TO CAFF0301
VCOND = ".T."
SET CURSOR OFF
SPREAD(10,09,16,67,"CAFF030",VCOND,"DISP_2","DISP_1","EDIT_1","EXC_KEY")
CLOSEBOX()
DELETE FOR BANCO=0
PACK
SET CURSOR ON
RETURN

PROCEDURE DISP_1
PRIVATE LIN
PARAMETERS LIN
IF BANCO <> 0
   @ 18,21 SAY ALIGN(NOME,"C",46)
ELSE
   @ 18,21 SAY ALIGN("<<< INCLUSAO DE DADOS >>>","C",46)
   @ 19,21 SAY SPACE(46)
ENDIF
RETURN

FUNCTION DISP_2
PARAMETERS LIN,TMN,TIP
IF TIP
   SVT=SETCOLOR(SD_C1)
   @ LIN,13 SAY CHR(179)
   @ LIN,20 SAY CHR(179)
   @ LIN,30 SAY CHR(179)
   @ LIN,46 SAY CHR(179)
   @ LIN,50 SAY CHR(179)
   SETCOLOR(SVT)
   @ LIN,10 SAY BANCO     PICTURE "999"
   @ LIN,15 SAY BANCO     PICTURE "9999"
   @ LIN,22 SAY CHEQUE    PICTURE "9999999"    
   @ LIN,32 SAY VALOR     PICTURE "99,999,999.99"
   @ LIN,48 SAY " "
   @ LIN,51 SAY VALOR     PICTURE "99,999,999,999.99"
   SETCOLOR(SVT)
ELSE
   @ LIN,10 SAY SPACE(3)
   @ LIN,15 SAY SPACE(4)
   @ LIN,22 SAY SPACE(7)
   @ LIN,32 SAY SPACE(13)
   @ LIN,48 SAY SPACE(1)
   @ LIN,51 SAY SPACE(17)
ENDIF
RETURN(.T.)

FUNCTION EDIT_1
PRIVATE LIN,ET,XDIA,REC
PARAMETERS LIN,TMN
SET CURSOR ON
INC = .F.
IF BANCO = 0
   APPEND BLANK
   INC = .T.
ENDIF
SVT=SETCOLOR(SD_C1)
@ LIN,13 SAY CHR(179)
@ LIN,20 SAY CHR(179)
@ LIN,30 SAY CHR(179)
@ LIN,46 SAY CHR(179)
@ LIN,50 SAY CHR(179)
SETCOLOR(SVT)
IF INC
   STORE 0 TO XBANCO
   @ LIN,10 GET XBANCO PICTURE "999"
   READ
   IF XBANCO <> 0
      REPLACE BANCO       WITH XBANCO
   ENDIF
   IF XBANCO = 0 .OR. LASTKEY() = 27
      DELETE
      GOTO TOP
      RETURN(.F.)
   ENDIF
ENDIF
XTIPOMOV = " "
XNOME = NOME
@ LIN,15 GET BANCO     PICTURE "9999"
@ LIN,22 GET CHEQUE    PICTURE "9999999"    
@ LIN,32 GET VALOR     PICTURE "99,999,999.99"
@ LIN,48 GET XTIPOMOV  PICTURE "!" VALID (XTIPOMOV $ "CDcd")
@  18,21 GET XNOME    PICTURE "@!"
READ
RET = UPDATED()
REPLACE NOME WITH XNOME
IF INC
   GOTO TOP
ELSE
   IF RET
      SKIP -TMN
   ENDIF
ENDIF
SET CURSOR OFF
RETURN(.NOT. RET)

FUNCTION EXC_KEY
PRIVATE KEY,LIN,TMN
PARAMETERS KEY,LIN,TMN
@ 00,60 SAY TIME()
IF KEY = -2 .AND. BANCO <> 0
   MESSAGE()
   IF CONFIRM("EXCLUSAO DO LANCAMENTO DA PLANILHA")
      DELETE
      SKIP -TMN
      SET CURSOR OFF
      RETURN(.F.)
   ENDIF
   SET CURSOR OFF
ENDIF
RETURN(.T.)

*-----------------------------------------------------------------------------*
*                            SPREAD - PLANILHA DE DADOS                       *
*-----------------------------------------------------------------------------*
*
*   SPREAD(10,09,16,67,"LANCAMENTO",VCOND,"DISP_DAT2L","DISP_DAT1L"...)
*   (linha 1,coluna 1,linha 2,coluna 2,area,condicao,UDF 1,UDF 2,...)
*
*   UDF 1 -> Responsavel pela Visualizacao e Limpeza da regiao da planiha.
*
*   Parametros Passados: Linha Atual,Tamanho da Janela
*                        (.T.) se p/ Visualizacao e (.F.) se p/ Limpeza.
*   UDF 2 -> Responsavel pela Visualizacao de Dados Extensos em outra regiao
*            da tela (Especie de MESSAGE da planilha).
*   Parametros Passados: Linha Atual
*
*   UDF 3 -> Responsavel pela Edicao dos dados da Planilha.
* 
*   Parametros Passados: Linha Atual,Tamanho da Janela
*
*   UDF 4 -> Responsavel pela Visualizacao de dados constantemente alterados
*            como a hora, e teclas de excessao.
*   Parametros Passados: Ultima Tecla Pressionada,Linha Atual,Tamanho da Janela
*
*-----------------------------------------------------------------------------*

