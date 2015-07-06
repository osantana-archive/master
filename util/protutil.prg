* ┌───────────────────────────────────────────────────────────────────────────┐
* │≡≡≡ UTIL.LIB - Biblioteca de Funcöes Específicas para Clipper Summer'87 ≡≡≡│
* ├───────────────────────────────────────────────────────────────────────────┤
* │                    Utilizada pela Teenagers Associates                    │
* │                                                                           │
* │ Autores...........: Wagner Longo de Castro                                │
* │ Ultima atualizaçäo: 21/09/92                                              │
* │ No de funçöes.....: 30                                                    │
* ├───────────────────────────────────────────────────────────────────────────┤
* │                     Indice das Funçöes da Biblioteca                      │
* ╞══════════»» BCO DE DADOS ««════════╤═══════════»» IMPRESSAO ««════════════╡
* │ - ADDDBF()    - ADICIONA BCO DADOS │ - PRTON()     - ESPERA AVISO IMPRES. │
* │ - INITDBF()   -     ABRE BCO DADOS │ - ESCPRINT()  - INTERROMPE IMPRESSAO │
* │ - INDEXDBF()  -   INDEXA BCO DADOS │ - HEAD()      -     COLOCA CABECALHO │
* │ - CLOSEDBF()  -    FECHA BCO DADOS │ - FOOT()      -     COLOCA    RODAPE │
* │ - ZAPDBF()    -    LIMPA BCO DADOS │                                      │
* │ - SPREAD()    -  PLANILHA DE DADOS ╞════════»» ENTRADA DE DADOS ««════════╡
* ╞═════════════»» TELAS ««════════════╡ - ACESSCODE() -        AGUARDA SENHA │
* │ - SIGNAL()    - QUADRO ADVERTENCIA ╞═══════»» VALIDACAO DE DADOS ««═══════╡
* │ - MESSAGE()   - MENSAGEM NO RODAPE │ - CHKGET()    - VALIDA ENTRADA - GET │
* │ - WARNING()   -    AVISO NO RODAPE │ - VERIFY()    - VERIFICA INTEGRIDADE │
* │ - CONFIRM()   -        CONFIRMACAO ╞════════»» FUNCOES SORTIDAS ««════════╡
* │ - DSP()       -  MOSTRA EM REVERSO │                                      │
* │ - FADE()      -  EFEITOS ESPECIAIS │ - NAME()      - RETORNA NOMES COMUNS │
* └────────────────────────────────────┴──────────────────────────────────────┘
* ┌───────────────────────────────────────────────────────────────────────────┐
* │≡≡≡ UTIL.LIB - Biblioteca de Funcöes Específicas para Clipper Summer'87 ≡≡≡│
* ├───────────────────────────────────────────────────────────────────────────┤
* │                    Utilizada pela Teenagers Associates                    │
* │                                                                           │
* │  Variáveis Pré-Definidas:                                                 │
* │  - SYS_CR - Cores Empregadas pelas Funçöes                                │
* │                                                                           │
* │                                                                           │
* └───────────────────────────────────────────────────────────────────────────┘

PUBLIC SYS_CR[10],SYS_FM[13],SYS_PRC[05]
SYS_CR[01] = "N/W,W+/BG,,,GR+/B"      && Cor de Borda de Quadros Internos
SYS_CR[02] = "W/B,W+/BG,,,W/RB"       && Cor de Preenchimento (Say/Get).
SYS_CR[03] = "W+/BG,W+/B,,,W/RB"      && Cor de Escrita em destaque 1.
SYS_CR[04] = "W/B,W+/BG,,,W+/B"       && Cor de Edicao em Menu Prompt().
SYS_CR[05] = "G+/B,GR+/BG,,,W/RB"     && Cor de Escrita em Destaque 2.
SYS_CR[06] = "B/N,W+/BG,,,W/RB"       && Cor de Preenchimento do hachurado.
SYS_CR[07] = "W+/G,W+/BG,,,W+/RB"     && Cor de Escrita em destaque 3.
SYS_CR[08] = "W+/B,W+/BG,,,N/W"       && Cor de Quadros para Indexaçäo.
SYS_CR[09] = "W+/R,GR+/GR,,,W+/R"     && Cor de Quadros de Advertência.
SYS_CR[10] = "W+/B,BG/BG,,,BG/BG"     && Cor de Ediçäo de Senhas.
FOR UTL_TMP = 1 TO 13
    SYS_FM[UTL_TMP] = FRAMES(UTL_TMP)
NEXT
SYS_PRC[01]= CHR(032)
SYS_PRC[02]= CHR(176)
SYS_PRC[03]= CHR(177)
SYS_PRC[04]= CHR(178)
SYS_PRC[05]= CHR(219)
RETURN
*--------------------- FUNCOES ESPECIAIS DE INTERFACE -----------------------*

FUNCTION SPC_TYPE
PRIVATE VR_X
PARAMETERS VR_X
TP = TYPE("VR_X")
DO CASE
CASE TP = "C" .OR. TP = "M"
   RETURN(SPACE(LEN(VR_X)))
CASE TP = "N"
   RETURN(SPACE(LEN(LTRIM(STR(VR_X)))))
CASE TP = "D"
   RETURN(SPACE(8))
CASE TP = "L"
   RETURN(" ")
ENDCASE
RETURN("")

FUNCTION INIT_DBF
PRIVATE CT,ARQ,AR,CMT,CP,MCR,IND,ADF,RET
PARAMETERS CT
ARQ = TRIM(UTL_DBF[DIM(CT,1,5)])
AR  = TRIM(UTL_DBF[DIM(CT,3,5)])
CMT = UTL_DBF[DIM(CT,2,5)]
ADF = UTL_DBF[DIM(CT,5,5)]
RET = .F.
IF FILE(ARQ) .AND. ACT
   MESSAGE("Aguarde...Abrindo Banco de Dados : " + CMT + "...")
   SELECT 0
   PROTECT(ARQ,.F.)
   USE &ARQ ALIAS &AR
   MESSAGE("Aguarde...Abrindo Indices do Arquivo : " + CMT + "...") 
   FOR CP = 1 TO 15
       MCR = STRZERO(CP,2)
       IND = TRIM(LEFT(UTL_DBF[DIM(CT,1,5)],6)) + MCR + ".NTX"
       IF FILE(IND)
          ID&MCR = IND
       ELSE
          ID&MCR = ""
          RET = IF(CP>ADF,RET,.T.)
       ENDIF
   NEXT
   SET INDEX TO &ID01,&ID02,&ID03,&ID04,&ID05,&ID06,&ID07,&ID08,&ID09,;
   &ID10,&ID11,&ID12,&ID13,&ID14,&ID15
ENDIF
RETURN(RET)

FUNCTION CLOS_DBF
PRIVATE CT,ARQ,CMT,AR,EXC
PARAMETERS CT
ARQ = TRIM(UTL_DBF[DIM(CT,1,5)])
CMT = UTL_DBF[DIM(CT,2,5)]
AR  = LTRIM(STR(CT,3))
IF ACT
   SELECT &AR
   IF USED()
      MESSAGE("Aguarde...Fechando Arquivo : " + CMT + "...")
      COUNT FOR DELETED() TO EXC
      IF EXC <> 0
         PACK
      ENDIF
      USE
      PROTECT(ARQ,.T.)
   ENDIF
ENDIF
RETURN(.F.)

FUNCTION ZAP_DBF
PRIVATE CT,ARQ,CMT,AR
PARAMETERS CT
ARQ = TRIM(UTL_DBF[DIM(CT,1,5)])
CMT = UTL_DBF[DIM(CT,2,5)]
AR  = LTRIM(STR(CT,3))
IF ACT
   SELECT &AR
   IF USED()
      MESSAGE("Aguarde...Limpando Arquivo : " + CMT + "...")
      ZAP
   ENDIF
ENDIF
RETURN(.T.)

FUNCTION FINDEX
PRIVATE CMP_X
PARAMETERS CMP_X
IF .NOT. EOF()
   PERCENT(RECNO(),RECCOUNT(),18,26,44)
ENDIF
RETURN(&CMP_X)

FUNCTION OPEN_BOX
PRIVATE L1,C1,L2,C2,TIT,MOLD,C_MP,C_PR,C_TIT,VF,TP,VD,SVT,TL1
IF TYPE("UTL_IBOX") = "U"
   PUBLIC UTL_BOX[50],UTL_IBOX
   STORE 0 TO UTL_IBOX
ENDIF
UTL_IBOX = UTL_IBOX + 1
PARAMETERS L1,C1,L2,C2,TIT,MOLD,C_MP,C_PR,C_TIT,VF,TP,VD
IF TYPE("L1") = "C" .AND. UTL_IBOX = 1
   RETURN(.F.)
ELSEIF TYPE("L1") = "C" .OR. TYPE("L1") = "U"
   VD    = IF(TYPE("C_PR") = "U",0,C_PR)
   TP    = IF(TYPE("C_MP") = "U",1,C_MP)
   VF    = IF(TYPE("MOLD") = "U",.T.,MOLD)
   C_TIT = IF(TYPE("TIT")  = "U",GETCOLOR(2),TIT)
   C_PR  = IF(TYPE("C2")   = "U",GETCOLOR(1),C2)
   C_MP  = IF(TYPE("L2")   = "U",GETCOLOR(5),L2)
   MOLD  = IF(TYPE("C1")   = "U",SYS_FM[01]+SYS_PRC[01],C1)
   TIT   = IF(TYPE("L1")   = "U","",L1)
   L1    = 02 + VAL(SUBSTR(UTL_BOX[UTL_IBOX-1],01,02))
   C1    = 04 + VAL(SUBSTR(UTL_BOX[UTL_IBOX-1],03,02))
   L2    = 02 + VAL(SUBSTR(UTL_BOX[UTL_IBOX-1],05,02))
   C2    = 04 + VAL(SUBSTR(UTL_BOX[UTL_IBOX-1],07,02))
ELSEIF TYPE("L1") = "N"
   VF    = IF(TYPE("VF")   = "U",.T.,VF)
   TP    = IF(TYPE("TP")   = "U",1,TP)
   VD    = IF(TYPE("VD")   = "U",0,VD)
   C_MP  = IF(TYPE("C_MP") = "U",GETCOLOR(5),C_MP)
   C_PR  = IF(TYPE("C_PR") = "U",GETCOLOR(1),C_PR)
   C_TIT = IF(TYPE("C_TIT")= "U",GETCOLOR(2),C_TIT) 
   MOLD  = IF(TYPE("MOLD") = "U",SYS_FM[01]+SYS_PRC[01],MOLD)
   TIT   = IF(TYPE("TIT")  = "U","",TIT)
ENDIF
SAVE SCREEN TO TL1
SVT = SETCOLOR()
UTL_BOX[UTL_IBOX] = STRZERO(L1,2) + STRZERO(C1,2) + STRZERO(L2,2) + STRZERO(C2,2);
 + ALIGN(MOLD,"L",9) + ALIGN(C_MP,"L",6) + TL1
SETCOLOR(C_MP)
EXPLODE(L1,C1,L2,C2,MOLD,0,TP,VD)
SETCOLOR(C_PR)
@ L1+1,C1+1 CLEAR TO L2-1,C2-1
IF VF
   BORDER(L1,C1,L2,C2,8)
ENDIF
SETCOLOR(C_TIT)
@ L1 , C1 + ROUND((C2 - C1 - LEN(TIT)) / 2,0)  SAY TIT
SETCOLOR(SVT)
RETURN(.T.)

FUNCTION CLOS_BOX
IF TYPE("UTL_BOX[UTL_IBOX]") = "U"
   RELEASE UTL_BOX,UTL_IBOX
   RETURN(.F.)
ENDIF
PRIVATE TE,SVT
PARAMETERS TE
TE = IF(TYPE("TE")="U",0,TE)
SVT = SETCOLOR(SUBSTR(UTL_BOX[UTL_IBOX],18,6))
IMPLODE(VAL(SUBSTR(UTL_BOX[UTL_IBOX],01,02)),VAL(SUBSTR(UTL_BOX[UTL_IBOX],03,02)),;
VAL(SUBSTR(UTL_BOX[UTL_IBOX],05,02)),VAL(SUBSTR(UTL_BOX[UTL_IBOX],07,02)),;
SUBSTR(UTL_BOX[UTL_IBOX],09,09),SUBSTR(UTL_BOX[UTL_IBOX],24),0,TE)
SETCOLOR(SVT)
UTL_IBOX = UTL_IBOX - 1
RETURN(.F.)

PROCEDURE SPRD_01
PRIVATE S1_T1,S1_T2
SETCOLOR(SD_C1)
S1_T1 = SD_P7+"(SD_L1,SD_LG,.T.)"
S1_T2 = &S1_T1
RETURN

PROCEDURE SPRD_02
PRIVATE S2_T1,S2_T2
S2_T1 = SD_P8+"(SD_L1)"
S2_T2 = &S2_T1
SETCOLOR(SD_C2)
S2_T1 = SD_P7+"(SD_L1,SD_LG,.T.)"
S2_T2 = &S2_T1
SETCOLOR(SD_C1)
RETURN

*----------------- FUNCOES ESPECIAIS DA BIBLIOTECA UTIL.LIB ------------------*

*------------------------FUNCOES ESPECIAIS PARA ARQUIVOS ---------------------*

*******************************************************************************
* FUNCAO: ADDDBF                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: INCLUI BCOS DE DADOS NA LISTA                                   *
* SINTAXE   : ADDDBF (nome do arquivo,comentario,alias,matriz de nomes de     *
*                     indices,matriz de comentarios de indices)               *
*******************************************************************************

FUNCTION ADDDBF
PRIVATE ARQ,CMT,AR,NTX,CMP,ADI,ADF
PARAMETERS ARQ,CMT,AR,NTX,CMP
IF TYPE("UTL_DBF") <> "A"
   PUBLIC UTL_DBF[1],UTL_NTX[1],UTL_IDBF,UTL_INTX
   UTL_IDBF = 0
   UTL_INTX = 1
ENDIF
ADI = UTL_INTX
ADF = LEN(NTX)
FOR CT = 1 TO ADF
   AADD(UTL_NTX,NTX[CT]         ,DIM(UTL_INTX,1,2))
   AADD(UTL_NTX,LEFT(CMP[CT],20),DIM(UTL_INTX,2,2))
   UTL_INTX = UTL_INTX + 1
NEXT
UTL_IDBF = UTL_IDBF + 1
AADD(UTL_DBF,LEFT(ARQ,12),DIM(UTL_IDBF,1,5))
AADD(UTL_DBF,LEFT(CMT,20),DIM(UTL_IDBF,2,5))
AADD(UTL_DBF,LEFT(AR,10) ,DIM(UTL_IDBF,3,5))
AADD(UTL_DBF,ADI         ,DIM(UTL_IDBF,4,5))
AADD(UTL_DBF,ADF         ,DIM(UTL_IDBF,5,5))
RETURN(FILE(ARQ))

*******************************************************************************
* FUNCAO: INITDBF                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: ABRE OS ARQUIVOS COM SEUS RESPECTIVOS INDICES                   *
* SINTAXE   : INITDBF (matriz logica com os arquivos a abrir)                 *
*******************************************************************************

FUNCTION INITDBF
PRIVATE TLX,SVX,ARS,LP,ARQIDX,RTS,RTT,ID01,ID02,ID03,ID04,ID05,ID06,ID07,ID08,;
ID09,ID10,ID11,ID12,ID13,ID14,ID15
PARAMETERS ARQIDX
ARQIDX = IF(TYPE("ARQIDX")="U",.T.,ARQIDX)
IF TYPE("UTL_DBF") <> "A"
   RETURN(.F.)
ENDIF
SAVE SCREEN TO TLX
SVX = SETCOLOR(SYS_CR[2])
ARS = ALIAS()
RTS = .F.
IF TYPE("ARQIDX") <> "N"
   FOR LP = 1 TO UTL_IDBF
      IF LP = 2
         ALTD(2)
      ENDIF  
      ACT = IF(TYPE("ARQIDX")="L",ARQIDX,ARQIDX[LP])
      RTT = INIT_DBF(LP)
      RTS = IF(RTS,.T.,RTT)
   NEXT
ELSE
   ACT = .T.
   RTS = INIT_DBF(ARQIDX)
ENDIF
IF SELECT(ARS) <> 0
   SELECT &ARS.
ENDIF
RESTORE SCREEN FROM TLX
SETCOLOR(SVX)
RETURN(RTS)

*******************************************************************************
* FUNCAO: INDEXDBF                                                            *
*-----------------------------------------------------------------------------*
* UTILIZACAO: INDEXA OS ARQUIVOS DA LISTA                                     *
* SINTAXE   : INDEXDBF (matriz logica com os arquivos a indexar,nivel index.) *
*******************************************************************************

FUNCTION INDEXDBF
PRIVATE SCR,SVX,AR,CT,ARQ,AR,CMT,ADI,ADF,TL,CP,ARX,LN,LM,NOSL,EXPR,COMM,ANTX,;
ARQIDX,FIDX,MCR,IND,ID01,ID02,ID03,ID04,ID05,ID06,ID07,ID08,ID09,ID10,ID11,;
ID12,ID13,ID14,ID15
PARAMETERS ARQIDX,FIDX
ARQIDX = IF(TYPE("ARQIDX")="U",.T.,ARQIDX)
FIDX = IF(TYPE("FIDX")="U",.F.,FIDX)
IF TYPE("UTL_DBF") <> "A"
   RETURN(.F.)
ENDIF
SAVE SCREEN TO SCR
SVX = SETCOLOR(SYS_CR[08])
AR  = SELECT()
OPEN_BOX(08,02,14,26," Bancos de Dados ")
OPEN_BOX(08,49,14,73," Arquivos Indice ")
OPEN_BOX(17,25,19,75," Gráfico de Operaçäo Realizada ",SYS_FM[11]+SYS_PRC[01])
DECLARE ARQ[UTL_IDBF],AR[UTL_IDBF],CMT[UTL_IDBF],ADI[UTL_IDBF],ADF[UTL_IDBF]
FOR CT = 1 TO UTL_IDBF
   ARQ[CT] = TRIM(UTL_DBF[DIM(CT,1,5)])
   AR [CT] = TRIM(UTL_DBF[DIM(CT,3,5)])
   CMT[CT] = UTL_DBF[DIM(CT,2,5)]
   ADI[CT] = UTL_DBF[DIM(CT,4,5)]
   ADF[CT] = UTL_DBF[DIM(CT,5,5)]
NEXT
TL = IF(UTL_IDBF>5,5,UTL_IDBF)
FOR CP = 1 TO TL
   @ 08+CP,05 SAY CMT[CP]
NEXT
DSP(09,05,CMT[1],20)
FOR CP = 1 TO UTL_IDBF
   IF CP >= 6
      @ 13,05 SAY ALIGN(CMT[CP-1],"L",20)
      SCROLL(09,03,13,24,1)
      LN = 5
   ELSE
      LN = CP
   ENDIF
   ARX = AR[CP]
   IF SELECT(ARX) <> 0
      SELECT &ARX
      MESSAGE("Aguarde...Organizando Indices : " + CMT[CP] + "...")
   ENDIF
   DSP(08+LN,05,CMT[CP],20)
   IF LN <> 1
      @ 08+LN-1,05 SAY ALIGN(CMT[CP-1],"L",20)
   ENDIF
   SETCOLOR(LEFT(SYS_CR[08],AT(",",SYS_CR[08])-1)+"*")
   @ 08+LN,03 SAY CHR(7)
   SETCOLOR(SYS_CR[08])
   SCROLL(09,50,13,72,5)
   NOSL = 1
   DECLARE EXPR[ADF[CP]],COMT[ADF[CP]]
   FOR CT = 0 TO ADF[CP]-1
      EXPR[CT+1] = TRIM(UTL_NTX[DIM(ADI[CP]+CT,1,2)])
      COMT[CT+1] = UTL_NTX[DIM(ADI[CP]+CT,2,2)]
   NEXT
   TL = IF(ADF[CP]>5,5,ADF[CP])
   FOR CT = 1 TO TL
      @ 08+CT,52 SAY COMT[CT]
   NEXT
   DSP(09,52,COMT[1],20)
   IF IF(TYPE("ARQIDX")="L",ARQIDX,IF(TYPE("ARQIDX")="N",CP=ARQIDX,ARQIDX[CP]))
      STORE "" TO ID01,ID02,ID03,ID04,ID05,ID06,ID07,ID08,ID09,ID10,ID11,;
      ID12,ID13,ID14,ID15
      FOR CT = 1 TO ADF[CP]
         ANTX = TRIM(ARQ[CP])
         ANTX = LEFT(LEFT(ANTX,AT(".",ANTX)-1),6)+STRZERO(NOSL,2)+".NTX"
         IF CT >= 6
            @ 13,52 SAY ALIGN(COMT[CT-1],"L",20)
            SCROLL(09,50,13,72,1)
            LM = 5
         ELSE
            LM = CT
         ENDIF
         DSP(08+LM,52,COMT[CT],20)
         IF LM <> 1
            @ 08+LM-1,52 SAY ALIGN(COMT[CT-1],"L",20)
         ENDIF
         SETCOLOR(LEFT(SYS_CR[08],AT(",",SYS_CR[08])-1)+"*")
         @ 08+LM,50 SAY CHR(7)
         SETCOLOR(SYS_CR[08])
         IF .NOT. FILE(ANTX) .OR. FIDX
            INDEX ON FINDEX(EXPR[CT]) TO (ANTX)
            CLOSE INDEX          
            CHV = EXPR[CT]+REPLICATE(CHR(0),255-LEN(EXPR[CT]))
            PRIVATE HND
            HND = FOPEN(ANTX,2)
            FSEEK(HND,22,0)
            FWRITE(HND,CHV,255)
            FCLOSE(HND)
         ELSE
            PERCENT(1,1,18,26,44)
         ENDIF
         MCR = STRZERO(CT,2)
         ID&MCR = ANTX
         @ 08+LM,50 SAY CHR(251)
         NOSL = NOSL + 1
      NEXT
      MESSAGE("Aguarde...Abrindo Indices do Arquivo : " + CMT[CP] + "...") 
      SET INDEX TO &ID01,&ID02,&ID03,&ID04,&ID05,&ID06,&ID07,&ID08,&ID09,;
      &ID10,&ID11,&ID12,&ID13,&ID14,&ID15
   ENDIF
   @ 08+LN,03 SAY CHR(251)
NEXT
CLOS_BOX()
CLOS_BOX()
CLOS_BOX()
RESTORE SCREEN FROM SCR
SETCOLOR(SVX)
RETURN(DOSERROR())

*******************************************************************************
* FUNCAO: CLOSEDBF                                                            *
*-----------------------------------------------------------------------------*
* UTILIZACAO: FECHA OS ARQUIVOS DA LISTA                                      *
* SINTAXE   : CLOSEDBF (matriz logica com os arquivos a fechar)               *
*******************************************************************************

FUNCTION CLOSEDBF
PRIVATE SCR,SVX,ARS,LP,ACT,ARQIDX
PARAMETERS ARQIDX
ARQIDX = IF(TYPE("ARQIDX")="U",.T.,ARQIDX)
IF TYPE("UTL_DBF") <> "A"
   RETURN(.F.)
ENDIF
SAVE SCREEN TO SCR
SVX = SETCOLOR(SYS_CR[02])
ARS = ALIAS()
IF TYPE("ARQIDX") <> "N"
   FOR LP = 1 TO UTL_IDBF
      ACT = IF(TYPE("ARQIDX")="L",ARQIDX,ARQIDX[LP])
      CLOS_DBF(LP)
   NEXT
ELSE
   ACT = .T.
   CLOS_DBF(ARQIDX)
ENDIF
IF SELECT(ARS) <> 0
   SELECT &ARS.
ENDIF
RESTORE SCREEN FROM SCR
SETCOLOR(SVX)
RETURN(DOSERROR())

*******************************************************************************
* FUNCAO: ZAPDBF                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: LIMPA OS ARQUIVOS DA LISTA                                      *
* SINTAXE   : ZAPDBF (matriz logica com os arquivos a limpar)                 *
*******************************************************************************

FUNCTION ZAPDBF
PRIVATE TLX,SVX,ARS,LP,ACT,ARQIDX
PARAMETERS ARQIDX
ARQIDX = IF(TYPE("ARQIDX")="U",.T.,ARQIDX)
IF TYPE("UTL_DBF") <> "A"
   RETURN(.F.)
ENDIF
SAVE SCREEN TO TLX
SVX = SETCOLOR(SYS_CR[02])
ARS = SELECT()
IF TYPE("ARQIDX") <> "N"
   FOR LP = 1 TO UTL_IDBF
      ACT = IF(TYPE("ARQIDX")="L",ARQIDX,ARQIDX[LP])
      ZAP_DBF(LP)
   NEXT
ELSE
   ACT = .T.
   ZAP_DBF(ARQIDX)
ENDIF
INDEXDBF()
IF SELECT(ARS) <> 0
   SELECT &ARS.
ENDIF
RESTORE SCREEN FROM TLX
SETCOLOR(SVX)
RETURN(DOSERROR())

*******************************************************************************
* FUNCAO: SPREAD                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: CRIA UMA PLANILHA DE APRESENTACAO DE DADOS NA TELA.             *
* SINTAXE   : SPREAD (linha 1,coluna 1,linha 2,coluna 2,area,condicao,UDF 1,  *
*             UDF 2,UDF 3,UDF 4)                                              *
*   UDF 1 -> Responsavel pela Visualizacao e Limpeza da regiao da planiha.    *
*   Parametros Passados: Linha Atual,Tamanho da Janela                        *
*                        (.T.) se p/ Visualizacao e (.F.) se p/ Limpeza.      *
*   UDF 2 -> Responsavel pela Visualizacao de Dados Extensos em outra regiao  *
*            da tela (Especie de MESSAGE da planilha).                        *
*   Parametros Passados: Linha Atual                                          *
*   UDF 3 -> Responsavel pela Edicao dos dados da Planilha.                   *
*   Parametros Passados: Linha Atual,Tamanho da Janela                        *
*   UDF 4 -> Responsavel pela Visualizacao de dados constantemente alterados  *
*            como a hora, e teclas de excessao.                               *
*   Parametros Passados: Ultima Tecla Pressionada,Linha Atual,Tamanho Janela  *
*******************************************************************************

FUNCTION SPREAD
PRIVATE SD_P1,SD_P2,SD_P3,SD_P4,SD_P5,SD_P6,SD_P7,SD_P8,SD_AR,SD_FT,SD_CT
PRIVATE SD_L3,SD_T1,SD_T2,SD_T3,SD_C1,SD_C2,SD_L1,SD_KY,SD_L2,SD_LG,SD_RC
PARAMETERS SD_P1,SD_P2,SD_P3,SD_P4,SD_P5,SD_P6,SD_P7,SD_P8,SD_P9,SD_P10
SD_AR = ALIAS()
SD_FT = DBFILTER()
SELECT &SD_P5
SET FILTER TO &SD_P6
GOTO TOP
SD_CT = .T.
SD_L1 = SD_P1
SD_KY =  0
SD_L2 = SD_P3
SD_LG = SD_P3-SD_P1
SD_C1 = GETCOLOR(1)
SD_C2 = GETCOLOR(2)
DO WHILE SD_CT
   SD_L3 = SD_P1
   SD_T1 = SD_P7+"(SD_L3,SD_LG,.T.)"
   DO WHILE SD_L3 <= SD_P3 .AND. .NOT. EOF()
       SD_T2 = &SD_T1
       IF .NOT. SD_T2
          SD_L1 = SD_P1
          EXIT
       ENDIF
       SD_L3 = SD_L3 + 1
       SKIP +1
   ENDDO
   SD_T2 = SD_P7+"(SD_T1,SD_LG,.F.)"      
   FOR SD_T1 = SD_L3 TO SD_P3
       SD_T3 = &SD_T2
   NEXT
   SD_L2 = SD_L3
   SKIP SD_L1-SD_L3      
   SPRD_02() 
   DO WHILE .T.
      SD_KY = INKEY()
      SD_T1 = SD_P10+"(SD_KY,SD_L1,SD_LG)"
      SD_T2 = &SD_T1
      IF .NOT. SD_T2
         SD_L1 = SD_P1
         EXIT
      ENDIF
      DO CASE
      CASE SD_KY = 0
         LOOP
      CASE SD_KY = 24
         IF SD_L1 < SD_P3
            SPRD_01()
            SKIP +1
            IF EOF()
               SKIP -1
            ELSE
               SD_L1 = SD_L1 + 1
            ENDIF
            SPRD_02()
            LOOP
         ELSE
            SPRD_01()
            SKIP +1
            IF EOF()
               SKIP -1
            ELSE
               SCROLL(SD_P1,SD_P2,SD_P3,SD_P4,1)       
            ENDIF
            SPRD_02()
            LOOP
         ENDIF
      CASE SD_KY = 5
         IF SD_L1 > SD_P1
            SPRD_01()
            SKIP -1
            IF .NOT. BOF()
               SD_L1 = SD_L1 - 1
            ENDIF
            SPRD_02()
            LOOP
         ELSE
            SPRD_01()
            SKIP -1
            IF .NOT. BOF()
               SCROLL(SD_P1,SD_P2,SD_P3,SD_P4,-1) 
            ENDIF
            SPRD_02()
            LOOP
         ENDIF      
      CASE SD_KY = 1
         GOTO TOP
         SD_L1 = SD_P1
         EXIT
      CASE SD_KY = 6
         GOTO BOTTOM
         IF LASTREC() > SD_LG
            SKIP -(SD_LG)
            SD_L1 = SD_P3
         ELSE
            GOTO TOP
         ENDIF
         EXIT
      CASE SD_KY = 18
         SKIP -(SD_LG)
         EXIT
      CASE SD_KY = 3
         SD_T1 = RECNO()
         SKIP (SD_LG)
         IF EOF()
            GOTO SD_T1
            SD_L1 = SD_P1
         ENDIF
         EXIT
      CASE SD_KY = 13
         SD_T1 = SD_P9+"(SD_L1,SD_LG)"
         SD_T2 = &SD_T1
         IF .NOT. SD_T2
            EXIT
         ENDIF                  
      CASE SD_KY = 27
         SD_CT = .NOT. SD_CT
         EXIT
      ENDCASE
   ENDDO
ENDDO
SET FILTER TO &SD_FT
RETURN(.T.)

*------------------------ FUNCOES ESPECIAIS PARA TELAS -----------------------*

*******************************************************************************
* FUNCAO: SIGNAL                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA UM QUADRO DE ADVERTENCIA NA TELA                         *
* SINTAXE   : SIGNAL (Titulo 1,Titulo 2,Titulo 3)                             *
*******************************************************************************

FUNCTION SIGNAL
PRIVATE T1,T2,T3,SVT
PARAMETERS T1,T2,T3
SVT = SETCOLOR(SYS_CR[09])
OPEN_BOX(9,20,15,60,"  Advertência  ")
@ 11,(41-LEN(T1))/2+20 SAY T1
@ 12,(41-LEN(T2))/2+20 SAY T2
@ 13,(41-LEN(T3))/2+20 SAY T3
TONE(600,2)
INKEY(180)
CLOS_BOX()
SETCOLOR(SVT)
RETURN(.T.)

*******************************************************************************
* FUNCAO: MESSAGE                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA UMA MENSAGEM EM UMA LINHA DEFINIDA                       *
* SINTAXE   : MESSAGE (Titulo,Flag de Centralizacao,Flag de Atributo de Cor)  *
*******************************************************************************

FUNCTION MESSAGE
PRIVATE T1,CT,CL,PS,RT,SVT
PARAMETERS T1,CT,PS
CT  = IF(TYPE("CT")="U",.F.,CT)
PS  = IF(TYPE("PS")="U",.F.,PS)
T1  = IF(TYPE("T1")="U",SPACE(68),LEFT(T1,68))
CL  = 11
RT  = CL+LEN(T1)
T1  =  IF(CT,ALIGN(T1,"C",68),T1+SPACE(68-LEN(T1)))
SVT = SETCOLOR(SYS_CR[02])
@ 23,01 SAY "Mensagem: "
SETCOLOR(IF(PS,LEFT(SYS_CR[02],AT(",",SYS_CR[02])-1)+"*",SYS_CR[02]))
@ 23,CL SAY T1
SETCOLOR(SVT)
RETURN(RT)

*******************************************************************************
* FUNCAO: WARNING                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA UMA MENSAGEM E AGUARDA UMA TECLA QUALQUER                *
* SINTAXE   : WARNING(Titulo,Flag de Centralizacao,Tempo de Espera)           *
*******************************************************************************

FUNCTION WARNING
PRIVATE T1,CT,TE,SAV
PARAMETERS T1,CT,TE
TE = IF(TYPE("TE")="U",0,TE)
CT = IF(TYPE("CT")="U",.F.,CT)
SAV = SAVESCREEN(23,01,23,78)
MESSAGE(T1,CT)
INKEY(TE)
RESTSCREEN(23,01,23,78,SAV)
RETURN(.T.)

*******************************************************************************
* FUNCAO: CONFIRM                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: SOLICITA A ENTRADA DE UMA CONFIRMACAO                           *
* SINTAXE   : CONFIRM (Referencia a Confirmacao)                              *
*******************************************************************************

FUNCTION CONFIRM
PRIVATE SVT,SAV,UQ,CF,COL
PARAMETERS UQ
SVT = SETCOLOR()
SAV = SAVESCREEN(23,01,23,78)
COL = MESSAGE("Confirma "+uq+" (S/N) ")
CF  = SPACE(1)
SET CURSOR ON
SET INTENSITY OFF
@ 23,COL GET CF VALID (CF $ "SNsn")
READ
SET INTENSITY ON
SET CURSOR OFF
SETCOLOR(SVT)
RESTSCREEN(23,01,23,78,SAV)
RETURN(UPPER(CF) = "S" .AND. .NOT. LASTKEY() = 27)

*******************************************************************************
* FUNCAO: DSP                                                                 *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA OS DADOS NA COORDENADA ESPECIFICADA COM COR REALCE       *
* SINTAXE   : DSP (Linha,Coluna,Mensagem,Tamanho Maximo)                      *
*******************************************************************************

FUNCTION DSP
PRIVATE CY,CX,MSX,LGH,SVT
PARAMETERS CY,CX,MSX,LGH
LGH = IF(TYPE("LGH")="U",LEN(MSX),LGH)
SVT = SETCOLOR(SYS_CR[03])
@ CY,CX SAY ALIGN(MSX,"L",LGH)
SETCOLOR(SVT)
RETURN(.T.)

*******************************************************************************
* FUNCAO: FADE                                                                *
*-----------------------------------------------------------------------------*
* UTILIZACAO: FAZ EFEITOS ESPECIAIS DE ENTRADA DE TELA (Explode,Implode,Etc.) *
* SINTAXE   : FADE (Linha 1,Coluna 1,Linha 2,Coluna 2,Tela,Tipo)              *
*******************************************************************************

FUNCTION FADE
PRIVATE VW_P1,VW_P2,VW_P3,VW_P4,VW_TP,VW_TM,VW_SV,VW_TL
VW_TP = ROUND(VAL(RIGHT(TIME(),2))/5.5,0)
PARAMETERS VW_P1,VW_P2,VW_P3,VW_P4,VW_SV,VW_TP
VW_M1 = INT((VW_P3-VW_P1)/2)
VW_M2 = INT((VW_P4-VW_P2)/2)
DO CASE
CASE VW_TP = 1
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 2
   FOR VW_TM = VW_P1 TO VW_P3
       RESTSCREEN(VW_TM,VW_P2,VW_TM,VW_P4,SECTORSCR(VW_SV,VW_TM,VW_P2,VW_TM,VW_P4))
       DELAY(40)
   NEXT
CASE VW_TP = 3
   FOR VW_TM = VW_P3 TO VW_P1 STEP -1
       RESTSCREEN(VW_TM,VW_P2,VW_TM,VW_P4,SECTORSCR(VW_SV,VW_TM,VW_P2,VW_TM,VW_P4))
       DELAY(40)
   NEXT
CASE VW_TP = 4
   FOR VW_TM = VW_P1 TO VW_P3-1
       VW_TL = SAVESCREEN(VW_P1,VW_P2,VW_P3,VW_P4)
       RESTSCREEN(VW_TM+1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_TL,VW_TM-VW_P1,00,VW_P3-VW_P1-1,VW_P4-VW_P2,VW_P4-VW_P2+1))
       RESTSCREEN(VW_TM,VW_P2,VW_TM,VW_P4,SECTORSCR(VW_SV,VW_TM,VW_P2,VW_TM,VW_P4))   
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 5
   FOR VW_TM = VW_P3 TO VW_P1 STEP -1
       VW_TL = SAVESCREEN(VW_P1,VW_P2,VW_P3,VW_P4)
       RESTSCREEN(VW_TM,VW_P2,VW_TM,VW_P4,SECTORSCR(VW_SV,VW_TM,VW_P2,VW_TM,VW_P4))   
       RESTSCREEN(VW_P1,VW_P2,VW_TM-1,VW_P4,SECTORSCR(VW_TL,01,00,VW_TM-VW_P1,VW_P4-VW_P2,VW_P4-VW_P2+1))
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 6
   FOR VW_TM = VW_P2 TO VW_P4-1
       VW_TL = SAVESCREEN(VW_P1,VW_P2,VW_P3,VW_P4)
       RESTSCREEN(VW_P1,VW_TM+1,VW_P3,VW_P4,SECTORSCR(VW_TL,00,VW_TM-VW_P2,VW_P3-VW_P1,VW_P4-VW_P2-1,VW_P4-VW_P2+1))
       RESTSCREEN(VW_P1,VW_TM,VW_P3,VW_TM,SECTORSCR(VW_SV,VW_P1,VW_TM,VW_P3,VW_TM))
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 7
   FOR VW_TM = VW_P4 TO VW_P2+1 STEP -1
       VW_TL = SAVESCREEN(VW_P1,VW_P2,VW_P3,VW_P4)
       RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_TM-1,SECTORSCR(VW_TL,00,01,VW_P3-VW_P1,VW_TM-VW_P2,VW_P4-VW_P2+1))
       RESTSCREEN(VW_P1,VW_TM,VW_P3,VW_TM,SECTORSCR(VW_SV,VW_P1,VW_TM,VW_P3,VW_TM))
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 8
   VW_SC = IF(VW_P3-VW_P1>VW_P4-VW_P2,(VW_P3-VW_P1)/(VW_P4-VW_P2),(VW_P4-VW_P2)/(VW_P3-VW_P1))
   FOR VW_TM = 1 TO VW_M1
       RESTSCREEN(VW_P1+(VW_M1+1 - VW_TM),VW_P2+ROUND(VW_M2+1-VW_TM*VW_SC,0),VW_P1+(VW_M1 + VW_TM),VW_P2+;
       ROUND(VW_M2+1+VW_TM*VW_SC,0),SECTORSCR(VW_SV,VW_P1+(VW_M1+1 - VW_TM),VW_P2+;
       ROUND(VW_M2+1-VW_TM*VW_SC,0),VW_P1+(VW_M1 + VW_TM),VW_P2+ROUND(VW_M2+1+VW_TM*VW_SC,0)))
       DELAY(25)
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 9
   SAVE SCREEN TO VW_TL
   VW_SC = IF(VW_P3-VW_P1>VW_P4-VW_P2,(VW_P3-VW_P1)/(VW_P4-VW_P2),(VW_P4-VW_P2)/(VW_P3-VW_P1))
   FOR VW_TM = VW_M1 TO 1 STEP -1
       RESTSCREEN(VW_P1+(VW_M1+1 - VW_TM),VW_P2+ROUND(VW_M2+1-VW_TM*VW_SC,0),VW_P1+(VW_M1 + VW_TM),VW_P2+;
       ROUND(VW_M2+1+VW_TM*VW_SC,0),SECTORSCR(VW_TL,VW_P1+(VW_M1+1 - VW_TM),VW_P2+;
       ROUND(VW_M2+1-VW_TM*VW_SC,0),VW_P1+(VW_M1 + VW_TM),VW_P2+ROUND(VW_M2+1+VW_TM*VW_SC,0)))
       RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
       DELAY(25)
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))   
CASE VW_TP = 10
   FOR VW_TM = 1 TO VW_M1+1
      SCROLL(VW_P1,VW_P2,VW_P1+VW_M1,VW_P4,1)
      SCROLL(VW_P1+VW_M1+1,VW_P2,VW_P3,VW_P4,-1)
      DELAY(50)
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))
CASE VW_TP = 11
   FOR VW_TM = 1 TO VW_M1+1
      SCROLL(VW_P1,VW_P2,VW_P1+VW_M1,VW_P4,-1)
      SCROLL(VW_P1+VW_M1+1,VW_P2,VW_P3,VW_P4,1)
      DELAY(50)
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))      
OTHERWISE
   FOR VW_TM = 1 TO VW_M1+1
       SCROLL(VW_P1,VW_P2,VW_P1+VW_M1,VW_P2+VW_M2,-1)
       SCROLL(VW_P1+VW_M1+1,VW_P2+VW_M2+1,VW_P3,VW_P4, 1)
       SCROLL(VW_P1,VW_P2+VW_M2+1,VW_P1+VW_M1,VW_P4, 1)
       SCROLL(VW_P1+VW_M1+1,VW_P2,VW_P3,VW_P2+VW_M2,-1)
       DELAY(50)
   NEXT
   RESTSCREEN(VW_P1,VW_P2,VW_P3,VW_P4,SECTORSCR(VW_SV,VW_P1,VW_P2,VW_P3,VW_P4))      
ENDCASE
RETURN(.T.)

*--------------------- FUNCOES ESPECIAIS PARA IMPRESSAO ----------------------*

*******************************************************************************
* FUNCAO: PRTON                                                               *
*-----------------------------------------------------------------------------*
* UTILIZACAO: SOLICITA A ATIVACAO DA IMPRESSORA                               *
* SINTAXE   : PRTON()  -  RETORNA UM RESULTADO LOGICO CONFORME A IMPRESSORA   *
*******************************************************************************

FUNCTION PRTON
PRIVATE SAV
SAV = SAVESCREEN(23,01,23,78)
MESSAGE("Por Favor...Ligue a Impressora...")
DO WHILE INKEY() <> 27 .AND. .NOT. ISPRINTER()
ENDDO
RESTSCREEN(23,01,23,78,SAV)
RETURN(.NOT. LASTKEY() = 27)

*******************************************************************************
* FUNCAO: ESCPRINT                                                            *
*-----------------------------------------------------------------------------*
* UTILIZACAO: ESCAPA DE UMA IMPRESSAO DE RELATORIO                            *
* SINTAXE   : ESCPRINT () - RETORNA UM RESULTADO LOGICO INDICANDO SE CONTINUA *
*******************************************************************************

FUNCTION ESCPRINT
PRIVATE RET
RET = .T.
IF INKEY() = 27
   SET DEVICE TO SCREEN
   RET = .NOT. CONFIRM("Finalizaçäo da Impressäo")
   SET DEVICE TO PRINT
ENDIF
RETURN(RET)

*******************************************************************************
* FUNCAO: HEAD                                                                *
*-----------------------------------------------------------------------------*
* UTILIZACAO: IMPRIME UM CABECALHO DE RELATORIO                               *
* SINTAXE   : HEAD (Titulo Principal,Titulo do Sistema,                       *
*                   Sub-Titulo 1,Sub-Titulo 2,Tamanho em Colunas)             *
*******************************************************************************

FUNCTION HEAD
PRIVATE T1,T2,T3,T4,LG
PARAMETERS T1,T4,T2,T3,LG
LG = IF(TYPE("LG")="U",80,LG)
T2 = IF(TYPE("T2")="U","",T2)
T3 = IF(TYPE("T3")="U","",T3)
IF ISPRINTER()
   IF TYPE("UTL_PG1") = "U"
      PUBLIC UTL_PG1
      UTL_PG1 = 1
   ENDIF
   @ 00,00 SAY ALIGN(T1,"C",LG)
   IF .NOT. EMPTY(T2)
      @ PROW()+1,00 SAY ALIGN(T2,"C",LG)
   ENDIF
   IF .NOT. EMPTY(T3)
      @ PROW()+2,00 SAY ALIGN(T3,"C",LG)
   ENDIF
   @ PROW()+2,00 SAY ALIGN("Data de Emissäo: "+DTOC(DATE())+ "  -  "+T4;
   +"  -  Página: " + UTL_PG1,"C",LG)
   @ PROW()+1,00 SAY REPLICATE("-",LG)
   UTL_PG1 = UTL_PG1 + 1
ENDIF
RETURN(.T.)

*******************************************************************************
* FUNCAO: FOOT                                                                *
*-----------------------------------------------------------------------------*
* UTILIZACAO: IMPRIME UM RODAPE DE RELATORIO                                  *
* SINTAXE   : HEAD (Mensagem 1,Mensagem 2,Titulo do Rodape,                   *
*                   Tamanho em Colunas)                                       *
*******************************************************************************

FUNCTION FOOT
PRIVATE T1,T2,T3,LG
PARAMETERS T2,T3,T1,LG
T1 = IF(TYPE("T1")="U","",T1)
LG = IF(TYPE("LG")="U",80,LG)
IF ISPRINTER()
   @ PROW()+1,00 SAY REPLICATE("-",LG)
   IF .NOT. EMPTY(T1)
      @ PROW()+1,00 SAY ALIGN(T1,"C",LG)
   ENDIF
   @ PROW()+2,00 SAY ALIGN(T2,"L",LG)
   @ PROW()+1,00 SAY ALIGN(T3,"L",LG)
   @ PROW()+1,00 SAY REPLICATE("=",LG)
ENDIF
RETURN(.T.)

*----------------- FUNCOES ESPECIAIS PARA ENTRADAS DE DADOS ------------------*

*******************************************************************************
* FUNCAO: ACESSCODE                                                           *
*-----------------------------------------------------------------------------*
* UTILIZACAO: PERMITE A ENTRADA DE UMA SENHA DE ACESSO                        *
* SINTAXE   : ACESSCODE (Senha Valida,Picture para a senha a ser digitada)    *
* TABELA    : VALORES DE RETORNO:                                             *
*                                  0 -> SENHA DIGITADA CORRETAMENTE           *
*                                  1 -> SENHA NAO DIGITADA                    *
*                                  2 -> SENHA DIGITADA ERRONEAMENTE           *
*******************************************************************************

FUNCTION ACESSCODE
PRIVATE SVT,SAV,SCR,SNH,PCT,RET
PARAMETERS SNH
SAVE SCREEN TO SCR
LNS  = LEN(SNH)
X1   = 30 - INT(LNS/2)
X2   = 50 + INT(LNS/2)
XT   = 2
XSNH = SPACE(LNS)
SVT  = SETCOLOR(SYS_CR[08])
OPEN_BOX(9,X1,13,X2,"  Senha de Acesso  ")
RET = 0
DO WHILE .T.
   SETCOLOR(SYS_CR[02])
   @ 11,X1+2 SAY "SENHA: ["+SPACE(LNS)+"]"
   SETCOLOR(SYS_CR[03])
   SET CURSOR ON
   XSNH = PASSWORD(11,X1+10,"·"," ",LNS)
   SET CURSOR OFF
   SETCOLOR(SYS_CR[02])
   IF XSNH = SPACE(LNS)
      RET = 1
   ELSEIF XSNH <> SNH .AND. XT <> 0
      XT = XT - 1
      SAV = SAVESCREEN(11,X1+1,11,X2-1)
      @ 11,X1+1 SAY ALIGN("Senha Invalida !","C",X2-X1-1)
      INKEY(2)
      RESTSCREEN(11,X1+1,11,X2-1,SAV)
      LOOP
   ELSEIF XSNH <> SNH
      RET = 2
   ELSEIF XSNH = SNH
      RET = 0
   ENDIF 
   EXIT
ENDDO
CLOS_BOX()
SETCOLOR(SVT)
RETURN(RET)

*******************************************************************************
* FUNCAO: ADDSCR                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: ADICIONA UMA TELA DE EDICAO NA MATRIZ DE TELAS                  *
* SINTAXE   : ADDSCR (variaveis,telas,mascaras,linhas,colunas,validades,cor)  *
*******************************************************************************

FUNCTION ADDSCR
PRIVATE RET,CR0,VAR,TLS,MSC,LIN,COL,VLD,ADI,ADF,MX,L1,C1,L2,C2,LTP,CTP,ATL,AT,;
CTX,VLR
PARAMETERS VAR,TLS,MSC,LIN,COL,VLD,CR0
IF TYPE("UTL_SCR") <> "A"
   PUBLIC UTL_SCR[1],UTL_VRS[1],UTL_ISCR,UTL_IVRS
   UTL_ISCR = 0
   UTL_IVRS = 1
ENDIF
CR0 = IF(TYPE("CR0")="C",CR0,SYS_CR[03])
ADI = UTL_IVRS
ADF = LEN(VAR)
STORE 80 TO L1,C1
STORE  0 TO L2,C2
FOR CT = 1 TO ADF
   ATL = IF(TYPE("LIN[CT]")="N",LIN[CT],ATL+1)
   ATC = IF(TYPE("COL[CT]")="N",COL[CT],ATC)
   AADD(UTL_VRS,VAR[CT]                            ,DIM(CT,1,6))
   AADD(UTL_VRS,TLS[CT]                            ,DIM(CT,2,6))
   AADD(UTL_VRS,MSC[CT]                            ,DIM(CT,3,6))
   AADD(UTL_VRS,IF(TYPE("LIN[CT]")="N",LIN[CT],ATL),DIM(CT,4,6))
   AADD(UTL_VRS,IF(TYPE("COL[CT]")="N",COL[CT],ATC),DIM(CT,5,6))
   AADD(UTL_VRS,VLD[CT]                            ,DIM(CT,6,6))
   CTX = IF(TYPE("COL[CT]")="N",COL[CT],0)
   VLR = VAR[CT]
   L1  = MIN(IF(TYPE("LIN[CT]")="N",LIN[CT],80)    ,L1)
   L2  = MAX(IF(TYPE("LIN[CT]")="N",LIN[CT],0)     ,L2)
   C1  = MIN(IF(TYPE("COL[CT]")="N",COL[CT],80)    ,C1)
   C2  = MAX(CTX+LEN(TLS[CT]+SPC_TYPE(&VLR))       ,C2)
   UTL_IVRS = UTL_IVRS + 1
NEXT
UTL_ISCR = UTL_ISCR + 1
AADD(UTL_SCR,L1-2,DIM(UTL_ISCR,1,7))
AADD(UTL_SCR,C1-2,DIM(UTL_ISCR,2,7))
AADD(UTL_SCR,L2+2,DIM(UTL_ISCR,3,7))
AADD(UTL_SCR,C2+2,DIM(UTL_ISCR,4,7))
AADD(UTL_SCR,CR0 ,DIM(UTL_ISCR,5,7))
AADD(UTL_SCR,ADI ,DIM(UTL_ISCR,6,7))
AADD(UTL_SCR,ADF ,DIM(UTL_ISCR,7,7))
RETURN(UTL_ISCR)

*******************************************************************************
* FUNCAO: DSPSCR                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA A TELA DE EDICAO DAS TELAS DA LISTA                      *
* SINTAXE   : DSPSCR (codigo da tela)                                         *
*******************************************************************************

FUNCTION DSPSCR
PRIVATE CDM,L1,C1,L2,C2,TIT,ADI,ADF,SVX,CR0
PARAMETERS CDM,CR0
IF TYPE("UTL_SCR") <> "A"
   RETURN(.F.)
ENDIF
L1  = UTL_SCR[DIM(CDM,1,7)]
C1  = UTL_SCR[DIM(CDM,2,7)]
L2  = UTL_SCR[DIM(CDM,3,7)]
C2  = UTL_SCR[DIM(CDM,4,7)]
CR0 = IF(TYPE("CR0")="C",CR0,UTL_SCR[DIM(CDM,5,7)])
ADI = UTL_SCR[DIM(CDM,6,7)]
ADF = UTL_SCR[DIM(CDM,7,7)]
SVX = SETCOLOR(UTL_SCR[DIM(CDM,5,7)])
OPEN_BOX(L1,C1,L2,C2,"")
SETCOLOR(CR0)
FOR CT = ADI TO ADF
   @ UTL_VRS[DIM(CT,4,6)],UTL_VRS[DIM(CT,5,6)] SAY UTL_VRS[DIM(CT,2,6)]
NEXT
SETCOLOR(SVX)
RETURN(.T.)

*******************************************************************************
* FUNCAO: DSPPCT                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA A MASCARA DE EDICAO DAS TELAS DA LISTA                   *
* SINTAXE   : DSPPCT (codigo da tela)                                         *
*******************************************************************************

FUNCTION DSPPCT
PRIVATE CDM,ADI,ADF,SVX,CR0,LY,CX,VL
PARAMETERS CDM,CR0
IF TYPE("UTL_SCR") <> "A"
   RETURN(.F.)
ENDIF
CR0 = IF(TYPE("CR0")="C",CR0,UTL_SCR[DIM(CDM,5,7)])
ADI = UTL_SCR[DIM(CDM,6,7)]
ADF = UTL_SCR[DIM(CDM,7,7)]
SVX = SETCOLOR(CR0)
FOR CT = ADI TO ADF
   LY = UTL_VRS[DIM(CT,4,6)]
   CX = UTL_VRS[DIM(CT,5,6)] + LEN(UTL_VRS[DIM(CT,2,6)])
   VL = UTL_VRS[DIM(CT,1,6)]
   @ LY,CX SAY TRANSFORM(SPC_TYPE(&VL),UTL_VRS[DIM(CT,3,6)])
NEXT
SETCOLOR(SVX)
RETURN(.T.)


*******************************************************************************
* FUNCAO: EDTSCR                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: EDITA A MASCARA DE EDICAO DAS TELAS DA LISTA                    *
* SINTAXE   : EDTSCR (codigo da tela)                                         *
*******************************************************************************

FUNCTION EDTSCR
PRIVATE CDM,ADI,ADF,SVX,CR0,LY,CX,VL,VC
PARAMETERS CDM,CR0
IF TYPE("UTL_SCR") <> "A"
   RETURN(.F.)
ENDIF
CR0 = IF(TYPE("CR0")="C",CR0,UTL_SCR[DIM(CDM,5,7)])
ADI = UTL_SCR[DIM(CDM,6,7)]
ADF = UTL_SCR[DIM(CDM,7,7)]
SVX = SETCOLOR(CR0)
FOR CT = ADI TO ADF
   LY = UTL_VRS[DIM(CT,4,6)]
   CX = UTL_VRS[DIM(CT,5,6)] + LEN(UTL_VRS[DIM(CT,2,6)])
   VL = UTL_VRS[DIM(CT,1,6)]
   VC = UTL_VRS[DIM(CT,6,6)]
   @ LY,CX GET &VL PICTURE UTL_VRS[DIM(CT,3,6)] VALID &VC
NEXT
SETCOLOR(SVX)
RETURN(.T.)

*******************************************************************************
* FUNCAO: ZAPSCR                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: RETIRA A TELA DE EDICAO DA TELA RESTAURANDO-A                   *
* SINTAXE   : ZAPSCR (codigo da tela)                                         *
*******************************************************************************

FUNCTION ZAPSCR
PRIVATE L1,C1,L2,C2
IF TYPE("UTL_SCR") <> "A"
   RETURN(.F.)
ENDIF
L1  = UTL_SCR[DIM(UTL_ISCR,1,7)]
C1  = UTL_SCR[DIM(UTL_ISCR,2,7)]
L2  = UTL_SCR[DIM(UTL_ISCR,3,7)]
C2  = UTL_SCR[DIM(UTL_ISCR,4,7)]
UTL_ISCR = UTL_ISCR - 1
CLOS_BOX()
RETURN(.T.)

*******************************************************************************
* FUNCAO: READSCR                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: LE OS DADOS COLOCADOS PELA FUNCAO EDTSCR()                      *
* SINTAXE   : READSCR (codigo da tela)                                        *
*******************************************************************************

FUNCTION READSCR
PRIVATE CDM,CR0
PARAMETERS CDM
IF TYPE("UTL_SCR") <> "A"
   RETURN(.F.)
ENDIF
CR0 = UTL_SCR[DIM(CDM,5,7)]
SVX = SETCOLOR(CR0)
READ
SETCOLOR(SVX)
RETURN(.T.)

*******************************************************************************
* FUNCAO: CLEARSCR                                                            *
*-----------------------------------------------------------------------------*
* UTILIZACAO: CANCELA OS DADOS COLOCADOS PELA FUNCAO EDTSCR()                 *
* SINTAXE   : CLEARSCR ()                                                     *
*******************************************************************************

FUNCTION CLEARSCR
IF TYPE("UTL_SCR") <> "A"
   RETURN(.F.)
ENDIF
CLEAR GETS
RETURN(.T.)

*--------------- FUNCOES ESPECIAIS PARA CRIACAO DE MENUS ---------------------*

*******************************************************************************
* FUNCAO: OPTMENU                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: CRIA UM MENU POP-UP EM JANELA                                   *
* SINTAXE   : OPTMENU (Matriz de Opcoes,Titulo da janela,Linha superior,      *
*                      Coluna superior,Linha inferior,Coluna inferior)        *
*******************************************************************************

FUNCTION OPTMENU
PRIVATE L1,C1,L2,C2,MT1,OPC,TIT,SVT
PARAMETERS MT1,TIT,L1,C1,L2,C2
TIT = IF(TYPE("TIT")="U","",TIT)
L1  = IF(TYPE("L1")<>"N",07,L1+1)
C1  = IF(TYPE("C1")<>"N",06,C1+1)
L2  = IF(TYPE("L2")<>"N",IF(LEN(MT1)>12,L1+12,L1+LEN(MT1)-1),L2-1)
C2  = IF(TYPE("C2")<>"N",C1+LEN(MT1[1])+1,C2-1)
SVT = SETCOLOR(SYS_CR[03])
OPEN_BOX(L1-1,C1-1,L2+1,C2+1,TIT)
IF TYPE("OPTFUNC()")="UI"
   OPC = ACHOICE(L1,C1,L2,C2,MT1,.T.,"OPTFUNC")
ELSE
   OPC = ACHOICE(L1,C1,L2,C2,MT1,.T.,"")
ENDIF
CLOS_BOX()
SETCOLOR(SVT)
RETURN(OPC)

*******************************************************************************
* FUNCAO: ADDMENU                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: ADICIONA UM MENU NA MATRIZ DE MENUS                             *
* SINTAXE   : ADDMENU (opcoes,hotkeys,linha1,coluna1,linha2,coluna2,          *
*                      titulo da janela,frame,cor,UDF,mensagens)              *
*******************************************************************************

FUNCTION ADDMENU
PRIVATE RET,L1,C1,L2,C2,FRM,TIT,CR0,OPC,PRC,ADI,ADF,MX,MSG,TCL
STORE "" TO TIT,PRC
PARAMETERS OPC,TCL,L1,C1,L2,C2,TIT,FRM,CR0,PRC,MSG
FRM = IF(TYPE("FRM")="U",FRAMES(1,32),FRM)
TIT = IF(TYPE("TIT")="U","",TIT)
PRC = IF(TYPE("PRC")="U","",PRC)
IF TYPE("UTL_MNU") <> "A"
   PUBLIC UTL_MNU[1],UTL_OPC[1],UTL_IMNU,UTL_IOPC
   UTL_IMNU = 0
   UTL_IOPC = 1
ENDIF
IF TYPE("TCL")<>"A"
   DECLARE TCL[LEN(OPC)]
   AFILL(TCL,0)
ENDIF
FRM = IF(TYPE("FRM")="C",IF(FRM=="",SYS_FM[01]+SYS_PRC[01],FRM),SYS_FM[01]+SYS_PRC[01])
CR0 = IF(TYPE("CR0")="C",IF(CR0=="",SYS_CR[03],CR0),SYS_CR[03])
ADI = UTL_IOPC
ADF = LEN(OPC)
MX  = 0
FOR CT = 1 TO ADF
   AADD(UTL_OPC,OPC[CT]                       ,DIM(ADI+CT-1,1,3))
   AADD(UTL_OPC,IF(TYPE("MSG")="A",MSG[CT],""),DIM(ADI+CT-1,2,3))
   AADD(UTL_OPC,IF(TYPE("TCL")="A",TCL[CT],0) ,DIM(ADI+CT-1,3,3))
   MX = MAX(LEN(OPC[CT]),MX)
   UTL_IOPC = UTL_IOPC + 1
NEXT
UTL_IMNU = UTL_IMNU + 1
AADD(UTL_MNU,L1                                 ,DIM(UTL_IMNU,1,9))
AADD(UTL_MNU,C1                                 ,DIM(UTL_IMNU,2,9))
AADD(UTL_MNU,IF(TYPE("L2")<>"N",L1+ADF+1,L1)    ,DIM(UTL_IMNU,3,9))
AADD(UTL_MNU,IF(TYPE("C2")<>"N",C1+MX+1,C2)     ,DIM(UTL_IMNU,4,9))
AADD(UTL_MNU,CR0                                ,DIM(UTL_IMNU,5,9))
AADD(UTL_MNU,TIT                                ,DIM(UTL_IMNU,6,9))
AADD(UTL_MNU,PRC                                ,DIM(UTL_IMNU,7,9))
AADD(UTL_MNU,ADI                                ,DIM(UTL_IMNU,8,9))
AADD(UTL_MNU,ADF                                ,DIM(UTL_IMNU,9,9))
RETURN(UTL_IMNU)

*******************************************************************************
* FUNCAO: BOXMENU                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: MOSTRA A JANELA DE OPCOES DO MENU DA LISTA                      *
* SINTAXE   : BOXMENU (codigo do menu)                                        *
*******************************************************************************

FUNCTION BOXMENU
PRIVATE CDM,L1,C1,L2,C2,TIT,ADI,ADF,SVX,CR0
PARAMETERS CDM,CR0
IF TYPE("UTL_MNU") <> "A"
   RETURN(.F.)
ENDIF
L1  = UTL_MNU[DIM(CDM,1,9)]
C1  = UTL_MNU[DIM(CDM,2,9)]
L2  = UTL_MNU[DIM(CDM,3,9)]
C2  = UTL_MNU[DIM(CDM,4,9)]
TIT = UTL_MNU[DIM(CDM,6,9)]
ADI = UTL_MNU[DIM(CDM,8,9)]
ADF = UTL_MNU[DIM(CDM,9,9)]
CR0 = IF(TYPE("CR0")="C",CR0,UTL_MNU[DIM(CDM,5,9)])
SVX = SETCOLOR(UTL_MNU[DIM(CDM,5,9)])
OPEN_BOX(L1,C1,L2,C2,TIT)
SETCOLOR(CR0)
FOR CT = ADI TO ADI+ADF-1
   @ L1+(CT-ADI+1),C1+1 SAY ALIGN(UTL_OPC[DIM(CT,1,3)],"L",C2-C1-1)
NEXT
SETCOLOR(SVX)
RETURN(.T.)

*******************************************************************************
* FUNCAO: READMENU                                                            *
*-----------------------------------------------------------------------------*
* UTILIZACAO: LE O MENU DA LISTA                                              *
* SINTAXE   : READMENU (codigo do menu) - RETORNA A OPCAO ESCOLHIDA           *
*******************************************************************************

FUNCTION READMENU
PRIVATE RET,CDM,Y1,X1,Y2,X2,TIT,PRC,ADI,ADF,SVX,HOP,HMS,HHO,CR0
PARAMETERS CDM,CR0
CR0 = IF(TYPE("CR0")="C",CR0,UTL_MNU[DIM(CDM,5,9)])
IF TYPE("UTL_MNU") <> "A"
   RETURN(.F.)
ENDIF
Y1  = UTL_MNU[DIM(CDM,1,9)]
X1  = UTL_MNU[DIM(CDM,2,9)]
X2  = UTL_MNU[DIM(CDM,4,9)]
PRC = UTL_MNU[DIM(CDM,7,9)]
ADI = UTL_MNU[DIM(CDM,8,9)]
ADF = UTL_MNU[DIM(CDM,9,9)]
SVX = SETCOLOR(CR0)
FOR CT = ADI TO ADI+ADF-1
   HOP = UTL_OPC[DIM(CT,1,3)]
   HMS = UTL_OPC[DIM(CT,2,3)]
   HHO = UTL_OPC[DIM(CT,3,3)]
   PROMPT(Y1+(CT-ADI+1),X1+1,ALIGN(HOP,"L",X2-X1-1),HHO,HMS)
NEXT
IF TYPE("PRC")<>"C" .OR. EMPTY(PRC)
   RET = MENUTO(Y1,X1)
ELSE
   RET = MENUTO(Y1,X1,PRC)
ENDIF
SETCOLOR(SVX)
RETURN(RET)

*******************************************************************************
* FUNCAO: ZAPMENU                                                             *
*-----------------------------------------------------------------------------*
* UTILIZACAO: RETIRA O MENU DA TELA RESTAURANDO-A                             *
* SINTAXE   : ZAPMENU (codigo do menu)                                        *
*******************************************************************************

FUNCTION ZAPMENU
PRIVATE L1,C1,L2,C2
IF TYPE("UTL_MNU") <> "A"
   RETURN(.F.)
ENDIF
L1  = UTL_MNU[DIM(UTL_IMNU,1,9)]
C1  = UTL_MNU[DIM(UTL_IMNU,2,9)]
L2  = UTL_MNU[DIM(UTL_IMNU,3,9)]
C2  = UTL_MNU[DIM(UTL_IMNU,4,9)]
UTL_IMNU = UTL_IMNU - 1
CLOS_BOX()
RETURN(.T.)

*---------------- FUNCOES ESPECIAIS PARA VALIDACAO DE DADOS ------------------*

*******************************************************************************
* FUNCAO: CHKGET                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: VERIFICA A INTEGRIDADE DE UM GET ATIVO                          *
* SINTAXE   : CHKGET (codigo a validar,condicao,mensagem de erro,area do      *
*                     bco de dados a procurar,UDF de apoio <caso a condicao   *
*                     for verdadeira.)                                        *
*******************************************************************************

FUNCTION CHKGET
PRIVATE UTL_SEK,XRC,XAR,NCD,ARE,NMS,PRC,RET
PARAMETERS UTL_SEK,NCD,NMS,ARE,PRC
NCD = IF(TYPE("NCD")="U","FOUND()",NCD)
NMS = IF(TYPE("NMS")="U","",NMS)
PRC = IF(TYPE("PRC")="U","",PRC)
IF .NOT. EMPTY(ARE)
   XAR = ALIAS()
   SELECT &ARE
   XRC = RECNO()
   SEEK UTL_SEK
ENDIF
IF &NCD
   IF .NOT. EMPTY(PRC)
      DO &PRC
   ENDIF
   RET = .T.
ELSE
   IF .NOT. EMPTY(NMS)
      WARNING(NMS)
   ENDIF
   RET = .F.
ENDIF
IF .NOT. EMPTY(ARE)
   GOTO XRC
   SELECT &XAR
ENDIF
RETURN(RET)

*--------------- FUNCOES ESPECIAIS PARA PROTECAO DE SISTEMAS -----------------*

*******************************************************************************
* FUNCAO: VERIFY                                                              *
*-----------------------------------------------------------------------------*
* UTILIZACAO: VERIFICA A INTEGRIDADE DO SISTEMA                               *
* SINTAXE   : VERIFY (codigo,tipo de verificacao) 1> SERIAL         2> LABEL  *
*******************************************************************************

FUNCTION VERIFY
PRIVATE FL,HND,XLBL,XTP,XTMP
PARAMETERS XLBL,XTP
XTP = IF(TYPE("XTP")="U",1,XTP)
RUN VOL >TMP.$$$
FL  = MEMOREAD("TMP.$$$")
HND = FCREATE("TMP.$$$",0)
FCLOSE(HND)
ERASE TMP.$$$
XTMP = IF(XTP=1,SUBSTR(MEMOLINE(FL,80,3),32,9),SUBSTR(MEMOLINE(FL,80,2),26,11))
IF .NOT. XTMP = XLBL
   RETURN(.F.)
ENDIF
RETURN(.T.)   

*------------------------- FUNCOES ESPECIAIS SORTIDAS ------------------------*

*******************************************************************************
* FUNCAO: NAMES                                                               *
*-----------------------------------------------------------------------------*
* UTILIZACAO: RETORNA NOMES COMUNS ENTRE DIVERSIFICADOS SISTEMAS              *
* SINTAXE   : NAMES (codigo do nome)                                          *
*******************************************************************************

FUNCTION NAMES
PRIVATE XCD,XNM
PARAMETERS XCD
DO CASE
CASE XCD = 0
   XNM = "Sistema"
CASE XCD = 1
   XNM = "Inclusäo"
CASE XCD = 2
   XNM = "Alteraçäo"
CASE XCD = 3
   XNM = "Consulta"
CASE XCD = 4
   XNM = "Exclusäo"
CASE XCD = 5
   XNM = "Manutençäo"
CASE XCD = 6
   XNM = "Controle"
CASE XCD = 7
   XNM = "Cadastro"
CASE XCD = 8
   XNM = "Menu de Opçöes"
CASE XCD = 9
   XNM = "Relatório"
CASE XCD = 10
   XNM = "Finalizar Operaçöes"
ENDCASE
RETURN(XNM)

 *************************** »»» ROTINAS PENDENTES ««« ************************

FUNCTION DISPWIND
SAVE SCREEN TO TL_X
SELECT CURSO
GO TOP
CTT = RECCOUNT()
DECLARE CD_CUR1[CTT],CD_CUR2[CTT],CD_CUR3[CTT],CD_CUR4[CTT],CD_CUR5[CTT],;
CD_CUR6[CTT],CD_CUR7[CTT]
STORE 0 TO CT
MESSAGE()
MESSAGE("AGUARDE...PREENCHENDO QUADRO DE CURSOS...")
DO WHILE CT < CTT
   CT = CT + 1
   CD_CUR1[CT] = COD
   CD_CUR2[CT] = NOME
   CD_CUR3[CT] = DIAS1 + "-" + DIAS2
   CD_CUR4[CT] = HORAINC
   CD_CUR5[CT] = HORAFIN
   CD_CUR6[CT] = PROF
   SELECT PROFS
   SEEK CD_CUR6[CT]
   CD_CUR7[CT] = NOME
   SELECT CURSO
   SKIP +1
ENDDO
GO TOP
SELECT ALUNOS
MESSAGE()
IF CT = 0
   IMPLODE(15,13,21,73,SYS_FM[01]+SYS_PRC[01],TL_X)
   RETURN(0)
ENDIF
OPEN_BOX(15,13,21,73," JANELA PARA ESCOLHA DE CURSOS ")
@ 17,18 SAY "PROFESSOR:"
@ 18,18 SAY "CURSO:"
@ 19,18 SAY "HORARIO:"
@ 19,55 SAY "as"
SVT = SETCOLOR("W+/BG")
@ 17,29 SAY "  "
@ 17,36 SAY SPACE(35)
@ 18,29 SAY "  "
@ 18,36 SAY SPACE(35)
@ 19,36 SAY "   -   "
@ 19,49 SAY "  :  "
@ 19,58 SAY "  :  "
SETCOLOR(SVT)
PT = 1
SET CURSOR OFF
DO WHILE .T.
   XCCUR  = CD_CUR1[PT]
   XNCUR  = CD_CUR2[PT]
   XDIAS  = CD_CUR3[PT]
   XHINC  = CD_CUR4[PT]
   XHFIN  = CD_CUR5[PT]
   XPROF  = CD_CUR6[PT]
   XPROFN = CD_CUR7[PT]
   * MOSTRA NA TELA OS DADOS
   KEY = INKEY(0)
   IF (KEY = 5 .OR. KEY = 19) .AND. PT > 1
    PT = PT - 1
   ELSEIF (KEY = 24 .OR. KEY = 4) .AND. PT < CT
    PT = PT + 1
   ELSEIF KEY = 27 .OR. KEY = 13
    EXIT
   ENDIF
ENDDO
IMPLODE(15,13,21,73,SYS_FM[01]+SYS_PRC[01],TL_X)
SET CURSOR ON
RETURN(CD_CUR1[PT])

*******************************************************************************
*                        FUNCOES PENDENTES - UTIL.LIB                         *
*-----------------------------------------------------------------------------*
* CRIAR FUNCAO PARA CRIAR JANELAS DE CONSULTAS EXTENSIVAS NA TELA COM OU SEM  *
* EDICAO                                                                      *
* COMO:             (WINDSEEK)                                                *
*                                                                             *
* MOVIMENTACAO TOTAL DENTRO DO ARQUIVO,DEFINICAO DE FILTROS (INTERATIVO E     *
* DIRETO) COM PROCURA RAPIDA E PESQUISA ANALITICA, CONTAGEM DE REGISTROS E    *
* SOMATORIA DE CAMPOS.                                                        *
*-----------------------------------------------------------------------------*
* CRIAR FUNCOES ESPECIAIS PARA MANIPULAR DATAS DE 5 DIGITOS dd/mm E mm/aa.    *
* COMO:                                                                       *
*                                                                             *
* IDATE() => INVERTER A DATA NO FORMATO dd/mm OU mm/aa PARA UMA INDEXACAO.    *
* VDATE() => VALIDAR UMA DATA DESSE TIPO EM UM GET. (CONFERIR COM A MASTER).  *
* DATEREF() => LER A DATA NOS FORMATOS INDICADOS.                             *
*******************************************************************************




*******************************************************************************
