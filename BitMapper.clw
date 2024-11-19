!Bit Mapper a tool by Carl T. Barnes - Copyright (c) 2024 - Released under the MIT License
!---------------------------------------------------------------------------------------------------
!Region ---- About and History ---------------------------------------------------------------------
! Working with Windows API many functions have large Bit Maps that can take time to decode.
! An example would be GetWindowLong()   https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getwindowlonga
!       for Window Styles GWL_Style     https://learn.microsoft.com/en-us/windows/win32/winmsg/window-styles
!       for Extended      GWL_ExStyle   https://learn.microsoft.com/en-us/windows/win32/winmsg/extended-window-styles
! Inputs here will accept C/C++/C# Constants as defined in API .H Files and PInvoke: e.g. 0x00C00000L  
! Also VB6 VB.Net &H##### Hex
!---------------------------------------------------------------------------------------------------
! Change History:
! Circa  2000  Written a long time ago in C5
! Circa  2016  Convert to C10
! 18-Nov-2024  Release on GitHub 
! 
! Future changes:
!   Have NAND ... could add NOR NXOR as seen in Windows Programmer Calculator. Maybe change Button('~AND') to 'NB...' and Popup('NAND|NOR|NXOR')
!
!EndRegion -------------------------------------------------------------------------------
!#########################################################################
    PROGRAM 
!Region --- To build with PasteDscLogin:CSSTWQDOQ UnComment **WndPrv Lines. ---
!_CbWndPreview_  EQUATE(1)
!    INCLUDE('CbWndPreview.inc'),ONCE       !Download https://github.com/CarlTBarnes/WindowPreview
!    COMPILE('!**WndPrv',_CbWndPreview_)
!WndPrvCls CBWndPreviewClass,THREAD         !Init puts Secret Flat Button at Window Top
!EndRegion    !**WndPrv

    INCLUDE('keycodes.clw')
    MAP
BitMapper       PROCEDURE(STRING StartReRunGrp)
BindEquates     PROCEDURE()                                                 !BIND() Equates to use in Inputs
ChangeAndSelect PROCEDURE(LONG ControlFEQ, ? NewValue, BOOL TouchIt=True)   !Change a Control Value, Select it, Touch It
ComboQueueAdd   PROCEDURE(*InpStringQ ComboQ, STRING inTheStr, LONG inTheLong, BOOL AddLast=False)
BinNumber       PROCEDURE(ULONG ULng, BOOL TrimLeadZeros=False),STRING      !Return Long in Binary of 32 
HexNumber       PROCEDURE(LONG inLongValue),STRING                          !Return Long as 8 Hex Digits
PopupUnder      PROCEDURE(LONG CtrlFEQ, STRING PopMenu),LONG

!      IMO FYI using LtoA() is a bad idea as it tends to GPF with Lib Builds
!      MODULE('RTL') ; LtoA(LONG Val2Cvt, *CSTRING OutStr, LONG Base),LONG,PROC,RAW,NAME('_ltoa'),dll(1) ; END 
    END

InpStringQ  QUEUE,PRE(InpStrQ)      !Global so shared with ReRun instances
TheStr        STRING(255)           !InpStrQ:TheStr
TheLong       ULONG                 !InpStrQ:TheLong
TheHex        STRING(8)             !InpStrQ:TheHex
UprStr        STRING(255)           !InpStrQ:UprStr     !To Sort
            END
BopStringQ  QUEUE(InpStringQ),PRE(BopStrQ)  !For Operand Combo
            END
            
    CODE
    SYSTEM{7A58h}=True  !7A58h = PROP:PropVScroll Proportional Thumb added in C10 
    SYSTEM{7A7Dh}=2     !7A7Dh = PROP:MsgModeDefault   2=MSGMODE:CANCOPY 
    SYSTEM{PROP:FontName}='Segoe UI' ; SYSTEM{PROP:FontSize}=11      

    BitMapper('')
    RETURN
!-----------------------------------------------------------------------------
BitMapper   PROCEDURE(STRING StartReRunGrp)
ReRunGrp   GROUP,PRE(ReRn),AUTO
WinX            LONG    !ReRn:WinX
WinY            LONG    !ReRn:WinY
WinW            LONG    !ReRn:WinW
WinH            LONG    !ReRn:WinH
            END
InpStr      STRING(255)      !Input as STRING so can do various forms
InpLong     LONG             !Got to Convert to Long

BopStr      STRING(255)      !Input as STRING so can do various Bitwise Ops
BopLong     LONG             !Got to Convert to Long

BMQ     QUEUE,PRE(BMQ)
BitNo      STRING(4)         !BMQ:BitNo
HexBm      STRING(8+2+2)     !BMQ:HexBm     put space between 4s
DecBm      STRING(11+2)      !BMQ:DecBm     -1123123123
BinBm      STRING(32+3+1)    !BMQ:BinBm     put space between
!ByteMe     STRING(3+4)      !BMQ:ByteMe    maybe show the BYTE value for some ?
        END
Comment2Clp BYTE(1)          !Prefix Lines on Clip with '! ' so commets to paste into code
Ndx         LONG

Window WINDOW('Bit Mapper aka Long Splitter, Hex Mapper, Bit Master, Masterbitter'),AT(,,346,400),GRAY, |
            SYSTEM,ICON('BitmaprY.ICO'),FONT('Segoe UI',9,,FONT:regular),RESIZE
        PROMPT('&Value to Bit Map - Integer, HEX h, BINARY b, OCTAL o, C 0x, VB &&H, Java #, or Eval' & |
                'uate String:'),AT(3,1),USE(?InpStr:Prompt),TRN
        COMBO(@s255),AT(3,11,247,12),USE(InpStr),VSCROLL,DROP(30),FROM(InpStringQ),DROPID('DragBMQ'), |
                ALRT(EnterKey),FORMAT('152L(2)|M~Value~S(400)@s255@43R(2)|M~Long Value~C(0)@n13@40L(' & |
                '2)~Hex~@s8@')
        BUTTON('&Refresh'),AT(258,11,31,12),USE(?RefreshBtn),SKIP,TIP('Regenerate the list with curr' & |
                'ent value<13,10>Clears any Bitwise Operations')
        BUTTON('ASCII'),AT(302,11,27,12),USE(?ASCIIBtn),SKIP,TIP('Generate ASCII Table')
        GROUP('Bitwise calculation using above value'),AT(2,26,247,42),USE(?Group1),BOXED
            PROMPT('Op&erand:'),AT(8,38),USE(?Prompt:BopStr)
            COMBO(@s255),AT(40,36,204,12),USE(BopStr),VSCROLL,DROP(30),FROM(BopStringQ),DROPID('DragBMQ'), |
                    ALRT(EnterKey),FORMAT('110L(2)|M~Operand~S(400)@s255@43R(2)|M~Long Value~C(0)@n1' & |
                    '3@40L(2)~Hex~@s8@')
            PROMPT('Operator:'),AT(8,53),USE(?Prompt:Operate)
            BUTTON('&AND'),AT(39,51,22,12),USE(?OP_AND),TIP('AND(Value,Operand)')
            BUTTON('&OR'),AT(65,51,20,12),USE(?OP_OR),TIP('OR(Value,Operand)')
            BUTTON('&XOR'),AT(89,51,22,12),USE(?OP_XOR),TIP('XOR(Value,Operand)')
            BUTTON('~A&ND'),AT(115,51,25,12),USE(?OP_NAND),TIP('NOT AND<13,10>i.e. AND the Mask Comp' & |
                    'lement <13,10>=BAND(Op1, BXOR(Op2,-1))<13,10>Think of it as Strip Bits ')
            BUTTON('+'),AT(172,51,14,12),USE(?OP_Plus),KEY(AltPlus),TIP('Plus Operand <13,10>= Value' & |
                    ' + Operand <13,10>Not a Bitwise Operation')
            BUTTON('-'),AT(189,51,14,12),USE(?OP_Minus),KEY(AltMinus),TIP('Minus Operand <13,10>= Va' & |
                    'lue - Operand <13,10>Not a Bitwise Operation')
            BUTTON('&Shift'),AT(144,51,22,12),USE(?OP_Shift),TIP('BSHIFT( Value, Operand) <13,10>' & |
                    '<13,10>Enter number of Bits to BSHIFT <13,10>    Positive = Left Shift  <13,10>' & |
                    '    Negative = Right Shift  ')
            BUTTON('~'),AT(209,51,14,12),USE(?OP_Tilde),KEY(448),TIP('Complement Value = XOR(Value,F' & |
                    'FFFFFFF)<13><10>Operand is not used <13,10>Hot key is Shift Tilde ')
            BUTTON('ff'),AT(228,51,16,12),USE(?ffffffffBtn),TIP('Set Operand to FFFFFFFF <13,10>Popu' & |
                    'p allows many others')
        END
        BUTTON('Font'),AT(255,54,24,14),USE(?FontChgListBtn),SKIP,TIP('Change List Font')
        BUTTON('&Copy'),AT(284,54,41,14),USE(?Cpy2ClpBtn),SKIP,ICON(ICON:Copy),TIP('Copy Bitmap List' & |
                ' Below to Clipboard<13,10>To Copy One line Double Click on the line<13,10>Right-Cli' & |
                'ck on List for other Copy options'),LEFT
        CHECK('!'),AT(330,56),USE(Comment2Clp),SKIP,TIP('Prefix Clipboard Lines with "!"  <13,10>i.e' & |
                '. Clarion !Comments')
        LIST,AT(2,72),FULL,USE(?List:BMQ),VSCROLL,FONT('Courier New',11,,FONT:bold),FROM(BMQ), |
                DRAGID('DragBMQ'),FORMAT('20L(2)|FM~No~@s3@54L(2)|FM~Hex~C(0)@s9@64L(2)|FM~Decimal~C' & |
                '(0)@s11@128L(2)F~Binary~@s35@'),ALRT(DeleteKey)
        BUTTON('ReRun'),AT(302,30,31,12),USE(?ReRunBtn),SKIP,FONT(,8),TIP('Run another instance thread')
        BUTTON('Halt'),AT(267,30,22,12),USE(?HaltBtn),SKIP,FONT(,8),HIDE,TIP('Halt all threads')
    END

    !FONT('Courier New',10,,FONT:bold) ,FONT('Consolas',10,,FONT:bold)  FONT('Consolas',11,,FONT:regular)  !,FONT('Fixedsys',9,,)  FONT('Courier New',10,,FONT:bold)
    !I like the 'Courier New' in the LIST because the Zero is not slashed

Local   CLASS
AsciiTable      PROCEDURE()
BitOperation    PROCEDURE(LONG Op1, LONG Op2, LONG OperatorBtnFEQ),LONG,PROC   !Perform Bitwise OP for Button Clicked. Returns Outcome but not used
Copy2Clip       PROCEDURE(BOOL Just1Line=False)             !Copy all Lines or 1 Line to Clip   
ffBtnPopup      PROCEDURE()                                 ! FF Button Popup Code
FmtColHead      PROCEDURE(String HeadText)
FmtDashs        PROCEDURE(<STRING inBitNo>)
FmtHex          PROCEDURE(LONG Val1),STRING                 !Format LONG as 8 byte Hex for BMQ List
FmtLine         PROCEDURE(LONG Val1, STRING inBitNo)        !Format 1 Queue Line
FmtNum          PROCEDURE(LONG Val1, LONG nBase),STRING     !Form in Hex, Dec or Binary
FontChange      PROCEDURE()                                 !Change LIST Font
FontDbg         PROCEDURE(LONG FEQ),STRING
MapBits         PROCEDURE(LONG Lval)                        !Bit Map the
MapHex          PROCEDURE(LONG Lval)                        !Nib/Hex Map the
PrepareWindow   PROCEDURE()
RightClickList  PROCEDURE()                                 !Popup() menu for Right-Click to Copy various ways
TakeHexInput    PROCEDURE(*STRING EntryValue,*LONG OutVal),BOOL      !Clean the HexInput and Output a Long
        END

    CODE
    OPEN(Window)
    COMPILE('!***',_CbWndPreview_)
        WndPrvCls.Init(2)           !Add Carl's Window Preview Class to allow runtime window design
             !***    
    0{PROP:MinWidth}  = 0{PROP:Width}  * .50 ; 0{PROP:MaxWidth}  = 0{PROP:Width}  * 1.5     !Limit so cannot drag too small or big
    0{PROP:MinHeight} = 0{PROP:Height} * .33 ; 0{PROP:MaxHeight} = 0{PROP:Height} * 2.0     !there is no resizing beyond FULL on List
    Local.PrepareWindow()
    IF StartReRunGrp <> ''  THEN
       ReRunGrp  = StartReRunGrp
       SETPOSITION(0,ReRn:WinX+20,ReRn:WinY+20,ReRn:WinW,ReRn:WinH)
       UNHIDE(?HaltBtn)
    END    
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow
           IF StartReRunGrp='' THEN             !First Run
              InpStr = '1208184320'             !A value to test with 
              POST(EVENT:Accepted,?InpStr)  
           END
        OF EVENT:Drag
           IF DRAGID() THEN 
              GET(BMQ, CHOICE(?List:BmQ))
              SETDROPID(CLIP(LEFT(BMQ:DecBm)))
           END
        OF EVENT:Drop
           !dbg  0{PROP:Text}='Drop Field='& Field() &'  DRAGID()='& DRAGID()  &'  DROPID()='& DROPID() 
           ChangeAndSelect(FIELD(), DROPID())           
        END 
        IF EVENT()=EVENT:AlertKey AND KEYCODE()=EnterKey
           UPDATE
           POST(EVENT:ACCEPTED,?InpStr)
        END

        CASE ACCEPTED()
        OF ?InpStr OROF ?RefreshBtn   
           IF ~Local.TakeHexInput(InpStr,InpLong) THEN CYCLE.
           ComboQueueAdd(InpStringQ, InpStr, InpLong)
           Local.MapBits(InpLong)
           Local.MapHex(InpLong)
           DISPLAY
           SELECT(?InpStr)

        OF ?BopStr
           IF ~Local.TakeHexInput(BopStr,BopLong) THEN CYCLE.
           ComboQueueAdd(BopStringQ, BopStr, BopLong)

        OF ?OP_AND TO ?OP_Tilde
            Local.BitOperation(InpLong,BopLong,Accepted())
            ?List:BMQ{Prop:Selected} = RECORDS(BMQ)

        OF ?List:BMQ
            GET(BMQ,CHOICE(?List:BMQ))
            CASE KEYCODE()
            OF MouseLeft2 ; Local.Copy2Clip(1)
            OF MouseRight ; Local.RightClickList()
            OF DeleteKey  ; DELETE(BMQ)
            END 

        OF ?ffffffffBtn     ; Local.ffBtnPopup()
        OF ?Cpy2ClpBtn      ; Local.Copy2Clip()
        OF ?ASCIIBtn        ; Local.AsciiTable()
        OF ?FontChgListBtn  ; Local.FontChange()

        OF ?ReRunBtn    ; GETPOSITION(0,ReRn:WinX,ReRn:WinY,ReRn:WinW,ReRn:WinH)
                          START(BitMapper, , ReRunGrp)
                          UNHIDE(?HaltBtn)
        OF ?HaltBtn     ; IF Message('Terminate Bit Mapper Tool? {20}',0{PROP:Text},ICON:Hand,'Keep Open|Halt Tool')=1 THEN CYCLE.
                          LOOP Ndx=1 TO 64 ; POST(EVENT:CloseWindow,,Ndx,1) ; END
        END
        CASE FIELD()
        OF ?List:BMQ
            GET(BMQ,CHOICE(?List:BMQ))
            IF EVENT()=EVENT:AlertKey THEN 
               CASE KEYCODE()
               OF DeleteKey  ; DELETE(BMQ)      !Let Delete Key remove lines I don't want to see
               END 
            END 
        END 
    END
    CLOSE(Window)
    RETURN
!----------------
Local.PrepareWindow PROCEDURE()
ListFEQ LONG
    CODE
    BindEquates()   !Bind some Equates to allow using them in an ENTRY 'cause it uses EVALUATE()

    SETFONT(?InpStr,'Consolas',11)          !Set at runtime so Drop List in Previewer uses smaller windows font 
    ListFEQ=?InpStr{PROP:ListFEQ}
    SETFONT(ListFEQ,'Segoe UI',9)   
    ! ListFEQ{PROP:LineHeight} = 1 + ListFEQ{PROP:LineHeight}   !too spread out

    SETFONT(?BopStr,'Consolas',11)          
    ListFEQ=?BopStr{PROP:ListFEQ}
    SETFONT(ListFEQ,'Segoe UI',9)   !  SETFONT(ListFEQ,'Consolas',9)
    ! ListFEQ{PROP:LineHeight} = 1 + ListFEQ{PROP:LineHeight} 
    
    ?InpStr{PROP:Tip} = 'Enter a Long Integer or ... ' & |
                        '<13,10>' & |
                            '<13,10,9>Clarion HEX h ' & |
                            '<13,10,9>Clarion BINARY b ' & |
                            '<13,10,9>Clarion OCTAL o ' & |
                            '<13,10,9>C/C++ Hex 0x## or Long "L" suffix ' & |
                            '<13,10,9>Visual Basic &H Hex ' & |
                            '<13,10,9>Java # Hex ' & |
                        '<13,10>' & |
                        '<13,10>Prefix with " Quote to enter String e.g. "ABCD ' & |
                        '<13,10>' & |
                        '<13,10>Enter any valid Clarion EVALUATE() expression'
    ?BopStr{PROP:Tip} = ?InpStr{PROP:Tip}                 
    RETURN
!----------------
Local.ffBtnPopup PROCEDURE()
PopTxt  CSTRING(2000)    !Len 1600 and 72 items
PpHx    PSTRING(12),DIM(5 * 8 + 32)
PrePop  PSTRING(12)
F8      STRING(8)
ULPp    ULONG
MaxP    SHORT
XP      SHORT

HexPik  STRING(16)
    CODE
    F8='FFFFFFFF'
    PopTxt = F8 & '{{'
        LOOP XP=1 TO 8 ; DO AddF8 ; F8='0' & F8[1:7] ; END  !FFFFFFFF .. 0000000F  
    PopTxt = PopTxt & '|-|'  
    F8='FFFFFFFF' 
        LOOP XP=1 TO 8 ; DO AddF8 ; F8=F8[2:8] & '0' ; END  !FFFFFFFF .. F0000000

    F8='FFFF0000' ; PopTxt = PopTxt & '}|' & F8 & '<9>16 Bit{{' ; DO RotateF8R   !16 bits
    F8='FF000000' ; PopTxt = PopTxt & '}|' & F8 & '<9>8 Bit{{'  ; DO RotateF8R   !8 bits
    F8='F0000000' ; PopTxt = PopTxt & '}|' & F8 & '<9>4 Bit{{'  ; DO RotateF8R   !4 bits
    
    PopTxt = PopTxt & '}|&32 Bits {{'
    LOOP XP=1 TO 32 
         IF XP > 1 AND XP % 8 = 1 THEN PopTxt = PopTxt & '|-'.
         ULPp = 2 ^ (XP-1)
         PrePop = XP &' - '
         F8=HexNumber(ULPp) 
         DO AddF8 
    END
    PopTxt = PopTxt & '}'   
    ! PopTxt = PopTxt &'|~Pop Len = '& LEN(PopTxt) &'  MaxP='& MaxP  ; SetClipboard(PopTxt)  !Debug PopTxt build

    XP = PopupUnder(?ffffffffBtn,PopTxt)
    IF XP THEN
       HexPik = PpHx[XP]
       IF HexPik[1] >= 'A' THEN HexPik = '0' & HexPik.      !Chg FFFh to 0FFFh but the code handles it
       ChangeAndSelect(?BopStr, HexPik)
    END

RotateF8R ROUTINE !Rotate [8] to [1] and AddF8
    LOOP XP=1 TO 8 ; DO AddF8 ; F8=F8[8] & F8[1:7] ; END
    
AddF8 ROUTINE
    MaxP+=1
    PpHx[MaxP] = CLIP(F8) & 'h'
    ULPp = EVALUATE('0' & PpHx[MaxP])
    PopTxt = PopTxt & |
             CHOOSE(XP<2,'','|') & |
             PrePop & |
             PpHx[MaxP] &'<9>'& CLIP(LEFT(FORMAT(ULPp,@n-14)))    
    PrePop=''
    EXIT
!----------------
Local.FontChange  PROCEDURE() 
FName  STRING(64)
FSize  LONG
FColor LONG(Color:None)
FStyle LONG(FONT:regular)
    CODE
    CASE PopupUnder(?FontChgListBtn,'Courier New 11|Consolas 11|Fixed Sys 9|-|Select Font ...|-|Copy List FORMAT() to Clipboard')  
    OF 0 ; RETURN
    OF 1 ; FName='Courier New' ; FSize=11 ; FStyle=FONT:bold
    OF 2 ; FName='Consolas'    ; FSize=11
    OF 3 ; FName='Fixedsys'    ; FSize=9
    OF 4
        GETFONT(?List:BMQ ,FName,FSize,FColor,FStyle)  
        IF ~FONTDIALOG('Select Font for List',FName,FSize,FColor,FStyle)  THEN 
            RETURN 
        END 

    OF 5 ; SetClipboard('BMQ List:  '& Local.FontDbg(?List:BMQ) & '<13,10>Format('''& QUOTE(?List:BMQ{PROP:Format}) &''')'& |            
           '<13,10,13,10>Inp Combo: '& Local.FontDbg(?InpStr)   & '<13,10>Format('''& QUOTE(?InpStr{PROP:Format}  ) &''')'& |
           '<13,10,13,10>Bop Combo: '& Local.FontDbg(?BopStr)   & '<13,10>Format('''& QUOTE(?BopStr{PROP:Format}  ) &''')'  ) 
           MESSAGE('FORMATs are on the Clipboard |Note: below "Pipes" cause line breaks in Message()||'& Clipboard(),'FORMATs on Clip')
           RETURN  !So can Size at Runtime then get Column Widths for various Font choices
    END
    IF FName AND FSize THEN 
       SETFONT(?List:BMQ ,FName,FSize,FColor,FStyle)
    END
    RETURN   
!----------------
Local.FontDbg PROCEDURE(LONG FEQ)!,STRING
    CODE    !The color will likely be COLOR:WindowText  EQUATE(80000008H) = -2147483640 so omit: &' , '& FEQ{PROP:FontColor}
    RETURN 'FONT('''& FEQ{PROP:FontName} &''' , '& FEQ{PROP:FontSize} &' , , '& FEQ{PROP:FontStyle} &' )'
!----------------
Local.RightClickList PROCEDURE()
Pik LONG,AUTO
DecmP PSTRING(16),AUTO
Hex8h STRING(10),AUTO        !Remove spaces + h and leading '0' if needed
Bin2b STRING(33),AUTO        !Remove leading Zeros?
    CODE
    DecmP = CLIP(LEFT(BMQ:DecBm))
    Hex8h = BMQ:HexBm[1:4] & BMQ:HexBm[6:9] &'h'
    IF Hex8h[1]>='A' THEN Hex8h= '0'& Hex8h.
    Bin2b = SUB(BMQ:BinBm,1,8) & SUB(BMQ:BinBm,10,8) & SUB(BMQ:BinBm,19,8) & SUB(BMQ:BinBm,28,8) 
    Ndx=INSTRING('1',Bin2b,1)
    IF Ndx < 1 OR Ndx > 25 THEN Ndx=25.     !Show at least 8 digits
    Bin2b=CLIP(SUB(Bin2b,Ndx,99)) &'b'
    EXECUTE Popup('Copy Selected Line|Copy All Lines' & |
               '|-|Copy Hex|Copy Decimal|Copy Binary|Copy Binary 4 Parts|Copy "Decimal => Hex => Binary"' & |
               '|-|Set Value to '& DecmP & '|Set Operand to '& DecmP  &|
               '|-|Set Value to '& Hex8h & '|Set Operand to '& Hex8h  &|
               '')
       Local.Copy2Clip(1)
       Local.Copy2Clip()
       SETCLIPBOARD(Hex8h)                  !|-|Copy Hex
       SETCLIPBOARD(DecmP) 
       SETCLIPBOARD(Bin2b) 
       SETCLIPBOARD(CLIP(BMQ:BinBm) ) 
       SETCLIPBOARD(DecmP &' => '& Hex8h &' => '& Bin2b )
       ChangeAndSelect(?InpStr,DecmP)       !|-|Set Value
       ChangeAndSelect(?BopStr,DecmP)
       ChangeAndSelect(?InpStr,Hex8h)
       ChangeAndSelect(?BopStr,Hex8h)
    END
    RETURN  
!----------------
Local.Copy2Clip PROCEDURE(BOOL Just1Line=False)
ClpB     ANY
PComment PSTRING(3)
    CODE         
    PComment=CHOOSE(~Comment2Clp,'','! ')
    IF Just1Line THEN 
       SETCLIPBOARD(PComment & BMQ)
       RETURN 
    END 
 
    clear(BMQ)
    BMQ:HexBm = '     Hex'
    BMQ:DecBm = '    Decimal'
    BMQ:BinBm = 'Binary'
    ClpB=PComment & CLIP(BMQ) 
    LOOP Ndx=1 TO RECORDS(BMQ)
         GET(BMQ,Ndx)
         ClpB=ClpB & CLIP('<13,10>' & PComment & BMQ)
    END
    SETCLIPBOARD(ClpB)
    RETURN 
!----------------
Local.AsciiTable    PROCEDURE()
ANo     LONG,AUTO
    CODE
    FREE(BMQ)
    LOOP ANo = 1 TO 255
         SELF.FmtLine(ANo, CHOOSE(ANo<33,FORMAT(ANo,@n02),CHR(ANo)) )
         BMQ:BinBm = SUB(BMQ:BinBm,9*3+1,8)                     !Only show 8 Bits not 32
         ADD(BMQ)
    END
    return

!----------------
Local.MapBits      PROCEDURE(LONG Lval)               !Bit Map the
BNo     LONG,AUTO
TheBit  LONG(1)
    CODE
    FREE(BMQ)
    SELF.FmtLine(Lval,'') ; ADD(BMQ)
    Self.FmtDashs()       ; ADD(BMQ)
    LOOP BNo = 1 TO 32
         IF BAND(LVal,TheBit)
            SELF.FmtLine(TheBit,BNo) ; ADD(BMQ)
         END
         TheBit = BSHIFT(TheBit,1)
    END
    return
!----------------
Local.MapHex       PROCEDURE(LONG Lval)               !Bit Map the
HexDig  LONG,AUTO
Nib     LONG,AUTO
AndMap  LONG,AUTO
HDigits STRING('123456789ABCDEF')
Hx      STRING(8)
!HxNbh   STRING(10)
    CODE
    BMQ=all('-') ; BMQ:HexBm='-Nib Map-----'
    Hx=Local.FmtHex(Lval)
    BMQ:BinBm=hx[1] & '---' & hx[2] & '--- ' & |
              hx[3] & '---' & hx[4] & '--- ' & |
              hx[5] & '---' & hx[6] & '--- ' & |
              hx[7] & '---' & hx[8] & '--- '
    ADD(BMQ)
    SELF.FmtLine(Lval,'') ; ADD(BMQ)
    Self.FmtDashs(' ')    ; ADD(BMQ)

    AndMap = 0F0000000h
    LOOP HexDig = 1 TO 8
         Nib=BAND(Lval,AndMap)  
         SELF.FmtLine(Nib,Hx[HexDig]) ; ADD(BMQ)
         AndMap = BSHIFT(AndMap, -4)
    END

    BMQ=all('-') ; BMQ:HexBm='Byte Map-----'
    Hx=Local.FmtHex(Lval)
    BMQ:BinBm= '---' & hx[1:2] & '--- ' & |
               '---' & hx[3:4] & '--- ' & |
               '---' & hx[5:6] & '--- ' & |
               '---' & hx[7:8] & '--- '
    ADD(BMQ)

    AndMap = 0FF000000h
    LOOP HexDig = 1 TO 4
         Nib=BAND(Lval,AndMap)  
         SELF.FmtLine(Nib, SUB(Hx,(HexDig-1)*2+1,2) ) ; ADD(BMQ)
         AndMap = BSHIFT(AndMap, -8)
    END

    RETURN
!----------------
Local.FmtHex      PROCEDURE(LONG Val1)!STRING  !Return 8 byte Hex for BMQ List
    CODE
    RETURN HexNumber(Val1)
!----------------
Local.FmtLine     PROCEDURE(LONG Val1, STRING inBitNo)                 !Format 1 Queue Line
    CODE
    CLEAR(BMQ)
    BMQ:BitNo = inBitNo
    BMQ:HexBm = Self.FmtNum(Val1,16)
    BMQ:DecBm = Self.FmtNum(Val1,10)
    BMQ:BinBm = Self.FmtNum(Val1,2)
    RETURN

!----------------
Local.FmtNum      PROCEDURE(LONG Val1, LONG nBase)!,STRING     !Form in Hex, Dec or Binary
ULongVal    ULONG,AUTO
NumBase     STRING(32),AUTO    
CFmtNm      CSTRING(64)
    CODE
    CASE nBase
    OF 16  ; NumBase = HexNumber(Val1) ; CFmtNm=sub(NumBase,1,4) &' '& sub(NumBase,5,4)  &'h'    !HEX XXXX XXXX
    OF  2  ; NumBase = BinNumber(Val1) ; CFmtNm=sub(NumBase,1,8) &' '& sub(NumBase,9,8)  &' '& |
                                               sub(NumBase,17,8) &' '& sub(NumBase,25,8) &'b'    !Binary 8 8 8 8
    ELSE     !Decimal and unknown
             ULongVal = Val1
             CFmtNm = RIGHT(ULongVal,11)
    END
    RETURN CFmtNm
!-------------------------------
Local.BitOperation PROCEDURE(LONG Op1, LONG Op2, LONG OperatorBtnFEQ) 
OpName  STRING(32)
Op3     LONG(0)
    CODE
    IF BopStr='' AND OperatorBtnFEQ <> ?OP_Tilde THEN
       SELECT(?BopStr) 
       Message('Please enter an Operand value for the Operator button "'& UPPER(OperatorBtnFEQ{PROP:Value}) &'"' ,'Bitwise Operation')
       RETURN 0
    END
    BMQ=ALL('=') ; BMQ:HexBm='-BitCalc-----' ; ADD(BMQ)
    
    SELF.FmtLine(Op1,'') ; ADD(BMQ)          !The original Number
    OpName = OperatorBtnFEQ{prop:Value} 
    OpName = OpName[1] & Lower(OpName[2:3])
    CASE OperatorBtnFEQ
    OF ?OP_AND   ; Op3=BAND(Op1,Op2)            ! BUTTON('AND')  USE(?OP_AND)
    OF ?OP_OR    ; Op3=BOR (Op1,Op2)            ! BUTTON('OR')   USE(?OP_OR)
    OF ?OP_XOR   ; Op3=BXOR(Op1,Op2)            ! BUTTON('XOR')  USE(?OP_XOR)
    OF ?OP_NAND  ; Op3=BAND(Op1,BXOR(-1,Op2))   ! BUTTON('~AND')  USE(?OP_NAND)
                   OpName='~And'

    OF ?OP_Plus  ; Op3=Op1+Op2                  ! BUTTON('+')  USE(?OP_Plus)
    OF ?OP_Minus ; Op3=Op1-Op2                  ! BUTTON('-')  USE(?OP_Minus)

    OF ?OP_Tilde ; Op2=-1 ; Op3=BXOR(Op1,Op2)   ! BUTTON('~') USE(?OP_Tilde)
                   OpName='XOR Complement'    

    OF ?OP_Shift                                ! BUTTON('Shift') USE(?OP_Shift)
                   IF ~INRANGE(Op2,-32,32) THEN
                       Message('The Operand "'& Op2 &'" is invalid for the Shift Operator.' & |
                               '||The Operand must be between -32 and +32.','Bit Shift', ICON:Asterisk)
                       SELECT(?BopStr)
                       RETURN 0
                   END
                   Op3=BSHIFT(Op1,Op2)
                   Self.FmtColHead('Shift ' & Op2) 
                       BMQ:BitNo=CHOOSE(Op2 < 0,'>','<<') & ABS(Op2)
                       BMQ:BinBm='BSHIFT('& Op1 &','& Op2 &')  '& ABS(Op2) &' '& CHOOSE(Op2 < 0,'Right','Left')
                       ADD(BMQ)
                   
                   Self.FmtLine(Op3,'=')         ; ADD(BMQ)
                   return Op3

    END

    Self.FmtLine(Op2, OpName ) ; ADD(BMQ)
    Self.FmtLine(Op3, '=')     ; ADD(BMQ)

    CASE OperatorBtnFEQ
    OF ?OP_NAND
        Self.FmtColHead('--i.e.'& all('-',32))  ; BMQ:BinBm='-- i.e. BAND(Op1,BXOR(Op2,-1)) ' & all('-',32)   ; ADD(BMQ)
        Self.FmtLine(Op2,'Msk')        ; ADD(BMQ)
        Self.FmtColHead('Complement')  ; BMQ:BinBm='Mask "NOT" is Below = XOR(Msk,-1)' ; ADD(BMQ)

        Self.FmtLine(BXOR(-1,Op2) , '~ =') ; ADD(BMQ)
        SELF.FmtLine(Op1          , 'And') ; ADD(BMQ)
        Self.FmtLine(Op3          , '='  ) ; ADD(BMQ)
    END

    RETURN Op3

!-------------------------------    
Local.FmtColHead     PROCEDURE(String HeadText)
    CODE
    CLEAR(BMQ)
    !BMQ:BitNo = ''
    BMQ:HexBm = HeadText
    BMQ:DecBm = HeadText
    BMQ:BinBm = HeadText
    RETURN

    ! =---=---=---=---=---=---=---=---
    ! 8---4---2---1---8---4---2---1---
!-------------------------------
Local.FmtDashs     PROCEDURE(<STRING inBitNo>)
    CODE
    BMQ=ALL('-')
    IF ~OMITTED(inBitNo) THEN BMQ:BitNo = inBitNo.
    RETURN

!-------------------------------
Local.TakeHexInput   PROCEDURE(*STRING EntryValue,*LONG OutVal)!,BOOL      !Clean the HexInput and Output a Long
EvalStr     STRING(260)      !What we are evaluating, assume entry is max 255
EvLen       LONG
String4     STRING(4),AUTO
Long4       LONG,OVER(String4),AUTO
    CODE
    EvalStr = LEFT(upper(EntryValue))
    EvLen  = LEN(CLIP(EvalStr))
    IF ~EvLen THEN RETURN 0. !CYCLE

    IF EvalStr[1:2]='0X' OR INSTRING(EvalStr[1],'&O0123456789-',1)      !Could it be a C or VB constant?
        LOOP 3 TIMES                                                    !C can have UL LL so loop up to 2 times, VB trailing & is a Long
           IF ~EvLen OR ~INSTRING(EvalStr[EvLen],' LU&',1) THEN BREAK.  !LONG flag in C++ trailing L or U, VB &, meaningless to me
           EvalStr[EvLen]=''                                            !I hope this does not screw up an EVAL string, it could :(
           EvLen -= 1
        END                                                        
        IF ~EvLen THEN RETURN 0. !CYCLE
    END

    IF EvalStr[1:2]='0X' OR EvalStr[1:2]='&H'                  !Is it a C type 0x00000C00 or VB &H00000C00
       EvalStr=CLIP(LEFT(SUB(EvalStr,3,EvLen-2))) & 'H'        !    then add H to end
    ELSIF EvalStr[1]='#'                                       !Is it an HTML Hex Constant
       EvalStr=CLIP(LEFT(SUB(EvalStr,2,EvLen-1))) & 'H'        !    then add H to end
    ELSIF EvalStr[1]='O'                                       !Is it a C Octal type O
       EvalStr=CLIP(LEFT(SUB(EvalStr,2,EvLen-1))) & 'O'        !    then add O to end
    ELSIF EvalStr[1:2]='&O'                                    !Is it VB Octal &O00000C00
       EvalStr=CLIP(LEFT(SUB(EvalStr,3,EvLen-2))) & 'O'        !    then add O to end
    ELSIF EvalStr[1]='"'                                       !" is String constant up to 4 bytes
        IF EvLen >= 3 AND EvalStr[EvLen]='"' THEN              !In case a trailing Queue is entered 
           EvalStr[EvLen]=''                                   !Cut it off
           EvLen -= 1
        END
        EvalStr = EvalStr[2 : SIZE(EvalStr) ]
        String4 = CLIP(EvalStr) & '<0,0,0,0>'   !Do I do anything with Little Endian?
        EvalStr=Long4
    END
    EvLen  = LEN(CLIP(EvalStr))

!           Message('EvLen=' & EvLen & |
!                  '|EvalStr=' & clip(EvalStr) & |
!                  '|NUMERIC(EvalStr)=' & NUMERIC(EvalStr) & |                                    !Look for Heximdecimal w/o H
!                  '|INSTRING(' & EvalStr[EvLen] &'=HO=' & INSTRING(EvalStr[EvLen],'HO',1) &  |   !Not Hex,Oct,Binary
!                  '|MATCH ^-?[0-9]+$=' & MATCH(EvalStr,'^-?[0-9]+$',MATCH:REGULAR) & |
!                  '|MATCH ^[0-9A-F]+$=' & MATCH(EvalStr,'^[0-9A-F]+$',MATCH:REGULAR)  )      !and is Hex Digits

    IF ~EvLen THEN RETURN 0.

    !Look for Heximdecimal w/o trailing 'H'
    IF  INRANGE(EvLen,2,10)                          |   !-23456789h
    AND ~INSTRING(EvalStr[EvLen],'HOB',1)            |   !Not Hex or Octal or Binary already
    AND ~MATCH(EvalStr,'^-?[0-9]+$',MATCH:REGULAR)   |   !and not all numers
    AND MATCH(EvalStr,'^-?[0-9A-F]+$',MATCH:REGULAR)     !and is Hex Digits with optional leading -
        EvLen += 1            
        EvalStr[EvLen]='H'                                !so make it a H
    END

    IF EvalStr[EvLen]='H' THEN                                  !Need '0' Prefix in Hex ?
       IF EvalStr[1] >= '0' AND EvalStr[1] <= '9' THEN 
             !Byte 1 is a Number so no need to go further
       ELSIF ~MATCH(EvalStr,'^-?[0-9A-F]+H$',MATCH:REGULAR) THEN   !Is only Hex Digits + H ?
             !Not only Hex Digits so can skip rest of checks
       ELSIF EvalStr[1] >= 'A' THEN
             EvalStr = '0' & left(EvalStr)                 !FFh fails eval, change to 0FFh
       ELSIF EvalStr[1] = '-' AND EvalStr[2] >= 'A' THEN
             EvalStr = '-0' & SUB(EvalStr,2,999)           !-FFh fails eval, change to -0FFh
       END
    END

!    ?InpStr{PROP:Tip}='InpStr=' & InpStr &'<13,10>EvalStr=' & EvalStr  !Debug this 

    OutVal=EVALUATE(EvalStr)          !Simple for now
    IF ERRORCODE() THEN
         Message('Error Evaluating Entry ' & |
                 '|Input: <9>' & clip(InpStr) & |
                 '|Evalute:<9>' & clip(EvalStr) & |
                 '|Error: <9>' & errorcode()&' '&clip(error()) & |
                 '||Remember Hex constants must start with a number' & |
                 'i.e. FFh will error, correct eway is 0FFh' )
         return 0
    END

    return 1        !We worked

!===========================================================================
ChangeAndSelect   PROCEDURE(LONG ControlFEQ, ? NewValue, BOOL TouchIt=True)
    CODE
    CHANGE(ControlFEQ,NewValue) 
    SELECT(ControlFEQ) ; 
    IF TouchIt THEN ControlFEQ{PROP:Touched}=True.
    RETURN
!===========================================================================
ComboQueueAdd PROCEDURE(*InpStringQ ComboQ, STRING inTheString, LONG inTheLong, BOOL AddLast=False)
    CODE
    IF AddLast=False THEN 
       ComboQ.UprStr = UPPER(inTheString)  
       GET(ComboQ,ComboQ.UprStr) 
       IF ~ERRORCODE() THEN DELETE(ComboQ).
    END
    
    ComboQ.TheStr  = inTheString 
    ComboQ.TheLong = inTheLong 
    ComboQ.TheHex  = HexNumber(inTheLong)
    ComboQ.UprStr  = UPPER(inTheString)
    IF ~AddLast THEN  
        Add(ComboQ,1)
    ELSE 
        Add(ComboQ)
    END     
    RETURN  
!===========================================================================   
BinNumber PROCEDURE(ULONG ULng, BOOL TrimLeadZeros=False)!,STRING    !Return Long in Binary
Bits    STRING(32),AUTO
OneBit  LONG,AUTO  
BNo     USHORT,AUTO
HiBNo   USHORT,AUTO
    CODE
    Bits=ALL('0') ; OneBit=1 ; HiBNo=32
    LOOP BNo = 32 TO 1 BY -1
         IF BAND(ULng,OneBit) THEN Bits[BNo]='1' ; HiBNo=BNo.
         OneBit = BSHIFT(OneBit,1)
    END
    IF TrimLeadZeros THEN RETURN SUB(Bits,HiBNo,32). 
    return Bits
!===========================================================================       
HexNumber PROCEDURE(LONG Lng)!,STRING    !Return 8 digit hex
LngAdj  LONG,AUTO 
L       BYTE,DIM(4),OVER(LngAdj)
Dgt     STRING('0123456789ABCDEF'),STATIC
HX      STRING(8),AUTO 
  CODE
  LngAdj = BAND(BSHIFT(Lng, -4),0F0F0F0Fh) + 01010101h
  HX[1]=Dgt[L[4]] ; HX[3]=Dgt[L[3]] ; HX[5]=Dgt[L[2]] ; HX[7]=Dgt[L[1]]
  LngAdj=BAND(Lng,0F0F0F0Fh)  + 01010101h
  HX[2]=Dgt[L[4]] ; HX[4]=Dgt[L[3]] ; HX[6]=Dgt[L[2]] ; HX[8]=Dgt[L[1]]
  RETURN HX 
!Alternate Code calling LtoA() but for me this GPFs in LIB builds:
!CFmtd  cstring(64)
!    CODE
!    LtoA(Lng, CFmtd, 16)
!    CFmtd=all('0', 8 - len(CFmtd)) & UPPER(CFmtd)
!    RETURN CLIP(CFmtd)  
!===========================================================================
PopupUnder PROCEDURE(LONG CtrlFEQ, STRING PopMenu)!,LONG
X LONG,AUTO
Y LONG,AUTO
H LONG,AUTO
    CODE
    GETPOSITION(CtrlFEQ,X,Y,,H)
    IF CtrlFEQ{PROP:InToolBar} THEN Y -= (0{PROP:ToolBar}){PROP:Height}.
    RETURN POPUP(PopMenu,X,Y+H+1,1) 

!===========================================================================
BindEquates PROCEDURE()  !Bind some Equates to allow using them in an ENTRY 'cause it uses EVALUATE()
    MAP                  !The Font equate are a little interesting for Bit Ops. For Colors Bit Ops to work with RGB 
Bind1Equate     PROCEDURE(STRING EqtLabel, ? EqtValue) 
    END
Add2DropQs BYTE         !0=No 1=InpQ; 2=BopQ; 3=Both
AddLast2Q  BOOL(True)
    CODE
    Add2DropQs = 1
    Bind1Equate('FONT:Thin'     , FONT:Thin     )  ! 100     64h
    Bind1Equate('FONT:Regular'  , FONT:Regular  )  ! 400    190h
    Bind1Equate('FONT:Bold'     , FONT:Bold     )  ! 700    2BCh
    Add2DropQs = 3
    Bind1Equate('FONT:Weight'   , FONT:Weight   )  !  07FFh
    Bind1Equate('FONT:Fixed'    , FONT:Fixed    )  !  0800h
    Bind1Equate('FONT:Italic'   , FONT:Italic   )  !  1000h
    Bind1Equate('FONT:Underline', FONT:Underline)  !  2000h
    Bind1Equate('FONT:Strikeout', FONT:Strikeout)  !  4000h    

    Bind1Equate('COLOR:Red'    , COLOR:Red    )  ! 00000FFH     !RGB Positive Colors
    Bind1Equate('COLOR:Lime'   , COLOR:Lime   )  ! 000FF00H     !This is Green with FF in Byte 3
    Bind1Equate('COLOR:Blue'   , COLOR:Blue   )  ! 0FF0000H
    Add2DropQs = 1
    Bind1Equate('COLOR:Aqua'   , COLOR:Aqua   )  ! 0FFFF00H     !Cyan       CMYK Negative Colors
    Bind1Equate('COLOR:Fuchsia', COLOR:Fuchsia)  ! 0FF00FFH     !Magenta
    Bind1Equate('COLOR:Yellow' , COLOR:Yellow )  ! 000FFFFH     !Yellow
    Bind1Equate('COLOR:Black'  , COLOR:Black  )  ! 0000000H
    Bind1Equate('COLOR:Silver' , COLOR:Silver )  ! 0C0C0C0H
    Bind1Equate('COLOR:Gray'   , COLOR:Gray   )  ! 0808080H
    Bind1Equate('COLOR:White'  , COLOR:White  )  ! 0FFFFFFH

    Bind1Equate('COLOR:Green'  , COLOR:Green  )  ! 0008000H
    Bind1Equate('COLOR:Maroon' , COLOR:Maroon )  ! 0000080H
    Bind1Equate('COLOR:Navy'   , COLOR:Navy   )  ! 0800000H
    Bind1Equate('COLOR:Olive'  , COLOR:Olive  )  ! 0008080H
    Bind1Equate('COLOR:Orange' , COLOR:Orange )  ! 00080FFH
    Bind1Equate('COLOR:Purple' , COLOR:Purple )  ! 0800080H
    Bind1Equate('COLOR:Teal'   , COLOR:Teal   )  ! 0808000H
    RETURN 
!----------
Bind1Equate PROCEDURE(STRING EqtLabel, ? EqtValue)  !Local Map proc
    CODE
    BindExpression(EqtLabel, EqtValue)
    IF THREAD()=1 THEN      !May not want the Bind Equates in Queue, but good for testing 
       IF BAND(Add2DropQs,1) THEN ComboQueueAdd(InpStringQ, EqtLabel , EqtValue, AddLast2Q).  ! True = Add Last      
       IF BAND(Add2DropQs,2) THEN ComboQueueAdd(BopStringQ, EqtLabel , EqtValue, AddLast2Q).
    END
    RETURN 
!===========================================================================   