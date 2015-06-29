{******************************************************************************}
{*                                                                            *}
{* (C) 2007 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit SingleThreadStrings;

{$D-}

interface

uses
  Windows;

procedure _LStrEqual{left: AnsiString; right: AnsiString};
procedure _WStrEqual{left: WideString; right: WideString};
procedure _LStrCmp_LStrEqual{left: AnsiString; right: AnsiString};
procedure _WStrCmp_WStrEqual{left: WideString; right: WideString};

implementation

const
  CurProcess = Cardinal(-1); // GetCurrentProcess returns EAX = DWORD(-1)

type
  TInjectRec = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Addr: ^Pointer;
  end;

  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;

function IsWin9xDebugThunk(AnAddr: Pointer): Boolean;
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.PUSH, $68
  JNE @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.JMP, $E9
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@exit
@@NoThunk:
  XOR EAX, EAX
@@exit:
end;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then
  begin
    if {(SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) and} IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

procedure CodeRedirect(Proc: Pointer; NewProc: Pointer);
var
  OldProtect: Cardinal;
begin
  if Proc = nil then
    Exit;
  Proc := GetActualAddr(Proc);
  if VirtualProtectEx(CurProcess, Proc, 5, PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    TInjectRec(Proc^).Jump := $E9;
    TInjectRec(Proc^).Offset := Integer(NewProc) - (Integer(Proc) + 5);
    VirtualProtectEx(CurProcess, Proc, 5, OldProtect, @OldProtect);
  end;
end;



var
  OrgLStrCmp: Pointer;
  OrgWStrCmp: Pointer;

type
  PStrRec = ^StrRec;
  StrRec = packed record
    refCnt: Longint;
    length: Longint;
  end;

const
  skew = SizeOf(StrRec);
  rOff = SizeOf(StrRec); { refCnt offset }
  overHead = SizeOf(StrRec) + 1;

procedure _LStrAsg(var dest; const source);
asm
        { ->    EAX pointer to dest   str      }
        { ->    EDX pointer to source str      }

                TEST    EDX,EDX                           { have a source? }
                JE      @@2                               { no -> jump     }

                MOV     ECX,[EDX-skew].StrRec.refCnt
                INC     ECX
                JG      @@1                               { literal string -> jump not taken }

                PUSH    EAX
                PUSH    EDX
                MOV     EAX,[EDX-skew].StrRec.length
                CALL    System.@NewAnsiString
                MOV     EDX,EAX
                POP     EAX
                PUSH    EDX
                MOV     ECX,[EAX-skew].StrRec.length
                CALL    Move
                POP     EDX
                POP     EAX
                JMP     @@2

@@1:
           {LOCK} INC     [EDX-skew].StrRec.refCnt

@@2:
                //XCHG    EDX,[EAX]
                mov ecx,[eax]
                mov [eax],edx
                mov edx,ecx

                TEST    EDX,EDX
                JE      @@3
                MOV     ECX,[EDX-skew].StrRec.refCnt
                DEC     ECX
                JL      @@3
           {LOCK} DEC     [EDX-skew].StrRec.refCnt
                JNE     @@3
                LEA     EAX,[EDX-skew].StrRec.refCnt
                CALL    System.@FreeMem
@@3:
end;

procedure _LStrLAsg(var dest; const source);
asm
{ ->    EAX     pointer to dest }
{       EDX     source          }

                TEST    EDX,EDX
                JE      @@sourceDone

                { bump up the ref count of the source }

                MOV     ECX,[EDX-skew].StrRec.refCnt
                INC     ECX
                JLE     @@sourceDone                    { literal assignment -> jump taken }
           {LOCK} INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

                { we need to release whatever the dest is pointing to   }
                //XCHG    EDX,[EAX]                       { fetch str                    }
                mov ecx,[eax]
                mov [eax],edx
                mov edx,ecx

                TEST    EDX,EDX                         { if nil, nothing to do        }
                JE      @@done
                MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
                DEC     ECX                             { if < 0: literal str          }
                JL      @@done
           {LOCK} DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
                JNE     @@done
                LEA     EAX,[EDX-skew].StrRec.refCnt    { if refCnt now zero, deallocate}
                CALL    System.@FreeMem
@@done:
end;


procedure _LStrClr(var S);
asm
        { ->    EAX pointer to str      }

        MOV     EDX,[EAX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@done
        MOV     dword ptr [EAX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@done
   {LOCK} DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@done
        PUSH    EAX
        LEA     EAX,[EDX-skew].StrRec.refCnt    { if refCnt now zero, deallocate}
        CALL    System.@FreeMem
        POP     EAX
@@done:
end;

function _LStrAddRef(var str): Pointer;
asm
        { ->    EAX     str     }
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[EAX-skew].StrRec.refCnt
        INC     EDX
        JLE     @@exit
   {LOCK} INC     [EAX-skew].StrRec.refCnt
@@exit:
end;

const
  oleaut = 'oleaut32.dll';

procedure SysFreeString(const S: WideString); stdcall;
  external oleaut name 'SysFreeString';

procedure WStrSet(var S: WideString; P: PWideChar);
asm
    // WideStrings are not reference counted under Windows
        MOV     ECX,[EAX]
        MOV     [EAX],EDX
        TEST    ECX,ECX
        JZ      @@1
        PUSH    ECX
        CALL    SysFreeString
@@1:
end;


{------------------------------------------------------------------------------}
 (* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrEqual is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche
*
* ***** END LICENSE BLOCK ***** *)
procedure _LStrEqual;
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     68 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @CompareDoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @CompareDonePop
@CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @CompareLoop
@CompareDonePop:
  pop ebx
@CompareDoneNoPop:
  ret
@Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

{------------------------------------------------------------------------------}

type
  TLongCall = packed record
    Call: Byte;
    Offset: Integer;
    NextCmd: record end;
  end;

  PPointer = ^Pointer;

  PImportEntry = ^TImportEntry;
  TImportEntry = packed record
    Jmp: Word;
    Destination: PPointer;
    Magic: Word;
  end;

procedure ReplaceLStrCmpByLStrEqual(var ProcAddr: TLongCall);
var
  OldProtect: Cardinal;
  ImportEntry: PImportEntry;
begin
  ImportEntry := Pointer(Integer(@ProcAddr.NextCmd) + ProcAddr.Offset);
  if (ImportEntry = @_LStrCmp_LStrEqual) or (ImportEntry = OrgLStrCmp) then
  begin
    if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      ProcAddr.Offset := Integer(@_LStrEqual) - (Integer(@ProcAddr.NextCmd));
      VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
    end;
  end
  else
  begin
    try
      { protect against non-Delphi conform import tables }
      if //not IsBadReadPtr(ImportEntry, SizeOf(TImportEntry)) and
         (ImportEntry.Jmp = $25FF) and ((ImportEntry.Magic = $C08B) or (ImportEntry.Magic = $25FF)) then
      begin
        if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          ProcAddr.Offset := Integer(@_LStrEqual) - (Integer(@ProcAddr.NextCmd));
          VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
        end;
      end;
    except
    end;
  end;
end;

{------------------------------------------------------------------------------}
(* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrCmp is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche,
*                 Andreas Hausladen (inline function replacing)
*
* ***** END LICENSE BLOCK ***** *)
procedure _LStrCmp_LStrEqual{left: AnsiString; right: AnsiString};
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       CF = 1 if S1 < S2, CF = 0 otherwise
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     88+41 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @DoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString

@BothStringsNonNil:
  // can _LStrEqual be used
  mov ecx, [esp] // read return address
  mov cx, WORD PTR [ecx] // read first byte at the return address

{  cmp cl, $75 // JNZ near
  je @LStrEqual
  jg @StartCompare
  cmp cl, $74 // JZ near
  je @LStrEqual}
  add cl, $8c
  sub cl, $02
  jb @LStrEqual

  // prefix opcodes
{  cmp cl, $0f
  jne @StartCompare}
  add cl, $67 //=$02-$8c-$0f
  jnz @StartCompare

{  cmp ch, $95 // SETNZ al
  je @LStrEqual
  jg @StartCompare
  cmp ch, $84 // JZ far
  je @LStrEqual
  jl @StartCompare
  cmp ch, $94 // SETZ al
  je @LStrEqual
  cmp ch, $85 // JNZ far
  je @LStrEqual}
  add ch, $7c
  sub ch, $02
  jb @LStrEqual
  add ch, $f2
  sub ch, $02
  jb @LStrEqual

@StartCompare:
  {Compare the first character. (There has to be a trailing #0, so this
   comparison is safe). In "random" string compares this can save significant
   CPU time.}
  movzx ecx, byte ptr [eax]
  sub cl, [edx]
  jne @DoneNoPop
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) <= length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative offset based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  {Next four bytes}
  add ecx, 4
  js @CompareLoop
  {All characters match up to the compare length}
@MatchUpToLength:
  {Restore the string length difference to eax}
  pop eax
  {Set the flags according to the length difference}
  add eax, eax
  {Restore ebx and return}
  pop ebx
@DoneNoPop:
  ret
@Mismatch:
  {Find the byte index that mismatched}
  bsf ebx, ebx
  shr ebx, 3
  {Is the mismatch beyond the compare length?}
  add ecx, ebx
  jns @MatchUpToLength
  {Compare the mismatched byte, setting the flags}
  mov al, [eax + ecx]
  cmp al, [edx + ecx]
  {Pop the length difference, restore ebx and return}
  pop ebx
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
{ --- <LStrEqual> --- }
@LStrEqual:
  push eax
  push edx
  mov eax, [esp+8]    // get caller...
  sub eax, 5          // ... and the calling opcode
  cmp BYTE PTR [eax], $E8
  jne @@CallFunction
  call ReplaceLStrCmpByLStrEqual
@@CallFunction:
  pop edx
  pop eax
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     xx bytes}

  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @LStrEqual_CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @LStrEqual_CompareDonePop
@LStrEqual_CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @LStrEqual_Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @LStrEqual_CompareLoop
@LStrEqual_CompareDonePop:
  pop ebx
@LStrEqual_CompareDoneNoPop:
  ret
@LStrEqual_Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
{ --- </LStrEqual> --- }
@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
 (* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrEqual is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche
*
* ***** END LICENSE BLOCK ***** *)
procedure _WStrEqual;
asm
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     68 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @CompareDoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString
@BothStringsNonNil:
  {Compare lengths}
  mov ecx, [eax - 4]
  cmp ecx, [edx - 4]
  jne @CompareDoneNoPop
  {Save ebx}
  push ebx
  {Get pointers to the 4th last bytes in the strings}
  lea edx, [edx + ecx - 4]
  lea ebx, [eax + ecx - 4]
  {Negate the loop counter}
  neg ecx
  {Compare the last four bytes. If the string length is less than four bytes
   then part of the length field is compared again - no harm done.}
  mov eax, [ebx]
  cmp eax, [edx]
  jne @CompareDonePop
@CompareLoop:
  {Next four bytes}
  add ecx, 4
  jns @Match
  {Compare four bytes per iteration}
  mov eax, [ebx + ecx]
  cmp eax, [edx + ecx]
  je @CompareLoop
@CompareDonePop:
  pop ebx
@CompareDoneNoPop:
  ret
@Match:
  {Strings match - set the zero flag}
  xor eax, eax
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret
@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

procedure ReplaceWStrCmpByWStrEqual(var ProcAddr: TLongCall);
type
  PInlineNilCmp = ^TInlineNilCmp;
  TInlineNilCmp = packed record
    TestEaxEax: Word;
    Nops: Word;
    Nop: Byte;
  end;

var
  OldProtect: Cardinal;
  ImportEntry: PImportEntry;
begin
  ImportEntry := Pointer(Integer(@ProcAddr.NextCmd) + ProcAddr.Offset);
  if (ImportEntry = @_WStrCmp_WStrEqual) or (ImportEntry = OrgWStrCmp)  then
  begin
    { Disabled because if there is .NET code in an IDE expert that calls an
      Win32 IDE function with a non-NULL zero length WideString will cause an
      access violation in the code that meight follow the inlined "if WS = '' then" }

    {if PWord(Cardinal(@ProcAddr) - 2)^ = $D233 then // xor edx,edx
    begin // => if WS = '' then ...
      if VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PInlineNilCmp(@ProcAddr).TestEaxEax := $C085; // test eax,eax
        PInlineNilCmp(@ProcAddr).Nops := $9090;
        PInlineNilCmp(@ProcAddr).Nop := $90;
        VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), OldProtect, @OldProtect);
      end;
    end
    else}
    if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      ProcAddr.Offset := Integer(@_WStrEqual) - (Integer(@ProcAddr.NextCmd));
      VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
    end;
  end
  else
  begin
    try
      { protect against non-Delphi conform import tables }
      if //not IsBadReadPtr(ImportEntry, SizeOf(TImportEntry)) and
         (ImportEntry.Jmp = $25FF) and ((ImportEntry.Magic = $C08B) or (ImportEntry.Magic = $25FF)) then
      begin
        { Disabled because if there is .NET code in an IDE expert that calls an
          Win32 IDE function with a non-NULL zero length WideString will cause an
          access violation in the code that meight follow the inlined "if WS = '' then" }

        {if PWord(Cardinal(@ProcAddr) - 2)^ = $D233 then // xor edx,edx
        begin // => if WS = '' then ...
          if VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), PAGE_EXECUTE_READWRITE, OldProtect) then
          begin
            PInlineNilCmp(@ProcAddr).TestEaxEax := $C085; // test eax,eax
            PInlineNilCmp(@ProcAddr).Nops := $9090;
            PInlineNilCmp(@ProcAddr).Nop := $90;
            VirtualProtectEx(CurProcess, @ProcAddr, SizeOf(ProcAddr), OldProtect, @OldProtect);
          end;
        end
        else}
        if VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          ProcAddr.Offset := Integer(@_WStrEqual) - (Integer(@ProcAddr.NextCmd));
          VirtualProtectEx(CurProcess, @ProcAddr.Offset, SizeOf(Integer), OldProtect, @OldProtect);
        end;
      end;
    except
    end;
  end;
end;

{------------------------------------------------------------------------------}
(* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1
*
* The assembly implementation of function _LStrCmp is subject to the
* Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License.
* You may obtain a copy of the License at http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Initial Developer of the Original Code is Pierre le Riche
*
* Portions created by the Initial Developer are Copyright (C) 2002-2007
* the Initial Developer. All Rights Reserved.
*
* Contributor(s): Pierre le Riche,
*                 Andreas Hausladen (inline function replacing)
*
* ***** END LICENSE BLOCK ***** *)
procedure _WStrCmp_WStrEqual{left: WideString; right: WideString};
asm
  // Check if this call is a candidate for _WStrEqual

  mov ecx, [esp]         { read return address }
  mov cx, WORD PTR [ecx] { read opcode at the return address }

  {cmp cl, $75 // JNZ near
  je @LStrEqual
  jg @StartCompare
  cmp cl, $74 // JZ near
  je @LStrEqual}
  add cl, $8c
  sub cl, $02
  jb @WStrEqual

  { prefixed opcodes }
  {cmp cl, $0f
  jne @StartCompare}
  add cl, $67 //=$02-$8c-$0f
  jnz @StartCompare

  {cmp ch, $95 // SETNZ al
  je @LStrEqual
  jg @StartCompare
  cmp ch, $84 // JZ far
  je @LStrEqual
  jl @StartCompare
  cmp ch, $94 // SETZ al
  je @LStrEqual
  cmp ch, $85 // JNZ far
  je @LStrEqual}
  add ch, $7c
  sub ch, $02
  jb @WStrEqual
  add ch, $f2
  sub ch, $02
  jb @WStrEqual

@StartCompare:
  {On entry:
     eax = @S1[1]
     edx = @S2[1]
   On exit:
     Result in flags:
       CF = 1 if S1 < S2, CF = 0 otherwise
       ZF = 1 if S1 = S2, ZF = 0 otherwise
   Destroys:
     eax, edx, ecx
   Code size:
     88+41 bytes}
  {Do S1 and S2 point to the same string data?}
  cmp eax, edx
  je @DoneNoPop
  {Is one of the two string pointers perhaps nil?}
  test eax, edx
  jz @PossibleNilString

@BothStringsNonNil:
  {Compare the first character. (There has to be a trailing #0, so this
   comparison is safe). In "random" string compares this can save significant
   CPU time.}
  movzx ecx, word ptr [eax]
  sub cx, [edx]
  jne @DoneNoPop
  {Save ebx}
  push ebx
  {Set ebx = length(S1)}
  mov ebx, [eax - 4]
  {Set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {Set ecx = 0 if length(S1) <= length(S2), $ffffffff otherwise}
  adc ecx, -1
  {Set ecx = - min(length(S1), length(S2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative offset based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jnz @Mismatch
  {Next four bytes}
  add ecx, 4
  js @CompareLoop
  {All characters match up to the compare length}
@MatchUpToLength:
  {Restore the string length difference to eax}
  pop eax
  {Set the flags according to the length difference}
  add eax, eax
  {Restore ebx and return}
  pop ebx
@DoneNoPop:
  ret
@Mismatch:
  {Find the byte index that mismatched}
  bsf ebx, ebx
  shr ebx, 3
  {Is the mismatch beyond the compare length?}
  add ecx, ebx
  jns @MatchUpToLength
  {Compare the mismatched byte, setting the flags}
  mov ax, [eax + ecx]
  cmp ax, [edx + ecx]
  {Pop the length difference, restore ebx and return}
  pop ebx
  pop ebx
  ret
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothStringsNonNil
  {S2 is nil - compare lengths of the strings}
  cmp [eax - 4], edx
  ret

@WStrEqual:
  push eax
  push edx
  mov eax, [esp+8]    // get caller...
  sub eax, 5          // ... and the calling opcode
  cmp BYTE PTR [eax], $E8
  jne @@CallFunction
  call ReplaceWStrCmpByWStrEqual
@@CallFunction:
  pop edx
  pop eax
  jmp _WStrEqual

@FirstStringNil:
  {S1 is nil - compare lengths of the strings}
  cmp eax, [edx - 4]
end;

{------------------------------------------------------------------------------}
{
E8xxxxxxxx       call WStrSet
5F               pop edi
5E               pop esi
5B               pop ebx
C3               ret
}

function GetWStrSetLength: Pointer; asm mov eax, OFFSET System.@WStrSetLength; end;

function GetWStrSet: Pointer;
type
  PLocation = ^TLocation;
  TLocation = packed record
    Call: Byte;
    Offset: Longint;
    PopRet: LongWord;
  end;

var
  P: PLocation;
  Count: Integer;
begin
  Count := $100;
  P := GetActualAddr(GetWStrSetLength);
  Inc(PByte(P), 16);
  while (Count > 0) and not IsBadReadPtr(P, SizeOf(TLocation)) do
  begin
    if (P.Call = $E8) and (P.PopRet = $C35B5E5F) then
    begin
      Result := Pointer(Integer(@P.PopRet) + Integer(P.Offset));
      if IsBadReadPtr(Result, 2) or (PWord(Result)^ <> $1087 {xchg [eax],edx}) then
        Result := nil;
      Exit;
    end;
    Inc(PByte(P));
    Dec(Count);
  end;
  Result := nil;
end;

function GetLStrLAsg: Pointer; asm mov eax, OFFSET System.@LStrLAsg; end;
function GetLStrAsg: Pointer; asm mov eax, OFFSET System.@LStrAsg; end;
function GetLStrClr: Pointer; asm mov eax, OFFSET System.@LStrClr; end;
function GetLStrAddRef: Pointer; asm mov eax, OFFSET System.@LStrAddRef; end;

function GetLStrCmp: Pointer; asm mov eax, OFFSET System.@LStrCmp; end;
function GetWStrCmp: Pointer; asm mov eax, OFFSET System.@WStrCmp; end;

initialization
  CodeRedirect(GetLStrLAsg, @_LStrLAsg);
  CodeRedirect(GetLStrAsg, @_LStrAsg);
  CodeRedirect(GetLStrClr, @_LStrClr);
  CodeRedirect(GetLStrAddRef, @_LStrAddRef);

  CodeRedirect(GetWStrSet, @WStrSet);

  OrgLStrCmp := GetActualAddr(GetLStrCmp);
  OrgWStrCmp := GetActualAddr(GetWStrCmp);

  CodeRedirect(OrgLStrCmp, @_LStrCmp_LStrEqual);
  CodeRedirect(OrgWStrCmp, @_WStrCmp_WStrEqual);

end.
