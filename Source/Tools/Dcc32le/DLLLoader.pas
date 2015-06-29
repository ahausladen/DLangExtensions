{*************************************************************************
 *                                                                       *
 * DLL linking is provided by Benjamin Rosseaux, www.0ok.de,             *
 * mailto:benjamin@0ok.de                                                *
 *                                                                       *
 * This DLL Loader code is coyyrighted: (C) 2004, Benjamin Rosseaux      *
 *                                                                       *
 *************************************************************************}

unit DLLLoader;

interface

uses
  Windows, SysUtils, Classes{, JclFileUtils};

const 
  IMPORTED_NAME_OFFSET = $00000002;
  IMAGE_ORDINAL_FLAG32 = $80000000;
  IMAGE_ORDINAL_MASK32 = $0000FFFF;

  RTL_CRITSECT_TYPE = 0;
  RTL_RESOURCE_TYPE = 1;

  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH = 2;
  DLL_THREAD_DETACH = 3;
  DLL_PROCESS_DETACH = 0;

  IMAGE_SizeHeader = 20;

  IMAGE_FILE_RELOCS_STRIPPED = $0001;
  IMAGE_FILE_EXECUTABLE_IMAGE = $0002;
  IMAGE_FILE_LINE_NUMS_STRIPPED = $0004;
  IMAGE_FILE_LOCAL_SYMS_STRIPPED = $0008;
  IMAGE_FILE_AGGRESIVE_WS_TRIM = $0010;
  IMAGE_FILE_BYTES_REVERSED_LO = $0080;
  IMAGE_FILE_32BIT_MACHINE = $0100;
  IMAGE_FILE_DEBUG_STRIPPED = $0200;
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = $0400;
  IMAGE_FILE_NET_RUN_FROM_SWAP = $0800;
  IMAGE_FILE_SYSTEM = $1000;
  IMAGE_FILE_DLL = $2000;
  IMAGE_FILE_UP_SYSTEM_ONLY = $4000;
  IMAGE_FILE_BYTES_REVERSED_HI = $8000;

  IMAGE_FILE_MACHINE_UNKNOWN = 0;
  IMAGE_FILE_MACHINE_I386 = $14C;
  IMAGE_FILE_MACHINE_R3000 = $162;
  IMAGE_FILE_MACHINE_R4000 = $166;
  IMAGE_FILE_MACHINE_R10000 = $168;
  IMAGE_FILE_MACHINE_ALPHA = $184;
  IMAGE_FILE_MACHINE_POWERPC = $1F0;

  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

  IMAGE_SUBSYSTEM_UNKNOWN = 0;
  IMAGE_SUBSYSTEM_NATIVE = 1;
  IMAGE_SUBSYSTEM_WINDOWS_GUI = 2;
  IMAGE_SUBSYSTEM_WINDOWS_CUI = 3;
  IMAGE_SUBSYSTEM_OS2_CUI = 5;
  IMAGE_SUBSYSTEM_POSIX_CUI = 7;
  IMAGE_SUBSYSTEM_RESERVED = 8;

  IMAGE_DIRECTORY_ENTRY_EXPORT = 0;
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1;
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2;
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3;
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4;
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5;
  IMAGE_DIRECTORY_ENTRY_DEBUG = 6;
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7;
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8;
  IMAGE_DIRECTORY_ENTRY_TLS = 9;
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10;
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT = 11;
  IMAGE_DIRECTORY_ENTRY_IAT = 12;

  IMAGE_SIZEOF_SHORT_NAME = 8;

  IMAGE_SCN_TYIMAGE_REG = $00000000;
  IMAGE_SCN_TYIMAGE_DSECT = $00000001;
  IMAGE_SCN_TYIMAGE_NOLOAD = $00000002;
  IMAGE_SCN_TYIMAGE_GROUP = $00000004;
  IMAGE_SCN_TYIMAGE_NO_PAD = $00000008;
  IMAGE_SCN_TYIMAGE_COPY = $00000010;
  IMAGE_SCN_CNT_CODE = $00000020;
  IMAGE_SCN_CNT_INITIALIZED_DATA = $00000040;
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = $00000080;
  IMAGE_SCN_LNK_OTHER = $00000100;
  IMAGE_SCN_LNK_INFO = $00000200;
  IMAGE_SCN_TYIMAGE_OVER = $0000400;
  IMAGE_SCN_LNK_REMOVE = $00000800;
  IMAGE_SCN_LNK_COMDAT = $00001000;
  IMAGE_SCN_MEM_PROTECTED = $00004000;
  IMAGE_SCN_MEM_FARDATA = $00008000;
  IMAGE_SCN_MEM_SYSHEAP = $00010000;
  IMAGE_SCN_MEM_PURGEABLE = $00020000;
  IMAGE_SCN_MEM_16BIT = $00020000;
  IMAGE_SCN_MEM_LOCKED = $00040000;
  IMAGE_SCN_MEM_PRELOAD = $00080000;
  IMAGE_SCN_ALIGN_1BYTES = $00100000;
  IMAGE_SCN_ALIGN_2BYTES = $00200000;
  IMAGE_SCN_ALIGN_4BYTES = $00300000;
  IMAGE_SCN_ALIGN_8BYTES = $00400000;
  IMAGE_SCN_ALIGN_16BYTES = $00500000;
  IMAGE_SCN_ALIGN_32BYTES = $00600000;
  IMAGE_SCN_ALIGN_64BYTES = $00700000;
  IMAGE_SCN_LNK_NRELOC_OVFL = $01000000;
  IMAGE_SCN_MEM_DISCARDABLE = $02000000;
  IMAGE_SCN_MEM_NOT_CACHED = $04000000;
  IMAGE_SCN_MEM_NOT_PAGED = $08000000;
  IMAGE_SCN_MEM_SHARED = $10000000;
  IMAGE_SCN_MEM_EXECUTE = $20000000;
  IMAGE_SCN_MEM_READ = $40000000;
  IMAGE_SCN_MEM_WRITE = LongWord($80000000);

  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGH = 1;
  IMAGE_REL_BASED_LOW = 2;
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_HIGHADJ = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR = 5;
  IMAGE_REL_BASED_SECTION = 6;
  IMAGE_REL_BASED_REL32 = 7;

  IMAGE_REL_BASED_MIPS_JMPADDR16 = 9;
  IMAGE_REL_BASED_IA64_IMM64 = 9;
  IMAGE_REL_BASED_DIR64 = 10;
  IMAGE_REL_BASED_HIGH3ADJ = 11;

  PAGE_NOACCESS = 1;
  PAGE_READONLY = 2;
  PAGE_READWRITE = 4;
  PAGE_WRITECOPY = 8;
  PAGE_EXECUTE = $10;
  PAGE_EXECUTE_READ = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_GUARD = $100;
  PAGE_NOCACHE = $200;
  MEM_COMMIT = $1000;
  MEM_RESERVE = $2000;
  MEM_DECOMMIT = $4000;
  MEM_RELEASE = $8000;
  MEM_FREE = $10000;
  MEM_PRIVATE = $20000;
  MEM_MAPPED = $40000;
  MEM_RESET = $80000;
  MEM_TOP_DOWN = $100000;
  SEC_FILE = $800000;
  SEC_IMAGE = $1000000;
  SEC_RESERVE = $4000000;
  SEC_COMMIT = $8000000;
  SEC_NOCACHE = $10000000;
  MEM_IMAGE = SEC_IMAGE;

type
  PPointer = ^Pointer;

  PLongWord = ^LongWord;
  PPLongWord = ^PLongWord;

  PWORD = ^Word;
  PPWORD = ^PWORD;

  HINST = LongWord;
  HMODULE = HINST;

  PWordArray = ^TWordArray;
  TWordArray = array[0..(2147483647 div SizeOf(Word)) - 1] of Word;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array [0..(2147483647 div SizeOf(LongWord)) - 1] of LongWord;

  PImageDOSHeader = ^TImageDOSHeader;
  TImageDOSHeader = packed record
    Signature: Word;
    PartPag: Word;
    PageCnt: Word;
    ReloCnt: Word;
    HdrSize: Word;
    MinMem: Word;
    MaxMem: Word;
    ReloSS: Word;
    ExeSP: Word;
    ChkSum: Word;
    ExeIP: Word;
    ReloCS: Word;
    TablOff: Word;
    Overlay: Word;
    Reserved: packed array[0..3] of Word;
    OEMID: Word;
    OEMInfo: Word;
    Reserved2: packed array[0..9] of Word;
    LFAOffset: LongWord;
  end;

  TISHMisc = packed record
    case Integer of
      0: (PhysicalAddress: LongWord);
      1: (VirtualSize: LongWord);
  end;

  PImageExportDirectory = ^TImageExportDirectory;
  TImageExportDirectory = packed record
    Characteristics: LongWord;
    TimeDateStamp: LongWord;
    MajorVersion: Word;
    MinorVersion: Word;
    Name: LongWord;
    Base: LongWord;
    NumberOfFunctions: LongWord;
    NumberOfNames: LongWord;
    AddressOfFunctions: PPLongWord;
    AddressOfNames: PPLongWord;
    AddressOfNameOrdinals: PPWORD;
  end;

  PImageSectionHeader = ^TImageSectionHeader;
  TImageSectionHeader = packed record
    Name: packed array[0..IMAGE_SIZEOF_SHORT_NAME - 1] of Byte;
    Misc: TISHMisc;
    VirtualAddress: LongWord;
    SizeOfRawData: LongWord;
    PointerToRawData: LongWord;
    PointerToRelocations: LongWord;
    PointerToLinenumbers: LongWord;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: LongWord;
  end;

  PImageSectionHeaders = ^TImageSectionHeaders;
  TImageSectionHeaders = array[0..(2147483647 div SizeOf(TImageSectionHeader)) - 1] of
    TImageSectionHeader;

  PImageDataDirectory = ^TImageDataDirectory;
  TImageDataDirectory = packed record
    VirtualAddress: LongWord;
    Size: LongWord;
  end;

  PImageFileHeader = ^TImageFileHeader;
  TImageFileHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: LongWord;
    PointerToSymbolTable: LongWord;
    NumberOfSymbols: LongWord;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;

  PImageOptionalHeader = ^TImageOptionalHeader;
  TImageOptionalHeader = packed record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: LongWord;
    SizeOfInitializedData: LongWord;
    SizeOfUninitializedData: LongWord;
    AddressOfEntryPoint: LongWord;
    BaseOfCode: LongWord;
    BaseOfData: LongWord;
    ImageBase: LongWord;
    SectionAlignment: LongWord;
    FileAlignment: LongWord;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: LongWord;
    SizeOfImage: LongWord;
    SizeOfHeaders: LongWord;
    CheckSum: LongWord;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: LongWord;
    SizeOfStackCommit: LongWord;
    SizeOfHeapReserve: LongWord;
    SizeOfHeapCommit: LongWord;
    LoaderFlags: LongWord;
    NumberOfRvaAndSizes: LongWord;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;

  PImageNTHeaders = ^TImageNTHeaders;
  TImageNTHeaders = packed record
    Signature: LongWord;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader;
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    OriginalFirstThunk: LongWord;
    TimeDateStamp: LongWord;
    ForwarderChain: LongWord;
    Name: LongWord;
    FirstThunk: LongWord;
  end;

  PImageBaseRelocation = ^TImageBaseRelocation;
  TImageBaseRelocation = packed record
    VirtualAddress: LongWord;
    SizeOfBlock: LongWord;
  end;

  PImageThunkData = ^TImageThunkData;
  TImageThunkData = packed record
    ForwarderString: LongWord;
    Funktion: LongWord;
    Ordinal: LongWord;
    AddressOfData: LongWord;
  end;

  PSection = ^TSection;
  TSection = packed record
    Base: Pointer;
    RVA: LongWord;
    Size: LongWord;
    Characteristics: LongWord;
  end;

  TSections = array of TSection;

  TDLLEntryProc = function(hinstDLL: HMODULE; dwReason: LongWord; lpvReserved: Pointer): Boolean; stdcall;
  TExeEntryPoint = function: Integer; stdcall;

  TNameOrID = (niName, niID);

  TExternalLibrary = record
    LibraryName: string;
    LibraryHandle: HINST;
  end;

  TExternalLibrarys = array of TExternalLibrary;

  PDLLFunctionImport = ^TDLLFunctionImport;
  TDLLFunctionImport = record
    NameOrID: TNameOrID;
    Name: string;
    ID: Integer;
  end;

  PDLLImport = ^TDLLImport;
  TDLLImport = record
    LibraryName: string;
    LibraryHandle: HINST;
    Entries: array of TDLLFunctionImport;
  end;

  TImports = array of TDLLImport;

  PDLLFunctionExport = ^TDLLFunctionExport;
  TDLLFunctionExport = record
    Name: string;
    Index: Integer;
    FunctionPointer: Pointer;
  end;

  TExports = array of TDLLFunctionExport;

  TExportTreeLink = Pointer;

  PExportTreeNode = ^TExportTreeNode;
  TExportTreeNode = record
    TheChar: Char;
    Link: TExportTreeLink;
    LinkExist: Boolean;
    Prevoius, Next, Up, Down: PExportTreeNode;
  end;

  TExportTree = class(TObject)
  private
    Root: PExportTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Dump;
    function Add(const FunctionName: string; Link: TExportTreeLink): Boolean;
    function Delete(const FunctionName: string): Boolean;
    function Find(const FunctionName: string; var Link: TExportTreeLink): Boolean;
  end;

  TDLLLoader = class(TObject)
  private
    FImageBase: Pointer;
    ImageBaseDelta: Integer;
    DLLProc: TDLLEntryProc;
    ExternalLibraryArray: TExternalLibrarys;
    ImportArray: TImports;
    ExportArray: TExports;
    Sections: TSections;
    ExportTree: TExportTree;
    FFilename: AnsiString;
    FName: AnsiString;
    FFilenameW: WideString;
    FNameW: WideString;
    FLibInitialized: Boolean;
    function FindExternalLibrary(const LibraryName: string): Integer;
    function LoadExternalLibrary(const LibraryName: string): Integer;
    function GetExternalLibraryHandle(const LibraryName: string): HINST;
  protected
    function DoLoadLibrary(const LibraryName: string): HMODULE; virtual;
    function DoFreeLibrary(hModule: HMODULE): Bool; virtual;
    function DoGetProcAddress(hModule: HMODULE; ProcName: PChar): Pointer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Load(Stream: TStream; const AFilename: string; InitLib: Boolean): HMODULE;
    function Unload: Boolean;
    function FindExport(const FunctionName: string): Pointer;
    function FindExportPerIndex(FunctionIndex: Integer): Pointer;
    function CreateExportList: TStrings;

    property ImageBase: Pointer read FImageBase;
    property EntryPoint: TDLLEntryProc read DLLProc;
    property Filename: AnsiString read FFilename;
    property Name: AnsiString read FName;
    property FilenameW: WideString read FFilenameW;
    property NameW: WideString read FNameW;
  end;

  TDLLLoaderClass = class of TDLLLoader;

var
  DllLoaderClass: TDLLLoaderClass = TDLLLoader;
  GlobalMemLibraryList: TList = nil;


function LoadExeMem(Filename: PChar): HMODULE;
function LoadLibraryMem(Filename: PChar): HMODULE;
function FreeLibraryMem(hLib: HMODULE): Bool;
function GetProcAddressMem(hLib: HMODULE; ProcName: PChar): Pointer; stdcall;
function GetModuleFileNameMem(hModule: HINST; lpFilename: PChar; nSize: DWORD): DWORD; stdcall;
function GetModuleHandleMem(lpModuleName: PChar): HMODULE; stdcall;

implementation

function SearchFile(const Filename: string): string;
var
  FilePart: PChar;
begin
  FilePart := nil;
  SetLength(Result, MAX_PATH);
  SetLength(Result, SearchPath(nil, PChar(Filename), nil, Length(Result), Pointer(Result), FilePart));
end;

function LoadExeMem(Filename: PChar): HMODULE;
var
  Loader: TDLLLoader;
  Stream: TStream;
  Name: string;
begin
  Name := SearchFile(Filename);
  Result := 0;
  Loader := DllLoaderClass.Create;
  try
    //Stream := TJclFileMappingStream.Create(Name, fmOpenRead or fmShareDenyWrite);
    Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
    try
      if Loader.Load(Stream, Name, False) <> 0 then
        Result := HMODULE(Loader);
    finally
      Stream.Free;
    end;
  except
    Loader.Free;
  end;
end;

function LoadLibraryMem(Filename: PChar): HMODULE;
var
  Loader: TDLLLoader;
  Stream: TStream;
  Name: string;
begin
  Name := SearchFile(Filename);
  Result := 0;
  Loader := DllLoaderClass.Create;
  try
    //Stream := TJclFileMappingStream.Create(Name, fmOpenRead or fmShareDenyWrite);
    Stream := TFileStream.Create(Name, fmOpenRead or fmShareDenyWrite);
    try
      if Loader.Load(Stream, Name, True) <> 0 then
        Result := HMODULE(Loader);
    finally
      Stream.Free;
    end;
  except
    Loader.Free;
  end;
end;

function FreeLibraryMem(hLib: HMODULE): Bool;
begin
  if hLib <> 0 then
  begin
    try
      TDLLLoader(hLib).Free;
      Result := True;
    except
      Result := False;
    end;
  end
  else
    Result := False;
end;

function GetProcAddressMem(hLib: HMODULE; ProcName: PChar): Pointer;
begin
  if (hLib <> 0) and (ProcName <> nil) then
  begin
    if (GlobalMemLibraryList = nil) or (GlobalMemLibraryList.IndexOf(Pointer(hLib)) = -1) then
      Result := GetProcAddress(hLib, ProcName)
    else
    begin
      try
        if (Cardinal(ProcName) and not IMAGE_ORDINAL_MASK32) <> 0 then
          Result := TDLLLoader(hLib).FindExport(ProcName)
        else
          Result := TDLLLoader(hLib).FindExportPerIndex(Integer(ProcName));
      except
        Result := nil;
      end;
    end;
  end
  else
    Result := nil;
end;

function GetModuleFileNameMem(hModule: HINST; lpFilename: PChar; nSize: DWORD): DWORD;
begin
  if hModule <> 0 then
  begin
    if (GlobalMemLibraryList = nil) or (GlobalMemLibraryList.IndexOf(Pointer(hModule)) = -1) then
      Result := GetModuleFileName(hModule, lpFilename, nSize)
    else
    begin
      try
        SetLastError(NOERROR);
        if lpFilename <> nil then
          StrLCopy(lpFilename, PChar(TDLLLoader(hModule).Filename), nSize);
        Result := Length(TDLLLoader(hModule).Filename);
        if Result > nSize then
          SetLastError(ERROR_INSUFFICIENT_BUFFER);
      except
        Result := 0;
      end;
    end;
  end
  else
    Result := 0;
end;

function GetModuleHandleMem(lpModuleName: PChar): HMODULE;
var
  I: Integer;
  ModuleName: string;
begin
  Result := GetModuleHandle(lpModuleName);
  if (Result = 0) and (lpModuleName <> nil) and (GlobalMemLibraryList <> nil) then
  begin
    ModuleName := lpModuleName;
    try
      for I := 0 to GlobalMemLibraryList.Count - 1 do
      begin
        Result := HMODULE(GlobalMemLibraryList[i]);
        if AnsiSameText(ExtractFileName(TDLLLoader(Result).Filename), ModuleName) then
          Exit;
      end;
    except
      on E: EExternalException do
        ;
    end;
  end;
end;

function CreateExportTreeNode(AChar: Char): PExportTreeNode;
begin
  GetMem(Result, SizeOf(TExportTreeNode));
  Result^.TheChar := AChar;
  Result^.Link := nil;
  Result^.LinkExist := False;
  Result^.Prevoius := nil;
  Result^.Next := nil;
  Result^.Up := nil;
  Result^.Down := nil;
end;

procedure DestroyExportTreeNode(Node: PExportTreeNode);
begin
  if Assigned(Node) then
  begin
    DestroyExportTreeNode(Node^.Next);
    DestroyExportTreeNode(Node^.Down);
    FreeMem(Node);
  end;
end;

constructor TExportTree.Create;
begin
  inherited Create;
  Root := nil;
end;

destructor TExportTree.Destroy;
begin
  DestroyExportTreeNode(Root);
  inherited Destroy;
end;

procedure TExportTree.Dump;
var
  Ident: Integer;

  procedure DumpNode(Node: PExportTreeNode);
  var
    SubNode: PExportTreeNode;
    IdentCounter, IdentOld: Integer;
  begin
    for IdentCounter := 1 to Ident do
      Write(' ');
    Write(Node^.TheChar);
    IdentOld := Ident;
    SubNode := Node^.Next;
    while Assigned(SubNode) do
    begin
      Write(SubNode.TheChar);
      if not Assigned(SubNode^.Next) then
        Break;
      Inc(Ident);
      SubNode := SubNode^.Next;
    end;
    WriteLn;
    Inc(Ident);
    while Assigned(SubNode) and (SubNode <> Node) do
    begin
      if Assigned(SubNode^.Down) then
        DumpNode(SubNode^.Down);
      SubNode := SubNode^.Prevoius;
      Dec(Ident);
    end;
    Ident := IdentOld;
    if Assigned(Node^.Down) then
      DumpNode(Node^.Down);
  end;

begin
  Ident := 0;
  DumpNode(Root);
end;

function TExportTree.Add(const FunctionName: string; Link: TExportTreeLink): Boolean;
var
  StringLength, Position, PositionCounter: Integer;
  NewNode, LastNode, Node: PExportTreeNode;
  StringChar, NodeChar: Char;
begin
  Result := False;
  StringLength := Length(FunctionName);
  if StringLength > 0 then
  begin
    LastNode := nil;
    Node := Root;
    for Position := 1 to StringLength do
    begin
      StringChar := FunctionName[Position];
      if Assigned(Node) then
      begin
        NodeChar := Node^.TheChar;
        if NodeChar = StringChar then
        begin
          LastNode := Node;
          Node := Node^.Next;
        end
        else
        begin
          while (NodeChar < StringChar) and Assigned(Node^.Down) do
          begin
            Node := Node^.Down;
            NodeChar := Node^.TheChar;
          end;
          if NodeChar = StringChar then
          begin
            LastNode := Node;
            Node := Node^.Next;
          end
          else
          begin
            NewNode := CreateExportTreeNode(StringChar);
            if NodeChar < StringChar then
            begin
              NewNode^.Down := Node^.Down;
              NewNode^.Up := Node;
              if Assigned(NewNode^.Down) then
                NewNode^.Down^.Up := NewNode;
              NewNode^.Prevoius := Node^.Prevoius;
              Node^.Down := NewNode;
            end
            else if NodeChar > StringChar then
            begin
              NewNode^.Down := Node;
              NewNode^.Up := Node^.Up;
              if Assigned(NewNode^.Up) then
                NewNode^.Up^.Down := NewNode;
              NewNode^.Prevoius := Node^.Prevoius;
              if not Assigned(NewNode^.Up) then
              begin
                if Assigned(NewNode^.Prevoius) then
                  NewNode^.Prevoius^.Next := NewNode
                else
                  Root := NewNode;
              end;
              Node^.Up := NewNode;
            end;
            LastNode := NewNode;
            Node := LastNode^.Next;
          end;
        end;
      end
      else
      begin
        for PositionCounter := Position to StringLength do
        begin
          NewNode := CreateExportTreeNode(FunctionName[PositionCounter]);
          if Assigned(LastNode) then
          begin
            NewNode^.Prevoius := LastNode;
            LastNode^.Next := NewNode;
            LastNode := LastNode^.Next;
          end
          else
          begin
            if not Assigned(Root) then
            begin
              Root := NewNode;
              LastNode := Root;
            end;
          end;
        end;
        Break;
      end;
    end;
    if Assigned(LastNode) then
    begin
      if not LastNode^.LinkExist then
      begin
        LastNode^.Link := Link;
        LastNode^.LinkExist := True;
        Result := True;
      end;
    end;
  end;
end;

function TExportTree.Delete(const FunctionName: string): Boolean;
var
  StringLength, Position: Integer;
  Node: PExportTreeNode;
  StringChar, NodeChar: Char;
begin
  Result := False;
  StringLength := Length(FunctionName);
  if StringLength > 0 then
  begin
    Node := Root;
    for Position := 1 to StringLength do
    begin
      StringChar := FunctionName[Position];
      if Assigned(Node) then
      begin
        NodeChar := Node^.TheChar;
        while (NodeChar <> StringChar) and Assigned(Node^.Down) do
        begin
          Node := Node^.Down;
          NodeChar := Node^.TheChar;
        end;
        if NodeChar = StringChar then
        begin
          if (Position = StringLength) and Node^.LinkExist then
          begin
            Node^.LinkExist := False;
            Result := True;
            Break;
          end;
          Node := Node^.Next;
        end;
      end
      else
        Break;
    end;
  end;
end;

function TExportTree.Find(const FunctionName: string; var Link: TExportTreeLink): Boolean;
var
  StringLength, Position: Integer;
  Node: PExportTreeNode;
  StringChar, NodeChar: Char;
begin
  Result := False;
  StringLength := Length(FunctionName);
  if StringLength > 0 then
  begin
    Node := Root;
    for Position := 1 to StringLength do
    begin
      StringChar := FunctionName[Position];
      if Assigned(Node) then
      begin
        NodeChar := Node^.TheChar;
        while (NodeChar <> StringChar) and Assigned(Node^.Down) do
        begin
          Node := Node^.Down;
          NodeChar := Node^.TheChar;
        end;
        if NodeChar = StringChar then
        begin
          if (Position = StringLength) and Node^.LinkExist then
          begin
            Link := Node^.Link;
            Result := True;
            Break;
          end;
          Node := Node^.Next;
        end;
      end 
      else 
      begin
        Break;
      end;
    end;
  end;
end;

constructor TDLLLoader.Create;
begin
  inherited Create;
  FImageBase := nil;
  DLLProc := nil;
  ExternalLibraryArray := nil;
  ImportArray := nil;
  ExportArray := nil;
  Sections := nil;
  ExportTree := nil;
  if GlobalMemLibraryList = nil then
    GlobalMemLibraryList := TList.Create;
  GlobalMemLibraryList.Add(Self);
end;

destructor TDLLLoader.Destroy;
begin
  GlobalMemLibraryList.Extract(Self);
  if GlobalMemLibraryList.Count = 0 then
    FreeAndNil(GlobalMemLibraryList);

  if @DLLProc <> nil then
    Unload;
  if Assigned(ExportTree) then
    ExportTree.Free;
  inherited Destroy;
end;

function TDLLLoader.DoLoadLibrary(const LibraryName: string): HMODULE;
begin
  Result := LoadLibrary(PChar(LibraryName));
end;

function TDLLLoader.DoFreeLibrary(hModule: HMODULE): Bool;
begin
  Result := FreeLibrary(hModule);
end;

function TDLLLoader.DoGetProcAddress(hModule: HMODULE; ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(hModule, ProcName);
end;

function TDLLLoader.FindExternalLibrary(const LibraryName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(ExternalLibraryArray) - 1 do
  begin
    if ExternalLibraryArray[I].LibraryName = LibraryName then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TDLLLoader.LoadExternalLibrary(const LibraryName: string): Integer;
begin
  Result := FindExternalLibrary(LibraryName);
  if Result < 0 then
  begin
    Result := Length(ExternalLibraryArray);
    SetLength(ExternalLibraryArray, Length(ExternalLibraryArray) + 1);
    ExternalLibraryArray[Result].LibraryName := LibraryName;
    ExternalLibraryArray[Result].LibraryHandle := DoLoadLibrary(LibraryName);
  end;
end;

function TDLLLoader.GetExternalLibraryHandle(const LibraryName: string): LongWord;
var 
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(ExternalLibraryArray) - 1 do
  begin
    if ExternalLibraryArray[I].LibraryName = LibraryName then 
    begin
      Result := ExternalLibraryArray[I].LibraryHandle;
      Exit;
    end;
  end;
end;

function TDLLLoader.Load(Stream: TStream; const AFilename: string; InitLib: Boolean): HMODULE;
var
  ImageDOSHeader: TImageDOSHeader;
  ImageNTHeaders: TImageNTHeaders;
  OldProtect: LongWord;

  function ConvertPointer(RVA: LongWord): Pointer;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Length(Sections) - 1 do
    begin
      if (RVA < (Sections[I].RVA + Sections[I].Size)) and (RVA >= Sections[I].RVA) then
      begin
        Result := Pointer(LongWord((RVA - LongWord(Sections[I].RVA)) + LongWord(Sections[I].Base)));
        Exit;
      end;
    end;
  end;

  function ReadImageHeaders: Boolean;
  begin
    Result := False;
    if Stream.Size > 0 then 
    begin
      FillChar(ImageNTHeaders, SizeOf(TImageNTHeaders), #0);
      if Stream.Read(ImageDOSHeader, SizeOf(TImageDOSHeader)) <> SizeOf(TImageDOSHeader) then
        Exit;
      if ImageDOSHeader.Signature <> $5A4D then
        Exit;
      if Stream.Seek(ImageDOSHeader.LFAOffset, soFromBeginning) <>
        Longint(ImageDOSHeader.LFAOffset) then Exit;
      if Stream.Read(ImageNTHeaders.Signature, SizeOf(LongWord)) <> SizeOf(LongWord) then
        Exit;
      if ImageNTHeaders.Signature <> $00004550 then
        Exit;
      if Stream.Read(ImageNTHeaders.FileHeader, SizeOf(TImageFileHeader)) <> SizeOf(TImageFileHeader) then
        Exit;
      if ImageNTHeaders.FileHeader.Machine <> $14C then
        Exit;
      with ImageNTHeaders do
        if Stream.Read(OptionalHeader, FileHeader.SizeOfOptionalHeader) <> FileHeader.SizeOfOptionalHeader then
          Exit;
      Result := True;
    end;
  end;

  function InitializeImage: Boolean;
  var
    SectionBase: Pointer;
    OldPosition: Integer;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
    begin
      FImageBase := VirtualAlloc(nil, ImageNTHeaders.OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_NOACCESS);
      ImageBaseDelta := LongWord(ImageBase) - ImageNTHeaders.OptionalHeader.ImageBase;
      SectionBase := VirtualAlloc(ImageBase, ImageNTHeaders.OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE);
      OldPosition := Stream.Position;
      Stream.Seek(0, soFromBeginning);
      Stream.Read(SectionBase^, ImageNTHeaders.OptionalHeader.SizeOfHeaders);
      VirtualProtect(SectionBase, ImageNTHeaders.OptionalHeader.SizeOfHeaders, PAGE_READONLY, OldProtect);
      Stream.Seek(OldPosition, soFromBeginning);
      Result := True;
    end;
  end;

  function ReadSections: Boolean;
  var
    I: Integer;
    Section: TImageSectionHeader;
    SectionHeaders: PImageSectionHeaders;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
    begin
      GetMem(SectionHeaders, ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader));
      with ImageNTHeaders.FileHeader do
      if Stream.Read(SectionHeaders^, (NumberOfSections * SizeOf(TImageSectionHeader))) <> (NumberOfSections *  SizeOf(TImageSectionHeader)) then
      begin
        FreeMem(SectionHeaders);
        Exit;
      end;
      SetLength(Sections, ImageNTHeaders.FileHeader.NumberOfSections);
      for I := 0 to ImageNTHeaders.FileHeader.NumberOfSections - 1 do
      begin
        Section := SectionHeaders^[I];
        Sections[I].RVA := Section.VirtualAddress;
        Sections[I].Size := Section.SizeOfRawData;
        if Sections[I].Size < Section.Misc.VirtualSize then
          Sections[I].Size := Section.Misc.VirtualSize;
        Sections[I].Characteristics := Section.Characteristics;
        Sections[I].Base := VirtualAlloc(Pointer(LongWord(Sections[I].RVA + LongWord(ImageBase))),
                                         Sections[I].Size, MEM_COMMIT, PAGE_READWRITE);
        FillChar(Sections[I].Base^, Sections[I].Size, #0);
        if Section.PointerToRawData <> 0 then
        begin
          Stream.Seek(Section.PointerToRawData, soFromBeginning);
          if Stream.Read(Sections[I].Base^, Section.SizeOfRawData) <> Longint(Section.SizeOfRawData) then
          begin
            FreeMem(SectionHeaders);
            Exit;
          end;
        end;
      end;
      FreeMem(SectionHeaders);
      Result := True;
    end;
  end;

  function ProcessRelocations: Boolean;
  var
    Relocations: PChar;
    Position: LongWord;
    BaseRelocation: PImageBaseRelocation;
    Base: Pointer;
    NumberOfRelocations: LongWord;
    Relocation: PWordArray;
    RelocationCounter: LONGINT;
    RelocationPointer: Pointer;
    RelocationType: LongWord;
  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress <> 0 then
    begin
      Result := False;
      Relocations := ConvertPointer(ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
      Position := 0;
      while Assigned(Relocations) and
           (Position < ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) do
      begin
        BaseRelocation := PImageBaseRelocation(Relocations);
        Base := ConvertPointer(BaseRelocation^.VirtualAddress);
        if not Assigned(Base) then
          Exit;
        NumberOfRelocations := (BaseRelocation^.SizeOfBlock - SizeOf(TImageBaseRelocation)) div SizeOf(Word);
        Relocation := Pointer(LongWord(LongWord(BaseRelocation) + SizeOf(TImageBaseRelocation)));
        for RelocationCounter := 0 to NumberOfRelocations - 1 do
        begin
          RelocationPointer := Pointer(LongWord(LongWord(Base) + (Relocation^[RelocationCounter] and $FFF)));
          RelocationType := Relocation^[RelocationCounter] shr 12;
          case RelocationType of
            IMAGE_REL_BASED_ABSOLUTE:
              ;

            IMAGE_REL_BASED_HIGH:
              begin
                PWORD(RelocationPointer)^ :=
                  (LongWord(((LongWord(PWORD(RelocationPointer)^ + LongWord(ImageBase)
                  - ImageNTHeaders.OptionalHeader.ImageBase)))) shr 16) and $FFFF;
              end;
            IMAGE_REL_BASED_LOW:
              begin
                PWORD(RelocationPointer)^ :=
                  LongWord(((LongWord(PWORD(RelocationPointer)^ + LongWord(ImageBase) -
                  ImageNTHeaders.OptionalHeader.ImageBase)))) and $FFFF;
              end;
            IMAGE_REL_BASED_HIGHLOW:
              begin
                PPointer(RelocationPointer)^ :=
                  Pointer((LongWord(LongWord(PPointer(RelocationPointer)^) + LongWord(ImageBase) -
                  ImageNTHeaders.OptionalHeader.ImageBase)));
              end;
            IMAGE_REL_BASED_HIGHADJ:
              begin
                // ???
              end;
            IMAGE_REL_BASED_MIPS_JMPADDR:
              begin
                // Only for MIPS CPUs ;)
              end;
          end;
        end;
        Relocations := Pointer(LongWord(LongWord(Relocations) + BaseRelocation^.SizeOfBlock));
        Inc(Position, BaseRelocation^.SizeOfBlock);
      end;
    end;
    Result := True;
  end;

  function ProcessImports: Boolean;
  var
    ImportDescriptor: PImageImportDescriptor;
    ThunkData: PLongWord;
    Name: PChar;
    DLLImport: PDLLImport;
    DLLFunctionImport: PDLLFunctionImport;
    FunctionPointer: Pointer;
  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress <> 0 then
    begin
      ImportDescriptor := ConvertPointer(ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
      if Assigned(ImportDescriptor) then
      begin
        SetLength(ImportArray, 0);
        while ImportDescriptor^.Name <> 0 do
        begin
          Name := ConvertPointer(ImportDescriptor^.Name);
          SetLength(ImportArray, Length(ImportArray) + 1);
          LoadExternalLibrary(Name);
          DLLImport := @ImportArray[Length(ImportArray) - 1];
          DLLImport^.LibraryName := Name;
          DLLImport^.LibraryHandle := GetExternalLibraryHandle(Name);
          DLLImport^.Entries := nil;
          if ImportDescriptor^.TimeDateStamp = 0 then
            ThunkData := ConvertPointer(ImportDescriptor^.FirstThunk)
          else
            ThunkData := ConvertPointer(ImportDescriptor^.OriginalFirstThunk);
          while ThunkData^ <> 0 do
          begin
            SetLength(DLLImport^.Entries, Length(DLLImport^.Entries) + 1);
            DLLFunctionImport := @DLLImport^.Entries[Length(DLLImport^.Entries) - 1];
            if (ThunkData^ and IMAGE_ORDINAL_FLAG32) <> 0 then
            begin
              DLLFunctionImport^.NameOrID := niID;
              DLLFunctionImport^.ID := ThunkData^ and IMAGE_ORDINAL_MASK32;
              DLLFunctionImport^.Name := '';
              FunctionPointer := DoGetProcAddress(DLLImport^.LibraryHandle,
                PChar(ThunkData^ and IMAGE_ORDINAL_MASK32));
            end
            else
            begin
              Name := ConvertPointer(LongWord(ThunkData^) + IMPORTED_NAME_OFFSET);
              DLLFunctionImport^.NameOrID := niName;
              DLLFunctionImport^.ID := 0;
              DLLFunctionImport^.Name := Name;
              FunctionPointer := DoGetProcAddress(DLLImport^.LibraryHandle, Name);
            end;
            PPointer(Thunkdata)^ := FunctionPointer;
            Inc(ThunkData);
          end;
          Inc(ImportDescriptor);
        end;
      end;
    end;
    Result := True;
  end;

  function ProtectSections: Boolean;
  var
    I: Integer;
    Characteristics: LongWord;
    Flags: LongWord;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
    begin
      for I := 0 to ImageNTHeaders.FileHeader.NumberOfSections - 1 do
      begin
        Characteristics := Sections[I].Characteristics;
        Flags := 0;
        if (Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
        begin
          if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
          begin
            if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
              Flags := Flags or PAGE_EXECUTE_READWRITE
            else
              Flags := Flags or PAGE_EXECUTE_READ;
          end
          else if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
            Flags := Flags or PAGE_EXECUTE_WRITECOPY
          else
            Flags := Flags or PAGE_EXECUTE;
        end
        else if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
        begin
          if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
            Flags := Flags or PAGE_READWRITE
          else
            Flags := Flags or PAGE_READONLY;
        end
        else if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
          Flags := Flags or PAGE_WRITECOPY
        else
          Flags := Flags or PAGE_NOACCESS;
        if (Characteristics and IMAGE_SCN_MEM_NOT_CACHED) <> 0 then
          Flags := Flags or PAGE_NOCACHE;
        VirtualProtect(Sections[I].Base, Sections[I].Size, Flags, OldProtect);
      end;
      Result := True;
    end;
  end;

  function InitializeLibrary: Boolean;
  begin
    @DLLProc := ConvertPointer(ImageNTHeaders.OptionalHeader.AddressOfEntryPoint);
    FLibInitialized := InitLib;
    if InitLib then
      Result := DLLProc(Cardinal(ImageBase), DLL_PROCESS_ATTACH, nil)
    else
      Result := Assigned(DLLProc);
  end;

  function ProcessExports: Boolean;
  var
    I: Integer;
    ExportDirectory: PImageExportDirectory;
    ExportDirectorySize: LongWord;
    FunctionNamePointer: Pointer;
    FunctionName: PChar;
    FunctionIndexPointer: Pointer;
    FunctionIndex: LongWord;
    FunctionPointer: Pointer;
    ForwarderCharPointer: PChar;
    ForwarderString: string;
    ForwarderLibrary: string;
    ForwarderLibraryHandle: HINST;

    function ParseStringToNumber(const AString: string): LongWord;
    var
      CharCounter: Integer;
    begin
      Result := 0;
      for CharCounter := 0 to Length(AString) - 1 do
      begin
        if AString[CharCounter] in ['0'..'9'] then
          Result := (Result * 10) + Byte(Byte(AString[CharCounter]) - Byte('0'))
        else
          Exit;
      end;
    end;

  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress <> 0 then
    begin
      ExportTree := TExportTree.Create;
      ExportDirectory := ConvertPointer(ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
      if Assigned(ExportDirectory) then
      begin
        ExportDirectorySize := ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
        SetLength(ExportArray, ExportDirectory^.NumberOfNames);
        for I := 0 to ExportDirectory^.NumberOfNames - 1 do
        begin
          FunctionNamePointer := ConvertPointer(LongWord(ExportDirectory^.AddressOfNames));
          FunctionNamePointer := ConvertPointer(PLongWordArray(FunctionNamePointer)^[I]);
          FunctionName := FunctionNamePointer;
          FunctionIndexPointer := ConvertPointer(LongWord(ExportDirectory^.AddressOfNameOrdinals));
          FunctionIndex := PWordArray(FunctionIndexPointer)^[I];
          FunctionPointer := ConvertPointer(LongWord(ExportDirectory^.AddressOfFunctions));
          FunctionPointer := ConvertPointer(PLongWordArray(FunctionPointer)^[FunctionIndex]);
          ExportArray[I].Name := FunctionName;
          ExportArray[I].Index := FunctionIndex;
          if (LongWord(ExportDirectory) < LongWord(FunctionPointer)) and
             (LongWord(FunctionPointer) < (LongWord(ExportDirectory) + ExportDirectorySize)) then
          begin
            ForwarderCharPointer := FunctionPointer;
            ForwarderString := ForwarderCharPointer;
            while ForwarderCharPointer^ <> '.' do
              Inc(ForwarderCharPointer);
            ForwarderLibrary := Copy(ForwarderString, 1, Pos('.', ForwarderString) - 1);
            LoadExternalLibrary(ForwarderLibrary);
            ForwarderLibraryHandle := GetExternalLibraryHandle(ForwarderLibrary);
            if ForwarderCharPointer^ = '#' then
            begin
              Inc(ForwarderCharPointer);
              ForwarderString := ForwarderCharPointer;
              ForwarderCharPointer := ConvertPointer(ParseStringToNumber(ForwarderString));
              ForwarderString := ForwarderCharPointer;
            end
            else
            begin
              ForwarderString := ForwarderCharPointer;
              ExportArray[I].FunctionPointer := DoGetProcAddress(ForwarderLibraryHandle, PChar(ForwarderString));
            end;
          end
          else
            ExportArray[I].FunctionPointer := FunctionPointer;
          ExportTree.Add(ExportArray[I].Name, ExportArray[I].FunctionPointer);
        end
      end;
    end;
    Result := True;
  end;

begin
  Result := 0;
  if Assigned(Stream) then
  begin
    FFilename := AFilename;
    //Stream.Seek(0, soFromBeginning);
    if (Stream.Size > 0) and
       ReadImageHeaders and
       InitializeImage and
       ReadSections and
       ProcessRelocations and
       ProcessImports and
       ProtectSections and
       InitializeLibrary and
       ProcessExports then
    begin
      Result := THandle(ImageBase);
    end
    else
      FFilename := '';
    FName := ExtractFileName(FFilename);
    FFilenameW := FFilename;
    FNameW := FName;
  end;
end;

function TDLLLoader.Unload: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if (@DLLProc <> nil) and FLibInitialized then
    DLLProc(LongWord(ImageBase), DLL_PROCESS_DETACH, nil);

  for I := 0 to Length(Sections) - 1 do
    if Assigned(Sections[I].Base) then
      VirtualFree(Sections[I].Base, 0, MEM_RELEASE);
  SetLength(Sections, 0);

  for I := 0 to Length(ExternalLibraryArray) - 1 do
  begin
    ExternalLibraryArray[I].LibraryName := '';
    DoFreeLibrary(ExternalLibraryArray[I].LibraryHandle);
  end;
  SetLength(ExternalLibraryArray, 0);

  for I := 0 to Length(ImportArray) - 1 do
  begin
    for J := 0 to Length(ImportArray[I].Entries) - 1 do
      ImportArray[I].Entries[J].Name := '';
    SetLength(ImportArray[I].Entries, 0);
  end;
  SetLength(ImportArray, 0);

  for I := 0 to Length(ExportArray) - 1 do
    ExportArray[I].Name := '';
  SetLength(ExportArray, 0);

  VirtualFree(ImageBase, 0, MEM_RELEASE);
  if Assigned(ExportTree) then
  begin
    ExportTree.Free;
    ExportTree := nil;
  end;
end;

function TDLLLoader.FindExport(const FunctionName: string): Pointer;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(ExportTree) then
    ExportTree.Find(FunctionName, Result)
  else
  begin
    for I := 0 to Length(ExportArray) - 1 do
    begin
      if ExportArray[I].Name = FunctionName then
      begin
        Result := ExportArray[I].FunctionPointer;
        Exit;
      end;
    end;
  end;
end;

function TDLLLoader.FindExportPerIndex(FunctionIndex: Integer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(ExportArray) - 1 do
  begin
    if ExportArray[I].Index = FunctionIndex then
    begin
      Result := ExportArray[I].FunctionPointer;
      Exit;
    end;
  end;
end;

function TDLLLoader.CreateExportList: TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  try
    for I := 0 to Length(ExportArray) - 1 do
      Result.Add(ExportArray[I].Name);
    TStringList(Result).Sort;
  except
    Result.Free;
    raise;
  end;
end;

end.
