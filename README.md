# DLangExtensions
DLangExtensions - Delphi Preprocessor IDE plugin - Legacy code base

This repository contains the legacy code base of the DLangExtensions Delphi IDE plugin as it was in 2009 when the project was discontinued.


How to install
--------------
1. Copy **DLangExtensions.dll** and the **CompileInterceptorW.dll** to **$(BDS)\bin**
2. Optional: Copy **DLangExt.exe** to **$(BDS)\bin**
3. Open **Regedit** and browse to the **HKCU\Software\CodeGear\BDS\6.0\Experts** key (you must create the **Experts** sub-key if it doesn't exist)
4. Add a new String-Value to the registry key with the name **DLangExtensions** and the value **Your BDS Directory\bin\DLangExtensions.dll**.


Prerequisites
-------------
The **CompileInterceptorW.dll** (2009 or newer) or **CompileInterceptor.dll** (5-2007) from the **CompileInterceptor** directory must be copied to the **Bin** directory.


Available Language Extensions
-----------------------------
- **File Macros**  
  Some macros to get the current file name, line, time and project name.
- **case-string-of**  
  Extends the case-of by adding the ability to use strings.
- **Extended for-in loop**  
  Extends the for-in loop with inline variable declaration, predefined enumerators and more.
- **Multiline Strings**  
  Adds the ability to use multiline strings without fighing with line breaks.
- **Unicode Switch**  
  Adds the ability change the meaning of "string", "Char" and "PChar" back to ANSI types.
