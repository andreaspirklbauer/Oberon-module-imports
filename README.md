# Oberon-module-imports
Improved handling of import and export for the Project Oberon 2013 (www.projectoberon.com) system.

Note: This implementation changes the symbol file format!

-------------------------------------------------------------------------------------

**Instructions for updating your Oberon system**

If you use the [**Extended Oberon**](http://github.com/andreaspirklbauer/Oberon-extended) system, the improvements of this repository are already implemented (except Variants 1 and 2). If you use the [**Project Oberon 2013**](http://www.projectoberon.com) system, you can update your system as follows:

* Load the files from [**Sources/FPGAOberon2013**](Sources/FPGAOberon2013) onto your Oberon system
* Rebuild the compiler and linker (ORS, ORB, ORG, ORP, ORL), see [**Oberon-building-tools**](http://github.com/andreaspirklbauer/Oberon-building-tools) for details
* Unload the old compiler and linker from memory using *System.Free* (ORS, ORB, ORG, ORP, ORL)
* Recompile the entire Oberon system (*and* the compiler again) using the new version of the compiler
* Execute *ORL.Link Modules* to generate a pre-linked binary file of the Oberon boot file (inner core)
* Execute *ORL.Load Modules.bin* to load the Oberon boot file onto the boot area of the local disk
* Restart your Oberon system

-------------------------------------------------------------------------------------

**Improvements made**

1. Eliminate the "fixup" technique for types during export and subsequent import
2. Propagate imported export numbers of type descriptor addresses to client modules
3. Handle alias type names among imported modules correctly
4. Allow reusing the original module name if a module has been imported under a module alias name
5. Allow re-imports to co-exist with module alias names and globally declared identifiers
6. Allow an explicit import after previous re-imports of types of the same module
7. Allow importing any number of modules
8. Detect invalid record field exports, ensure the array base type is exported if an array is exported

-------------------------------------------------------------------------------------

**Documentation:** [**Streamlining symbol files in Oberon**](Documentation/Streamlining-symbol-files-in-Oberon.pdf)


**Test suite:** [**Sources/TestImport**](Sources/TestImport). See the files *TestImport.Tool* and *TestImport.Mod*

-------------------------------------------------------------------------------------

**Variants** (presented in separate subfolders):

* *Variant 1:* Use a global module table 'modtab' to optimize writing module objects
* *Variant 2:* Modify the data structure for linking record base types in order to simplify the code

-------------------------------------------------------------------------------------

**APPENDIX**

-------------------------------------------------------------------------------------

**Implementing improvements 1-8**

**ORB.Mod**

```diff
--- FPGAOberon2013/ORB.Mod	2023-10-11 12:57:57
+++ Oberon-module-imports/Sources/FPGAOberon2013/ORB.Mod	2023-10-30 22:40:18
@@ -1,4 +1,4 @@
-MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 5.3.2019  in Oberon-07*)
+MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 5.3.2019  in Oberon-07 / AP 1.11.23*)
   IMPORT Files, ORS;
   (*Definition of data types Object and Type, which together form the data structure
     called "symbol table". Contains procedures for creation of Objects, and for search:
@@ -34,7 +34,7 @@
     ModDesc* = RECORD (ObjDesc) orgname*: ORS.Ident END ;
 
     TypeDesc* = RECORD
-      form*, ref*, mno*: INTEGER;  (*ref is only used for import/export*)
+      form*, mno*, ref, orgref: INTEGER;  (*ref and orgref are only used for import/export*)
       nofpar*: INTEGER;  (*for procedures, extension level for records*)
       len*: LONGINT;  (*for arrays, len < 0 => open array; for records: adr of descriptor*)
       dsc*, typobj*: Object;
@@ -67,11 +67,12 @@
     intType*, realType*, setType*, nilType*, noType*, strType*: Type;
     nofmod, Ref: INTEGER;
     typtab: ARRAY maxTypTab OF Type;
+    self: ORS.Ident;  (*name of module being compiled*)
 
   PROCEDURE NewObj*(VAR obj: Object; id: ORS.Ident; class: INTEGER);  (*insert new Object with name id*)
     VAR new, x: Object;
   BEGIN x := topScope;
-    WHILE (x.next # NIL) & (x.next.name # id) DO x := x.next END ;
+    WHILE (x.next # NIL) & ((x.next.name # id) OR (x.next.class = Mod) & ~x.next.rdo) DO x := x.next END ;
     IF x.next = NIL THEN
       NEW(new); new.name := id; new.class := class; new.next := NIL; new.rdo := FALSE; new.dsc := NIL;
       x.next := new; obj := new
@@ -83,7 +84,7 @@
     VAR s, x: Object;
   BEGIN s := topScope;
     REPEAT x := s.next;
-      WHILE (x # NIL) & (x.name # ORS.id) DO x := x.next END ;
+      WHILE (x # NIL) & ((x.name # ORS.id) OR (x.class = Mod) & ~x.rdo) DO x := x.next END ;
       s := s.dsc
     UNTIL (x # NIL) OR (s = NIL);
     RETURN x
@@ -91,15 +92,8 @@
 
   PROCEDURE thisimport*(mod: Object): Object;
     VAR obj: Object;
-  BEGIN
-    IF mod.rdo THEN
-      IF mod.name[0] # 0X THEN
-        obj := mod.dsc;
-        WHILE (obj # NIL) & (obj.name # ORS.id) DO obj := obj.next END
-      ELSE obj := NIL
-      END
-    ELSE obj := NIL
-    END ;
+  BEGIN (*mod.rdo*) obj := mod.dsc;
+    WHILE (obj # NIL) & (obj.name # ORS.id) DO obj := obj.next END ;
     RETURN obj
   END thisimport;
 
@@ -131,23 +125,26 @@
 
   PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
     VAR mod: Module; obj, obj1: Object;
-  BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
-    WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
-    IF obj = NIL THEN  (*new module, search for alias*)
-      obj := topScope.next;
-      WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
+  BEGIN obj1 := topScope;
+    IF decl THEN obj := obj1.next;  (*search for alias*)
+      WHILE (obj # NIL) & ((obj.name # name) OR ~obj.rdo) DO obj := obj.next END
+    ELSE obj := NIL
+    END ;
+    IF obj = NIL THEN obj := obj1.next;  (*search for module*)
+      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
       IF obj = NIL THEN (*insert new module*)
-        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
+        IF orgname = self THEN ORS.Mark("recursive import not allowed") END ;
+        NEW(mod); mod.class := Mod; mod.rdo := decl;
         mod.name := name; mod.orgname := orgname; mod.val := key;
-        mod.lev := nofmod; INC(nofmod); mod.dsc := NIL; mod.next := NIL;
-        IF decl THEN mod.type := noType ELSE mod.type := nilType END ;
+        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
         obj1.next := mod; obj := mod
-      ELSIF decl THEN
-        IF obj.type.form = NoTyp THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
-      ELSE ORS.Mark("conflict with alias")
+      ELSE (*module already present*)
+        IF obj.val # key THEN ORS.Mark("imported with bad key")
+        ELSIF decl THEN (*explicit import by declaration*)
+          IF obj.rdo THEN ORS.Mark("mult def") ELSE obj.name := name; obj.rdo := TRUE END
+        END
       END
-    ELSIF decl THEN (*module already present, explicit import by declaration*)
-      IF obj.type.form = NoTyp THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
+    ELSE ORS.Mark("mult def")
     END ;
     RETURN obj
   END ThisModule;
@@ -160,13 +157,29 @@
 
   PROCEDURE InType(VAR R: Files.Rider; thismod: Object; VAR T: Type);
     VAR key: LONGINT;
-      ref, class, form, np, readonly: INTEGER;
+      ref, orgref, class, form, np, readonly: INTEGER;
       fld, par, obj, mod, last: Object;
       t: Type;
       name, modname: ORS.Ident;
   BEGIN Read(R, ref);
     IF ref < 0 THEN T := typtab[-ref]  (*already read*)
-    ELSE NEW(t); T := t; typtab[ref] := t; t.mno := thismod.lev;
+    ELSE NEW(t); T := t; t.mno := thismod.lev; t.orgref := ref;
+      IF ref > 0 THEN  (*named type*)
+        Files.ReadString(R, modname);
+        IF modname[0] #  0X THEN  (*re-import*)
+          Files.ReadInt(R, key); Files.ReadString(R, name); Read(R, orgref);
+          mod := ThisModule(modname, modname, FALSE, key);
+          obj := mod.dsc;  (*search type*)
+          WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
+          IF obj # NIL THEN T := obj.type  (*type object found in object list of mod*)
+          ELSE (*insert new type object in object list of mod*)
+            NEW(obj); obj.name := name; obj.class := Typ; obj.next := mod.dsc; mod.dsc := obj; obj.type := t;
+            t.mno := mod.lev; t.typobj := obj; t.orgref := orgref
+          END
+        ELSIF typtab[ref] # NIL THEN T := typtab[ref]  (*already re-imported*)
+        END ;
+        typtab[ref] := T
+      END ;
       Read(R, form); t.form := form;
       IF form = Pointer THEN InType(R, thismod, t.base); t.size := 4
       ELSIF form = Array THEN InType(R, thismod, t.base); Files.ReadNum(R, t.len); Files.ReadNum(R, t.size)
@@ -189,53 +202,41 @@
           InType(R, thismod, par.type); par.next := obj; obj := par; INC(np); Read(R, class)
         END ;
         t.dsc := obj; t.nofpar := np; t.size := 4
-      END ;
-      Files.ReadString(R, modname);
-      IF modname[0] #  0X THEN  (*re-import ========*)
-        Files.ReadInt(R, key); Files.ReadString(R, name);
-        mod := ThisModule(modname, modname, FALSE, key);
-        obj := mod.dsc;  (*search type*)
-        WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
-        IF obj # NIL THEN T := obj.type   (*type object found in object list of mod*)
-        ELSE (*insert new type object in object list of mod*)
-          NEW(obj); obj.name := name; obj.class := Typ; obj.next := mod.dsc; mod.dsc := obj; obj.type := t;
-          t.mno := mod.lev; t.typobj := obj; T := t
-        END ;
-        typtab[ref] := T
       END
     END
   END InType;
 
   PROCEDURE Import*(VAR modid, modid1: ORS.Ident);
-    VAR key: LONGINT; class, k: INTEGER;
+    VAR key: LONGINT; class: INTEGER;
       obj, thismod: Object;
       t: Type;
       name, modname: ORS.Ident;
       F: Files.File; R: Files.Rider;
   BEGIN
     IF modid1 = "SYSTEM" THEN
-      thismod := ThisModule(modid, modid1, TRUE,  key); DEC(nofmod); thismod.lev := 0; thismod.dsc := system; thismod.rdo := TRUE
+      thismod := ThisModule(modid, modid1, TRUE, key); DEC(nofmod); thismod.lev := 0; thismod.dsc := system
     ELSE MakeFileName(name, modid1, ".smb"); F := Files.Old(name);
       IF F # NIL THEN
         Files.Set(R, F, 0); Files.ReadInt(R, key); Files.ReadInt(R, key); Files.ReadString(R, modname);
-        thismod := ThisModule(modid, modid1, TRUE, key); thismod.rdo := TRUE;
+        thismod := ThisModule(modid, modid1, TRUE, key);
+        FOR class := Record+1 TO maxTypTab-1 DO typtab[class] := NIL END ;
+        obj := thismod.dsc;  (*initialize typtab with already re-imported types*)
+        WHILE obj # NIL DO obj.type.mno := -obj.type.mno; typtab[obj.type.orgref] := obj.type; obj := obj.next END ;
         Read(R, class); (*version key*)
         IF class # versionkey THEN ORS.Mark("wrong version") END ;
         Read(R, class);
         WHILE class # 0 DO
-          NEW(obj); obj.class := class; Files.ReadString(R, obj.name);
-          InType(R, thismod, obj.type); obj.lev := -thismod.lev;
-          IF class = Typ THEN
-            t := obj.type; t.typobj := obj; Read(R, k);  (*fixup bases of previously declared pointer types*)
-            WHILE k # 0 DO typtab[k].base := t; Read(R, k) END
-          ELSE
-            IF class = Const THEN
-              IF obj.type.form = Real THEN Files.ReadInt(R, obj.val) ELSE Files.ReadNum(R, obj.val) END
+          Files.ReadString(R, name); InType(R, thismod, t);
+          IF t.mno < 0 THEN t.mno := -t.mno  (*type already re-imported via other modules*)
+          ELSE NEW(obj); obj.class := class; obj.name := name; obj.type := t; obj.lev := -thismod.lev;
+            IF class = Const THEN Files.ReadNum(R, obj.val)
             ELSIF class = Var THEN Files.ReadNum(R, obj.val); obj.rdo := TRUE
-            END
+            ELSIF t.typobj = NIL THEN t.typobj := obj
+            END ;
+            obj.next := thismod.dsc; thismod.dsc := obj
           END ;
-          obj.next := thismod.dsc; thismod.dsc := obj; Read(R, class)
-        END ;
+          Read(R, class)
+        END
       ELSE ORS.Mark("import not available")
       END
     END
@@ -275,13 +276,27 @@
   BEGIN
     IF t.ref > 0 THEN (*type was already output*) Write(R, -t.ref)
     ELSE obj := t.typobj;
-      IF obj # NIL THEN Write(R, Ref); t.ref := Ref; INC(Ref) ELSE (*anonymous*) Write(R, 0) END ;
+      IF obj # NIL THEN Write(R, Ref); t.ref := Ref; INC(Ref);
+        IF t.mno > 0 THEN  (*re-export, output name*)
+          mod := topScope.next;
+          WHILE (mod # NIL) & (mod.lev # t.mno) DO mod := mod.next END ;
+          IF mod # NIL THEN Files.WriteString(R, mod(Module).orgname); Files.WriteInt(R, mod.val);
+            Files.WriteString(R, obj.name); Write(R, t.orgref)
+          ELSE ORS.Mark("re-export not found"); Write(R, 0)
+          END
+        ELSE Write(R, 0)
+        END
+      ELSE (*anonymous*) Write(R, 0)
+      END ;
       Write(R, t.form);
       IF t.form = Pointer THEN OutType(R, t.base)
       ELSIF t.form = Array THEN OutType(R, t.base); Files.WriteNum(R, t.len); Files.WriteNum(R, t.size)
       ELSIF t.form = Record THEN
         IF t.base # NIL THEN OutType(R, t.base); bot := t.base.dsc ELSE OutType(R, noType); bot := NIL END ;
-        IF obj # NIL THEN Files.WriteNum(R, obj.exno) ELSE Write(R, 0) END ;
+        IF obj # NIL THEN
+          IF t.mno > 0 THEN Files.WriteNum(R, t.len) ELSE Files.WriteNum(R, obj.exno) END
+        ELSE Write(R, 0)
+        END ;
         Files.WriteNum(R, t.nofpar); Files.WriteNum(R, t.size);
         fld := t.dsc;
         WHILE fld # bot DO  (*fields*)
@@ -293,14 +308,6 @@
         END ;
         Write(R, 0)
       ELSIF t.form = Proc THEN OutType(R, t.base); OutPar(R, t.dsc, t.nofpar); Write(R, 0)
-      END ;
-      IF (t.mno > 0) & (obj # NIL) THEN  (*re-export, output name*)
-        mod := topScope.next;
-        WHILE (mod # NIL) & (mod.lev # t.mno) DO mod := mod.next END ;
-        IF mod # NIL THEN Files.WriteString(R, mod(Module).orgname); Files.WriteInt(R, mod.val); Files.WriteString(R, obj.name)
-        ELSE ORS.Mark("re-export not found"); Write(R, 0)
-        END
-      ELSE Write(R, 0)
       END
     END
   END OutType;
@@ -320,20 +327,8 @@
       IF obj.expo THEN
         Write(R, obj.class); Files.WriteString(R, obj.name);
         OutType(R, obj.type);
-        IF obj.class = Typ THEN
-          IF obj.type.form = Record THEN
-            obj0 := topScope.next;  (*check whether this is base of previously declared pointer types*)
-            WHILE obj0 # obj DO
-              IF (obj0.type.form = Pointer) & (obj0.type.base = obj.type) & (obj0.type.ref > 0) THEN Write(R, obj0.type.ref) END ;
-              obj0 := obj0.next
-            END
-          END ;
-          Write(R, 0)
-        ELSIF obj.class = Const THEN
-          IF obj.type.form = Proc THEN Files.WriteNum(R, obj.exno)
-          ELSIF obj.type.form = Real THEN Files.WriteInt(R, obj.val)
-          ELSE Files.WriteNum(R, obj.val)
-          END
+        IF obj.class = Const THEN
+          IF obj.type.form = Proc THEN Files.WriteNum(R, obj.exno) ELSE Files.WriteNum(R, obj.val) END
         ELSIF obj.class = Var THEN Files.WriteNum(R, obj.exno)
         END
       END ;
@@ -354,8 +349,8 @@
     END
   END Export;
 
-  PROCEDURE Init*;
-  BEGIN topScope := universe; nofmod := 1
+  PROCEDURE Init*(modid: ORS.Ident);
+  BEGIN self := modid; topScope := universe; nofmod := 1
   END Init;
 
   PROCEDURE type(ref, form: INTEGER; size: LONGINT): Type;
@@ -367,7 +362,7 @@
   PROCEDURE enter(name: ARRAY OF CHAR; cl: INTEGER; type: Type; n: LONGINT);
     VAR obj: Object;
   BEGIN NEW(obj); obj.name := name; obj.class := cl; obj.type := type; obj.val := n; obj.dsc := NIL;
-    IF cl = Typ THEN type.typobj := obj END ;
+    IF cl = Typ THEN type.typobj := obj; obj.expo := TRUE END ;
     obj.next := system; system := obj
   END enter;
 
@@ -432,4 +427,3 @@
   enter("PUT", SProc, noType, 112);
   enter("GET", SProc, noType, 102)
 END ORB.
-
```

**ORG.Mod**

The changes below are part of the implementation of improvement 7 (import any number of modules)

```diff
--- FPGAOberon2013/ORG.Mod  2019-05-30 17:58:14
+++ Oberon-module-imports/Sources/FPGAOberon2013/ORG.Mod  2023-10-10 18:04:57
@@ -1,4 +1,4 @@
-MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC*)
+MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC / AP 12.3.20*)
   IMPORT SYSTEM, Files, ORS, ORB;
   (*Code generator for Oberon compiler for RISC processor.
      Procedural interface to Parser OSAP; result in array "code".
@@ -7,7 +7,7 @@
   CONST WordSize* = 4;
     StkOrg0 = -64; VarOrg0 = 0;  (*for RISC-0 only*)
     MT = 12; SP = 14; LNK = 15;   (*dedicated registers*)
-    maxCode = 8000; maxStrx = 2400; maxTD = 160; C24 = 1000000H;
+    maxCode = 8800; maxStrx = 3200; maxTD = 160; C24 = 1000000H;
     Reg = 10; RegI = 11; Cond = 12;  (*internal item modes*)
 
   (*frequently used opcodes*)  U = 2000H; V = 1000H;
@@ -76,10 +76,20 @@
     code[pc] := ((op * 10H + a) * 10H + b) * 100000H + (off MOD 100000H); INC(pc)
   END Put2;
 
+  PROCEDURE Put2a(op, a, mno, disp: LONGINT);
+  BEGIN (*emit load/store instruction to be fixed up by loader*)
+    code[pc] := ((op * 10H + a) * 40H + mno) * 40000H + (disp MOD 40000H); INC(pc)
+  END Put2a;
+
   PROCEDURE Put3(op, cond, off: LONGINT);
   BEGIN (*emit branch instruction*)
     code[pc] := ((op+12) * 10H + cond) * 1000000H + (off MOD 1000000H); INC(pc)
   END Put3;
+
+  PROCEDURE Put3a(op, mno, pno, disp: LONGINT);
+  BEGIN (*emit BL instruction to be fixed up by loader; 0 <= mno < 64*)
+    code[pc] := (((op+12) * 40H + mno) * 100H + pno) * 4000H + (disp MOD 4000H); INC(pc)
+  END Put3a;
 
   PROCEDURE incR;
   BEGIN
@@ -147,7 +157,7 @@
   PROCEDURE GetSB(base: LONGINT);
   BEGIN
     IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0)
-    ELSE Put2(Ldr, RH, -base, pc-fixorgD); fixorgD := pc-1
+    ELSE Put2a(Ldr, RH, -base, pc-fixorgD); fixorgD := pc-1
     END
   END GetSB;
 
@@ -772,11 +782,7 @@
   BEGIN (*x.type.form = ORB.Proc*)
     IF x.mode = ORB.Const THEN
       IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
-      ELSE (*imported*)
-        IF pc - fixorgP < 1000H THEN
-          Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP); fixorgP := pc-1
-        ELSE ORS.Mark("fixup impossible")
-        END
+      ELSE (*imported*) Put3a(BL, -x.r, x.a, pc-fixorgP); fixorgP := pc-1
       END
     ELSE
       IF x.mode <= ORB.Par THEN load(x); DEC(RH)
```

**ORP.Mod**

```diff
--- FPGAOberon2013/ORP.Mod	2021-05-24 10:06:15
+++ Oberon-module-imports/Sources/FPGAOberon2013/ORP.Mod	2023-11-07 10:00:56
@@ -1,4 +1,4 @@
-MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07*)
+MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07 / AP 1.11.23*)
   IMPORT Texts, Oberon, ORS, ORB, ORG;
   (*Author: Niklaus Wirth, 2014.
     Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
@@ -16,7 +16,7 @@
     level, exno, version: INTEGER;
     newSF: BOOLEAN;  (*option flag*)
     expression: PROCEDURE (VAR x: ORG.Item);  (*to avoid forward reference*)
-    Type: PROCEDURE (VAR type: ORB.Type);
+    Type: PROCEDURE (VAR type: ORB.Type; expo, expoall: BOOLEAN);
     FormalType: PROCEDURE (VAR typ: ORB.Type; dim: INTEGER);
     modid: ORS.Ident;
     pbsList: PtrBase;   (*list of names of pointer base types*)
@@ -86,6 +86,11 @@
     ELSE expo := FALSE
     END
   END CheckExport;
+
+  PROCEDURE CheckExported(type: ORB.Type);
+  BEGIN (*if type is a non-imported named type, check whether it is exported*)
+    IF (type.mno <= 0) & (type.typobj # NIL) & ~type.typobj.expo THEN ORS.Mark("type not exported") END
+  END CheckExported;
 
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
   BEGIN (*t1 is an extension of t0*)
@@ -609,26 +614,27 @@
     END
   END IdentList;
 
-  PROCEDURE ArrayType(VAR type: ORB.Type);
+  PROCEDURE ArrayType(VAR type: ORB.Type; expo, expoall: BOOLEAN);
     VAR x: ORG.Item; typ: ORB.Type; len: LONGINT;
   BEGIN NEW(typ); typ.form := ORB.NoTyp;
     expression(x);
     IF (x.mode = ORB.Const) & (x.type.form = ORB.Int) & (x.a >= 0) THEN len := x.a
     ELSE len := 1; ORS.Mark("not a valid length")
     END ;
-    IF sym = ORS.of THEN ORS.Get(sym); Type(typ.base);
+    IF sym = ORS.of THEN ORS.Get(sym); Type(typ.base, expo, expoall);
+      IF expo THEN CheckExported(typ.base) END ;
       IF (typ.base.form = ORB.Array) & (typ.base.len < 0) THEN ORS.Mark("dyn array not allowed") END
-    ELSIF sym = ORS.comma THEN ORS.Get(sym); ArrayType(typ.base)
+    ELSIF sym = ORS.comma THEN ORS.Get(sym); ArrayType(typ.base, expo, expoall)
     ELSE ORS.Mark("missing OF"); typ.base := ORB.intType
     END ;
     typ.size := (len * typ.base.size + 3) DIV 4 * 4;
     typ.form := ORB.Array; typ.len := len; type := typ
   END ArrayType;
 
-  PROCEDURE RecordType(VAR type: ORB.Type);
+  PROCEDURE RecordType(VAR type: ORB.Type; expo, expoall: BOOLEAN);
     VAR obj, obj0, new, bot, base: ORB.Object;
       typ, tp: ORB.Type;
-      offset, off, n: LONGINT;
+      offset, off, n: LONGINT; fldexpo, fldexpoall: BOOLEAN;
   BEGIN NEW(typ); typ.form := ORB.NoTyp; typ.base := NIL; typ.mno := -level; typ.nofpar := 0; offset := 0; bot := NIL;
     IF sym = ORS.lparen THEN
       ORS.Get(sym); (*record extension*)
@@ -648,18 +654,21 @@
       Check(ORS.rparen, "no )")
     END ;
     WHILE sym = ORS.ident DO  (*fields*)
-      n := 0; obj := bot;
+      n := 0; obj := bot; fldexpo := FALSE; fldexpoall := TRUE;
       WHILE sym = ORS.ident DO
         obj0 := obj;
         WHILE (obj0 # NIL) & (obj0.name # ORS.id) DO obj0 := obj0.next END ;
         IF obj0 # NIL THEN ORS.Mark("mult def") END ;
         NEW(new); ORS.CopyId(new.name); new.class := ORB.Fld; new.next := obj; obj := new; INC(n);
         ORS.Get(sym); CheckExport(new.expo);
-        IF (sym # ORS.comma) & (sym # ORS.colon) THEN ORS.Mark("comma expected")
-        ELSIF sym = ORS.comma THEN ORS.Get(sym)
-        END
+        IF new.expo THEN fldexpo := TRUE;  (*at least one fld exported*)
+          IF ~expoall THEN ORS.Mark("invalid field export") END
+        ELSE fldexpoall := FALSE
+        END ;
+        IF sym = ORS.comma THEN ORS.Get(sym) ELSIF sym # ORS.colon THEN ORS.Mark("comma expected") END
       END ;
-      Check(ORS.colon, "colon expected"); Type(tp);
+      Check(ORS.colon, "colon expected"); Type(tp, expo & fldexpo, expoall & fldexpoall);
+      IF expo & fldexpo THEN CheckExported(tp) END ;
       IF (tp.form = ORB.Array) & (tp.len < 0) THEN ORS.Mark("dyn array not allowed") END ;
       IF tp.size > 1 THEN offset := (offset+3) DIV 4 * 4 END ;
       offset := offset + n * tp.size; off := offset; obj0 := obj;
@@ -737,7 +746,7 @@
     IF lev # 0 THEN ORS.Mark("ptr base must be global") END
   END CheckRecLevel;
 
-  PROCEDURE Type0(VAR type: ORB.Type);
+  PROCEDURE Type0(VAR type: ORB.Type; expo, expoall: BOOLEAN);
     VAR dmy: LONGINT; obj: ORB.Object; ptbase: PtrBase;
   BEGIN type := ORB.intType; (*sync*)
     IF (sym # ORS.ident) & (sym < ORS.array) THEN ORS.Mark("not a type");
@@ -749,9 +758,9 @@
         IF (obj.type # NIL) & (obj.type.form # ORB.NoTyp) THEN type := obj.type END
       ELSE ORS.Mark("not a type or undefined")
       END
-    ELSIF sym = ORS.array THEN ORS.Get(sym); ArrayType(type)
+    ELSIF sym = ORS.array THEN ORS.Get(sym); ArrayType(type, expo, expoall)
     ELSIF sym = ORS.record THEN
-      ORS.Get(sym); RecordType(type); Check(ORS.end, "no END")
+      ORS.Get(sym); RecordType(type, expo, expoall); Check(ORS.end, "no END")
     ELSIF sym = ORS.pointer THEN
       ORS.Get(sym); Check(ORS.to, "no TO");
       NEW(type);  type.form := ORB.Pointer; type.size := ORG.WordSize; type.base := ORB.intType;
@@ -767,7 +776,7 @@
           NEW(ptbase); ORS.CopyId(ptbase.name); ptbase.type := type; ptbase.next := pbsList; pbsList := ptbase
         END ;
         ORS.Get(sym)
-      ELSE Type(type.base);
+      ELSE Type(type.base, expo, expoall);
         IF (type.base.form # ORB.Record) OR (type.base.typobj = NIL) THEN ORS.Mark("must point to named record") END ;
         CheckRecLevel(level)
       END
@@ -782,7 +791,7 @@
   PROCEDURE Declarations(VAR varsize: LONGINT);
     VAR obj, first: ORB.Object;
       x: ORG.Item; tp: ORB.Type; ptbase: PtrBase;
-      expo: BOOLEAN; id: ORS.Ident;
+      expo, expoall: BOOLEAN; id: ORS.Ident;
   BEGIN (*sync*) pbsList := NIL;
     IF (sym < ORS.const) & (sym # ORS.end) & (sym # ORS.return) THEN ORS.Mark("declaration?");
       REPEAT ORS.Get(sym) UNTIL (sym >= ORS.const) OR (sym = ORS.end) OR (sym = ORS.return)
@@ -806,7 +815,8 @@
       WHILE sym = ORS.ident DO
         ORS.CopyId(id); ORS.Get(sym); CheckExport(expo);
         IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("=?") END ;
-        Type(tp);
+        Type(tp, expo, expo);
+        IF expo THEN CheckExported(tp) END ;
         ORB.NewObj(obj, id, ORB.Typ); obj.type := tp; obj.expo := expo; obj.lev := level;
         IF tp.typobj = NIL THEN tp.typobj := obj END ;
         IF expo & (obj.type.form = ORB.Record) THEN obj.exno := exno; INC(exno) ELSE obj.exno := 0 END ;
@@ -824,7 +834,13 @@
     IF sym = ORS.var THEN
       ORS.Get(sym);
       WHILE sym = ORS.ident DO
-        IdentList(ORB.Var, first); Type(tp);
+        IdentList(ORB.Var, first); obj := first; expo := FALSE; expoall := TRUE;
+        WHILE obj # NIL DO
+          IF obj.expo THEN expo := TRUE (*at least one var exported*) ELSE expoall := FALSE END ;
+          obj := obj.next
+        END ;
+        Type(tp, expo, expoall);
+        IF expo THEN CheckExported(tp) END ;
         obj := first;
         WHILE obj # NIL DO
           obj.type := tp; obj.lev := level;
@@ -893,22 +909,24 @@
     END
   END ProcedureDecl;
 
-  PROCEDURE Import;
+  PROCEDURE ImportList;
     VAR impid, impid1: ORS.Ident;
   BEGIN
-    IF sym = ORS.ident THEN
-      ORS.CopyId(impid); ORS.Get(sym);
-      IF sym = ORS.becomes THEN
-        ORS.Get(sym);
-        IF sym = ORS.ident THEN ORS.CopyId(impid1); ORS.Get(sym)
-        ELSE ORS.Mark("id expected"); impid1 := impid
-        END
-      ELSE impid1 := impid
-      END ;
-      ORB.Import(impid, impid1)
-    ELSE ORS.Mark("id expected")
-    END
-  END Import;
+    REPEAT ORS.Get(sym);
+      IF sym = ORS.ident THEN
+        ORS.CopyId(impid); ORS.Get(sym);
+        IF sym = ORS.becomes THEN
+          ORS.Get(sym);
+          IF sym = ORS.ident THEN ORS.CopyId(impid1); ORS.Get(sym)
+          ELSE ORS.Mark("id expected"); impid1 := impid
+          END
+        ELSE impid1 := impid
+        END ;
+        ORB.Import(impid, impid1)
+      ELSE ORS.Mark("id expected")
+      END
+    UNTIL sym # ORS.comma
+  END ImportList;
 
   PROCEDURE Module;
     VAR key: LONGINT;
@@ -916,18 +934,14 @@
     IF sym = ORS.module THEN
       ORS.Get(sym);
       IF sym = ORS.times THEN version := 0; dc := 8; Texts.Write(W, "*"); ORS.Get(sym) ELSE dc := 0; version := 1 END ;
-      ORB.Init; ORB.OpenScope;
       IF sym = ORS.ident THEN
         ORS.CopyId(modid); ORS.Get(sym);
         Texts.WriteString(W, modid); Texts.Append(Oberon.Log, W.buf)
       ELSE ORS.Mark("identifier expected")
       END ;
       Check(ORS.semicolon, "no ;"); level := 0; exno := 1; key := 0;
-      IF sym = ORS.import THEN
-        ORS.Get(sym); Import;
-        WHILE sym = ORS.comma DO ORS.Get(sym); Import END ;
-        Check(ORS.semicolon, "; missing")
-      END ;
+      ORB.Init(modid); ORB.OpenScope;
+      IF sym = ORS.import THEN ImportList; Check(ORS.semicolon, "; missing") END ;
       ORG.Open(version); Declarations(dc); ORG.SetDataSize((dc + 3) DIV 4 * 4);
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
       ORG.Header;
@@ -994,7 +1008,7 @@
     Oberon.Collect(0)
   END Compile;
 
-BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Compiler  8.3.2020");
+BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Compiler  8.3.2020 / AP 1.11.23");
   Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
   NEW(dummy); dummy.class := ORB.Var; dummy.type := ORB.intType;
   expression := expression0; Type := Type0; FormalType := FormalType0
```

**ORL.Mod**

The changes below are part of the implementation of improvement 7 (import any number of modules)

```diff
--- FPGAOberon2013/ORL.Mod  2023-10-11 12:21:00
+++ Oberon-module-imports/Sources/FPGAOberon2013/ORL.Mod  2023-10-10 18:04:57
@@ -72,7 +72,7 @@
       disp, adr, inst, pno, vno, dest, offset: INTEGER;
       name1, impname: ModuleName;
       F: Files.File; R: Files.Rider;
-      import: ARRAY 16 OF Module;
+      import: ARRAY 64 OF Module;
   BEGIN mod := root; error(noerr, name); nofimps := 0;
     WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END ;
     IF mod = NIL THEN (*link*)
@@ -150,9 +150,9 @@
         adr := mod.code + fixorgP*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV C20 MOD C4;
-          pno := inst DIV C12 MOD C8;
-          disp := inst MOD C12;
+          mno := inst DIV C22 MOD C6;
+          pno := inst DIV C14 MOD C8;
+          disp := inst MOD C14;
           SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
           SYSTEM.GET(impmod.ent + pno*4, dest); dest := dest + impmod.code;
           offset := (dest - adr - 4) DIV 4;
@@ -163,8 +163,8 @@
         adr := mod.code + fixorgD*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV C20 MOD C4;
-          disp := inst MOD C12;
+          mno := inst DIV C18 MOD C6;
+          disp := inst MOD C18;
           IF mno = 0 THEN (*global*)
             SYSTEM.PUT(adr, (inst DIV C24 * C4 + MT) * C20 + mod.num * 4)
           ELSE (*import*)
@@ -181,7 +181,7 @@
         adr := mod.data + fixorgT*4;
         WHILE adr # mod.data DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV C24 MOD C4;
+          mno := inst DIV C24 MOD C6;
           vno := inst DIV C12 MOD C12;
           disp := inst MOD C12;
           IF mno = 0 THEN (*global*) inst := mod.data - Start + vno
```

**Modules.Mod**

The changes below are part of the implementation of improvement 7 (import any number of modules)

```diff
--- FPGAOberon2013/Modules.Mod  2020-02-26 01:15:33
+++ Oberon-module-imports/Sources/FPGAOberon2013/Modules.Mod  2023-10-10 18:04:53
@@ -1,4 +1,4 @@
-MODULE Modules;  (*Link and load on RISC; NW 20.10.2013 / 8.1.2019*)
+MODULE Modules;  (*Link and load on RISC; NW 20.10.2013 / 8.1.2019 / AP 12.3.20*)
   IMPORT SYSTEM, Files;
   CONST versionkey = 1X; MT = 12; DescSize = 80;
 
@@ -55,7 +55,7 @@
       disp, adr, inst, pno, vno, dest, offset: INTEGER;
       name1, impname: ModuleName;
       F: Files.File; R: Files.Rider;
-      import: ARRAY 16 OF Module;
+      import: ARRAY 64 OF Module;
   BEGIN mod := root; error(0, name); nofimps := 0;
     WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END ;
     IF mod = NIL THEN (*load*)
@@ -135,9 +135,9 @@
         adr := mod.code + fixorgP*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 100000H MOD 10H;
-          pno := inst DIV 1000H MOD 100H;
-          disp := inst MOD 1000H;
+          mno := inst DIV 400000H MOD 40H;
+          pno := inst DIV 4000H MOD 100H;
+          disp := inst MOD 4000H;
           SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
           SYSTEM.GET(impmod.ent + pno*4, dest); dest := dest + impmod.code;
           offset := (dest - adr - 4) DIV 4;
@@ -148,8 +148,8 @@
         adr := mod.code + fixorgD*4;
         WHILE adr # mod.code DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 100000H MOD 10H;
-          disp := inst MOD 1000H;
+          mno := inst DIV 40000H MOD 40H;
+          disp := inst MOD 40000H;
           IF mno = 0 THEN (*global*)
             SYSTEM.PUT(adr, (inst DIV 1000000H * 10H + MT) * 100000H + mod.num * 4)
           ELSE (*import*)
@@ -166,7 +166,7 @@
         adr := mod.data + fixorgT*4;
         WHILE adr # mod.data DO
           SYSTEM.GET(adr, inst);
-          mno := inst DIV 1000000H MOD 10H;
+          mno := inst DIV 1000000H MOD 40H;
           vno := inst DIV 1000H MOD 1000H;
           disp := inst MOD 1000H;
           IF mno = 0 THEN (*global*) inst := mod.data + vno
```

-------------------------------------------------------------------------------------

**Notes on implementing improvement 7**

Project Oberon 2013 (see http://www.projectoberon.com) allows a maximum of 15 modules to be imported by any single module. This typically doesn't pose any issues, as it aligns with the good programming practice of structuring the module hierarchy in a way that only a small number of modules are imported.

However, this upper limit also includes modules from which types are (only indirectly) re-imported. These modules don't necessarily have to be explicitly listed in the import statement; their imports can remain hidden. Therefore, in deep module hierarchies, there may arise a desire to lift this restriction.

To address this need, the code in this repository increases the maximum number of modules that can be directly or indirectly imported from 15 to 63, providing greater flexibility in managing complex module structures.

This is achieved by adjusting all instructions and data elements concerned to use 6 bits instead of 4 bits for the module number (mno) and by adapting their associated fixup mechanisms in the module loader accordingly.

**1. BL instructions for external procedure calls**

Change BL instructions for external procedure calls, as generated by the compiler (see *ORG.Call*) from

     | BL (4) | cond (4) | mno (4) | pno (8) | pc-fixorgP (12) |    (max mno = 15, max pno = 255, max disp = 2^12-1 = 4K-1 words)

to

     | BL (4) | mno (6) | pno (8) | pc-fixorgP (14) |    (max mno = 63, max pno = 255, max disp = 2^14-1 = 16K-1 words)

which the module loader will fix up to

     | BL (4) | cond (4) | offset relative to PC (24) |    (max offset = 2^24-1 = 16M-1 words)

Thus, the *cond* field is eliminated from the instruction, as generated by the compiler, and re-inserted by the loader.

This is possible because BL instructions only ever use *cond* = 7 (=always) and the module loader already inserts *BL 7* as a hardcoded constant in the fixed up instruction anyway (see the fixup code at the end of procedure *Modules.Load*).

The new (compile-time) instruction encoding also increases the maximum displacement between two BL instructions in the fixup chain from 2^12-1 = 4K-1 words to 2^14-1 = 16K-1 words. Given that the array *ORG.code* holds only 8K words, this eliminates the need for an extra check in *ORG.Call* whether a fixup is possible (*IF pc-fixorgP < 1000H*).

We keep the first 4 bits of the BL instruction so that *ORTool.DecObj* can recognize it as such. An alternative would have been to keep only the first 2 bits, leaving 8 bits for the module number (mno). But we opted to keep the U and V bits as well, as they are interpreted by *ORTool.DecObj*. Using 6 bits for the module number is no real limitation, as 2^6-1 = 63 imported modules should be enough for most purposes, even when taking into account indirectly imported modules.

**2. LD instructions for loading the static base of a module**

Change LD instructions for loading the static base of a module, as generated by the compiler (see *ORG.GetSB*) from

     | LD (4) | reg (4) | mno (4) | pc-fixorgD (20) |    (max mno = 15, max disp = 2^20-1 = 1M-1 words)

to

     | LD (4) | reg (4) | mno (6) | pc-fixorgD (18) |    (max mno = 63, max disp = 2^18-1 = 256K-1 words)

which the module loader will fix up to

     | LD (4) | reg (4) | MT (4) | offset for imported module in MT table (20) |    (max offset = 2^20-1 = 1M-1 words)

**3. Entries in type descriptor extension tables**

Change entries in type descriptor extension tables, as generated by the compiler (see *ORG.Q*) from

     | unused (4) | mno (4) | TDadr/exno (12) | pc-fixorgT (12) |    (max mno = 15, max TDadr/exno = 4095, max disp = 2^12-1 = 4K-1 words)

to

     | unused (2) | mno (6) | TDadr/exno (12) | pc-fixorgT (12) |    (max mno = 63, max TDadr/exno = 4095, max disp = 2^12-1 = 4K-1 words)

which the module loader will fix up to

     | absolute memory address of type descriptor (32) |

Extension table entries generated by the compiler already allowed 8 bits for the module number (mno), but the module loader previously extracted only the least significant 4 bits during the fixup phase. It now extracts 6 bits.

**4. Entries in method tables (Extended Oberon only)**

Change entries in method tables, as generated by the compiler (see *ORG.BuildTD*) from

     | unused (4) | mno (4) | mthadr/exno (14) | pc-fixorgM (10) |    (max mno = 15, max disp = 2^10-1 = 1023 words)

to

     | mno (6) | mthadr/exno (16) | pc-fixorgM (10) |    (max mno = 63, max disp = 2^10-1 = 1023 words)

which the module loader will fix up to

     | absolute memory address of method (32) |

Method table entries generated by the compiler already allowed 8 bits for the module number (mno), but the module loader previously extracted only the least significant 4 bits during the fixup phase. It now extracts 6 bits.

Note: We use 16 bits for the mthadr field (mthadr is the offset from mod.code in bytes), because 2^16 = 64KB. This is enough, since the code array of a module is only 8K words = 32K bytes. Alternatively, one could have decided to let the compiler (in ORG.BuildTD) insert the mthadr in words, use only 14 bits for the mthadr and adapt the loader/linker accordingly. But there was no need to do so.

**5. BL instructions for external super calls (Extended Oberon only)**

Change BL instructions for external super calls, as generated by the compiler (see *ORG.Call*) from

     | BL (4) | cond (4) | mno (4) | pno (8) | pc-fixorgP (12) |    (max mno = 15, max pno = 255, max disp = 2^12-1 = 4K-1 words)

to

     | BL (4) | mno (6) | pno (8) | pc-fixorgP (14) |    (max mno = 63, max pno = 255, max disp = 2^14-1 = 16K-1 words)

which the module loader will fix up to

     | BL (4) | cond (4) | offset relative to PC (24) |    (max offset = 2^24-1 = 16M-1 words)

-------------------------------------------------------------------------------------

**Notes on implementing improvement 8**

In Project Oberon 2013, the following program leads to a stack overflow when compiling module A followed by compiling module B:

     MODULE A;
       TYPE X* = POINTER TO XD;
         XD = RECORD a*: POINTER TO XD END ;
     END A.

     MODULE B;
       IMPORT A;
       PROCEDURE P*(a: A.X);
       END P;
     END B.

If module A is replaced with

     MODULE A;
       TYPE X* = POINTER TO XD;
         XD = RECORD a*: X END ;
     END A.

no error occurs when compiling module B.


The solution is to add boolean parameters 'expo' and 'expoall' in various procedures in ORP that is passed along when parsing recursive data structures. They have the following meaning:

* **expo** = at least one variable (or field) is exported

* **expoall** = all variables (or record fields) are exported

Procedure *ORB.enter* also needs to be changed to set *obj.expo* to TRUE for basic types, in order for the new procedure *ORP.CheckExported* to work correctly.

-------------------------------------------------------------------------------------

**Variant 1: Use a global module table 'modtab' to optimize writing module objects**

Instead of writing the module name and key for *each* re-exported type, use a global *modtab* to write the module name and module key only once (first occurence) and reference numbers for subsequent occurrences.

Note: The table 'modtab' corresponds to ...

* the 'LMod' table in the paper 'Griesemer R. On the Linearization of Graphs and Writing Symbol Files. Computersysteme ETH Zürich, Technical Report No. 156a (1991)'
* the 'LModtab' table in https://github.com/lboasso/oberonc/blob/master/src/OJB.Mod. 

Note that in these implementations, the module name and key is written for *each* exported type, not just for *re-exported* ones. By contrast, in our implementation we write the module name and key only for *re-exported* types (for the other types, this information is implicit). Since re-exports are rare, we would therefore gain very little by using a global module table.

The savings in the symbol files are negligible and are listed below:
						
                  PO2013	PO2013 with modtab	%
						
     Kernel          388	388			100,00%
     FileDir         580	580			100,00%
     Files           760	760			100,00%
     Modules         328	328			100,00%
     Input           112	112			100,00%
     Display         464	464			100,00%
     Viewers         548	516			94,16%
     Fonts           200	200			100,00%
     Texts          1260	1256			99,68%
     Oberon         1692	1624			95,98%
     MenuViewers     364	332			91,21%
     TextFrames     1644	1572			95,62%
     System          392	392			100,00%
     Edit            156	156			100,00%
     Tools           116	116			100,00%
     PCLink1          44	44			100,00%
     Clipboard        72	72			100,00%
     Out             160	160			100,00%
     RS232           204	204			100,00%
     SCC             216	216			100,00%
     Net             184	184			100,00%
     Graphics       1992	1968			98,80%
     GraphicsFrames 1476	1344			91,06%
     GraphTool        96	96			100,00%
     MacroTool        96	96			100,00%
     Draw            168	168			100,00%
     Curves          672	604			89,88%
     Sierpinksi       36	36			100,00%
     Hilbert          32	32			100,00%
     Stars            96	96			100,00%
     Rectangles      656	584			89,02%
     Checkers         32	32			100,00%
     Math             88	88			100,00%
     PIO              60	60			100,00%
     Blink            40	40			100,00%

     TOTAL         15424	14920			96,73%

As one can see, the sizes of symbol files increase only marginally (and when counted in disk blocks, not at all).

-------------------------------------------------------------------------------------
**Implementing Variant 1**

**ORB.Mod**

```diff
--- Oberon-module-imports/Sources/FPGAOberon2013/ORB.Mod  2023-10-10 20:45:51
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant1/ORB.Mod  2023-10-10 23:01:35
@@ -7,7 +7,7 @@
     Import and Export. This module contains the list of standard identifiers, with which
     the symbol table (universe), and that of the pseudo-module SYSTEM are initialized. *)
 
-  CONST versionkey* = 1; maxTypTab = 64;
+  CONST versionkey* = 1; maxTypTab = 64; maxModTab = 64;
     (* class values*) Head* = 0;
       Const* = 1; Var* = 2; Par* = 3; Fld* = 4; Typ* = 5;
       SProc* = 6; SFunc* = 7; Mod* = 8;
@@ -31,7 +31,7 @@
       val*: LONGINT
     END ;
 
-    ModDesc* = RECORD (ObjDesc) orgname*: ORS.Ident END ;
+    ModDesc* = RECORD (ObjDesc) orgname*: ORS.Ident; ref: INTEGER END ;  (*ref is only used for import/export*)
 
     TypeDesc* = RECORD
       form*, mno*, ref, orgref: INTEGER;  (*ref and orgref are only used for import/export*)
@@ -65,8 +65,9 @@
   VAR topScope*, universe, system*: Object;
     byteType*, boolType*, charType*: Type;
     intType*, realType*, setType*, nilType*, noType*, strType*: Type;
-    nofmod, Ref: INTEGER;
+    nofmod, Ref, modRef: INTEGER;
     typtab: ARRAY maxTypTab OF Type;
+    modtab: ARRAY maxModTab OF Object;
     self: ORS.Ident;  (*name of module being compiled*)
 
   PROCEDURE NewObj*(VAR obj: Object; id: ORS.Ident; class: INTEGER);  (*insert new Object with name id*)
@@ -157,7 +158,7 @@
 
   PROCEDURE InType(VAR R: Files.Rider; thismod: Object; VAR T: Type);
     VAR key: LONGINT;
-      ref, orgref, class, form, np, readonly: INTEGER;
+      ref, orgref, modref, class, form, np, readonly: INTEGER;
       fld, par, obj, mod, last: Object;
       t: Type;
       name, modname: ORS.Ident;
@@ -165,10 +166,13 @@
     IF ref < 0 THEN T := typtab[-ref]  (*already read*)
     ELSE NEW(t); T := t; t.mno := thismod.lev; t.orgref := ref;
       IF ref > 0 THEN  (*named type*)
-        Files.ReadString(R, modname);
-        IF modname[0] #  0X THEN  (*re-import*)
-          Files.ReadInt(R, key); Files.ReadString(R, name); Read(R, orgref);
-          mod := ThisModule(modname, modname, FALSE, key);
+        Read(R, modref);
+        IF modref # 0 THEN  (*re-import*)
+          IF modref < 0 THEN mod := modtab[-modref]  (*already read*)
+          ELSE Files.ReadString(R, modname); Files.ReadInt(R, key);
+            mod := ThisModule(modname, modname, FALSE, key); modtab[modref] := mod
+          END ;
+          Files.ReadString(R, name); Read(R, orgref);
           obj := mod.dsc;  (*search type*)
           WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
           IF obj # NIL THEN T := obj.type  (*type object found in object list of mod*)
@@ -280,9 +284,13 @@
         IF t.mno > 0 THEN  (*re-export, output name*)
           mod := topScope.next;
           WHILE (mod # NIL) & (mod.lev # t.mno) DO mod := mod.next END ;
-          IF mod # NIL THEN Files.WriteString(R, mod(Module).orgname); Files.WriteInt(R, mod.val);
+          IF mod = NIL THEN ORS.Mark("re-export not found"); Write(R, 0)
+          ELSE
+            IF mod(Module).ref > 0 THEN (*module was already output*) Write(R, -mod(Module).ref)
+            ELSE Write(R, modRef); mod(Module).ref := modRef; INC(modRef);
+              Files.WriteString(R, mod(Module).orgname); Files.WriteInt(R, mod.val)
+            END ;
             Files.WriteString(R, obj.name); Write(R, t.orgref)
-          ELSE ORS.Mark("re-export not found"); Write(R, 0)
           END
         ELSE Write(R, 0)
         END
@@ -317,7 +325,7 @@
       obj, obj0: Object;
       filename: ORS.Ident;
       F, F1: Files.File; R, R1: Files.Rider;
-  BEGIN Ref := Record + 1; MakeFileName(filename, modid, ".smb");
+  BEGIN Ref := Record + 1; modRef := 1; MakeFileName(filename, modid, ".smb");
     F := Files.New(filename); Files.Set(R, F, 0);
     Files.WriteInt(R, 0); (*placeholder*)
     Files.WriteInt(R, 0); (*placeholder for key to be inserted at the end*)
@@ -336,6 +344,7 @@
     END ;
     REPEAT Write(R, 0) UNTIL Files.Length(F) MOD 4 = 0;
     FOR Ref := Record+1 TO maxTypTab-1 DO typtab[Ref] := NIL END ;
+    FOR modRef := 0 TO maxModTab-1 DO modtab[modRef] := NIL END ;
     Files.Set(R, F, 0); sum := 0; Files.ReadInt(R, x);  (* compute key (checksum) *)
     WHILE ~R.eof DO sum := sum + x; Files.ReadInt(R, x) END ;
     F1 := Files.Old(filename); (*sum is new key*)
```

-------------------------------------------------------------------------------------

**Variant 2: Modify the data structure for linking record base types in order to simplify the code**

In the modified compiler, we enforce the following invariant for each record type t

     t.base # NIL

This is achieved by assigning ORB.noType to the base type of a record when it is originally created (in *ORP.RecordType*)

     typ.base := ORB.noType;

and by replacing any test

     IF t.base # NIL THEN

with

     IF t.base # ORB.noType THEN

in various places in modules ORB, ORG and ORP.

This small change in the data structure simplifies the code in multiple places, for example:

a. Instead of setting the field t.base to NIL in ORB.InType as follows

     IF t.base.form = NoTyp THEN t.base := NIL; obj := NIL ELSE obj := t.base.dsc END ;

one can now simply write (noting that ORB.noType.dsc is always NIL)

     obj := t.base.dsc

b. Instead of writing (in ORB.OutType)

     fld := t.dsc;
     IF t.base # NIL THEN (*...*) bot := t.base.dsc ELSE (*...*) bot := NIL END ;
     WHILE fld # bot DO  (*fields*)
       ...
       fld := fld.next
     END ;

one can now simply write

     fld := t.dsc; bot := t.base.dsc;
     WHILE fld # bot DO  (*fields*)
       ...
       fld := fld.next
     END ;

-------------------------------------------------------------------------------------
**Implementing Variant 2**

**ORB.Mod**

```diff
--- Oberon-module-imports/Sources/FPGAOberon2013/ORB.Mod  2023-10-10 20:45:51
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant2/ORB.Mod  2023-10-10 23:58:29
@@ -183,8 +183,7 @@
       Read(R, form); t.form := form;
       IF form = Pointer THEN InType(R, thismod, t.base); t.size := 4
       ELSIF form = Array THEN InType(R, thismod, t.base); Files.ReadNum(R, t.len); Files.ReadNum(R, t.size)
-      ELSIF form = Record THEN InType(R, thismod, t.base);
-        IF t.base.form = NoTyp THEN t.base := NIL; obj := NIL ELSE obj := t.base.dsc END ;
+      ELSIF form = Record THEN InType(R, thismod, t.base); obj := t.base.dsc;
         Files.ReadNum(R, t.len); Files.ReadNum(R, t.nofpar); Files.ReadNum(R, t.size);  (*TD adr exno, ext level, size*)
         Read(R, class); last := NIL;
         WHILE class # 0 DO  (*fields*)
@@ -291,14 +290,13 @@
       Write(R, t.form);
       IF t.form = Pointer THEN OutType(R, t.base)
       ELSIF t.form = Array THEN OutType(R, t.base); Files.WriteNum(R, t.len); Files.WriteNum(R, t.size)
-      ELSIF t.form = Record THEN
-        IF t.base # NIL THEN OutType(R, t.base); bot := t.base.dsc ELSE OutType(R, noType); bot := NIL END ;
+      ELSIF t.form = Record THEN OutType(R, t.base);
         IF obj # NIL THEN
           IF t.mno > 0 THEN Files.WriteNum(R, t.len) ELSE Files.WriteNum(R, obj.exno) END
         ELSE Write(R, 0)
         END ;
         Files.WriteNum(R, t.nofpar); Files.WriteNum(R, t.size);
-        fld := t.dsc;
+        fld := t.dsc; bot := t.base.dsc;
         WHILE fld # bot DO  (*fields*)
           IF fld.expo THEN
             Write(R, Fld); Files.WriteString(R, fld.name); OutType(R, fld.type); Files.WriteNum(R, fld.val)  (*offset*)
```

**ORG.Mod**

```diff
--- Oberon-module-imports/Sources/FPGAOberon2013/ORG.Mod  2023-10-10 18:04:57
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant2/ORG.Mod  2023-10-10 23:58:30
@@ -328,7 +328,7 @@
 
   PROCEDURE Q(T: ORB.Type; VAR dcw: LONGINT);
   BEGIN (*one entry of type descriptor extension table*)
-    IF T.base # NIL THEN
+    IF T.base # ORB.noType THEN
       Q(T.base, dcw); data[dcw] := (T.mno*1000H + T.len) * 1000H + dcw - fixorgT;
       fixorgT := dcw; INC(dcw)
     END
```

**ORP.Mod**

```diff
--- Oberon-module-imports/Sources/FPGAOberon2013/ORP.Mod  2023-10-10 18:04:57
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant2/ORP.Mod  2023-10-10 23:58:30
@@ -89,7 +89,7 @@
 
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
   BEGIN (*t1 is an extension of t0*)
-    RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
+    RETURN (t0 = t1) OR (t1 # ORB.noType) & IsExtension(t0, t1.base)
   END IsExtension;
 
   (* expressions *)
@@ -98,7 +98,7 @@
     VAR xt: ORB.Type;
   BEGIN xt := x.type;
     IF (T.form = xt.form ) & ((T.form = ORB.Pointer) OR (T.form = ORB.Record) & (x.mode = ORB.Par)) THEN
-      WHILE (xt # T) & (xt # NIL) DO xt := xt.base END ;
+      WHILE (xt # T) & (xt # ORB.noType) DO xt := xt.base END ;
       IF xt # T THEN xt := x.type;
         IF xt.form = ORB.Pointer THEN
           IF IsExtension(xt.base, T.base) THEN ORG.TypeTest(x, T.base, FALSE, guard); x.type := T
@@ -629,7 +629,7 @@
     VAR obj, obj0, new, bot, base: ORB.Object;
       typ, tp: ORB.Type;
       offset, off, n: LONGINT;
-  BEGIN NEW(typ); typ.form := ORB.NoTyp; typ.base := NIL; typ.mno := -level; typ.nofpar := 0; offset := 0; bot := NIL;
+  BEGIN NEW(typ); typ.form := ORB.NoTyp; typ.base := ORB.noType; typ.mno := -level; typ.nofpar := 0; offset := 0; bot := NIL;
     IF sym = ORS.lparen THEN
       ORS.Get(sym); (*record extension*)
       IF level # 0 THEN ORS.Mark("extension of local types not implemented") END ;
```
