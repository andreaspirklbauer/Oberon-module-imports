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
2. Propagate imported export numbers of type descriptor addresses to client modules (needed for 3. and 4.)
3. Allow an explicit import of a module M after types of M have previously been re-imported via other modules
4. Allow importing any number of modules in any order
5. Allow names of re-imported modules to co-exist with module alias names and globally declared identifiers
6. Allow reusing the original module name if a module has been imported under a module alias name
7. Handle type alias names among imported and re-imported modules correctly
8. Write the module anchor of re-exported types before (instead of after) the type description to the symbol file
9. Detect invalid record field exports and ensure the array base type is also exported if an array is exported
10. Allow exporting and importing string constants (as in [**Oberon-importing-string-constants**](http://github.com/andreaspirklbauer/Oberon-importing-string-constants))

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

**Implementing improvements 1-11**

**ORB.Mod**

```diff
--- FPGAOberon2013/ORB.Mod	2023-10-11 12:57:57
+++ Oberon-module-imports/Sources/FPGAOberon2013/ORB.Mod	2024-04-18 12:29:22
@@ -1,4 +1,4 @@
-MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 5.3.2019  in Oberon-07*)
+MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 5.3.2019  in Oberon-07 / AP 18.4.24*)
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
@@ -320,18 +327,9 @@
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
+        IF obj.class = Const THEN
           IF obj.type.form = Proc THEN Files.WriteNum(R, obj.exno)
-          ELSIF obj.type.form = Real THEN Files.WriteInt(R, obj.val)
+          ELSIF obj.type.form = String THEN Files.WriteNum(R, obj.exno + obj.val DIV 100000H (*len*) * 100000H)
           ELSE Files.WriteNum(R, obj.val)
           END
         ELSIF obj.class = Var THEN Files.WriteNum(R, obj.exno)
@@ -354,8 +352,8 @@
     END
   END Export;
 
-  PROCEDURE Init*;
-  BEGIN topScope := universe; nofmod := 1
+  PROCEDURE Init*(modid: ORS.Ident);
+  BEGIN self := modid; topScope := universe; nofmod := 1
   END Init;
 
   PROCEDURE type(ref, form: INTEGER; size: LONGINT): Type;
@@ -367,7 +365,7 @@
   PROCEDURE enter(name: ARRAY OF CHAR; cl: INTEGER; type: Type; n: LONGINT);
     VAR obj: Object;
   BEGIN NEW(obj); obj.name := name; obj.class := cl; obj.type := type; obj.val := n; obj.dsc := NIL;
-    IF cl = Typ THEN type.typobj := obj END ;
+    IF cl = Typ THEN type.typobj := obj; obj.expo := TRUE END ;
     obj.next := system; system := obj
   END enter;
 
@@ -432,4 +430,3 @@
   enter("PUT", SProc, noType, 112);
   enter("GET", SProc, noType, 102)
 END ORB.
-
```

**ORG.Mod**

```diff
--- FPGAOberon2013/ORG.Mod	2019-05-30 17:58:14
+++ Oberon-module-imports/Sources/FPGAOberon2013/ORG.Mod	2024-04-18 12:29:22
@@ -1,4 +1,4 @@
-MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC*)
+MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC / AP 18.4.24*)
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
@@ -76,11 +76,21 @@
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
 
+  PROCEDURE Put3a(op, mno, pno, disp: LONGINT);
+  BEGIN (*emit BL instruction to be fixed up by loader; 0 <= mno < 64*)
+    code[pc] := (((op+12) * 40H + mno) * 100H + pno) * 4000H + (disp MOD 4000H); INC(pc)
+  END Put3a;
+
   PROCEDURE incR;
   BEGIN
     IF RH < MT-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
@@ -147,7 +157,7 @@
   PROCEDURE GetSB(base: LONGINT);
   BEGIN
     IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0)
-    ELSE Put2(Ldr, RH, -base, pc-fixorgD); fixorgD := pc-1
+    ELSE Put2a(Ldr, RH, -base, pc-fixorgD); fixorgD := pc-1
     END
   END GetSB;
 
@@ -223,7 +233,9 @@
   END loadTypTagAdr;
 
   PROCEDURE loadStringAdr(VAR x: Item);
-  BEGIN GetSB(0); Put1a(Add, RH, RH, varsize+x.a); x.mode := Reg; x.r := RH; incR
+  BEGIN
+    IF x.r >= 0 THEN GetSB(0); Put1a(Add, RH, RH, varsize+x.a) ELSE GetSB(x.r); Put1(Add, RH, RH, x.a) (*exno*) END ;
+    x.mode := Reg; x.r := RH; incR
   END loadStringAdr;
 
   (* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)
@@ -238,7 +250,7 @@
 
   PROCEDURE MakeStringItem*(VAR x: Item; len: LONGINT); (*copies string from ORS-buffer to ORG-string array*)
     VAR i: LONGINT;
-  BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; i := 0;
+  BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; x.r := 0; i := 0;
     IF strx + len + 4 < maxStrx THEN
       WHILE len > 0 DO str[strx] := ORS.str[i]; INC(strx); INC(i); DEC(len) END ;
       WHILE strx MOD 4 # 0 DO str[strx] := 0X; INC(strx) END
@@ -249,10 +261,10 @@
   PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: LONGINT);
   BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo;
     IF y.class = ORB.Par THEN x.b := 0
-    ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*) ;
+    ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.r := y.lev;
+      x.a := y.val MOD 100000H; (*strx / exno*) x.b := y.val DIV 100000H (*len*)
     ELSE x.r := y.lev
-    END ;
-    IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN ORS.Mark("not accessible ") END
+    END
   END MakeItem;
 
   (* Code generation for Selectors, Variables, Constants *)
@@ -772,11 +784,7 @@
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
@@ -1095,7 +1103,9 @@
     obj := ORB.topScope.next;
     WHILE obj # NIL DO  (*entries*)
       IF obj.exno # 0 THEN
-        IF (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) OR (obj.class = ORB.Var) THEN
+        IF (obj.class = ORB.Const) & (obj.type.form = ORB.String) THEN
+          Files.WriteInt(R, varsize + obj.val MOD 100000H) (*strx converted to SB-relative*)
+        ELSIF (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) OR (obj.class = ORB.Var) THEN
           Files.WriteInt(R, obj.val);
         ELSIF obj.class = ORB.Typ THEN
           IF obj.type.form = ORB.Record THEN Files.WriteInt(R,  obj.type.len MOD 10000H)
```

**ORP.Mod**

```diff
1c1
< MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07*)
---
> MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07 / AP 18.4.24*)
19c19
<     Type: PROCEDURE (VAR type: ORB.Type);
---
>     Type: PROCEDURE (VAR type: ORB.Type; expo, expoall: BOOLEAN);
39a40,41
>     ELSIF (obj.lev > 0) & (obj.lev # level) &
>       ((obj.class # ORB.Const) OR (obj.type.form # ORB.Proc)) THEN ORS.Mark("not accessible")
89a92,96
>   PROCEDURE CheckExported(type: ORB.Type);
>   BEGIN (*if type is a non-imported named type, check whether it is exported*)
>     IF (type.mno <= 0) & (type.typobj # NIL) & ~type.typobj.expo THEN ORS.Mark("type not exported") END
>   END CheckExported;
> 
612c619
<   PROCEDURE ArrayType(VAR type: ORB.Type);
---
>   PROCEDURE ArrayType(VAR type: ORB.Type; expo, expoall: BOOLEAN);
619c626,627
<     IF sym = ORS.of THEN ORS.Get(sym); Type(typ.base);
---
>     IF sym = ORS.of THEN ORS.Get(sym); Type(typ.base, expo, expoall);
>       IF expo THEN CheckExported(typ.base) END ;
621c629
<     ELSIF sym = ORS.comma THEN ORS.Get(sym); ArrayType(typ.base)
---
>     ELSIF sym = ORS.comma THEN ORS.Get(sym); ArrayType(typ.base, expo, expoall)
628c636
<   PROCEDURE RecordType(VAR type: ORB.Type);
---
>   PROCEDURE RecordType(VAR type: ORB.Type; expo, expoall: BOOLEAN);
631c639
<       offset, off, n: LONGINT;
---
>       offset, off, n: LONGINT; fldexpo, fldexpoall: BOOLEAN;
651c659
<       n := 0; obj := bot;
---
>       n := 0; obj := bot; fldexpo := FALSE; fldexpoall := TRUE;
658,660c666,670
<         IF (sym # ORS.comma) & (sym # ORS.colon) THEN ORS.Mark("comma expected")
<         ELSIF sym = ORS.comma THEN ORS.Get(sym)
<         END
---
>         IF new.expo THEN fldexpo := TRUE;  (*at least one fld exported*)
>           IF ~expoall THEN ORS.Mark("invalid field export") END
>         ELSE fldexpoall := FALSE
>         END ;
>         IF sym = ORS.comma THEN ORS.Get(sym) ELSIF sym # ORS.colon THEN ORS.Mark("comma expected") END
662c672,673
<       Check(ORS.colon, "colon expected"); Type(tp);
---
>       Check(ORS.colon, "colon expected"); Type(tp, expo & fldexpo, expoall & fldexpoall);
>       IF expo & fldexpo THEN CheckExported(tp) END ;
740c751
<   PROCEDURE Type0(VAR type: ORB.Type);
---
>   PROCEDURE Type0(VAR type: ORB.Type; expo, expoall: BOOLEAN);
752c763
<     ELSIF sym = ORS.array THEN ORS.Get(sym); ArrayType(type)
---
>     ELSIF sym = ORS.array THEN ORS.Get(sym); ArrayType(type, expo, expoall)
754c765
<       ORS.Get(sym); RecordType(type); Check(ORS.end, "no END")
---
>       ORS.Get(sym); RecordType(type, expo, expoall); Check(ORS.end, "no END")
770c781
<       ELSE Type(type.base);
---
>       ELSE Type(type.base, expo, expoall);
785c796
<       expo: BOOLEAN; id: ORS.Ident;
---
>       expo, expoall: BOOLEAN; id: ORS.Ident;
797,798c808,811
<         ORB.NewObj(obj, id, ORB.Const); obj.expo := expo;
<         IF x.mode = ORB.Const THEN obj.val := x.a; obj.lev := x.b; obj.type := x.type
---
>         ORB.NewObj(obj, id, ORB.Const); obj.expo := expo; obj.lev := level;
>         IF x.mode = ORB.Const THEN obj.type := x.type;
>           IF expo & (obj.type.form = ORB.String) THEN obj.exno := exno; INC(exno) ELSE obj.exno := 0 END ;
>           IF obj.type.form = ORB.String THEN obj.val := x.a (*strx*) + x.b (*len*) * 100000H ELSE obj.val := x.a END
800c813
<         END;
---
>         END ;
809c822,823
<         Type(tp);
---
>         Type(tp, expo, expo);
>         IF expo THEN CheckExported(tp) END ;
827c841,847
<         IdentList(ORB.Var, first); Type(tp);
---
>         IdentList(ORB.Var, first); obj := first; expo := FALSE; expoall := TRUE;
>         WHILE obj # NIL DO
>           IF obj.expo THEN expo := TRUE (*at least one var exported*) ELSE expoall := FALSE END ;
>           obj := obj.next
>         END ;
>         Type(tp, expo, expoall);
>         IF expo THEN CheckExported(tp) END ;
896c916
<   PROCEDURE Import;
---
>   PROCEDURE ImportList;
899,911c919,933
<     IF sym = ORS.ident THEN
<       ORS.CopyId(impid); ORS.Get(sym);
<       IF sym = ORS.becomes THEN
<         ORS.Get(sym);
<         IF sym = ORS.ident THEN ORS.CopyId(impid1); ORS.Get(sym)
<         ELSE ORS.Mark("id expected"); impid1 := impid
<         END
<       ELSE impid1 := impid
<       END ;
<       ORB.Import(impid, impid1)
<     ELSE ORS.Mark("id expected")
<     END
<   END Import;
---
>     REPEAT ORS.Get(sym);
>       IF sym = ORS.ident THEN
>         ORS.CopyId(impid); ORS.Get(sym);
>         IF sym = ORS.becomes THEN
>           ORS.Get(sym);
>           IF sym = ORS.ident THEN ORS.CopyId(impid1); ORS.Get(sym)
>           ELSE ORS.Mark("id expected"); impid1 := impid
>           END
>         ELSE impid1 := impid
>         END ;
>         ORB.Import(impid, impid1)
>       ELSE ORS.Mark("id expected")
>       END
>     UNTIL sym # ORS.comma
>   END ImportList;
919d940
<       ORB.Init; ORB.OpenScope;
926,930c947,948
<       IF sym = ORS.import THEN
<         ORS.Get(sym); Import;
<         WHILE sym = ORS.comma DO ORS.Get(sym); Import END ;
<         Check(ORS.semicolon, "; missing")
<       END ;
---
>       ORB.Init(modid); ORB.OpenScope;
>       IF sym = ORS.import THEN ImportList; Check(ORS.semicolon, "; missing") END ;
997c1015
< BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Compiler  8.3.2020");
---
> BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Compiler  8.3.2020 / AP 18.4.24");
```

**ORL.Mod**

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

**Notes on implementing improvement 9**

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

**Notes on implementing improvement 10**

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

**Notes on implementing improvement 11**

See [**Oberon-importing-string-constants**](http://github.com/andreaspirklbauer/Oberon-importing-string-constants).

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

**IMPLEMENTATION NOTE:** Consider the following example:

     MODULE D
       IMPORT A, B, C;
     END D.

and assume that modules B and C both re-export module A.

When creating the symbol files of modules A, B and C, each of these modules will use their "own" modtab for re-exported modules.

For example, module B might re-export module A using modref = 1 and module C might re-export module A using modref = 2 (procedure ORB.Export is called during *different* compilations, once during compilation of module B and once during compilation of module C).

Now, when module D imports module B, it will set modtab[1] = A, and when it imports module C, which will set modtab[2] = A.

Because the module references for module A are not the same in the symbol files of modules B and C, this method does not work across multiple modules A, B and C, but only if the same module A is re-imported multiple times by the same module. In the above example, module B might re-import module A three times.

Consequence: The *modtab* table can only be used on a per-imported-module basis. However, is not necessary to initialize *modtab* before *each* of the imports A, B and C (i.e. in *ORB.Import*). This is because the *modtab* table is only used to *store* a module reference, but not to *check* whether the module has already been read.

     Read(R, modref);
     IF modref # 0 THEN  (*re-import*)
       IF modref < 0 THEN mod := modtab[-modref]  (*already read*)
       ELSE ... modtab[modref] := mod   (*<--- simply overwrite any values that might exist from the import of another module*)
       END
     END

Any existing entry modtab[modref] - stemming from the import of another module - will simply be overwritten.

Note: The *modtab* table could be declared local to *ORB.Import*, but it is, just like the *typtab* table, declared globally. To make the garbage collector collect all module references *after* a compilation, all entries are set to NIL at the end of *ORB.Export*.

    FOR modRef := 0 TO maxModTab-1 DO modtab[modRef] := NIL END ;

-------------------------------------------------------------------------------------
**Implementing Variant 1**

**ORB.Mod**

```diff
--- Oberon-module-imports/Sources/FPGAOberon2013/ORB.Mod	2024-04-18 12:29:22
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant1/ORB.Mod	2024-04-18 12:39:35
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
+    ModDesc* = RECORD (ObjDesc) orgname*: ORS.Ident; modref: INTEGER END ;  (*modref is only used for import/export*)
 
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
+            IF mod(Module).modref > 0 THEN (*module was already output*) Write(R, -mod(Module).modref)
+            ELSE Write(R, modRef); mod(Module).modref := modRef; INC(modRef);
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
@@ -339,6 +347,7 @@
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
--- Oberon-module-imports/Sources/FPGAOberon2013/ORB.Mod  2024-04-18 12:29:22
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant2/ORB.Mod  2024-04-18 12:32:52
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
--- Oberon-module-imports/Sources/FPGAOberon2013/ORG.Mod  2024-04-18 12:29:22
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant2/ORG.Mod  2024-04-18 12:32:53
@@ -330,7 +330,7 @@
 
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
--- Oberon-module-imports/Sources/FPGAOberon2013/ORP.Mod  2024-04-18 12:29:23
+++ Oberon-module-imports/Sources/FPGAOberon2013/Variant2/ORP.Mod  2024-04-18 12:32:54
@@ -96,7 +96,7 @@
 
   PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
   BEGIN (*t1 is an extension of t0*)
-    RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
+    RETURN (t0 = t1) OR (t1 # ORB.noType) & IsExtension(t0, t1.base)
   END IsExtension;
 
   (* expressions *)
@@ -105,7 +105,7 @@
     VAR xt: ORB.Type;
   BEGIN xt := x.type;
     IF (T.form = xt.form ) & ((T.form = ORB.Pointer) OR (T.form = ORB.Record) & (x.mode = ORB.Par)) THEN
-      WHILE (xt # T) & (xt # NIL) DO xt := xt.base END ;
+      WHILE (xt # T) & (xt # ORB.noType) DO xt := xt.base END ;
       IF xt # T THEN xt := x.type;
         IF xt.form = ORB.Pointer THEN
           IF IsExtension(xt.base, T.base) THEN ORG.TypeTest(x, T.base, FALSE, guard); x.type := T
@@ -637,7 +637,7 @@
     VAR obj, obj0, new, bot, base: ORB.Object;
       typ, tp: ORB.Type;
       offset, off, n: LONGINT; fldexpo, fldexpoall: BOOLEAN;
-  BEGIN NEW(typ); typ.form := ORB.NoTyp; typ.base := NIL; typ.mno := -level; typ.nofpar := 0; offset := 0; bot := NIL;
+  BEGIN NEW(typ); typ.form := ORB.NoTyp; typ.base := ORB.noType; typ.mno := -level; typ.nofpar := 0; offset := 0; bot := NIL;
     IF sym = ORS.lparen THEN
       ORS.Get(sym); (*record extension*)
       IF level # 0 THEN ORS.Mark("extension of local types not implemented") END ;
```
