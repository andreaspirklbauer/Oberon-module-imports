**CASE ANALYSIS OF ORB.ThisModule**

The following implementation of *ORB.ThisModule*

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope;
      IF decl THEN obj := obj1.next;  (*search for alias*)
        WHILE (obj # NIL) & ((obj.name # name) OR ~obj.rdo) DO obj := obj.next END  (*skip over re-imports*)
      ELSE obj := NIL
      END ;
      IF obj = NIL THEN obj := obj1.next;  (*search for module*)
        WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
        IF obj = NIL THEN (*insert new module*)
          NEW(mod); mod.class := Mod; mod.rdo := decl;
          mod.name := name; mod.orgname := orgname; mod.val := key;
          mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
          obj1.next := mod; obj := mod
        ELSE (*module already present*)
          IF obj.val # key THEN ORS.Mark("imported with bad key")
          ELSIF decl THEN (*explicit import by declaration*)
            IF obj.rdo THEN ORS.Mark("mult def") ELSE obj.name := name; obj.rdo := TRUE END
          END
        END
      ELSE ORS.Mark("mult def")
      END ;
      RETURN obj
    END ThisModule;

in conjunction with the following versions of *ORB.NewObj* and *ORB.thisObj* which **skip over** re-imports

    PROCEDURE NewObj*(VAR obj: Object; id: ORS.Ident; class: INTEGER);  (*insert new Object with name id*)
      VAR new, x: Object;
    BEGIN x := topScope;
      WHILE (x.next # NIL)
        & ((x.next.name # id) OR (x.next.class = Mod) & ~x.next.rdo) DO x := x.next END ;  (*skip over re-imports*)
      IF x.next = NIL THEN
        NEW(new); new.name := id; new.class := class; new.next := NIL; new.rdo := FALSE; new.dsc := NIL;
        x.next := new; obj := new
      ELSE obj := x.next; ORS.Mark("mult def")
      END
    END NewObj;

    PROCEDURE thisObj*(): Object;
      VAR s, x: Object;
    BEGIN s := topScope;
      REPEAT x := s.next;
        WHILE (x # NIL) & ((x.name # ORS.id) OR (x.class = Mod) & ~x.rdo) DO x := x.next END ;  (*skip over re-imports*)
        s := s.dsc
      UNTIL (x # NIL) OR (s = NIL);
      RETURN x
    END thisObj;

implements the Oberon import mechanism as follows:

a) It allows reusing the original module name if a module has been imported under an alias name, i.e. it allows the following scenarios

     MODULE A1; IMPORT M0 := M, M := M1; END A1. (*M imported under alias M1, M then reused as alias for M1*)
     MODULE A2; IMPORT M0 := M, M := M0; END A2. (*M imported under alias M0, M then reused as alias for M0 itself*)
     MODULE A3; IMPORT M0 := M, X := M0; END A3. (*M imported under alias M0, M0 then imported under alias X*)

b) It allows re-imports to co-exist with module alias names and globally declared identifiers

c) It allows an explicit import to come after a previous re-import of (types of) the same module, i.e. the following scenarios:

* a module M is explicitly imported once (via the IMPORT statement)
* a module M is first explicitly imported once, then re-imported N times
* a module M is re-imported once or multiple times (via types that have been re-exported by M)
* a module M is first re-imported once or multiple times, and then explicitly imported via the IMPORT statement

d) It assumes the following semantics of an *explicit* import under an alias name **IMPORT M := M1**:

* It is module M1 that is imported, not M.
* During compilation of the current module, the imported module M1 is given a temporary (alias) name M, such that an exported object x declared within M1 is referenced during compilation of the current module as M.x .
* After compilation of the current module, the alias name M is no longer used and does not appear anywhere (for example, it is not part of the generated symbol or object file). It's as if has never been there.

This semantics is in line with the official [**Oberon-07 language report**](http://inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf), which states: *If the form "M := M1" is used in the import list, an exported object x declared within M1 is referenced in the importing module as M.x .*

There are other possible semantics for the import statement that the language report - at least in principle - allows. For example, one could define the import statement *IMPORT M := M1* to mean that module M is imported, not M1, and that M1 is simply the name of the (symbol) file where the compiler should look for when importing M (file redirection). However, this semantics leads to issues with re-imports (for example, when the symbol file of M1 contains types that were re-imported from another module M0).

---------------------------------------------------

We will use the following notations and conventions:

* **<name, orgname>** = the name and orgname of the module to be imported
* **<obj.name, obj.orgname>** = an existing module object found in the module list (if one is found)

and

* **<X, M>** = explicit import by declaration, where the programmer wrote IMPORT X := M
* **[M, M]** = re-import of module M (note: re-imports don't ever carry alias names)

and

* **decl = TRUE** means that <name, orgname> is imported *explicitly* (via the IMPORT statement)
* **decl = FALSE** means that [name, orgname = name] is *re-imported* (via ORB.InType)

and

* **obj.rdo = TRUE** means that the found module object *obj* was originaly imported *explicitly*
* **obj.rdo = FALSE** means that the found module object *obj* was originaly *re-imported*

--------------------------------------------------------------------------------------------------------

Create two *separate* versions of ORB.ThisModule: one for **explicit imports** by declaration and one for **re-imports**

    (*explicit import by declaration = ORB.ThisModule with decl = TRUE*)

    PROCEDURE ExplicitImport(name, orgname: ORS.Ident; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;  (*search for alias*)
      WHILE (obj # NIL) & ((obj.name # name) OR ~obj.rdo) DO obj := obj.next END  (*skip over re-imports*)
      IF obj = NIL THEN obj := obj1.next;  (*search for module*)
        WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
        IF obj = NIL THEN (*insert new module*)
          NEW(mod); mod.class := Mod; mod.rdo := TRUE; (*!*)
          mod.name := name; mod.orgname := orgname; mod.val := key;
          mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
          obj1.next := mod; obj := mod
        ELSE (*module already present*)
          IF obj.val # key THEN ORS.Mark("imported with bad key")
          ELSE
            IF obj.rdo THEN ORS.Mark("mult def") ELSE obj.name := name; obj.rdo := TRUE END
          END
        END
      ELSE ORS.Mark("mult def")
      END ;
      RETURN obj
    END ExplicitImport;

    (*re-import = ORB.ThisModule with decl = FALSE*)

    PROCEDURE Reimport(name, orgname: ORS.Ident; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
      IF obj = NIL THEN (*insert new module*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE; (*!*)
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        obj1.next := mod; obj := mod
      ELSE (*module already present*)
        IF obj.val # key THEN ORS.Mark("imported with bad key") END
      END ;
      RETURN obj
    END Reimport;

---------------------------------------------------

**CASE ANALYSIS FOR EXPLICIT IMPORT BY DECLARATION**

    PROCEDURE ExplicitImport(name, orgname: ORS.Ident; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;  (*search for alias*)
      WHILE (obj # NIL) & ((obj.name # name) OR ~obj.rdo) DO obj := obj.next END  (*skip over re-imports*)
      IF obj = NIL THEN obj := obj1.next;  (*search for module*)
        WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;

        IF obj = NIL THEN (*insert new module*)

          (*CASE A: obj = NIL and for all objects obj in the module list:
                    (obj.orgname # orgname)  & ((obj.name # name) OR ~obj.rdo
                    i.e. the module is not present and
                    the alias name is not used in an explicit import,
                    but it may be used in a re-import*)

          NEW(mod); mod.class := Mod; mod.rdo := TRUE; (*!*)
          mod.name := name; mod.orgname := orgname; mod.val := key;
          mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
          obj1.next := mod; obj := mod

        ELSE (*module already present*)

          IF obj.val # key THEN ORS.Mark("imported with bad key")
          ELSIF obj.rdo THEN ORS.Mark("mult def")

            (*CASE B: obj # NIL & obj.rdo = TRUE and for all objects obj in the module list:
                      (obj.name # name) OR ~obj.rdo
                      i.e. the module is present as an explicit import (obj.rdo = TRUE)
                      the alias name is not used in an explicit import,
                      but it may be used in a re-import*)
		      
          ELSE

            (*CASE C: obj # NIL & obj.rdo = FALSE and for all objects obj in the module list:
                      (obj.name # name) OR ~obj.rdo
                      i.e. the module is present as an re-import (obj.rdo = FALSE)
                      the alias name is not used in an explicit import,
                      but it may be used in a re-import*)

          END

        END

      ELSE ORS.Mark("mult def")

        (*CASE D: obj # NIL & (obj.name = name) & obj.rdo
                  i.e. the alias name is used in an explicit import (obj.rdo = TRUE)*)

      END ;
      RETURN obj
    END ExplicitImport;

We must be either in case A, B, C or D (there are no other cases).

--------------------------------------------------------------------------------------------------------

**Case A:**

Since we are in case A, we must have:

       obj = NIL

and for all other objects obj in the module list:

       a. obj.orgname # orgname
       b. (obj.name # name) OR ~obj.rdo

Condition b. means that:

       b1. obj.name # name for all explicitly imported objects obj (i.e. with obj.rdo = TRUE)
       b2. either obj.name # name for all re-imported objects obj (i.e. with obj.rdo = FALSE)
       b3. or there is a re-imported object obj (i.e. with obj.rdo = FALSE) with obj.name = name

Cases b1 and b2 are trivial (we did not find anything in the module list), and we simply insert the new module.

Now let's focus on case b3:

Since the first loop ("search for alias") in *ORB.ThisImport* skips over re-imports (i.e. over objects with obj.rdo = FALSE), there could in fact be a re-imported module obj = [M, M] in the module list, with

       obj.name = name = M.   (which was skipped over)

But if that's the case, we can immediately see that in this case the programmer could not possibly have written:

    IMPORT M;          --> <name, orgname> = <M, M>

because *orgname = M* would have landed him in case B or C (since the second loop in *ORB.ThisModule* would have found the module).

So the programmer must have written an import statement with an *orgname* different from M:

    IMPORT M := M1;    --> <name, orgname> = <M, M1>

As noted above, it was a deliberate choice to allow this case, i.e. an alias name to co-exist with the name of a re-imported module.

Thus, a new module *M1* is added to the module list. The alias name M is *not* in conflict with the already existing re-import [M, M].

--------------------------------------------------------------------------------------------------------

**Case B:**

Since we are in case B, we must have:

       obj # NIL
       obj.rdo = TRUE  (i.e. we found an explicit import, unaliased or aliased)
       obj.orgname = orgname (= M)
       obj.name # name

Note: In this scenario, there could *not* be another object *obj1* with obj1.name & ~obj1.rdo in the module list, because a module M is either e re-import or an explicit import. It can't be both (a re-import can be convert to an explicit import though, see case C).
       
Now let's check which possible objects we may have found in the module list, given the above conditions

*Case B.1.* We found an **unaliased explicit import** <obj.name, obj.orgname> = <M, M> in the module list (obj.rdo = TRUE), i.e.

       a. IMPORT M;          => "mult def"  (don't allow importing the same module M twice)

*Case B.2.* We found an **aliased explicit import** <obj.name, obj.orgname> = <M, M1> in the module list (obj.rdo = TRUE), i.e.

       b. IMPORT M1 := M;    => "mult def"  (don't allow importing an existing module M under another alias name M1)

--------------------------------------------------------------------------------------------------------

**Case C:**

Since we are in case C, we must have:

       obj # NIL
       obj.rdo = FALSE  (i.e. we found an re-import, which by definition is unaliased)
       obj.orgname = orgname (= M)
       obj.name = name (=M)

Now let's check which possible objects we may have found in the module list, given the above conditions

The only possible object we could have found is a **re-import** [M, M] = <obj.name, obj.orgname> = <M, M> (obj.rdo = FALSE)

Since obj.orgname = orgname, the programmer must have written one of the following:

       a. IMPORT M;          => we convert the re-import to an explicit import
       b. IMPORT M1 := M;    => we convert the re-import to an explicit import
                                AND then change the alias name to M1

--------------------------------------------------------------------------------------------------------

**Case D:**

Since we are in case D, we must have:

       obj # NIL
       obj.rdo = TRUE  (i.e. we found an explicit import, unaliased or aliased)
       obj.name = name (= M)

i.e. the alias name is used in an explicit import (obj.rdo = TRUE)

This is a simple name conflict and we report a "mult def" error.

---------------------------------------------------

**CASE ANALYSIS FOR RE-IMPORT**

    PROCEDURE Reimport(name, orgname: ORS.Ident; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
      IF obj = NIL THEN (*insert new module*)

        (*CASE E: the module is not present*)

        NEW(mod); mod.class := Mod; mod.rdo := FALSE; (*!*)
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        obj1.next := mod; obj := mod

      ELSE (*module already present*)

        (*CASE F: obj.orgname = orgname => the module is present*)

        IF obj.val # key THEN ORS.Mark("imported with bad key") END

      END ;
      RETURN obj
    END Reimport;

We must be either in case E or F (there are no other cases).

--------------------------------------------------------------------------------------------------------

**Case E:**

Since we are in case E, we must have:

       obj = NIL

and for all objects obj in the module list:

       obj.orgname # orgname

This means that neither an explicit import nor a re-import with obj.orgname = M exits in the module list.

There could, however, be an **aliased explicit import** <M, M1> already present in the module list, i.e. an entry with alias name M

       obj.name = name = M

i.e. where the programmer previously wrote

       IMPORT M := M1

and we are now inserting a re-import [M, M] into the module list.

But the existing alias name M (for module M1) does not conflict with the newly inserted re-import [M, M] because a re-import.

--------------------------------------------------------------------------------------------------------

**Case F:**

Since we are in case F, we must have:

       obj # NIL
       obj.orgname = orgname

Now let's check which possible objects we may have found in the module list, given the above conditions

*Case F.1.* We found an **unaliased explicit import** <obj.name, obj.orgname> = <M, M> in the module list (obj.rdo = TRUE)

       => do nothing (module M was already explicitly imported, so we just return that module)

*Case F.2.* We found an **aliased explicit import** <obj.name, obj.orgname> = <M1, M> in the module list (obj.rdo = TRUE)

       => do nothing (module M was already explicitly imported (albeit with an alias), so we just return that module)

*Case F.3.* We found another **re-import** [M, M]

       => do nothing (module M was already re-imported before (via a different module), so we just return that module)
       => recall that a module M may be re-imported multiple times (via different modules)



