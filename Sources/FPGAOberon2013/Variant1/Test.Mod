MODULE Test; (*imports more than 15 modules*)

  IMPORT Kernel, FileDir, Files, Modules,            (*4*)
    Input, Display, Viewers, Fonts, Texts, Oberon,   (*6*)
    MenuViewers, TextFrames,                         (*2*)
    Graphics, GraphicFrames,                         (*2*)
    ORS, ORB, ORG, ORP;                              (*4*)

  PROCEDURE Go*;
  END Go;

END Test.

#
# BEFORE
#
ORP.Compile Test.Mod/s ~  # compilation succeeds
Test.Go ~                 # but loading results in a TRAP


#
# AFTER (with new versions of modules Modules, ORB, ORB and ORL)
#
ORP.Compile ORS.Mod/s ORB.Mod/s ~
ORP.Compile ORG.Mod/s ORP.Mod/s ~
ORP.Compile ORL.Mod/s ~
System.Free ORTool ORP ORG ORB ORS ORL ~

ORP.Compile Kernel.Mod/s FileDir.Mod/s Files.Mod/s Modules.Mod/s ~
ORL.Link Modules ~
ORL.Load Modules.bin ~

ORP.Compile Input.Mod/s Display.Mod/s Viewers.Mod/s ~
ORP.Compile Fonts.Mod/s Texts.Mod/s Oberon.Mod/s ~
ORP.Compile MenuViewers.Mod/s TextFrames.Mod/s ~
ORP.Compile System.Mod/s Edit.Mod/s Tools.Mod/s ~

ORP.Compile ORS.Mod/s ORB.Mod/s ORG.Mod/s ORP.Mod/s ~
ORP.Compile ORTool.Mod/s ORL.Mod/s ~

Now restart the system, then execute:

ORP.Compile Test.Mod/s ~  # compilation succeeds
Test.Go ~                 # loading NO LONGER results in a TRAP




