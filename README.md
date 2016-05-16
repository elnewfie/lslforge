# LSLForge

<img src="doc/logo100x100.jpg" align="right" width="100px" height="100px" />

LSLForge is a fork of the popular LSLPlus editing evironment, to provide Linden Scripting Language (LSL) support in Eclipse.

## Fork Purpose

This particular fork is to maintain LSL definitions for functions and constants. Original one is not maintained at the moment, so you have to do some extra steps in order to get it updated as described below.


## Description

LSLForge is a plugin for the Eclipse platform. The plugin allows editing, "compiling" (gathering code included in modules), executing, and unit testing your Second Life® Linden Scripting Language (LSL) code.

Because it is an Eclipse plugin, when you use LSL Plus you can take advantage of many of the useful features of Eclipse. Integrated support of a source code control system such as CVS or Subversion comes for "free", and there are many other features -- task list management, integration with bug tracking tools, etc. You will of course need to make sure you get the appropriate plugins to use these features.
Second Life® Group

The official group for LSLForge Editor tool is LSLForge Users [secondlife:///app/group/381ff28c-1171-27ac-77f5-ded3471b6245/about]. General announcements, questions and answers.

## Installing

### Eclipse Plugin

To install a plugin into Eclipse, choose ``Help`` > ``Install New Software``. Add a site for updating:

``https://raw.githubusercontent.com/raysilent/lslforge/0.1.7/eclipse/``

Important! If you don't see any items for installing, try to uncheck "Group items by category"

Uncheck all except 2 items:

* "LSLForge"
* One of native parts according to your environment. 

Install, Accept, Reboot

Switch to LSLForge Perspective and create new LSLForge Project 

### New Definitions

Now you will find out that you need to recompile haskell piece in order to take advantage of all new constant and function definitions from this fork. Currently you'll have to do that manually as described below.

## Native Library Compilation Example

### Tested Environment

* Windows 8.1 64bit
* Eclipse:
    * Eclipse Luna Release (4.4.0)
    * Eclipse Mars.1 Release (4.5.1)

### Compiling Haskell native LSLForge binary

GHC 6.10.1 (http://www.haskell.org/ghc/download_ghc_6_10_1) should be used and after installation, system "Path" variable should be updated to include Haskell \bin directory.

For all below packages, downloaded to some temp folder, 3 steps should be done:

> Note: Some of these actions may require administrative rights. Run your command prompt as administrator in case you see errors.

```
runhaskell Setup.hs configure
runhaskell Setup.hs build
runhaskell Setup.hs install

(Setup may be called "Setup.lhs")

```

https://hackage.haskell.org/package/utf8-string-0.3.6

https://hackage.haskell.org/package/polyparse-1.1

https://hackage.haskell.org/package/pretty-1.0.1.0

https://hackage.haskell.org/package/HaXml-1.19.6

https://hackage.haskell.org/package/transformers-0.1.4.0

https://hackage.haskell.org/package/monads-fd-0.0.0.1

https://hackage.haskell.org/package/fclabels-0.4.2.1

https://hackage.haskell.org/package/binary-0.4.1

https://hackage.haskell.org/package/pureMD5-0.2.4

https://hackage.haskell.org/package/template-haskell-2.3.0.0

Now we configure, build, install LSLForge itself from its inner "haskell" folder in a similar way.

### Post-compilation

Exe-file will appear at ``C:\Program Files (x86)\Haskell\bin`` folder (look at the message after install). **Permission system may prevent file(s) copying to the folder**. Or it will reside close to source folder, if your "install" failed. You will be able to link to it anyway.

Now we specify this *.exe in Eclipse, ``Preferences`` > ``LSLForge`` settings.

Eclipse should be restarted

