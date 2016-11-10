# LSLForge

<img src="doc/logo100x100.jpg" align="right" width="100px" height="100px" margin="20px" padding="20px"/>

LSLForge is a fork of the popular LSLPlus editing evironment, to provide Linden Scripting Language (LSL) support in Eclipse.

## Fork Purpose

This fork is to maintain LSL definitions for functions and constants, as well as maintaining Eclipse Plugin Update Site.

### News

* 2016-10-20 LSLForge **0.1.7** (Windows only) test release
* 2016-10-22 LSLForge **0.1.8** (Windows only) test release
    * Fixing XP_ERROR_REQUEST_PERM_TIMEOUT missing
* 2016-11-08 LSLForge **0.1.9** (Windows, Linux, Mac) test release
    * JSON_APPEND, CLICK_ACTION_ZOOM added

## Description

LSLForge is a plugin for the Eclipse platform. The plugin allows editing, "compiling" (gathering code included in modules), executing, and unit testing your Second Life® Linden Scripting Language (LSL) code.

Because it is an Eclipse plugin, when you use LSL Plus you can take advantage of many of the useful features of Eclipse. Integrated support of a source code control system such as Git, CVS and Subversion comes for "free", and there are many other features -- task list management, integration with bug tracking tools, etc. You will of course need to make sure you get the appropriate plugins to use these features.

## Second Life® Group

The official group for LSLForge Editor tool is LSLForge Users [secondlife:///app/group/381ff28c-1171-27ac-77f5-ded3471b6245/about]. General announcements, questions and answers.

## Installing

### Eclipse Plugin

Any of the Eclipse installations were found working:

* Eclipse Juno RC2 (4.2.2)
* Eclipse Luna (4.4.0)
* Eclipse Mars.1 (4.5.1)
* Eclipse Mars.2 (4.5.2)
* Eclipse Neon (4.6.0)

To install a plugin into Eclipse, choose ``Help`` > ``Install New Software``. Click ``Add...``, give it a name and enter the link for location:

``https://raw.githubusercontent.com/raysilent/lslforge/master/eclipse/``

> Important! If you don't see any items for installing, try to uncheck "Group items by category"

Uncheck all except 2 items:

* "LSLForge"
* One of native parts according to your environment. 

Install, accept and restart Eclipse

Switch to **LSLForge Perspective** and create a new LSLForge Project 

## Known Issues

* ``*.lslp`` files compilation issues although everything is correct. It may happen when a lot of ``$import`` keywords are used and at some point the compiler gets stuck. What may help is:

    * Adding a fake ``*.lslm`` module along the project, it could be called ``Fake.lslm``. Opening it and adding a space, then removing it and hitting **Save** will force the project to be recompiled
    * Forcing recompilation of a module that is referenced by ``*.lslp`` file by opening it, doing some fake change, and hitting **Save**

## Native Library Compilation Example

### Example Environment

* Windows 
    * 8.1 64bit
    * 10 64bit

> If you succeed with compilation for Linux or Mac please add an issue with step-by-step instructions

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

If your "install" was successful, exe-file will appear at ``C:\Program Files (x86)\Haskell\bin`` folder (look at the message after install). **Permission system may prevent file(s) copying to the folder**. It may as well reside close to source folder under **build**, if your "install" failed. You will be able to link to it anyway.

Now you only need to specify this *.exe in Eclipse, ``Preferences`` > ``LSLForge`` settings.

**Eclipse should be restarted**

