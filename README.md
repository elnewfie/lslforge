# LSLForge

An LSL Script Editor/compiler for Second Life's Scripting Language.

<img src="doc/logo100x100.jpg" align="right" width="100px" height="100px" margin="20px" padding="20px"/>

LSLForge is a fork of the popular LSLPlus editing evironment, to provide Linden Scripting Language (LSL) support in Eclipse.

## Fork Purpose

This fork is to maintain LSL definitions for functions and constants, as well as maintaining Eclipse Plugin Update Site.

### News (newest first)

* 2018-09-10 LSLForge **0.1.9.4** (**Windows**, **Mac** Only - **TESTING IN PROGRESS**)
    * Fixed ``Tuple*.java`` disappearance (Thanks [@PellSmit](https://github.com/PellSmit))
    * Upgraded Haskell (Thanks [@simon-nicholls](https://github.com/simon-nicholls))
    * HTTP_USER_AGENT
    * OBJECT_RENDER_WEIGHT
    * key llName2Key(string name)
    * key llRequestUserKey(string name)
* 2017-02-10 LSLForge **0.1.9.3** (**Windows**, **Linux** (Thanks [@Trapez](https://github.com/Trapez)), **Mac** (Thanks [@PellSmit](https://github.com/PellSmit)))
    * ATTACH_FACE_TONGUE misspelled
* 2017-01-07 LSLForge **0.1.9.2** (**Windows**, **Linux** (Thanks [@Trapez](https://github.com/Trapez)), **Mac** (Thanks [@PellSmit](https://github.com/PellSmit)))
    * OBJECT_ATTACHED_SLOTS_AVAILABLE
    * llGetEnv("region_object_bonus") (in comments)
* 2016-11-13 LSLForge **0.1.9.1** (**Windows**, **Linux** (Thanks [@Trapez](https://github.com/Trapez)), **Mac** (Thanks [@PellSmit](https://github.com/PellSmit)))
    * OBJECT_GROUP_TAG, OBJECT_TEMP_ATTACHED added
    * Bug fixes (Thanks [@PellSmit](https://github.com/PellSmit)):
        * [#35](https://github.com/raysilent/lslforge/issues/35) (negative out of range index)
        * [#6](https://github.com/raysilent/lslforge/issues/6) (backslash in string)
        * [#26](https://github.com/raysilent/lslforge/issues/26) (multiline string bug)
    * Bug fix [#37](https://github.com/raysilent/lslforge/issues/37) (cannot Run -> Run as -> Launch in LSL Sim)
    * Bug fix some null pointer exceptions during recompiled
* 2016-11-08 LSLForge **0.1.9** (Windows, Linux, Mac)
    * JSON_APPEND, CLICK_ACTION_ZOOM added
* 2016-10-22 LSLForge **0.1.8** (Windows only)
    * XP_ERROR_REQUEST_PERM_TIMEOUT missing added
* 2016-10-20 LSLForge **0.1.7** (Windows only)

## Description

LSLForge is a plugin for the Eclipse platform. The plugin allows editing, "compiling" (gathering code included in modules), executing, and unit testing your Second Life® Linden Scripting Language (LSL) code.

Because it is an Eclipse plugin, when you use LSL Plus you can take advantage of many of the useful features of Eclipse. Integrated support of a source code control system such as Git, CVS and Subversion comes for "free", and there are many other features -- task list management, integration with bug tracking tools, etc. You will of course need to make sure you get the appropriate plugins to use these features.

## Second Life® Group

The official group for LSLForge Editor tool is [LSLForge Users](secondlife:///app/group/381ff28c-1171-27ac-77f5-ded3471b6245/about). General announcements, questions and answers.

## Installing

### Eclipse Plugin

Any of the Eclipse installations were found working:

* Eclipse Juno RC2 (4.2.2)
* Eclipse Luna (4.4.0)
* Eclipse Mars.1 (4.5.1)
* Eclipse Mars.2 (4.5.2)
* Eclipse Neon (4.6.0)

To install a plugin into Eclipse, choose ``Help`` > ``Install New Software``. Click ``Add...``, give it a name and enter the link for location:

``https://raw.githubusercontent.com/elnewfie/lslforge/master/eclipse/``

This way you'll get the newest release.

Alternatively you may switch to a development fork and try a specific version since ``0.1.8`` (including work in progress branches):

* ``https://raw.githubusercontent.com/raysilent/lslforge/0.1.9.4/eclipse/``
* ``https://raw.githubusercontent.com/raysilent/lslforge/0.1.9.3/eclipse/``
* ``https://raw.githubusercontent.com/raysilent/lslforge/0.1.9.2/eclipse/``
* ``https://raw.githubusercontent.com/raysilent/lslforge/0.1.9.1/eclipse/``
* ``https://raw.githubusercontent.com/raysilent/lslforge/0.1.9/eclipse/``
* ``https://raw.githubusercontent.com/raysilent/lslforge/0.1.8/eclipse/``

For even older version, clone the whole repo and link your Eclipse to a particular folder under ``eclipse\archive``.

> If you don't see any items for installing, try to uncheck "Group items by category"

> Run ``eclipse -clean`` to make it forget cached downloads

Checkbox 2 items:

* "LSLForge"
* One of the native parts according to your environment.

Install, accept and restart Eclipse

Switch to **LSLForge Perspective** and create a new LSLForge Project

## Known Issues

* ``*.lslp`` files compilation issues although everything is correct. It may happen when a lot of ``$import`` keywords are used and at some point the compiler gets stuck. What may help is:

    * Adding a fake ``*.lslm`` module along the project, it could be called ``Fake.lslm``. Opening it and adding a space, then removing it and hitting **Save** will force the project to be recompiled
    * Forcing recompilation of a module that is referenced by ``*.lslp`` file by opening it, doing some fake change, and hitting **Save**

## Tips & Tricks

### Importing Modules

This demonstrates:

* How to use folders when importing modules (dot notation)
* How to import a module with a paramater

**`Modules/Debug.lslm`** :

```
$module (integer DEBUG)
// pragma inline
bug(string place, string message) {
  if (DEBUG) llOwnerSay("["+llGetScriptName()+"."+place+"]: "+message);
}
```

**`Script.lslp`** :

```
integer DEBUG=TRUE; // has to be a variable
$import Modules.Debug.lslm(DEBUG=DEBUG) de;

do() {
  debug("do()", "This is a call of 'bug' function from 'de' module");
}

```

### Referencing Modules From Other Projects

Imagine you move ``Modules`` folder to a separate project called ``ModulesProject`` to use it from different other projects.
In the main project that uses ``ModulesProject``, place a checkmark along its name under ``Project settings > Project References``.

``ModulesProject`` directory tree becomes part of the project's tree. It will still be imported as ``$import Modules.Debug.lslm`` without any additions.

## Native Library Compilation Example

### Requirements

To compile the native LSLForge binary, you must have the cross-platform Haskell `Stack` tool installed. Stack can then ensure that the correct compiler and dependencies for the project will be automatically downloaded and installed for you.

To install Stack, please visit the [Stack Homepage](https://www.haskellstack.org/) and follow the instructions. It is likely that you can find a package available for many package managers e.g. chocolatey, homebrew and pacman, but check before installing that their stack version is up to date.

#### Configure stack folder if necessary:
* ``STACK_ROOT`` environment variable if you do not want stack files appear under ``C:\sr`` under Windows.
* To configure downloaded programs location, open stack's root `config.yaml` and add a line `local-programs-path: <path>` with the path desired, after that commands like `stack ghci` will download files right into that folder
* To configure where `stack install` will place the files, add the following line to `config.yaml`: ``local-bin-path: <path>`` with the path desired. This folder may be added to the ``PATH`` environment variable. (The reminder will be given after ``stack install`` copies the file there).

### Compiling Haskell native LSLForge binary

In a terminal, change directory to the project's `lslforge/haskell` subdirectory, and enter `stack install` to build and install the LSLForge binary.

You will need to enter `stack setup` beforehand, if you have freshly installed Stack, or don't have the relevant compiler already set up. Stack will tell you if you need to run this additional step.

### Post-compilation

If your "install" was successful, an executable will appear at ``%APPDATA%\local\bin`` folder for Windows, or ``$HOME/.local/bin`` for other platforms (look at the message after install) - unless you changed the ``local-bin-path`` parameter in ``confif.yaml`` to override default location

Now you only need to specify this executable in Eclipse, ``Preferences`` > ``LSLForge`` settings.

**Eclipse should be restarted**

### Running tests

You can optionally run the LSLForge binary tests by executing `stack test` in the terminal, from the haskell subdirectory.

The tests require that the `LSLFORGE_TEST_DATA` environment variable be set for the terminal session, but for casual needs you can also use `LSLFORGE_TEST_DATA=../testing/data stack test`.
