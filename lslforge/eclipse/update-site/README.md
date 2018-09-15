* After calling compile_haskell_win32.bat (which does all the steps), you open up Eclipse RCP to generate
  the eclipse update site.

* Test if the sequence of inits in ../generated/InitAll.java matters
( it used to be:
        ....
        Maybe_Just.init(xstream);
        Tuple2.init(xstream);
        Tuple3.init(xstream);
        CodeElement.init(xstream);
        ....
)

* Remove "features", "plugins", "artifacts.jar", "content.jar" before attempting to generate the site. It won't do anything if
  they already exists (run ``clean.bat``)
* When testing on Eclipse, start clean or carefully remove every folder from eclipse cache.
  > Run ``eclipse -clean`` to force it to forget cached downloads

