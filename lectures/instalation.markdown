---
title: Installing and running GHC with Stack
author: Lucilia FIgueiredo
---

We will install **Stack**, a modern tool to maintain different
versions of  **GHC** and to work with different packages. Perform the following steps:

  #. Install Stack. Visit
  <https://docs.haskellstack.org/en/stable/README/>  and follow the
  instructions  for your operating system.

  #. Check that Stack works for your system by running the
  command `stack --version`  at the command prompt.

  #. Check the latest GHC version by visiting
 <https://www.haskell.org/ghc/>. Set up GHC on your box by providing
 the GHC version number:

       ![](../images/ghc-version.png){width=50% height=50%}

  #. Pick up your editor. You can set up your favorite editor to edit
  Haskell code.  Preferable editors are [Emacs][emacs] and [Vi][vi]. Once you
  have picked up  your favorite editor, ensure that the executables
  for the editor  remain in your path or note down the full path to the executable.

  #. Let's create a new project, `hello`. Create the new project
  `hello` by  running the following command in the command prompt
  in an empty directory:
  
      ![](../images/stack-new-hello.png){width=50% height=50%}

  #. Change to project directory (`hello`) and run `stack setup`.  
  When  run from the new project directory, Stack
  automatically downloads  the  corresponding GHC and sets it up.

  #. Compile and build the project:
  
       ![](../images/stack-build.png){width=50% height=50%}
 
   #. You can now run the project using the following command:

       ![](../images/stack-exec.png){width=50% height=50%}

       You should see the reply someFunc printed on the console. It means
       that  the  program compilation and execution was successful. Inspect
       the `hello` project by opening an explorer (or file finder) and
       exploring the `hello` directory:
  
   #. Inspect the `hello project` by opening an explorer (or file
      finder)  and  exploring the `hello` directory.  The project
      contains  two main directories, `app` and `src`. The
      library  code goes into the `src` folder, whereas the
      `main`  executable producing code goes into the `app`
      folder. In the `hello` project directory, open the `app/Main.hs`
      file in the editor and enter the following source in the editor: 

             module Main where
	  
             -- Single line comment!
		     main :: IO ()
		     main = putStrLn "Hello World!”

   #. Save the source file and exit. Run `stack ghci`. You will
        see  that GHCi has successfully loaded the saved file:
		
		| [2 of 2] Compiling Main
		| ( d:\projects\hello\app\Main.hs, interpreted )
		| Ok, modules loaded: Lib, Main.
		| *Main>
	  
    #. Type `main` at GHCi prompt and you see the `Hello
	 World` message.
	 
   #. Exit the GHCi by running `:quit` in the prompt.
	 
   #. You can also rebuild and run the program by running the
        following  commands:
	 
        | stack build
	    | stack exec -- hello-exe
		
	   You will again see the output `Hello World`. 
	 

[emacs]: https://www.gnu.org/software/emacs/
[vi]: https://www.vim.org/

