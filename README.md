# Introduction to Haskell TNG workshop

Prerequisites: interest to some abstract nonsense.

First steps:
* [Install stack](#how-to-install-stack)
* Clone the repository with exercises.
* run `stack run` in the project folder and see the welcome message.
* run `stack test` and verify that every test except 1 fail.
* run `stack ghci` to enter the REPL mode. Try something like `2 + 2`. Type `:q` to quit.

## IDEs

* `VSCode` + `Haskell` and `Haskell Syntax Highlighting` extensions. Requires haskell language server.
* `Sublime Text 3` + `SublimeHaskell`, `SublimeREPL` and `LSP` extensions. `LSP` courses periodical crashes though. 
* `IntelliJ` + `IntelliJ-Haskell` plugin. Didn't work for me very well, the plugin is super unstable.
* `Vim` + ???. Whoever know how to use vim (not me) may find this as the most convenient option

## Links and tools

* [Hoogle](https://hoogle.haskell.org/): the main source of information.
* [Hackage](https://hackage.haskell.org/): the Haskell community's central package archive.
* **Haskell**: a programming language. Key properties:
  * High-level
  * Functional Programming paradigm
  * Strong Static Typing
  * Lazy Evaluation
  * Garbage Collection
  * Compiled
  
* **GHC** (Glasgow Haskell Compiler): a compiler for the Haskell programming language.

* **GHCi** (GHC Interactive): an interactive environment for Haskell development. Allows you to interactively evaluate Haskell expressions, so perform some testing.

* **Stack**: a build tool and package manager for Haskell projects. It helps manage project dependencies, build configurations, and provides a consistent and reproducible development environment.

* **Cabal**: also a build tool, but less convenient in use than stack. But stack actually uses cabal.

* **GHCup**: a tool for installing and managing different versions of GHC, Stack, Cabal and language server.

* **HLS** (Haskell Language Server): a language server for Haskell. It provides IDE-like features such as code completion, type checking, and refactoring tools for Haskell development. It can be integrated with various code editors.

## How to install stack

If you have **Linux** or **Windows and the user folder does not contain spaces**:

1. Install `ghcup`: https://www.haskell.org/ghcup/install/
2. Install every option (`Stack` and `HLS`) it asks you about.
   * Verify installation with 
      ```bash 
      ghcup tui
      ``` 
   and check that `Stack`, `HLS`, `cabal`, `GHC` are installed.
   * Verify also that `stack` command is recognisable with
      ```bash
      stack --version
      ```
    * Verify that `HLS` is installed with
      ```bash
      haskell-language-server-wrapper --version
      ```
    * It might be that `HLS` version is not compatible with current `GHC` version. Use `ghcup tui` to install compatible, for example `HLS : 2.6.0.0` and `GHC: >= 9.4.8`

If you have Windows and your user folder contains spaces:
1. May the gods help you.
2. Option 1: rename your user folder using [instruction in the end](#how-to-change-user-folder) and follow previous steps. 
3. Option 2 (limited installation):
   * Install `stack` using https://get.haskellstack.org/stable/windows-x86_64-installer.exe.
   * It should install stack to `C:\sr`.
   * Open `C:\sr\config.yaml` and add there a line
     ```yaml
     local-programs-path: "SomePathWithoutSpaces"
     ```
     SomePathWithoutSpaces can be, for example `C:\Users\Public\stack`.
   * In folder `C:\sr\setup-exe-src` remove any `.o` and `.hi` files, leave only `.hs`.
   * You won't be able to use language-server for features like "Go to definition" (well, you can try to install it manually). 


## How to change User folder
* Enable administrative account with `Win + r` -> `cmd` ->
  ```shell
  net user administrator /active:yes
  ```
* `Ctrl+Alt+Del`, switch user to Administrator.
* Go to users, rename your user folder to something which does not contain spaces.
* **Important:** Create a symbolic link:
  * Open PowerShell in Users directory
  * Run the following command, where "Old name" is the name of your old user folder (with spaces), and "CurrentName" is what you have recently created
    ```shell
    New-Item -ItemType Junction -Path "Old name" -Target "CurrentName"
    ```
* Go back to your user (if you have done everything correctly, then it should load correctly).
* `Win + r` -> `regedit`
* Change `%USERPROFILE%` variable:
  * Navigate to `HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList`.
  * Find the profile with your old username. This can be done by clicking on each `S-1-5` folder and checking the `ProfileImagePath` entry.
    Once you find the correct profile, double-click on `ProfileImagePath` and change the value to the new path that reflects the new username.
* Change `%HOME%` variable
  * Navigate to `HKEY_CURRENT_USER\Environment`
  * On the right pane, look for a value named `HOME`
  * Change it according to your username
  * You can also find some other places the old path was used, for example `PATH` and change it there.