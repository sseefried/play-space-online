# Play Space Online!


## Deploying

On an AWS micro instance you can do the following:

* sudo apt-get install ghc6
* sudo apt-get install cabal-install
* cabal install shady-graphics

Linking the application takes a lot of memory, too much for a micro instance.
Build the application on your own Linux machine (or Linux in a VirtualBox or Parallels perhaps?)
and copy the binary up to the micro instance.

Then run it inside the utility 'screen'.
