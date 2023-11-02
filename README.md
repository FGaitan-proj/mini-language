Required Files to run wml:

C.class  
CSupport.class  
basics.wml  
basics.wml.j  
driver.amd64-linux  or  driver.amd64-darwin
jasmin.jar


https://formulae.brew.sh/cask/smlnj
```
brew install --cask smlnj
brew install openjdk@17

```
echo 'export PATH=/usr/local/smlnj/bin:"$PATH"' >> ~/.zshrc


For the system Java wrappers to find this JDK, symlink it with
  sudo ln -sfn /usr/local/opt/openjdk@17/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-17.jdk

openjdk@17 is keg-only, which means it was not symlinked into /usr/local,
because this is an alternate version of another formula.

If you need to have openjdk@17 first in your PATH, run:
  echo 'export PATH="/usr/local/opt/openjdk@17/bin:$PATH"' >> ~/.zshrc

For compilers to find openjdk@17 you may need to set:
  export CPPFLAGS="-I/usr/local/opt/openjdk@17/include"

ml-build driver.cm Main.main

sml ./driver

sml @SMLload=driver compilePgm ../test.wml

java -jar jasmin.jar ../test.wml.j

java C

*   After building this driver, execute
*
*     $ ./driver
*
*   in the shell to get usage instructions.
*
 a structure for environments.
*
*  An environment is a map with domain Ast.ident.