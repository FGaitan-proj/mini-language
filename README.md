# WML Project Setup

## Overview

This guide provides step-by-step instructions to set up and run the WML project. The project requires specific files and dependencies, which are detailed below.

## Required Files

Make sure you have the following files in your project directory:
- `C.class`
- `CSupport.class`
- `basics.wml`
- `basics.wml.j`
- `driver.amd64-linux` or `driver.amd64-darwin`
- `jasmin.jar`

## Installation (MacOS)

### Step 1: Install SML/NJ

First, install Standard ML of New Jersey (SML/NJ) using Homebrew:
Follow the instructions from [SML/NJ Installation Guide](http://www.smlnj.org/dist/working/110.95/install.html).

```sh
brew install --cask smlnj
```

### Step 2: Install OpenJDK (on MacOS)
Next, install OpenJDK 17 using Homebrew:
```
brew install openjdk@17
```
### Step 3: Update Environment Variables
Add SML/NJ to your PATH by appending the following line to your ~/.zshrc file:

```sh
echo 'export PATH=/usr/local/smlnj/bin:"$PATH"' >> ~/.zshrc
```
For the system Java wrappers to find this JDK, symlink it with the following command:

```sh
sudo ln -sfn /usr/local/opt/openjdk@17/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-17.jdk
```
Since OpenJDK 17 is keg-only, it is not symlinked into /usr/local by default. To have OpenJDK 17 first in your PATH, add this line to your ~/.zshrc file:

```sh
echo 'export PATH="/usr/local/opt/openjdk@17/bin:$PATH"' >> ~/.zshrc
```
For compilers to find OpenJDK 17, set the following environment variable:

```sh
export CPPFLAGS="-I/usr/local/opt/openjdk@17/include"
```

## Installation (Linux)

### Step 1: Download and Install SML/NJ

Follow the instructions from [SML/NJ Installation Guide](http://www.smlnj.org/dist/working/110.95/install.html).

### Step 2: Move the `bin` Directory

Move the `bin` directory to `/usr/bin`:
```sh
sudo mv bin/ /usr/bin
```
### Step 3: Update Environment Variables
Add SML/NJ to your PATH by appending the following line to /etc/bash.bashrc and ~/.zshrc:

```sh
echo 'export PATH=/usr/local/smlnj/bin:"$PATH"' >> /etc/bash.bashrc
echo 'export PATH=/usr/local/smlnj/bin:"$PATH"' >> ~/.zshrc
```
### Step 4: Install OpenJDK
Install OpenJDK 17 headless version:

```sh
sudo apt install openjdk-17-jre-headless
```
### Step 5: Symlink OpenJDK
For the system Java wrappers to find this JDK, symlink it:

```sh
sudo ln -sfn /usr/lib/jvm/java-17-openjdk-amd64 /usr/lib/jvm/default-java
```
### Step 6: Set Compiler Flags
For compilers to find OpenJDK 17, set the following environment variable in /etc/bash.bashrc and ~/.zshrc:

```sh
export CPPFLAGS="-I/usr/lib/jvm/java-17-openjdk-amd64/include"
```


## Build and Run WML

### Step 1: Build the Driver
Build the driver using the following command:

```sh
ml-build driver.cm Main.main
```
### Step 2: Run the Driver
Execute the driver with:

```sh
sml ./driver
```

### Step 3: Compile the WML Program
Compile the WML program using:

```sh
sml @SMLload=driver compilePgm ../test.wml
```

### Step 4: Assemble the Java Bytecode
Assemble the Java bytecode with:

```sh
java -jar jasmin.jar ../test.wml.j
```
### Step 5: Run the Java Program
Finally, run the Java program using:

```sh
java C
```
Usage Instructions
After building the driver, you can execute it with:

```sh
./driver
```
Follow the on-screen instructions to use the program.


