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

## Installation (on MacOS)

### Step 1: Install SML/NJ

First, install Standard ML of New Jersey (SML/NJ) using Homebrew:
```sh
brew install --cask smlnj
```

### Step 2: Install OpenJDK
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
Build and Run the Project
### Step 4: Build the Driver
Build the driver using the following command:

```sh
ml-build driver.cm Main.main
```
### Step 5: Run the Driver
Execute the driver with:

```sh
sml ./driver
```

### Step 6: Compile the WML Program
Compile the WML program using:

```sh
sml @SMLload=driver compilePgm ../test.wml
```

### Step 7: Assemble the Java Bytecode
Assemble the Java bytecode with:

```sh
java -jar jasmin.jar ../test.wml.j
```
### Step 8: Run the Java Program
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
