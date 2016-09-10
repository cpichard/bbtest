Black box tests runner 
======================

bbtest is a simple driver program to run functional tests. The tests should be organised in a directory structure and described in a yaml file. Some examples are provided, tailored for testing visual effect and animation softwares.

Compilation:
------------

install haskell stack: https://www.haskellstack.org/
then

```
stack install
```

Running bbtest examples
-----------------------

```
cd examples
bbtest example1.yaml
```
