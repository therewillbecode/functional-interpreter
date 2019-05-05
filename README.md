# Functional Interpeter
An Interpreter for a Simple Functional Language

<img src="https://cdn-images-1.medium.com/max/2600/1*ntgBBkoEipqEHD-NpgAspg.png">

You would write a `compose` function (.) as follows.
```Lambda "compose" (Lambda "g" (Lambda "x" (FunCall (Variable "f") (FunCall (Variable "g") (Variable "x))))```

Define a function which adds one to argument ((+1))
```(Lambda "plusOne" (Add (Variable "x") (Number 1)))```

Compose our plusOnes twice which evaluates to a function which adds 2 to the argument.
```(FunCall (FunCall (Variable "compose") (Variable "plusOne")) (Variable "plusOne"))```
