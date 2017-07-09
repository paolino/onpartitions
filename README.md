# Modification of part of a list, in place, as a whole.

This code is semantically equivalent to the application of *over* on *partsOf* from the astouding Control.Lens.

Aside being free of dependencies, the main feature is the possibility of changing the type by remapping everything to Either

Reverse multiple of 3 and show them , in place.

```
Prelude Data.List.OnPartition Data.Bool> onPartitionG (flip (bool Left Right) <*> (==) 0 . flip mod 3) (map show . reverse) [1..10]
[Left 1,Left 2,Right "9",Left 4,Left 5,Right "6",Left 7,Left 8,Right "3",Left 10]
```
