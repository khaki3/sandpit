# fin
Gauche input-wrapper for flexible reading.
This library is inspired by [hellman/sock](https://github.com/hellman/sock).

## Installation
```bash
./configure && sudo make install
```

## Caution
This library can be used on only byte-string environments for binary-reading.

```scm
gosh> (gauche-character-encoding)
none
```
