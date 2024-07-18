https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm 

## Grammar

* `(<>)*` = 0 or more
* `(<>)+` = 1 or more
* `(<>)?` = 0 or 1

`declaration-specifier` = `storage_class`
| `type-specifier`
| `type-qualifier`

`storage_class` =`auto`
| `register`
| `static`
| `extern`
| `typedef`

`type-qualifier` = `const` | `volatile`

`type-specifier` = `void`
| `char`
| `short`
| `int`
| `long`
| `float`
| `double`
| `signed`
| `unsigned`
| ~~`struct-or-union-specifier`~~
| `enum-specifier`
| `typedef-name`

`typedef-name` = `identifier`

`enum-specifier` =  `enum` `identifier`
| `enum` `{` `enumerator-list` `}`
| `enum` `identifier` `{` `enumerator-list` `}`

`enumerator-list` = `enumerator`
| `enumerator-list` `,` `enumerator`

`enumerator` = `identifier`
| `identifier` `=` `constant-expression`

`declaration` = `(declaration-specifier)+` `(init-declarator)*` `;`

`init-declarator` = `declarator`
| `declarator` `=` `initializer`

`initializer` = `assignment-expression`
| `{` `initializer-list` `}`
| `{` `initializer-list` `,` `}`

`declarator` = `(pointer)?` `direct-declarator`

`pointer` = `*` `(type-qualifier)*` `pointer`

`direct-declarator` = `identifier`
| `(` `declarator` `)`
| `direct-declarator` `[` `(constant-expression)?` `]`
| ~~`direct-declarator` `(` `parameter-type-list` `)`~~
| ~~`direct-declarator` `(` `(identifier)*` `)`~~


### Expression

...
