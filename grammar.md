based on: https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

## Grammar

* `(<>)*` = 0 or more
* `(<>)+` = 1 or more
* `(<>)?` = 0 or 1
* ~~crossed rule~~ = not implemented yet

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

`expression` = `assignment-expression`
| `expression` `,` `expression`

`assignment-expression` = `conditional-expression`
| `unary-expression` `assignment-operator` `assignment-expression`

`assignment-operator` = `=` | `*=` | `/=` | `%=` | `+=` | `-=` | `<<=` | `>>=` | `&=` | `^=` | `|=`

`unary-operator` = `&` | `*` | `+` | `-` | `~` | `!`

`conditional-expression` = `logical-or-expression`
| `logical-or-expression` `?` `expression` `:` `expression`

`logical-or-expression` = `logical-and-expression`
| `logical-or-expression` `||` `logical-and-expression`

`logical-and-expression` = `inclusive-or-expression`
| `logical-and-expression` `&&` `inclusive-or-expression`

`inclusive-or-expression` = `exclusive-or-expression`
| `inclusive-or-expression` `|` `exclusive-or-expression`

`exclusive-or-expression` = `and-expression`
| `exclusive-or-expression` `^` `and-expression`

`and-expression` = `equality-expression`
| `and-expression` `&` `equality-expression`

`equality-expression` = `relational-expression`
| `equality-expression` `==` `relational-expression`
| `equality-expression` `!=` `relational-expression`

`relational-expression` = `shift-expression`
| `relational-expression` `<` `shift-expression`
| `relational-expression` `>` `shift-expression`
| `relational-expression` `<=` `shift-expression`
| `relational-expression` `>=` `shift-expression`

`shift-expression` = `additive-expression`
| `shift-expression` `<<` `additive-expression`
| `shift-expression` `>>` `additive-expression`

`additive-expression` = `multiplicative-expression`
| `additive-expression` `+` `multiplicative-expression`
| `additive-expression` `-` `multiplicative-expression`

`multiplicative-expression` = `cast-expression`
| `multiplicative-expression` `*` `cast-expression`
| `multiplicative-expression` `/` `cast-expression`
| `multiplicative-expression` `%` `cast-expression`

`cast-expression` = `unary-expression`
| `(` `type-name` `)` `cast-expression`

`unary-expression` = `postfix-expression`
| `++` `unary-expression`
| `--` `unary-expression`
| `unary-operator` `cast-expression`
| `sizeof` `unary-expression`
| `sizeof` `(` `type-name` `)`

`postfix-expression` = `primary-expression`
| `postfix-expression` `[` `expression` `]`
| `postfix-expression` `(` `{<assignment-expression>}*` `)`
| `postfix-expression` `.` `identifier`
| `postfix-expression` `->` `identifier`
| `postfix-expression` `++`
| `postfix-expression` `--`

`primary-expression` = `identifier`
| `constant`
| `string`
| `(` `expression` `)`

`constant` = `integer-constant`
| `character-constant`
| `floating-constant`
| `enumeration-constant`

`type-name` = `(<specifier-qualifier>)+` ~~`(<abstract-declarator>)?`~~

`specifier-qualifier` = `type-specifier`
| `type-qualifier`