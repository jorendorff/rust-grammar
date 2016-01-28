grammar Rust;

import xidstart, xidcontinue;


// === Modules and items

crate:
    mod_body EOF;

mod_body:
    inner_attr* item*;

item:
    attr* 'pub'? pub_item
    | attr* impl_block
    | attr* item_macro_use;

pub_item:
    extern_crate     // `pub extern crate` is deprecated but still allowed
    | use_decl
    | mod_decl_short
    | mod_decl
    | extern_mod
    | static_decl
    | const_decl
    | fn_decl
    | type_decl
    | struct_decl
    | enum_decl
    | trait_decl;

item_macro_use:
    Ident '!' item_macro_tail;

item_macro_tail:
    Ident? tt_parens ';'
    | Ident tt_brackets ';'
    | Ident? tt_block;


// --- extern crate

extern_crate:
    'extern' 'crate' Ident rename? ';';


// --- use declarations

use_decl:
    'use' use_path ';';

use_path:
    '::'? '{' use_item_list '}'
    | '::'? any_ident ('::' any_ident)* use_suffix?;

use_suffix:
    '::' '*'
    | '::' '{' use_item_list '}'
    | rename;

use_item:
    any_ident rename?;

use_item_list:
    use_item (',' use_item)* ','?;

rename:
    'as' Ident;


// --- Modules

mod_decl_short:
    'mod' Ident ';';

mod_decl:
    'mod' Ident '{' mod_body '}';


// --- Foreign modules

extern_mod:
    extern_abi '{' inner_attr* foreign_item* '}';

foreign_item:
    attr* 'pub'? foreign_item_tail
    | attr* item_macro_use;

foreign_item_tail:
    'static' 'mut'? Ident ':' ty_sum ';'
    | foreign_fn_decl;


// --- static and const declarations

static_decl:
    'static' 'mut'? Ident ':' ty '=' expr ';';

const_decl:
    'const' Ident ':' ty '=' expr ';';


// --- Functions

fn_decl:
    fn_head '(' param_list? ')' rtype? where_clause? block_with_inner_attrs;

method_decl:
    fn_head '(' method_param_list? ')' rtype? where_clause? block_with_inner_attrs;

trait_method_decl:
    fn_head '(' trait_method_param_list? ')' rtype? where_clause? (block_with_inner_attrs | ';');

foreign_fn_decl:
    fn_head '(' variadic_param_list? ')' rtype? where_clause? ';';

// Parts of a `fn` definition up to the type parameters.
//
// `const` and `extern` are incompatible on a `fn`, but this grammar
// does not rule it out, holding that in a hypothetical Rust language
// specification, it would be clearer to specify this as a semantic
// rule, not a syntactic one. That is, not every rule that can be
// enforced gramatically should be.
fn_head:
    'const'? 'unsafe'? extern_abi? 'fn' Ident ty_params?;

param:
    pat ':' ty;

param_list:
    param (',' param)* ','?;

variadic_param_list:
    param (',' param)* (',' '...')? ','?;

self_param:
    'mut'? 'self' (':' ty)?
    | '&' Lifetime? 'mut'? 'self';

method_param_list:
    (param | self_param) (',' param)* ','?;

// Argument names are optional in traits. The ideal grammar here would be
// `(pat ':')? ty`, but parsing this would be unreasonably complicated.
// Instead, the `pat` is restricted to a few short, simple cases.
trait_method_param:
    restricted_pat ':' ty
    | ty;

restricted_pat:
    ('&' | '&&' | 'mut')? ('_' | Ident);

trait_method_param_list:
    (trait_method_param | self_param) (',' trait_method_param)* ','?;

rtype:
    '->' (ty | '!');


// --- type, struct, and enum declarations

type_decl:
    'type' Ident ty_params? '=' ty ';';

struct_decl:
    'struct' Ident ty_params? struct_tail;

struct_tail:
    where_clause? ';'
    | '(' tuple_struct_field_list ')' where_clause? ';'
    | where_clause? '{' field_decl_list? '}';  // braced empty structs are unstable (#29720)

tuple_struct_field:
    attr* 'pub'? ty;

tuple_struct_field_list:
    tuple_struct_field (',' tuple_struct_field)* ','?;

field_decl:
    attr* 'pub'? Ident ':' ty;

field_decl_list:
    field_decl (',' field_decl)* ','?;

enum_decl:
    'enum' Ident ty_params? '{' enum_variant_list? '}';

enum_variant:
    attr* enum_variant_main;

enum_variant_list:
    enum_variant (',' enum_variant)* ','?;

enum_variant_main:
    Ident '(' enum_tuple_field_list ')'
    | Ident '{' enum_field_decl_list '}'
    | Ident '=' expr
    | Ident;

// enum variants that are tuple-struct-like can't have `pub` on individual fields.
enum_tuple_field:
    attr* ty;

enum_tuple_field_list:
    enum_tuple_field (',' enum_tuple_field)* ','?;

// enum variants that are struct-like can't have `pub` on individual fields.
enum_field_decl:
    Ident ':' ty;

enum_field_decl_list:
    enum_field_decl (',' enum_field_decl)* ','?;


// --- Traits

trait_decl:
    'unsafe'? 'trait' Ident ty_params? colon_bound? where_clause? '{' trait_item* '}';

trait_item:
    attr* 'type' Ident colon_bound? ty_default? ';'
    | attr* 'const' Ident ':' ty_sum const_default? ';'  // experimental associated constants
    | attr* trait_method_decl;

ty_default:
    '=' ty;

const_default:
    '=' expr;


// --- impl blocks

impl_block:
    'unsafe'? 'impl' ty_params? impl_what where_clause? '{' impl_item* '}';

impl_what:
    '!' ty_sum 'for' ty_sum
    | ty_sum 'for' ty_sum
    | ty_sum 'for' '..'
    | ty_sum;

impl_item:
    attr* 'pub'? impl_item_tail;

impl_item_tail:
    method_decl
    | 'type' Ident '=' ty ';'
    | const_decl  // experimental associated constants
    | Ident '!' tt_parens ';'
    | Ident '!' tt_block;


// === Attributes and token trees

attr:
    '#' '[' tt* ']';

inner_attr:
    '#' '!' '[' tt* ']';

tt:
    ~('(' | ')' | '{' | '}' | '[' | ']')
    | tt_delimited;

tt_delimited:
    tt_parens
    | tt_brackets
    | tt_block;

tt_parens:
    '(' tt* ')';

tt_brackets:
    '[' tt* ']';

tt_block:
    '{' tt* '}';

macro_tail:
    '!' tt_delimited;


// === Paths
// (forward references: ty_sum, ty_args)

// This is very slightly different from the syntax read by rustc:
// whitespace is permitted after `self` and `super` in paths.
//
// In rustc, `self::x` is an acceptable path, but `self :: x` is not,
// because `self` is a strict keyword except when followed immediately
// by the exact characters `::`. Same goes for `super`. Pretty weird.
//
// So instead, this grammar accepts that `self` is a keyword, and
// permits it specially at the very front of a path. Whitespace is
// ignored. `super` is OK anywhere except at the end.
//
// Separately and more tentatively: in rustc, qualified paths are
// permitted in peculiarly constrained contexts. In this grammar,
// qualified paths are just part of the syntax of paths (for now -
// this is not clearly an OK change).

path:
    path_segment_no_super
    | path_parent? '::' path_segment_no_super;

path_parent:
    'self'
    | '<' ty_sum as_trait? '>'
    | path_segment
    | '::' path_segment
    | path_parent '::' path_segment;

as_trait:
    'as' ty;

path_segment:
    path_segment_no_super
    | 'super';

path_segment_no_super:
    simple_path_segment ('::' ty_args)?;

simple_path_segment:
    Ident
    | 'Self';


// === Type paths
// (forward references: ty_list, rtype, ty_sum, ty_args)

ty_path:
    for_lifetime? ty_path_main;

for_lifetime:
    'for' '<' lifetime_def_list '>';

lifetime_def_list:
    lifetime_def (',' lifetime_def)* ','?;

lifetime_def:
    Lifetime (':' lifetime_bound)?;

lifetime_bound:
    Lifetime
    | lifetime_bound '+' Lifetime;

ty_path_main:
    ty_path_tail
    | ty_path_parent? '::' ty_path_tail;

ty_path_tail:
    (Ident | 'Self') '(' ty_list? ')' rtype?
    | ty_path_segment_no_super;

ty_path_parent:
    'self'
    | '<' ty_sum as_trait? '>'
    | ty_path_segment
    | '::' ty_path_segment
    | ty_path_parent '::' ty_path_segment;

ty_path_segment:
    ty_path_segment_no_super
    | 'super';

ty_path_segment_no_super:
    (Ident | 'Self') ty_args?;


// === Type bounds

where_clause:
    'where' where_bound_list;

where_bound_list:
    where_bound (',' where_bound)* ','?;

where_bound:
    Lifetime ':' lifetime_bound
    | for_lifetime? ty colon_bound;

colon_bound:
    ':' bound;

bound:
    prim_bound
    | bound '+' prim_bound;

prim_bound:
    ty_path
    | '?' ty_path
    | Lifetime;


// === Types and type parameters

ty:
    '_'
    // The next 3 productions match exactly `'(' ty_sum_list? ')'`,
    // but (i32) and (i32,) are distinct types, so parse them with different rules.
    | '(' ')'                           // unit
    | '(' ty_sum ')'                    // grouping (parens are ignored)
    | '(' ty_sum ',' ty_sum_list? ')'   // tuple
    | '[' ty (';' expr)? ']'
    | '&' Lifetime? 'mut'? ty
    | '&&' Lifetime? 'mut'? ty          // meaning `& & ty`
    | '*' mut_or_const ty               // pointer type
    | for_lifetime? 'unsafe'? extern_abi? 'fn' '(' trait_method_param_list? ')' rtype?
    | ty_path;

mut_or_const:
    'mut'
    | 'const';

extern_abi:
    'extern' StringLit?;

ty_list:
    ty (',' ty)* ','?;

ty_args:
    '<' lifetime_list '>'
    | '<' (Lifetime ',')* ty_arg_list '>';

ty_sum:
    ty ('+' bound)?;

ty_sum_list:
    ty_sum (',' ty_sum)* ','?;

ty_arg:
    Ident '=' ty
    | ty_sum;

ty_arg_list:
    ty_arg (',' ty_arg)* ','?;

ty_params:
    '<' lifetime_param_list '>'
    | '<' (lifetime_param ',')* ty_param_list '>';

lifetime_list:
    Lifetime (',' Lifetime)* ','?;

lifetime_param:
    Lifetime (':' lifetime_bound)?;

lifetime_param_list:
    lifetime_param (',' lifetime_param)* ','?;

ty_param:
    Ident colon_bound? ty_default?;

ty_param_list:
    ty_param (',' ty_param)* ','?;


// === Patterns

pat:
    pat_no_mut
    | 'mut' Ident ('@' pat)?;

// A `pat_no_mut` is a pattern that does not start with `mut`.
// It is distinct from `pat` to rule out ambiguity in parsing the
// pattern `&mut x`, which must parse like `&mut (x)`, not `&(mut x)`.
pat_no_mut:
    '_'
    | pat_lit
    | pat_range_end '...' pat_range_end
    | Ident macro_tail
    | 'ref'? Ident ('@' pat)?
    | 'ref' 'mut' Ident ('@' pat)?
    | path '(' pat_list_with_dots ')'
    | path '{' pat_fields? '}'
    | path  // BUG: ambiguity with bare Ident case (above)
    | '(' ')'
    | '(' pat ')'
    | '(' pat ',' pat_list? ')'
    | '[' pat_list_with_dots? ']'
    | '&' pat_no_mut
    | '&' 'mut' pat
    | '&&' pat_no_mut   // `&& pat` means the same as `& & pat`
    | '&&' 'mut' pat
    | 'box' pat;

pat_range_end:
    path
    | pat_lit;

pat_lit:
    '-'? lit;

pat_list:
    pat (',' pat)* ','?;

pat_list_with_dots:
    '..'
    | pat (',' pat)* (',' '..' | ','?);

pat_fields:
    '..'
    | pat_field (',' pat_field)* (',' '..' | ','?);

pat_field:
    'ref'? 'mut'? Ident
    | Ident ':' pat;


// === Expressions

expr:
    assign_expr;

expr_no_struct:
    assign_expr_no_struct;

expr_list:
    expr (',' expr)* ','?;


// --- Blocks

block:
    '{' block_body '}';

block_with_inner_attrs:
    '{' inner_attr* block_body '}';

// OK, this is super tricky.
// This relies on ANTLR4's rule that in alternatives (as in JS regexps),
// earlier alternatives that match are preferred
// over later alternatives that match longer sequences of source tokens.
// The rule `stmt block_body` precedes `expr` because
// that correctly resolves the ambiguity in parsing blocks like
// `{ loop { } - 1 }`.
// This example is parsed like `{ (loop {}); (-1) }`,
// not like `{ (loop { } - 1) }`.
block_body:
    /*empty*/
    | stmt block_body
    | expr;

stmt:
    ';'
    | item  // Statement macros are included here.
    | attr* stmt_tail;

stmt_tail:
    'let' pat (':' ty)? ('=' expr)? ';'
    | blocky_expr
    | expr ';';

blocky_expr:
    block
    | 'if' cond_or_pat block ('else' 'if' cond_or_pat block)* ('else' block)?
    | 'match' expr_no_struct '{' match_arms? '}'
    | loop_label? 'while' cond_or_pat block
    | loop_label? 'for' pat 'in' expr_no_struct block
    | loop_label? 'loop' block
    | 'unsafe' block;

cond_or_pat:
    expr_no_struct
    | 'let' pat '=' expr;

loop_label:
    Lifetime ':';

match_arms:
    match_arm_intro blocky_expr ','? match_arms?
    | match_arm_intro expr (',' match_arms?)?;

match_arm_intro:
    attr* match_pat match_if_clause? '=>';

match_pat:
    pat
    | match_pat '|' pat;

match_if_clause:
    'if' expr;


// --- Primary expressions

prim_expr:
    prim_expr_no_struct
    | path '{' fields? '}';

prim_expr_no_struct:
    lit
    | 'self'
    | path macro_tail?
    // The next 3 productions match exactly `'(' expr_list ')'`,
    // but (e) and (e,) are distinct expressions, so match them separately
    | '(' ')'
    | '(' expr ')'
    | '(' expr ',' expr_list? ')'
    | '[' expr_list? ']'
    | '[' expr ';' expr ']'
    | 'move'? closure_params closure_tail
    | blocky_expr
    | 'break' Lifetime?
    | 'continue' Lifetime?
    | 'return' expr?;  // this is IMO a rustc bug, should be expr_no_struct

lit:
    'true'
    | 'false'
    | BareIntLit
    | FullIntLit
    | ByteLit
    | ByteStringLit
    | FloatLit
    | CharLit
    | StringLit;

closure_params:
    '||'
    | '|' closure_param_list? '|';

closure_param:
    pat (':' ty)?;

closure_param_list:
    closure_param (',' closure_param)* ','?;

closure_tail:
    rtype block
    | expr;

fields:
    struct_update_base
    | field (',' field)* (',' struct_update_base | ','?);

struct_update_base:
    '..' expr;  // this is IMO a bug in the grammar. should be or_expr or something.

field:
    Ident ':' expr;


// --- Operators

post_expr:
    prim_expr
    | post_expr post_expr_tail;

post_expr_tail:
    '[' expr ']'
    | '.' Ident (('::' ty_args)? '(' expr_list? ')')?
    | '.' BareIntLit
    | '(' expr_list? ')';

pre_expr:
    post_expr
    | '-' pre_expr
    | '!' pre_expr
    | '&' 'mut'? pre_expr
    | '&&' 'mut'? pre_expr   // meaning `& & expr`
    | '*' pre_expr
    | 'box' pre_expr
    | 'in' expr_no_struct block;  // placement new - possibly not the final syntax

cast_expr:
    pre_expr
    | cast_expr 'as' ty
    | cast_expr ':' ty;  // experimental type ascription

mul_expr:
    cast_expr
    | mul_expr '*' cast_expr
    | mul_expr '/' cast_expr
    | mul_expr '%' cast_expr;

add_expr:
    mul_expr
    | add_expr '+' mul_expr
    | add_expr '-' mul_expr;

shift_expr:
    add_expr
    | shift_expr '<' '<' add_expr
    | shift_expr '>' '>' add_expr;

bit_and_expr:
    shift_expr
    | bit_and_expr '&' shift_expr;

bit_xor_expr:
    bit_and_expr
    | bit_xor_expr '^' bit_and_expr;

bit_or_expr:
    bit_xor_expr
    | bit_or_expr '|' bit_xor_expr;

cmp_expr:
    bit_or_expr
    | bit_or_expr ('==' | '!=' | '<' | '<=' | '>' | '>' '=') bit_or_expr;

and_expr:
    cmp_expr
    | and_expr '&&' cmp_expr;

or_expr:
    and_expr
    | or_expr '||' and_expr;

range_expr:
    or_expr
    | or_expr '..' or_expr?
    | '..' or_expr?;

assign_expr:
    range_expr
    | range_expr ('=' | '*=' | '/=' | '%=' | '+=' | '-='
                      | '<<=' | '>' '>' '=' | '&=' | '^=' | '|=' ) assign_expr;


// --- Copy of the operator expression syntax but without structs

post_expr_no_struct:
    prim_expr_no_struct
    | post_expr_no_struct post_expr_tail;

pre_expr_no_struct:
    post_expr_no_struct
    | '-' pre_expr_no_struct
    | '!' pre_expr_no_struct
    | '&' 'mut'? pre_expr_no_struct
    | '&&' 'mut'? pre_expr_no_struct   // meaning `& & expr`
    | '*' pre_expr_no_struct
    | 'box' pre_expr_no_struct;

cast_expr_no_struct:
    pre_expr_no_struct
    | cast_expr_no_struct 'as' ty;

mul_expr_no_struct:
    cast_expr_no_struct
    | mul_expr_no_struct '*' cast_expr_no_struct
    | mul_expr_no_struct '/' cast_expr_no_struct
    | mul_expr_no_struct '%' cast_expr_no_struct;

add_expr_no_struct:
    mul_expr_no_struct
    | add_expr_no_struct '+' mul_expr_no_struct
    | add_expr_no_struct '-' mul_expr_no_struct;

shift_expr_no_struct:
    add_expr_no_struct
    | shift_expr_no_struct '<' '<' add_expr_no_struct
    | shift_expr_no_struct '>' '>' add_expr_no_struct;

bit_and_expr_no_struct:
    shift_expr_no_struct
    | bit_and_expr_no_struct '&' shift_expr_no_struct;

bit_xor_expr_no_struct:
    bit_and_expr_no_struct
    | bit_xor_expr_no_struct '^' bit_and_expr_no_struct;

bit_or_expr_no_struct:
    bit_xor_expr_no_struct
    | bit_or_expr_no_struct '|' bit_xor_expr_no_struct;

cmp_expr_no_struct:
    bit_or_expr_no_struct
    | bit_or_expr_no_struct ('==' | '!=' | '<' | '<=' | '>' | '>' '=') bit_or_expr_no_struct;

and_expr_no_struct:
    cmp_expr_no_struct
    | and_expr_no_struct '&&' cmp_expr_no_struct;

or_expr_no_struct:
    and_expr_no_struct
    | or_expr_no_struct '||' and_expr_no_struct;

range_expr_no_struct:
    or_expr_no_struct
    | or_expr_no_struct '..' or_expr_no_struct?
    | '..' or_expr_no_struct?;

assign_expr_no_struct:
    range_expr_no_struct
    | range_expr_no_struct ('=' | '*=' | '/=' | '%=' | '+=' | '-='
                                | '<<=' | '>' '>' '=' | '&=' | '^=' | '|=' ) assign_expr_no_struct;


// === Tokens

any_ident:
    Ident
    | 'Self'
    | 'self'
    | 'static'
    | 'super';

// `$` is recognized as a token, so it may be present in token trees,
// and `macro_rules!` makes use of it. But it is not mentioned anywhere
// else in this grammar.
CashMoney:
    '$';

fragment IDENT:
    XID_Start XID_Continue*;

Lifetime:
    [']IDENT;

Ident:
    IDENT;

fragment SIMPLE_ESCAPE:
    '\\' [0nrt'"\\];

fragment CHAR:
    ~['"\r\n\\\ud800-\udfff]          // a single BMP character other than a backslash, newline, or quote
    | [\ud800-\udbff][\udc00-\udfff]  // a single non-BMP character (hack for Java)
    | SIMPLE_ESCAPE
    | '\\x' [0-7] [0-9a-fA-F]
    | '\\u{' [0-9a-fA-F]+ '}';

CharLit:
    '\'' (CHAR | '"') '\'';

fragment OTHER_STRING_ELEMENT:
    '\''
    | '\\' '\r'? '\n' [ \t]*
    | '\r'
    | '\n';

fragment STRING_ELEMENT:
    CHAR
    | OTHER_STRING_ELEMENT;

fragment RAW_CHAR:
    ~[\ud800-\udfff]          // any BMP character
    | [\ud800-\udbff][\udc00-\udfff];  // any non-BMP character (hack for Java)

// Here we use a non-greedy match to implement the
// (non-regular) rules about raw string syntax.
fragment RAW_STRING_BODY:
    '"' RAW_CHAR*? '"'
    | '#' RAW_STRING_BODY '#';

StringLit:
    '"' STRING_ELEMENT* '"'
    | 'r' RAW_STRING_BODY;

fragment BYTE:
    ' '               // any ASCII character from 32 (space) to 126 (`~`),
    | '!'             // except 34 (double quote), 39 (single quote), and 92 (backslash)
    | [#-&]
    | [(-[]
    | ']'
    | '^'
    | [_-~]
    | SIMPLE_ESCAPE
    | '\\x' [0-9a-fA-F][0-9a-fA-F];

ByteLit:
    'b\'' (BYTE | '"') '\'';

fragment BYTE_STRING_ELEMENT:
    BYTE
    | OTHER_STRING_ELEMENT;

fragment RAW_BYTE_STRING_BODY:
    '"' [ -\x7f]*? '"'
    | '#' RAW_BYTE_STRING_BODY '#';

ByteStringLit:
    'b"' BYTE_STRING_ELEMENT* '"'
    | 'br' RAW_BYTE_STRING_BODY;

fragment DEC_DIGITS:
    [0-9][0-9_]*;

// BareIntLit and FullIntLit both match '123'; BareIntLit wins by virtue of
// appearing first in the file. (This comment is to point out the dependency on
// a less-than-obvious ANTLR rule.)
BareIntLit:
    DEC_DIGITS;

fragment INT_SUFFIX:
    [ui] ('8'|'16'|'32'|'64'|'size');

FullIntLit:
    DEC_DIGITS INT_SUFFIX?
    | '0x' '_'* [0-9a-fA-F] [0-9a-fA-F_]* INT_SUFFIX?
    | '0o' '_'* [0-7] [0-7_]* INT_SUFFIX?
    | '0b' '_'* [01] [01_]* INT_SUFFIX?;

fragment EXPONENT:
    [Ee] [+-]? '_'* [0-9] [0-9_]*;

fragment FLOAT_SUFFIX:
    'f32'
    | 'f64';

// Some lookahead is required here. ANTLR does not support this
// except by injecting some Java code into the middle of the pattern.
//
// A floating-point literal may end with a dot, but:
//
// *   `100..f()` is parsed as `100 .. f()`, not `100. .f()`,
//     contrary to the usual rule that lexers are greedy.
//
// *   Similarly, but less important, a letter or underscore after `.`
//     causes the dot to be interpreted as a separate token by itself,
//     so that `1.abs()` parses a method call. The type checker will
//     later reject it, though.
//
FloatLit:
    DEC_DIGITS '.' [0-9] [0-9_]* EXPONENT? FLOAT_SUFFIX?
    | DEC_DIGITS ('.' {
        /* dot followed by another dot is a range, not a float */
        _input.LA(1) != '.' &&
        /* dot followed by an identifier is an integer with a function call, not a float */
        _input.LA(1) != '_' &&
        !(_input.LA(1) >= 'a' && _input.LA(1) <= 'z') &&
        !(_input.LA(1) >= 'A' && _input.LA(1) <= 'Z')
    }?)
    | DEC_DIGITS EXPONENT FLOAT_SUFFIX?
    | DEC_DIGITS FLOAT_SUFFIX;

Whitespace:
    [ \t\r\n]+ -> skip;

LineComment:
    '//' ~[\r\n]* -> skip;

BlockComment:
    '/*' (~[*/] | '/'* BlockComment | '/'+ (~[*/]) | '*'+ ~[*/])* '*'+ '/' -> skip;

// BUG: only ascii identifiers are permitted
// BUG: doc comments are ignored
// BUG: associated constants are not supported
// BUG: probably most places that use `ty` and `ty_list` are wrong,
//      and should use `ty_sum` and `ty_sum_list`
// BUG: `ty_sum` does not include `?Send` but should
// BUG: look into unifying `ty_sum` and `bound`
// BUG: rename `lit` -> `literal`
// BUG: probably inner attributes are allowed in many more places
// BUG: refactor `use_path` syntax to be like `path`, remove `any_ident`
// BUG: `let [a, xs.., d] = out;` does not parse
// BUG: ambiguity between expression macros, stmt macros, item macros
// BUG: attrs on expressions not supported (`2 + #[inline] f()`)
