grammar Rust;

crate:
    mod_body EOF;

mod_body:
    inner_attr* item*;

item:
    attr* 'pub'? pub_able_item
    | attr* 'impl' ty_params? ty impl_for? where_clause? '{' impl_item* '}'
    | attr* item_macro_use;

item_macro_use:
    Ident '!' item_macro_tail;

item_macro_tail:
    Ident? tt_parens ';'
    | Ident tt_brackets ';'
    | Ident? tt_block;

pub_able_item:
    use_decl
    | extern_crate  // `pub extern crate` is deprecated but still exists.
    | foreign_mod
    | type_decl
    | static_decl
    | const_decl
    | fn_decl
    | mod_decl_short
    | mod_decl
    | struct_decl
    | enum_decl
    | trait_decl;

impl_for:
    'for' ty;

impl_item:
    attr* 'pub'? impl_item_tail;

impl_item_tail:
    method_decl
    | 'type' Ident '=' ty ';'
    | Ident '!' tt_parens ';'
    | Ident '!' tt_block;

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

extern_crate:
    'extern' 'crate' Ident rename? ';';

foreign_mod:
    abi '{' inner_attr* foreign_item* '}';

foreign_item:
    attr* 'pub'? foreign_item_tail
    | attr* item_macro_use;

foreign_item_tail:
    'static' 'mut'? Ident ':' ty_sum ';'
    | fn_head '(' param_list? ')' rtype? where_clause? ';';

type_decl:
    'type' Ident ty_params? '=' ty ';';

static_decl:
    'static' Ident ':' ty '=' expr ';';

const_decl:
    'const' Ident ':' ty '=' expr ';';

fn_decl:
    fn_head '(' param_list? ')' rtype? where_clause? block;

method_decl:
    fn_head '(' method_param_list? ')' rtype? where_clause? block;

trait_method_decl:
    fn_head '(' trait_method_param_list? ')' rtype? where_clause? (block | ';');

// Parts of a `fn` definition up to the type parameters.
//
// `const` and `extern` are incompatible on a `fn`, but this grammar
// does not rule it out, holding that in a hypothetical Rust language
// specification, it would be clearer to specify this as a semantic
// rule, not a syntactic one. That is, not every rule that can be
// enforced gramatically should be.
fn_head:
    'const'? 'unsafe'? abi? 'fn' Ident ty_params?;

param:
    pat ':' ty;

param_list:
    param (',' param)* ','?;

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

where_clause:
    'where' where_bound_list;

where_bound_list:
    where_bound (',' where_bound)* ','?;

where_bound:
    ty colon_bound;

mod_decl_short:
    'mod' Ident ';';

mod_decl:
    'mod' Ident '{' mod_body '}';

struct_decl:
    'struct' Ident ty_params? where_clause? struct_tail;

struct_tail:
    ';'
    | '(' tuple_struct_field_list ')' ';'
    | '{' field_decl_list '}';

tuple_struct_field:
    'pub'? ty;

tuple_struct_field_list:
    tuple_struct_field (',' tuple_struct_field)* ','?;

field_decl:
    'pub'? Ident ':' ty;

field_decl_list:
    field_decl (',' field_decl)* ','?;

enum_decl:
    'enum' Ident ty_params? '{' enum_variant_list? '}';

enum_variant:
    Ident '(' ty_list ')'
    | Ident '{' enum_field_decl_list '}'
    | Ident '=' expr
    | Ident;

enum_variant_list:
    enum_variant (',' enum_variant)* ','?;

// enum variants that are struct-like can't have `pub` on individual fields.
enum_field_decl:
    Ident ':' ty;

enum_field_decl_list:
    enum_field_decl (',' enum_field_decl)* ','?;

trait_decl:
    'unsafe'? 'trait' Ident ty_params? colon_bound? '{' trait_item* '}';

colon_bound:
    ':' bound;

trait_item:
    attr* 'type' Ident colon_bound? ';'
    | attr* trait_method_decl;


// Attributes

attr:
    '#' '[' tt* ']';

inner_attr:
    '#' '!' '[' tt* ']';


// Macros and token trees

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


// Types and type parameters

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
    | for_lifetime? 'unsafe'? abi? 'fn' '(' ty_list ')' rtype?
    | ty_path;

mut_or_const:
    'mut'
    | 'const';

for_lifetime:
    'for' '<' lifetime_def_list '>';

lifetime_def:
    Lifetime (':' lifetime_bound)?;

lifetime_bound:
    Lifetime
    | lifetime_bound '+' Lifetime;

lifetime_def_list:
    lifetime_def (',' lifetime_def)* ','?;

abi:
    'extern' StringLit?;

ty_list:
    ty (',' ty)* ','?;

ty_path:
    for_lifetime? '::'? ty_path_rel;

ty_path_rel:
    (Ident | 'Self') '(' ty_list? ')' rtype?
    | ty_path_segment
    | ty_path_segment '::' ty_path_rel;

ty_path_segment:
    (Ident | 'Self') ty_args?;

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
    Ident colon_bound?;

ty_param_list:
    ty_param (',' ty_param)* ','?;

prim_bound:
    ty_path
    | '?' ty_path
    | Lifetime;

bound:
    prim_bound
    | bound '+' prim_bound;


// Blocks and expressions

// This is very slightly different from the syntax read by rustc:
// whitespace is permitted after `self` and `super` in paths.
//
// In rustc, `self::x` is an acceptable path, but `self :: x` is not, because
// `self` is a strict keyword except when followed immediately by the exact
// characters `::`. Same goes for `super`. Pretty weird.
//
// So instead, this grammar accepts that `self` is a keyword, and permits it
// specially at the very front of a path. Whitespace is ignored. `super` is OK
// anywhere except at the end.

path:
    path_segment_no_super
    | path_parent? '::' path_segment_no_super;

path_parent:
    'self'
    | path_segment
    | '::' path_segment
    | path_parent '::' path_segment;

path_segment:
    path_segment_no_super
    | 'super';

path_segment_no_super:
    simple_path_segment ('::' ty_args)?;

simple_path_segment:
    Ident
    | 'Self';

block: '{' block_body '}';

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
    | attr* stmt_tail;

stmt_tail:
    'let' pat (':' ty)? ('=' expr)? ';'
    | item
    | Ident '!' Ident tt_parens ';'
    | Ident '!' Ident tt_brackets ';'
    | Ident '!' Ident tt_block
    | Ident '!' tt_delimited
    | blocky_expr
    | expr ';';

blocky_expr:
    block
    | 'if' cond_or_pat block ('else' 'if' cond_or_pat block)* ('else' block)?
    | 'match' expr_no_struct '{' match_arms? '}'
    | loop_label? loop_expr
    | 'unsafe' block;

loop_expr:
    'while' cond_or_pat block
    | 'for' pat 'in' expr_no_struct block
    | 'loop' block;

loop_label:
    Lifetime ':';

cond_or_pat:
    expr_no_struct
    | 'let' pat '=' expr;

match_arms:
    match_arm_intro blocky_expr ','? match_arms?
    | match_arm_intro expr (',' match_arms?)?;

match_arm_intro:
    match_pat match_if_clause? '=>';

match_pat:
    pat
    | match_pat '|' pat;

match_if_clause:
    'if' expr;

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

prim_expr_no_struct:
    path macro_tail?
    | 'self'
    | lit
    // The next 3 productions match exactly `'(' expr_list ')'`,
    // but (e) and (e,) are distinct expressions, so match them separately
    | '(' ')'
    | '(' expr ')'
    | '(' expr ',' expr_list? ')'
    | '[' expr_list? ']'
    | '[' expr ';' expr ']'
    | blocky_expr
    | 'break' Lifetime?
    | 'continue' Lifetime?
    | 'return' expr?  // this is IMO a rustc bug, should be expr_no_struct
    | closure_params closure_tail;

macro_tail:
    '!' tt_delimited;

prim_expr:
    path '{' fields? '}'
    | prim_expr_no_struct;

field:
    Ident ':' expr;

fields:
    struct_update_base
    | field (',' field)* (',' struct_update_base | ','?);

struct_update_base:
    '..' expr;  // this is IMO a bug in the grammar. should be or_expr or something.


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
    | '*' pre_expr;

cast_expr:
    pre_expr
    | cast_expr 'as' ty;

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
    | shift_expr '<<' add_expr
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
    | bit_or_expr ('==' | '!=' | '<' | '<=' | '>' | '>=') bit_or_expr;

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
                      | '<<=' | '>>=' | '&=' | '^=' | '|=' ) assign_expr;

expr:
    assign_expr;

expr_list:
    expr (',' expr)* ','?;


// The full copy of the expression syntax but without structs

post_expr_no_struct:
    prim_expr_no_struct
    | post_expr_no_struct post_expr_tail;

pre_expr_no_struct:
    post_expr_no_struct
    | '-' pre_expr_no_struct
    | '!' pre_expr_no_struct
    | '&' 'mut'? pre_expr_no_struct
    | '&&' 'mut'? pre_expr_no_struct    // meaning `& & expr`
    | '*' pre_expr_no_struct;

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
    | shift_expr_no_struct '<<' add_expr_no_struct
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
    | bit_or_expr_no_struct ('==' | '!=' | '<' | '<=' | '>' | '>=') bit_or_expr_no_struct;

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
                                | '<<=' | '>' '>=' | '&=' | '^=' | '|=' ) assign_expr_no_struct;

expr_no_struct:
    assign_expr_no_struct;




// Patterns

// A `pat_no_mut` is a pattern that does not start with `mut`.
// It is distinct from `pat` to rule out ambiguity in parsing the
// pattern `&mut x`, which must parse like `&mut (x)`, not `&(mut x)`.
pat_no_mut:
    '_'
    | lit
    | lit '...' lit
    | 'ref'? Ident ('@' pat)?
    | 'ref' 'mut' Ident ('@' pat)?
    | Ident macro_tail
    | path '(' enum_tuple_field_pats ')'
    | path '{' enum_struct_field_pats? '}'
    | path
    | '(' ')'
    | '(' pat ')'
    | '(' pat ',' pat_list? ')'
    | '&' pat_no_mut
    | '&' 'mut' pat
    | '&&' pat_no_mut   // `&& pat` means the same as `& & pat`
    | '&&' 'mut' pat;

pat:
    pat_no_mut
    | 'mut' Ident ('@' pat)?;

pat_list:
    pat (',' pat)* ','?;

enum_tuple_field_pats:
    '..'
    | pat (',' pat)* (',' '..' | ','?);

enum_struct_field_pats:
    '..'
    | field_pat (',' field_pat)* (',' '..' | ','?);

field_pat:
    'mut'? 'ref'? Ident
    | Ident ':' pat;



// Tokens

any_ident:
    Ident
    | 'Self'
    | 'self'
    | 'static'
    | 'super';

CashMoney:
    '$';

Lifetime:
    ['][A-Za-z_][0-9A-Za-z_]*;

Ident:
    [A-Za-z_][0-9A-Za-z_]*;

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
    [ !#-&(-~]    // any ASCII character from 32 (space) to 126 (`~`), except 34 (`"`) and 39 (`'`)
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

BareIntLit:
    '0'
    | [1-9][0-9]*;

fragment DEC_DIGITS:
    '0' '_'*
    | [1-9][0-9_]*;

fragment INT_SUFFIX:
    [ui] ('8'|'16'|'32'|'64'|'size');

FullIntLit:
    DEC_DIGITS INT_SUFFIX?
    | '0x' '_'* [0-9a-fA-F] [0-9a-fA-F_]* INT_SUFFIX?
    | '0o' '_'* [0-7] [0-7_]* INT_SUFFIX?;

fragment EXPONENT:
    [Ee] [+-]? '_'* [0-9] [0-9_]*;

fragment FLOAT_SUFFIX:
    'f32'
    | 'f64';

FloatLit:
    DEC_DIGITS '.' [0-9] [0-9_]* EXPONENT? FLOAT_SUFFIX?
    | DEC_DIGITS EXPONENT FLOAT_SUFFIX?
    | DEC_DIGITS FLOAT_SUFFIX;

Whitespace:
    [ \t\r\n]+ -> skip;

LineComment:
    '//' ~[\r\n]* -> skip;

BlockComment:
    '/*' (~[*/] | '/'* BlockComment | '/'+ (~[*/]) | '*'+ ~[*/])* '*'+ '/' -> skip;

// BUG: doc comments are ignored
// BUG: binary int literals `0b10010` are not supported
// BUG: not all string constant forms are supported
// BUG: paths and type paths starting with `<`, like `<Vec<i32>>::new`
//      and `<Frog as Animal>::move`, are not supported,
//      much less paths starting with `<<`
// BUG: `impl !Send` is not supported
// BUG, probably: if `for <'a> 'a` is a legal bound, it's not supported
// BUG: associated constants are not supported
// BUG: `unsafe trait` items are not supported
// BUG: `unsafe impl` is not supported
// BUG: variadic foreign functions are not supported
// BUG: `static mut` is not supported
// BUG: '?Sized` bound is not supported
// BUG: pointer types are not supported
