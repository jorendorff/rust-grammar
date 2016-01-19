grammar Rust;

crate:
    mod_body EOF;

mod_body:
    inner_attr* mod_element*;

mod_element:
    macro_use
    | attr* 'pub'? item
    | 'impl' ty '{' impl_item* '}';

item:
    use_decl
    | const_decl
    | fn_decl
    | mod_decl_short
    | mod_decl
    | struct_decl
    | enum_decl
    | macro_use;

impl_item:
    'pub'? method_decl;

use_decl:
    'use' path use_suffix? ';';

use_suffix:
    '::' '*'
    | '::' '{' (use_ident ',')* use_ident? '}';

use_ident:
    Ident
    | 'self';

const_decl:
    'const' Ident ':' ty '=' expr ';';

fn_decl:
    'fn' Ident '(' arg_list? ')' rtype? block;

method_decl:
    'fn' Ident '(' method_arg_list? ')' rtype? block;

arg:
    Ident ':' ty;

arg_list:
    arg (',' arg)* ','?;

self_arg:
    'self'
    | '&' 'self'
    | '&' 'mut' 'self';

method_arg_list:
    (arg | self_arg) (',' arg)* ','?;

rtype:
    '->' (ty | '!');

mod_decl_short:
    'mod' Ident ';';

mod_decl:
    'mod' Ident '{' mod_body '}';

struct_decl:
    'struct' Ident ty_params? struct_tail;

struct_tail:
    ';'
    | '(' tuple_struct_field_list ')'
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
    'enum' Ident ty_params? '{' variant_list? '}';

variant:
    Ident '(' ty_list ')'
    | Ident '{' enum_field_decl_list '}'
    | Ident '=' expr
    | Ident;

variant_list:
    variant (',' variant)* ','?;

// enum variants that are struct-like can't have `pub` on individual fields.
enum_field_decl:
    Ident ':' ty;

enum_field_decl_list:
    enum_field_decl (',' enum_field_decl)* ','?;


// Attributes

attr:
    '#' '[' tt* ']';

inner_attr:
    '#' '!' '[' tt* ']';


// Macros and token trees

macro_use:
    Ident '!' tt tt?;

tt:
    ~('(' | ')' | '{' | '}' | '[' | ']')
    | '(' tt* ')'
    | '{' tt* '}'
    | '[' tt* ']';



// Types and type parameters

ty:
    'bool'
    | 'char'
    | 'i8'
    | 'u8'
    | 'i16'
    | 'u16'
    | 'i32'
    | 'u32'
    | 'i64'
    | 'u64'
    | 'isize'
    | 'usize'
    // The next 3 productions match exactly `'(' ty_list ')'`,
    // but (i32) and (i32,) are distinct types, so parse them with different rules.
    | '(' ')'                           // unit
    | '(' ty ')'                        // grouping (parens are ignored)
    | '(' ty ',' ty_list ')'            // tuple
    | '&' Lifetime? 'mut'? ty
    | Ident ty_params?;

ty_list:
    ty (',' ty)* ','?;

ty_params:
    '<' (Lifetime ',')* ty_list '>';


// Blocks and expressions

path:
    path_prefix? path_segment ('::' path_segment)*;

path_prefix:
    '::'
    | 'self' '::'
    | 'super' '::';

path_segment:
    Ident
    | Ident '::' ty_params;

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
    | 'let' pat (':' ty)? ('=' expr)? ';'
    | item
    | blocky_expr
    | expr ';';

blocky_expr:
    block
    | 'if' cond_or_pat block ('else' 'if' cond_or_pat block)* ('else' block)?
    | 'match' expr_no_struct '{' arm_list? '}'
    | loop_label? loop_expr;

loop_expr:
    'while' cond_or_pat block
    | 'for' pat 'in' expr_no_struct block
    | 'loop' block;

loop_label:
    Lifetime ':';

cond_or_pat:
    expr_no_struct
    | 'let' pat '=' expr;

arm:
    pat '=>' expr;

arm_list:
    arm (',' arm)* ','?;

lit:
    'true'
    | 'false'
    | BareIntLit
    | FullIntLit
    | FloatLit
    | CharLit
    | StringLit;

prim_expr_no_struct:
    macro_use
    | path
    | 'self'
    | lit
    // The next 3 productions match exactly `'(' expr_list ')'`,
    // but (e) and (e,) are distinct expressions, so match them separately
    | '(' ')'
    | '(' expr ')'
    | '(' expr ',' expr_list ')'
    | blocky_expr
    | 'break' Lifetime?
    | 'continue' Lifetime?
    | 'return' expr?;  // this is IMO a rustc bug, should be expr_no_struct

prim_expr:
    path '{' field_list? '}'
    | prim_expr_no_struct;

field:
    Ident ':' expr;

field_list:
    field (',' field)* ','?;


post_expr:
    prim_expr
    | post_expr post_expr_tail;

post_expr_tail:
    '[' expr ']'
    | '.' Ident (('::' ty_params)? '(' expr_list? ')')?
    | '.' BareIntLit
    | '(' expr_list? ')';

pre_expr:
    post_expr
    | '-' pre_expr
    | '!' pre_expr
    | '&' 'mut'? pre_expr
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
    | shift_expr '>>' add_expr;

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
    | shift_expr_no_struct '>>' add_expr_no_struct;

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
                                | '<<=' | '>>=' | '&=' | '^=' | '|=' ) assign_expr_no_struct;

expr_no_struct:
    assign_expr_no_struct;




// Patterns

prim_pat:
    lit
    | lit '...' lit
    | '_'
    | 'mut'? 'ref'? Ident
    | path '(' pat_list? ')'
    | path '{' field_pat_list? '}'
    | path
    | '(' ')'
    | '(' pat ')'
    | '(' pat ',' pat_list? ')'
    | '&' pat;

pat:
    prim_pat
    | pat '|' prim_pat;

pat_list:
    pat (',' pat)* ','?;

field_pat:
    'mut'? 'ref'? Ident
    | Ident ':' pat;

field_pat_list:
    field_pat (',' field_pat)* ','?;



// Tokens

CashMoney:
    '$';

Lifetime:
    ['][A-Za-z_][0-9A-Za-z_]*;

Ident:
    [A-Za-z_][0-9A-Za-z_]*;

fragment CHAR:
    ~['"\r\n\\]
    | '\\' [0nt'"\\]
    | '\\x' [0-7] [0-9a-fA-F]
    | '\\u{' [0-9a-fA-F]+ '}';

StringLit:
    '"' (CHAR | '\'' )* '"';

CharLit:
    '\'' (CHAR | '"')* '\'';

BareIntLit:
    '0'
    | [1-9][0-9]*;

fragment DEC_DIGITS:
    '0' '_'*
    | [1-9][0-9_]*;

FullIntLit:
    DEC_DIGITS ([ui] ('8'|'16'|'32'|'64'|'size'))?;

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

Newline:
    '\r'? '\n' -> skip;

LineComment:
    '//' ~[\r\n]* -> skip;