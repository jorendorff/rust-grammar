grammar Rust;

crate:
    mod_body EOF;

mod_body:
    inner_attr* item*;

item:
    macro_use
    | attr* 'pub'? pub_able_item
    | attr* 'extern' 'crate' Ident ('as' Ident)? ';'
    | 'impl' ty_params? ty impl_for? '{' impl_item* '}';

pub_able_item:
    use_decl
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
    attr* 'pub'? method_decl
    | attr* 'type' Ident '=' ty ';';

use_decl:
    'use' use_path ';';

use_path:
    '::'? '{' use_item_list '}'
    | '::'? any_ident ('::' any_ident)* use_suffix?;

use_suffix:
    '::' '*'
    | '::' '{' use_item_list '}'
    | use_rename;

use_item:
    any_ident use_rename?;

use_item_list:
    use_item (',' use_item)* ','?;

use_rename:
    'as' Ident;

type_decl:
    'type' Ident ty_params? '=' ty ';';

static_decl:
    'static' Ident ':' ty '=' expr ';';

const_decl:
    'const' Ident ':' ty '=' expr ';';

fn_decl:
    'fn' Ident ty_params? '(' param_list? ')' rtype? where_clause? block;

method_decl:
    'fn' Ident ty_params? '(' method_param_list? ')' rtype? where_clause? block;

trait_method_decl:
    'fn' Ident ty_params? '(' trait_method_param_list? ')' rtype? where_clause? (block | ';');

param:
    pat ':' ty;

param_list:
    param (',' param)* ','?;

self_param:
    'self'
    | '&' 'self'
    | '&' 'mut' 'self';

method_param_list:
    (param | self_param) (',' param)* ','?;

trait_method_param:
    ('&' | '&&' | 'mut')? Ident ':' ty
    | ty;

trait_method_param_list:
    (trait_method_param | self_param) (',' trait_method_param)* ','?;

rtype:
    '->' (ty | '!');

where_clause:
    'where' where_bound_list;

where_bound_list:
    where_bound (',' where_bound)* ','?;

where_bound:
    ty ':' ty_bounds;

mod_decl_short:
    'mod' Ident ';';

mod_decl:
    'mod' Ident '{' mod_body '}';

struct_decl:
    'struct' Ident ty_params? struct_tail;

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
    'trait' Ident ty_params? trait_super? '{' trait_member* '}';

trait_super:
    ':' ty_bounds;

trait_member:
    'type' Ident ';'
    | trait_method_decl;


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
    '_'
    // The next 3 productions match exactly `'(' ty_sum_list? ')'`,
    // but (i32) and (i32,) are distinct types, so parse them with different rules.
    | '(' ')'                           // unit
    | '(' ty_sum ')'                    // grouping (parens are ignored)
    | '(' ty_sum ',' ty_sum_list ')'    // tuple
    | '[' ty (';' expr)? ']'
    | '&' Lifetime? 'mut'? ty
    | '&&' Lifetime? 'mut'? ty          // should treat as `& & ty`
    | 'fn' '(' ty_list ')' rtype?
    | fn_trait
    | ty_path;

ty_list:
    ty (',' ty)* ','?;

fn_trait:
    ty_path '(' ty_list ')' rtype?;  // BUG: ty_path is too permissive

ty_path:
    path_prefix? ty_path_segment ('::' ty_path_segment)*;

ty_path_segment:
    'Self'
    | Ident ty_args?;

ty_args:
    '<' lifetime_list '>'
    | '<' (Lifetime ',')* ty_arg_list '>';

ty_sum:
    ty ('+' ty_bounds)?;

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
    Ident (':' ty_bounds)?;

ty_param_list:
    ty_param (',' ty_param)* ','?;

lifetime_bound:
    Lifetime
    | lifetime_bound '+' Lifetime;

prim_ty_bound:
    ty_path
    | fn_trait
    | Lifetime;

ty_bounds:
    prim_ty_bound
    | ty_bounds '+' prim_ty_bound;


// Blocks and expressions

path:
    path_prefix? path_segment ('::' path_segment)*;

path_prefix:
    '::'
    | 'self' '::'
    | 'super' '::';

path_segment:
    Ident
    | Ident '::' ty_args;

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
    macro_use
    | path
    | 'self'
    | lit
    // The next 3 productions match exactly `'(' expr_list ')'`,
    // but (e) and (e,) are distinct expressions, so match them separately
    | '(' ')'
    | '(' expr ')'
    | '(' expr ',' expr_list ')'
    | '[' expr_list? ']'
    | '[' expr ';' expr ']'
    | blocky_expr
    | 'break' Lifetime?
    | 'continue' Lifetime?
    | 'return' expr?  // this is IMO a rustc bug, should be expr_no_struct
    | closure_params closure_tail;

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

pat:
    macro_use
    | lit
    | lit '...' lit
    | '_'
    | 'ref'? 'mut'? Ident
    | path '(' enum_tuple_field_pats ')'
    | path '{' enum_struct_field_pats? '}'
    | path
    | '(' ')'
    | '(' pat ')'
    | '(' pat ',' pat_list? ')'
    | '&' pat
    | Ident '@' pat;

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
    ~['"\r\n\\]
    | SIMPLE_ESCAPE
    | '\\x' [0-7] [0-9a-fA-F]
    | '\\u{' [0-9a-fA-F]+ '}';

fragment STRING_ELEMENT:
    CHAR
    | '\''
    | '\\' '\r'? '\n' [ \t]*
    | '\r'
    | '\n';

StringLit:
    '"' STRING_ELEMENT* '"';

CharLit:
    '\'' (CHAR | '"') '\'';

fragment BYTE:
    [ -&(-~]    // any ASCII character from 32 (space) to 126 (tilde), except 39 (single-quote)
    | SIMPLE_ESCAPE
    | '\\x' [0-9a-fA-F][0-9a-fA-F];

ByteLit:
    'b\'' BYTE '\'';

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
