#!/usr/bin/env python

import datashape as ds


DTYPES = ['bool',
          'int8',
          'int16',
          'int32',
          'int64',
          'int128',
          'uint8',
          'uint16',
          'uint32',
          'uint64',
          'uint128',
          'float16',
          'float32',
          'float64',
          'complex64',
          'complex128',
          # 'float128',
          # 'decimal32',
          # 'decimal64',
          # 'decimal128',
          # 'bignum',
          'real',
          'complex',
          # 'intptr',
          # 'uintptr',
          'string',
          # 'char',
          'bytes',
          'date'
          'datetime',
          'json',
          # 'void'
          ]

tokens = (
    'CONSTR',
    'NAME_LOWER',
    'NAME_UPPER',
    'NAME_OTHER',
    'ASTERISK',
    'COMMA',
    'EQUAL',
    'COLON',
    'LBRACKET',
    'RBRACKET',
    'LBRACE',
    'RBRACE',
    'LPAREN',
    'RPAREN',
    'ELLIPSIS',
    'RARROW',
    'INTEGER',
    'STRING',
    'OPTION',
    'VAR',
)

reserved = {
    'string': 'STRING_TYPE',
    'fixlen': 'FIXLEN',
    'encoding': 'ENCODING',
}

tokens += tuple(reserved.values())

t_CONSTR = r'datetime|bytes'
t_NAME_UPPER = r'[A-Z][a-zA-Z0-9_]*'
t_NAME_OTHER = r'_[a-zA-Z0-9_]*'
t_ASTERISK = r'\*'
t_COMMA = ','
t_EQUAL = '='
t_COLON = ':'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_ELLIPSIS = r'\.\.\.'
t_RARROW = '->'
t_OPTION = r'\?'
t_STRING = '\\"(?:[^"\\n\\r\\\\]|(?:\\\\.))*\\"'
t_ignore = ' \t'


def t_VAR(t):
    r'\bvar\b'
    t.value = ds.var
    return t


def t_NAME_LOWER(t):
    r'[a-z][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'NAME_LOWER')
    return t


def t_INTEGER(t):
    r'0(?![0-9])|[1-9][0-9]*'
    t.value = int(t.value)
    return t


def t_error(t):
    raise Exception(t)


CONSTR_MAP = dict(bytes=ds.Bytes, datetime=ds.DateTime)
NAME_MAP = dict(date=ds.date_, datetime=ds.datetime_, bool=ds.bool_)


import ply.lex as lex
lexer = lex.lex()


def p_datashape(t):
    "datashape : dim ASTERISK datashape"
    dims = (t[1],) if not isinstance(t[1], (tuple, list)) else t[1]
    t[0] = ds.DataShape(*(dims + (t[3],)))


def p_option_dtype(t):
    """option_dtype : OPTION base_dtype"""
    t[0] = ds.Option(t[2])


def p_ellipsis_typevar(t):
    "ellipsis_typevar : typevar ELLIPSIS"
    t[0] = ds.Ellipsis(typevar=t[1])


def p_typevar(t):
    "typevar : NAME_UPPER"
    t[0] = ds.TypeVar(t[1])


def p_string_type(t):
    "typ : STRING_TYPE"
    t[0] = ds.String()


def p_string_constr_fixlen_only(t):
    """typ : STRING_TYPE LBRACKET INTEGER RBRACKET"""
    t[0] = ds.String(fixlen=t[3])


def p_string_constr_kwarg_fixlen_only(t):
    """typ : STRING_TYPE LBRACKET FIXLEN EQUAL INTEGER RBRACKET"""
    t[0] = ds.String(fixlen=t[5])


def p_string_constr_kwarg_encoding_only(t):
    """typ : STRING_TYPE LBRACKET STRING RBRACKET"""
    t[0] = ds.String(encoding=t[3][1:-1])


def p_string_type_arg(t):
    "type_arg : STRING"
    import pdb; pdb.set_trace()
    t[0] = t[1][1:-1]


def p_string_constr_kwarg_no_kwarg(t):
    """typ : STRING_TYPE LBRACKET INTEGER COMMA string RBRACKET"""
    t[0] = ds.String(fixlen=t[3], encoding=t[5])


def p_string_constr_kwarg_fixlen_and_encoding(t):
    """typ : STRING_TYPE LBRACKET FIXLEN EQUAL INTEGER COMMA ENCODING EQUAL STRING INTEGER RBRACKET"""
    t[0] = ds.String(fixlen=t[5], encoding=t[9])


def p_type_constr(t):
    """type_constr : CONSTR LBRACKET type_arg_list RBRACKET"""
    t[0] = CONSTR_MAP[t[1]](*t[3])


def p_typ(t):
    "typ : NAME_LOWER"
    result = NAME_MAP.get(t[1], getattr(ds, t[1], None))
    assert result is not None
    t[0] = result


def p_struct_type(t):
    "struct_type : LBRACE struct_field_list RBRACE"

    t[0] = ds.Record([t[2]] if isinstance(t[2], tuple) else  t[2])


def p_struct_field_list(t):
    """struct_field_list : struct_field COMMA struct_field_list
       type_kwarg_list : type_kwarg COMMA type_kwarg_list
       type_kwarg : NAME_LOWER EQUAL type_arg
       type_arg_list : type_arg COMMA type_arg_list
    """
    t[0] = [t[1]] + (t[3] if isinstance(t[3], list) else [t[3]])


def p_struct_field(t):
    "struct_field : struct_field_name COLON datashape"
    t[0] = (t[1], t[3])


def p_funcproto_or_tuple_type(t):
    """funcproto_or_tuple_type : tuple_type RARROW datashape"""
    t[0] = ds.Function(*(t[1].dshapes + (t[3],)))


def p_terminals(t):
    """datashape : dtype
       funcproto_or_tuple_type : tuple_type
       tuple_item_list : datashape
       struct_field_name : NAME_LOWER
                         | NAME_UPPER
                         | NAME_OTHER
                         | STRING
       struct_field_list : struct_field
       type_arg_list : type_kwarg_list
                     | type_arg
       type_kwarg_list : type_kwarg
       type_arg : datashape
       dim : ellipsis_typevar
           | typevar
           | type_constr
           | typ
           | VAR
           | ELLIPSIS
       base_dtype : typevar
                  | typ
                  | type_constr
                  | struct_type
                  | funcproto_or_tuple_type
       dtype : option_dtype
             | base_dtype
    """
    t[0] = t[1]


def p_tuple_type(t):
    "tuple_type : LPAREN tuple_item_list RPAREN"
    t[0] = ds.Tuple(t[2])


def p_tuple_item_list(t):
    "tuple_item_list : datashape COMMA tuple_item_list"
    t[0] = (t[1], t[3])


import ply.yacc as yacc
parser = yacc.yacc()
parse = parser.parse


if __name__ == '__main__':
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('datashape', type=str)
    args = p.parse_args()
    result = parse(args.datashape)
    print(result)
