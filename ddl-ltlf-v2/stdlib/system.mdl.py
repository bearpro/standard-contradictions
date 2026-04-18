from __future__ import annotations

import mdl


_LIST_TYPES: dict[str, mdl.SumType] = {}


def List(item_type: mdl.MdlType) -> mdl.SumType:
    name = f"List<{item_type.name}>"
    existing = _LIST_TYPES.get(name)
    if existing is not None:
        return existing

    self_ref = mdl.TypeRef(name)
    cons_payload = mdl.ProductType(
        f"List.Cons<{item_type.name}>",
        {"head": item_type, "tail": self_ref},
    )
    list_type = mdl.SumType(name, {"Nil": None, "Cons": cons_payload})
    self_ref.bind(list_type)
    _LIST_TYPES[name] = list_type
    return list_type


STRING_LIST = List(mdl.STRING)


def nil(list_type: mdl.SumType) -> mdl.Term:
    return mdl.Variant(list_type, "Nil")


def cons(head: mdl.Term, tail: mdl.Term) -> mdl.Term:
    list_type = tail.type
    if not isinstance(list_type, mdl.SumType):
        raise TypeError("Cons tail must be a list sum type.")
    cons_payload = list_type.payload_type("Cons")
    if not isinstance(cons_payload, mdl.ProductType):
        raise TypeError("Cons payload must be a product type.")
    return mdl.Variant(
        list_type,
        "Cons",
        mdl.Product(cons_payload, {"head": head, "tail": tail}),
    )


def is_nil(xs: mdl.Term) -> mdl.LtlfFormula:
    return mdl.IsVariant(xs, "Nil")


def head(xs: mdl.Term) -> mdl.Term:
    return mdl.VariantPayload(xs, "Cons").field("head")


def tail(xs: mdl.Term) -> mdl.Term:
    return mdl.VariantPayload(xs, "Cons").field("tail")


def char_at(value: mdl.Term, index: mdl.Term | int) -> mdl.Term:
    return mdl.CharAt(value, index)


string_to_list_from = mdl.Function(
    "string_to_list_from",
    {"value": mdl.STRING, "index": mdl.INT},
    STRING_LIST,
    lambda value, index: mdl.If(
        index >= mdl.Len(value),
        nil(STRING_LIST),
        cons(mdl.CharAt(value, index), string_to_list_from(value, index + 1)),
    ),
)

string_to_list = mdl.Function(
    "string_to_list",
    {"value": mdl.STRING},
    STRING_LIST,
    lambda value: string_to_list_from(value, 0),
)

contains_string = mdl.Function(
    "contains_string",
    {"xs": STRING_LIST, "value": mdl.STRING},
    mdl.BOOL,
    lambda xs, value: mdl.If(
        is_nil(xs),
        mdl.Bool(False),
        mdl.If(
            mdl.Eq(head(xs), value),
            mdl.Bool(True),
            contains_string(tail(xs), value),
        ),
    ),
)

count_string = mdl.Function(
    "count_string",
    {"xs": STRING_LIST, "value": mdl.STRING},
    mdl.INT,
    lambda xs, value: mdl.If(
        is_nil(xs),
        mdl.Int(0),
        mdl.If(
            mdl.Eq(head(xs), value),
            mdl.Int(1) + count_string(tail(xs), value),
            count_string(tail(xs), value),
        ),
    ),
)

all_in_string = mdl.Function(
    "all_in_string",
    {"xs": STRING_LIST, "allowed": STRING_LIST},
    mdl.BOOL,
    lambda xs, allowed: mdl.If(
        is_nil(xs),
        mdl.Bool(True),
        mdl.If(
            mdl.Truth(contains_string(allowed, head(xs))),
            all_in_string(tail(xs), allowed),
            mdl.Bool(False),
        ),
    ),
)


LATIN_LETTERS = string_to_list(
    mdl.String("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
)
DIGITS = string_to_list(mdl.String("0123456789"))


with mdl.ModuleBuilder() as _module:
    _module.List = List
    _module.STRING_LIST = STRING_LIST
    _module.nil = nil
    _module.cons = cons
    _module.is_nil = is_nil
    _module.head = head
    _module.tail = tail
    _module.char_at = char_at
    _module.string_to_list_from = string_to_list_from
    _module.string_to_list = string_to_list
    _module.contains_string = contains_string
    _module.count_string = count_string
    _module.all_in_string = all_in_string
    _module.LATIN_LETTERS = LATIN_LETTERS
    _module.DIGITS = DIGITS

MODULE = _module.build()
SYSTEM = MODULE


__all__ = [
    "DIGITS",
    "LATIN_LETTERS",
    "List",
    "MODULE",
    "SYSTEM",
    "STRING_LIST",
    "all_in_string",
    "char_at",
    "cons",
    "contains_string",
    "count_string",
    "head",
    "is_nil",
    "nil",
    "string_to_list",
    "string_to_list_from",
    "tail",
]
