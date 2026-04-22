from __future__ import annotations

from typing import Protocol, cast

import mdl


class SystemModule(Protocol):
    STRING_LIST: mdl.SumType

    def is_nil(self, xs: mdl.Term) -> mdl.LtlfFormula: ...

    def head(self, xs: mdl.Term) -> mdl.Term: ...

    def tail(self, xs: mdl.Term) -> mdl.Term: ...

    def string_to_list(self, value: mdl.Term) -> mdl.Term: ...

    def all_in_string(self, xs: mdl.Term, allowed: mdl.Term) -> mdl.Term: ...

    def count_string(self, xs: mdl.Term, value: mdl.Term) -> mdl.Term: ...


def _system() -> SystemModule:
    with mdl.ModuleBuilder() as doc:
        doc.std = doc.import_mdl("../../stdlib/system.mdl.py")

    module = doc.build()
    system = module.std
    assert isinstance(system, mdl.Module)
    return cast(SystemModule, cast(object, system))


SYSTEM = _system()
ALLOWED_EMAIL_CHARS_VALUE = (
    "abcdefghijklmnopqrstuvwxyz"
    + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    + "0123456789"
    + ".-_@"
)


def starts_with_dot_com_body(xs: mdl.Term) -> mdl.Term:
    after_dot = SYSTEM.tail(xs)
    after_c = SYSTEM.tail(after_dot)
    after_o = SYSTEM.tail(after_c)
    after_m = SYSTEM.tail(after_o)

    return mdl.If(
        SYSTEM.is_nil(xs),
        mdl.Bool(False),
        mdl.If(
            mdl.Ne(SYSTEM.head(xs), mdl.String(".")),
            mdl.Bool(False),
            mdl.If(
                SYSTEM.is_nil(after_dot),
                mdl.Bool(False),
                mdl.If(
                    mdl.Ne(SYSTEM.head(after_dot), mdl.String("c")),
                    mdl.Bool(False),
                    mdl.If(
                        SYSTEM.is_nil(after_c),
                        mdl.Bool(False),
                        mdl.If(
                            mdl.Ne(SYSTEM.head(after_c), mdl.String("o")),
                            mdl.Bool(False),
                            mdl.If(
                                SYSTEM.is_nil(after_o),
                                mdl.Bool(False),
                                mdl.If(
                                    mdl.Ne(SYSTEM.head(after_o), mdl.String("m")),
                                    mdl.Bool(False),
                                    mdl.If(SYSTEM.is_nil(after_m), mdl.Bool(True), mdl.Bool(False)),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )


def first_is_not_at_body(xs: mdl.Term) -> mdl.Term:
    return mdl.If(
        SYSTEM.is_nil(xs),
        mdl.Bool(False),
        mdl.If(mdl.Eq(SYSTEM.head(xs), mdl.String("@")), mdl.Bool(False), mdl.Bool(True)),
    )


def build() -> mdl.Module:
    with mdl.ModuleBuilder() as doc:
        allowed_email_chars = SYSTEM.string_to_list(
            mdl.String(ALLOWED_EMAIL_CHARS_VALUE),
        )
        starts_with_dot_com = mdl.Function(
            "starts_with_dot_com",
            {"xs": SYSTEM.STRING_LIST},
            mdl.BOOL,
            starts_with_dot_com_body,
        )

        ends_with_dot_com: mdl.Function

        def ends_with_dot_com_body(xs: mdl.Term) -> mdl.Term:
            return mdl.If(
                SYSTEM.is_nil(xs),
                mdl.Bool(False),
                mdl.If(
                    mdl.Truth(starts_with_dot_com(xs)),
                    mdl.Bool(True),
                    ends_with_dot_com(SYSTEM.tail(xs)),
                ),
            )

        ends_with_dot_com = mdl.Function(
            "ends_with_dot_com",
            {"xs": SYSTEM.STRING_LIST},
            mdl.BOOL,
            ends_with_dot_com_body,
        )
        first_is_not_at = mdl.Function(
            "first_is_not_at",
            {"xs": SYSTEM.STRING_LIST},
            mdl.BOOL,
            first_is_not_at_body,
        )

        def validate_email_body(value: mdl.Term) -> mdl.Term:
            chars = SYSTEM.string_to_list(value)
            return mdl.If(
                mdl.And(
                    mdl.Eq(SYSTEM.count_string(chars, mdl.String("@")), mdl.Int(1)),
                    mdl.Truth(SYSTEM.all_in_string(chars, allowed_email_chars)),
                    mdl.Truth(first_is_not_at(chars)),
                    mdl.Truth(ends_with_dot_com(chars)),
                ),
                mdl.Bool(True),
                mdl.Bool(False),
            )

        doc.allowed_email_chars = allowed_email_chars
        doc.starts_with_dot_com = starts_with_dot_com
        doc.ends_with_dot_com = ends_with_dot_com
        doc.first_is_not_at = first_is_not_at
        doc.validate = mdl.Function(
            {"value": mdl.STRING},
            mdl.BOOL,
            validate_email_body,
        )

    return doc.build()


MODULE = build()
EMAIL = MODULE
